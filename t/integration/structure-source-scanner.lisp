(in-package #:aitools.integration.structure-test)

;;; ------------------------------------------------------- text scanning

(defun %strip-prose (text)
  "TEXT with every `;` line comment, `#|...|#` block comment (nested), string
literal and the character of every `#\\x` character literal replaced by
spaces, newlines kept so line numbers stay valid. Handles `\\\"` and `\\\\`
inside strings."
  (let ((out (copy-seq text)) (n (length text)) (i 0))
    (flet ((blank (index)
             (unless (char= (char text index) #\Newline)
               (setf (char out index) #\Space))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond
                   ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\\))
                    (incf i 2)
                    (when (< i n) (blank i) (incf i)))
                   ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\|))
                    (let ((depth 0))
                      (loop while (< i n)
                            do (cond
                                 ((and (char= (char text i) #\#) (< (1+ i) n) (char= (char text (1+ i)) #\|))
                                  (incf depth) (blank i) (blank (1+ i)) (incf i 2))
                                 ((and (char= (char text i) #\|) (< (1+ i) n) (char= (char text (1+ i)) #\#))
                                  (decf depth) (blank i) (blank (1+ i)) (incf i 2)
                                  (when (zerop depth) (return)))
                                 (t (blank i) (incf i))))))
                   ((char= c #\")
                    (blank i) (incf i)
                    (loop while (< i n)
                          do (let ((d (char text i)))
                               (blank i) (incf i)
                               (cond ((char= d #\\) (when (< i n) (blank i) (incf i)))
                                     ((char= d #\") (return))))))
                   ((char= c #\;)
                    (loop while (and (< i n) (not (char= (char text i) #\Newline)))
                          do (blank i) (incf i)))
                   (t (incf i))))))
    out))
(defun %symbol-constituent-p (char)
  (or (alphanumericp char) (find char ".+-*/<>=!?_%&$")))
(defun %line-at (text position)
  (1+ (count #\Newline text :end position)))
(defun %qualified-references (text)
  "Every package-qualified symbol in TEXT (already stripped) as a list
(PACKAGE SYMBOL INTERNAL-P LINE), names upcased."
  (let ((references '()) (n (length text)))
    (loop for i from 0 below n
          when (and (char= (char text i) #\:)
                    (plusp i) (%symbol-constituent-p (char text (1- i))))
            do (let ((start i))
                 (loop while (and (plusp start) (%symbol-constituent-p (char text (1- start))))
                       do (decf start))
                 (unless (and (plusp start) (find (char text (1- start)) "#:"))
                   (let* ((internal (and (< (1+ i) n) (char= (char text (1+ i)) #\:)))
                          (name-start (if internal (+ i 2) (1+ i)))
                          (name-end (or (position-if-not #'%symbol-constituent-p text :start name-start) n)))
                     (push (list (string-upcase (subseq text start i))
                                 (string-upcase (subseq text name-start name-end))
                                 internal
                                 (%line-at text start))
                           references)))))
    (nreverse references)))
(defvar *reader-package*
  (or (find-package "AITOOLS.STRUCTURE-TEST.READER")
      (make-package "AITOOLS.STRUCTURE-TEST.READER" :use '())))
(defun %read-form-at (text position)
  "The form in TEXT at POSITION, read without evaluation into a scratch
package, or :UNREADABLE."
  (handler-case
      (with-standard-io-syntax
        (let ((*read-eval* nil) (*package* *reader-package*))
          (read-from-string text t nil :start position)))
    (error () :unreadable)))
(defun %form-positions (stripped operator)
  "Start positions of every `(OPERATOR ` form in STRIPPED."
  (let ((needle (format nil "(~A" operator)) (positions '()))
    (loop for start = (search needle stripped :test #'char-equal) then (search needle stripped :start2 (1+ start) :test #'char-equal)
          while start
          do (let ((after (+ start (length needle))))
               (when (and (< after (length stripped)) (not (%symbol-constituent-p (char stripped after))))
                 (push start positions))))
    (nreverse positions)))
(defun %designator-name (designator)
  (and (or (symbolp designator) (stringp designator))
       (string-upcase (string designator))))
(defun %defpackage-dependencies (form)
  "(values dependencies nicknames defined-name) of a DEFPACKAGE FORM.
DEPENDENCIES is a list of (CLAUSE PACKAGE SYMBOLS), SYMBOLS being the
imported names or NIL for :USE and :LOCAL-NICKNAMES; NICKNAMES is an alist of
(NICKNAME . PACKAGE)."
  (let ((dependencies '()) (nicknames '()))
    (dolist (clause (cddr form))
      (when (consp clause)
        (let ((key (first clause)))
          (cond
            ((eq key :use)
             (dolist (designator (rest clause))
               (push (list :use (%designator-name designator) nil) dependencies)))
            ((member key '(:import-from :shadowing-import-from))
             (push (list key (%designator-name (second clause)) (mapcar #'%designator-name (cddr clause)))
                   dependencies))
            ((eq key :local-nicknames)
             (dolist (pair (rest clause))
               (when (consp pair)
                 (push (cons (%designator-name (first pair)) (%designator-name (second pair))) nicknames)
                 (push (list :local-nicknames (%designator-name (second pair)) nil) dependencies))))))))
    (values (nreverse dependencies) nicknames (%designator-name (second form)))))
(defun %context-layer-from-pathname (pathname)
  "(VALUES CONTEXT LAYER CORE-OR-FEATURE), derived from a pathname matching
packages/<core|feature>/<context>/src/<layer>/*.lisp, or NIL if PATHNAME
does not match that shape. CONTEXT and LAYER are strings; CORE-OR-FEATURE is
the keyword :CORE or :FEATURE."
  (let ((parts (pathname-directory pathname)))
    (loop for (a b c d e) on parts
          when (and (equal a "packages") (member b '("core" "feature") :test #'string=)
                    (equal d "src"))
            return (values c e (if (string= b "core") :core :feature)))))
(defun scan-file (pathname relative context layer core-or-feature nicknames)
  "Violation strings for one context source file. NICKNAMES is the alist of
(NICKNAME . PACKAGE) local nicknames its package declares."
  (let* ((text (uiop:read-file-string pathname))
         (stripped (%strip-prose text))
         (own (context-package-name context layer))
         (allowed (allowed-package-names context layer core-or-feature))
         (violations '())
         (defines-own nil))
    (labels ((violation (line control &rest arguments)
               (push (format nil "~A:~D: ~?" relative line control arguments) violations))
             (allowed-p (package symbol)
               (or (member package allowed :test #'string=)
                   (and symbol (symbol-allowed-p context layer package symbol)))))
      (dolist (start (%form-positions stripped "defpackage"))
        (let ((form (%read-form-at text start)) (line (%line-at text start)))
          (if (eq form :unreadable)
              (violation line "unreadable defpackage form")
              (multiple-value-bind (dependencies declared-nicknames defined) (%defpackage-dependencies form)
                (declare (ignore declared-nicknames))
                (if (string= defined own)
                    (setf defines-own t)
                    (violation line "defines package ~A, but a file in ~A/~(~A~) must define ~A"
                               defined context layer own))
                (loop for (clause package symbols) in dependencies
                      do (if (and symbols (not (member package allowed :test #'string=)))
                             (dolist (symbol symbols)
                               (unless (allowed-p package symbol)
                                 (violation line "~(~S~) ~A:~A, which ~A/~(~A~) may not depend on"
                                            clause package symbol context layer)))
                             (unless (allowed-p package nil)
                               (violation line "~(~S~) ~A, which ~A/~(~A~) may not depend on"
                                          clause package context layer))))))))
      (dolist (start (%form-positions stripped "in-package"))
        (let ((form (%read-form-at text start)) (line (%line-at text start)))
          (let ((name (and (consp form) (%designator-name (second form)))))
            (unless (or (equal name own) (and (equal name "CL-USER") defines-own))
              (violation line "in-package ~A, but a file in ~A/~(~A~) must be in ~A" name context layer own)))))
      (loop for (prefix symbol internal line) in (%qualified-references stripped)
            for package = (or (cdr (assoc prefix nicknames :test #'string=)) prefix)
            do (cond
                 ((and internal (%aitools-context package)
                       (string/= (%aitools-context package) (string-upcase context)))
                  (violation line "~A::~A is an internal symbol of another context" package symbol))
                 ((not (allowed-p package symbol))
                  (violation line "~A:~A, which ~A/~(~A~) may not depend on" package symbol context layer)))))
    (nreverse violations)))
(defun scan-source-tree (root)
  "Violation strings, sorted, for every context source file under
ROOT/packages/**/src/**/*.lisp, each prefixed by its ROOT-relative path and
line."
  (let ((pathnames (directory (merge-pathnames "packages/**/src/**/*.lisp" root)))
        (nicknames (make-hash-table :test #'equal))
        (violations '()))
    ;; A package's local nicknames are declared in its package.lisp but used
    ;; in every other file of the package.
    (dolist (pathname pathnames)
      (let ((text (uiop:read-file-string pathname)))
        (dolist (start (%form-positions (%strip-prose text) "defpackage"))
          (let ((form (%read-form-at text start)))
            (unless (eq form :unreadable)
              (multiple-value-bind (dependencies declared defined) (%defpackage-dependencies form)
                (declare (ignore dependencies))
                (setf (gethash defined nicknames) (append declared (gethash defined nicknames)))))))))
    (dolist (pathname pathnames)
      (multiple-value-bind (context layer-name core-or-feature) (%context-layer-from-pathname pathname)
        (when (and context layer-name)
          (let ((layer (intern (string-upcase layer-name) :keyword)))
            (setf violations
                  (append violations
                          (scan-file pathname (enough-namestring pathname root) context layer core-or-feature
                                     (gethash (context-package-name context layer) nicknames))))))))
    (sort violations #'string<)))
