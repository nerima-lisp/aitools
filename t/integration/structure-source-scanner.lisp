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
  "Every package-qualified symbol as (PACKAGE NAME INTERNAL-P LINE START NAME-START END)."
  (let ((length (length text))
        (references nil))
    (labels ((token-start-p (char)
               (or (%symbol-constituent-p char)
                   (char= char #\|)
                   (char= char #\\)))
             (token-end (start)
               (let ((position start)
                     (bars nil)
                     (escaped nil))
                 (loop while (< position length)
                       for char = (char text position)
                       do (cond
                            (escaped
                             (setf escaped nil)
                             (incf position))
                            ((char= char #\\)
                             (setf escaped t)
                             (incf position))
                            ((char= char #\|)
                             (setf bars (not bars))
                             (incf position))
                            ((and (not bars)
                                  (find char '(#\Space #\Tab #\Newline #\( #\)
                                               #\' #\" #\` #\, #\;)))
                             (return))
                            (t (incf position))))
                 position))
             (separator (start end)
               (let ((position start)
                     (bars nil)
                     (escaped nil))
                 (loop while (< position end)
                       for char = (char text position)
                       do (cond
                            (escaped
                             (setf escaped nil)
                             (incf position))
                            ((char= char #\\)
                             (setf escaped t)
                             (incf position))
                            ((char= char #\|)
                             (setf bars (not bars))
                             (incf position))
                            ((and (not bars) (char= char #\:))
                             (return position))
                            (t (incf position)))
                       finally (return nil))))
             (decode-name (start end)
               (with-output-to-string (output)
                 (let ((position start)
                       (bars nil)
                       (escaped nil))
                   (loop while (< position end)
                         for char = (char text position)
                         do (cond
                              (escaped
                               (write-char char output)
                               (setf escaped nil))
                              ((char= char #\\) (setf escaped t))
                              ((char= char #\|) (setf bars (not bars)))
                              (t (write-char (if bars char (char-upcase char)) output)))
                            (incf position))))))
      (let ((position 0))
        (loop while (< position length)
              do (if (token-start-p (char text position))
                     (let* ((end (token-end position))
                            (colon (separator position end)))
                       (when colon
                         (let* ((package (decode-name position colon))
                                (internal (and (< (1+ colon) end)
                                               (char= (char text (1+ colon)) #\:)))
                                (name-start (if internal (+ colon 2) (1+ colon)))
                                (name (decode-name name-start end)))
                           (when (and (plusp (length package))
                                      (plusp (length name))
                                      (not (string= package "#")))
                             (push (list package name internal (%line-at text position)
                                         position name-start end)
                                   references))))
                       (setf position (max (1+ position) end)))
                     (incf position))))
      (nreverse references))))
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
(defvar *liveness-reader-view-cache* (make-hash-table :test #'eq))

(defun %liveness-read-form-at (text position)
  "Read one form with occurrence metadata for qualified symbols and FIND-SYMBOL calls."
  (let* ((cache
           (or (gethash text *liveness-reader-view-cache*)
               (setf (gethash text *liveness-reader-view-cache*)
                     (%liveness-reader-view-cache-entry text))))
         (view (first cache))
         (replacements (second cache))
         (offset
           (+ position
              (loop for replacement in replacements
                    for start = (first replacement)
                    for end = (second replacement)
                    for token = (third replacement)
                    when (<= end position)
                      sum (- (+ 2 (length token)) (- end start)))))
         (origins (make-hash-table :test #'equal))
         (source-positions (make-hash-table :test #'equal))
         (source-names (make-hash-table :test #'equal)))
    (dolist (replacement replacements)
      (setf (gethash (third replacement) origins) (fourth replacement)
            (gethash (third replacement) source-positions) (first replacement)
            (gethash (third replacement) source-names) (fifth replacement)))
    (handler-case
        (with-standard-io-syntax
          (let ((*read-eval* nil)
                (*package* *reader-package*)
                (quasiquote-depth 0)
                (readtable (copy-readtable nil)))
            (labels ((read-quasiquote (stream character)
                       (declare (ignore character))
                       (incf quasiquote-depth)
                       (unwind-protect
                            (list :u05-quasiquote (read stream t nil t))
                         (decf quasiquote-depth)))
                     (read-unquote (stream character)
                       (declare (ignore character))
                       (when (zerop quasiquote-depth)
                         (error "Comma outside a quasiquote."))
                       (let ((splicing-p
                               (member (peek-char nil stream nil nil)
                                       '(#\@ #\.))))
                         (when splicing-p (read-char stream))
                         (decf quasiquote-depth)
                         (unwind-protect
                              (list (if splicing-p
                                        :u05-unquote-splicing
                                        :u05-unquote)
                                    (read stream t nil t))
                           (incf quasiquote-depth)))))
              (let ((*readtable* readtable))
                (set-macro-character #\` #'read-quasiquote nil)
                (set-macro-character #\, #'read-unquote nil)
                (let ((form (read-from-string view t nil :start offset))
                      (seen (make-hash-table :test #'eq)))
                  (labels ((annotate (object)
                             (cond
                               ((symbolp object)
                                (let* ((name (symbol-name object))
                                       (origin (gethash name origins))
                                       (source-name (gethash name source-names)))
                                  (when (or origin source-name)
                                    (setf (get object :u05-source-position)
                                          (gethash name source-positions))
                                    (when origin
                                      (setf (get object :u05-qualified-reference) origin))
                                    (when source-name
                                      (setf (get object :u05-source-name) source-name)))))
                               ((consp object)
                                (unless (gethash object seen)
                                  (setf (gethash object seen) t)
                                  (annotate (car object))
                                  (annotate (cdr object)))))))
                    (annotate form))
                  form)))))
      (error () :unreadable)))
)

(defun %liveness-reader-view (text)
  "Return reader text and unique metadata for qualified symbols and call occurrences."
  (let* ((code (%strip-prose text))
         (references (%qualified-references code))
         (operators (%form-positions code "find-symbol"))
         (prefix (loop for index from 0
                       for candidate = (format nil "U05Q-~D-" index)
                       unless (search candidate text :test #'char-equal)
                         return candidate))
         (descriptors
           (append
            (loop for reference in references
                  collect (list (fifth reference) (seventh reference)
                                (list (first reference) (second reference)) nil))
            (loop for position in operators
                  for start = (1+ position)
                  collect (list start (+ start (length "FIND-SYMBOL"))
                                nil "FIND-SYMBOL"))))
         (replacements
           (loop for descriptor in (sort descriptors #'< :key #'first)
                 for serial from 1
                 collect (list (first descriptor) (second descriptor)
                               (format nil "~A~D" prefix serial)
                               (third descriptor) (fourth descriptor)))))
    (values
     (with-output-to-string (output)
       (loop with cursor = 0
             for replacement in replacements
             for start = (first replacement)
             for end = (second replacement)
             for token = (third replacement)
             do (write-string (subseq text cursor start) output)
                (write-string "#:" output)
                (write-string token output)
                (setf cursor end)
             finally (write-string (subseq text cursor) output)))
     replacements)))

(defun %liveness-reader-view-cache-entry (text)
  (multiple-value-bind (view replacements)
      (%liveness-reader-view text)
    (list view replacements)))
(defun %form-positions (stripped operator)
  "Start positions of every operator form or exact keyword token in STRIPPED."
  (let* ((keyword-p (and (plusp (length operator))
                         (char= (char operator 0) #\:)))
         (needle (if keyword-p operator (format nil "(~A" operator)))
         (positions nil))
    (loop for start = (search needle stripped :test (function char-equal))
          then (search needle stripped :start2 (+ start (length needle))
                       :test (function char-equal))
          while start
          do (let ((after (+ start (length needle))))
               (when (and (or (not keyword-p)
                              (zerop start)
                              (not (%symbol-constituent-p
                                    (char stripped (1- start)))))
                          (or (= after (length stripped))
                              (not (%symbol-constituent-p (char stripped after)))))
                 (push start positions))))
    (nreverse positions)))
(defun %liveness-form-positions (stripped operator)
  (let ((positions (%form-positions stripped operator)))
    (labels ((escaped-head (start)
               (let ((position (1+ start))
                     (length (length stripped))
                     (bars nil)
                     (escaped nil)
                     (qualified nil)
                     (had-escape nil))
                 (loop while (and (< position length)
                                  (find (char stripped position)
                                        '(#\Space #\Tab #\Newline #\Return #\Page)))
                       do (incf position))
                 (values
                  (with-output-to-string (output)
                    (loop while (< position length)
                          for char = (char stripped position)
                          do (cond
                               (escaped
                                (write-char char output)
                                (setf escaped nil)
                                (incf position))
                               ((char= char #\\)
                                (setf escaped t
                                      had-escape t)
                                (incf position))
                               ((char= char #\|)
                                (setf bars (not bars)
                                      had-escape t)
                                (incf position))
                               ((and (not bars) (char= char #\:))
                                (setf qualified t)
                                (return))
                               ((and (not bars)
                                     (member char
                                             '(#\Space #\Tab #\Newline #\Return #\Page
                                               #\( #\) #\' #\" #\` #\, #\; #\#)))
                                (return))
                               (t
                                (write-char (if bars char (char-upcase char)) output)
                                (incf position)))))
                  qualified
                  had-escape))))
      (loop for start = (position #\( stripped)
            then (position #\( stripped :start (1+ start))
            while start
            do (multiple-value-bind (head qualified escaped)
                   (escaped-head start)
                 (when (and escaped
                            (not qualified)
                            (string= head operator))
                   (push start positions))))
      (dolist (reference (%qualified-references stripped))
        (when (string-equal (second reference) operator)
          (let ((start (position #\( stripped :end (fifth reference) :from-end t)))
            (when start (push start positions)))))
      (sort (remove-duplicates positions :test (function =)) (function <)))))
(defun %designator-name (designator)
  (cond
    ((stringp designator) (string-upcase designator))
    ((symbolp designator) (%liveness-symbol-name designator))))
(defun %liveness-symbol-name (object)
  (when (symbolp object)
    (or (second (get object :u05-qualified-reference))
        (get object :u05-source-name)
        (symbol-name object))))
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
      (loop for (prefix symbol internal line position) in (%qualified-references stripped)
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

(defun %liveness-top-level-forms (stripped)
  "Positions of outermost list forms in STRIPPED, which has prose blanked."
  (let ((depth 0) (positions '()))
    (loop for position from 0 below (length stripped)
          for character = (char stripped position)
          do (cond
               ((char= character #\()
                (when (zerop depth) (push position positions))
                (incf depth))
               ((char= character #\))
                (decf depth))))
    (nreverse positions)))

(defun %liveness-file-package (text)
  "Package named by the first top-level IN-PACKAGE or DEFPACKAGE form."
  (loop for position in (%liveness-top-level-forms (%strip-prose text))
        for form = (%read-form-at text position)
        when (and (consp form)
                  (member (string-upcase (symbol-name (first form)))
                          '("IN-PACKAGE" "DEFPACKAGE") :test #'string=))
          return (%designator-name (second form))))

(defun %liveness-files (root)
  "Source, data, test, and ASDF records for ROOT."
  (let ((records '())
        (patterns '("packages/**/src/**/*.lisp" "src/**/*.lisp"
                    "data/**/*.lisp" "t/**/*.lisp")))
    (dolist (pattern patterns)
      (dolist (pathname (directory (merge-pathnames pattern root)))
        (unless (member pathname records :key (lambda (record) (getf record :pathname))
                        :test #'equal)
          (let* ((relative (enough-namestring pathname root))
                 (text (uiop:read-file-string pathname))
                 (kind (cond
                         ((and (>= (length relative) 2)
                               (string= relative "t/" :end1 2))
                          :test)
                         ((and (>= (length relative) 5)
                               (string= relative "data/" :end1 5))
                          :data)
                         (t :production))))
            (push (list :pathname pathname :relative relative :text text
                        :code (%strip-prose text) :package (%liveness-file-package text)
                        :kind kind :ignored nil :tokens nil)
                  records)))))
    (let ((pathname (merge-pathnames "aitools.asd" root)))
      (when (probe-file pathname)
        (let ((text (uiop:read-file-string pathname)))
          (push (list :pathname pathname :relative "aitools.asd" :text text
                      :code (%strip-prose text) :package nil :kind :production
                      :ignored nil :tokens nil)
                records))))
    (sort records #'string< :key (lambda (record) (getf record :relative)))))

(defun %liveness-package-metadata (records)
  "Package exports, uses, imports, shadows, and source-local nicknames."
  (let ((packages (make-hash-table :test #'equal)))
    (dolist (record records)
      (when (eq (getf record :kind) :production)
        (let* ((text (getf record :text))
               (code (getf record :code))
               (starts (%liveness-top-level-forms code))
               (ends (append (rest starts) (list (length code)))))
          (loop for position in starts
                for end in ends
                for form = (%read-form-at text position)
                do (when (and (consp form)
                              (string= (symbol-name (first form)) "DEFPACKAGE"))
                     (let* ((name (%designator-name (second form)))
                            (exports '()) (uses '()) (imports '()) (shadows '())
                            (nicknames '())
                            (line (%line-at text position))
                            (form-code (subseq code position end))
                            (use-lines
                              (mapcar (lambda (offset)
                                        (%line-at text (+ position offset)))
                                      (%form-positions form-code ":use")))
                            (import-lines
                              (sort
                               (append
                                (mapcar (lambda (offset)
                                          (%line-at text (+ position offset)))
                                        (%form-positions form-code ":import-from"))
                                (mapcar (lambda (offset)
                                          (%line-at text (+ position offset)))
                                        (%form-positions form-code
                                                         ":shadowing-import-from")))
                               #'<))
                            (uses-declared-p
                              (some (lambda (clause)
                                      (and (consp clause) (eq (first clause) :use)))
                                    (cddr form))))
                       (dolist (clause (cddr form))
                         (when (consp clause)
                           (case (first clause)
                             (:export
                              (dolist (symbol (rest clause))
                                (push (%designator-name symbol) exports)))
                             (:use
                              (let ((clause-line (or (pop use-lines) line)))
                                (dolist (package (rest clause))
                                  (push (list (%designator-name package) clause-line) uses))))
                             ((:import-from :shadowing-import-from)
                              (let ((clause-line (or (pop import-lines) line)))
                                (dolist (symbol (cddr clause))
                                  (push (list (%designator-name (second clause))
                                              (%designator-name symbol) clause-line
                                              (first clause))
                                        imports))))
                             (:shadow
                              (dolist (symbol (rest clause))
                                (push (%designator-name symbol) shadows)))
                             (:nicknames
                              (dolist (alias (rest clause))
                                (push (cons (%designator-name alias) name) nicknames)))
                             (:local-nicknames
                              (dolist (pair (rest clause))
                                (when (consp pair)
                                  (push (cons (%designator-name (first pair))
                                              (%designator-name (second pair)))
                                        nicknames)))))))
                       (setf (gethash name packages)
                             (list :name name :file (getf record :relative) :line line
                                   :exports (remove-duplicates exports :test #'string=)
                                   :uses uses :uses-declared-p uses-declared-p
                                   :imports imports :shadows shadows
                                   :nicknames nicknames))))))))
    packages))

(defun %liveness-symbol-tokens (text)
  "Unqualified symbol tokens in TEXT as (NAME POSITION), excluding package syntax."
  (let ((tokens '()) (length (length text)) (position 0))
    (loop while (< position length)
          do (if (%symbol-constituent-p (char text position))
                 (let ((start position))
                   (loop while (and (< position length)
                                    (%symbol-constituent-p (char text position)))
                         do (incf position))
                   (unless (or (and (plusp start) (char= (char text (1- start)) #\:))
                               (and (< position length)
                                    (char= (char text position) #\:)))
                     (push (list (string-upcase (subseq text start position)) start)
                           tokens)))
                 (incf position)))
    (nreverse tokens)))

(defun %liveness-slot-function-names (slots)
  "Explicit readers, writers, and accessors from class or condition slots."
  (let ((names '()))
    (dolist (slot slots)
      (when (consp slot)
        (loop for options on (rest slot) by #'cddr
              while (cdr options)
              when (member (first options) '(:reader :writer :accessor))
                do (push (%designator-name (second options)) names))))
    (nreverse names)))

(defun %liveness-struct-defined-names (form)
  "Generated type and function names from a DEFSTRUCT form."
  (labels ((optional-name (object)
             (let ((name (%designator-name object)))
               (unless (equal name "NIL") name))))
    (let* ((spec (second form))
           (name (%designator-name (if (consp spec) (first spec) spec)))
           (options (if (consp spec) (rest spec) nil))
           (predicate (assoc :predicate options))
           (constructor-options
             (remove-if-not (lambda (option) (eq (first option) :constructor)) options))
           (copier (assoc :copier options))
           (conc-name (assoc :conc-name options))
           (prefix (if conc-name
                       (optional-name (second conc-name))
                       (and name (concatenate (quote string) name "-"))))
           (slots (if (stringp (third form))
                      (cdddr form)
                      (cddr form))))
      (when name
        (append
         (list (list :type name))
         (let ((predicate-name (if predicate
                                   (optional-name (second predicate))
                                   (concatenate (quote string) name "-P"))))
           (when predicate-name
             (list (list :function predicate-name))))
         (if constructor-options
             (loop for option in constructor-options
                   for constructor-name = (optional-name (second option))
                   when constructor-name
                     collect (list :function constructor-name))
             (list (list :function (concatenate (quote string) "MAKE-" name))))
         (let ((copier-name (if copier
                                (optional-name (second copier))
                                (concatenate (quote string) "COPY-" name))))
           (when copier-name
             (list (list :function copier-name))))
         (when prefix
           (loop for slot in slots
                 for slot-name = (%designator-name (if (consp slot) (first slot) slot))
                 when slot-name
                   collect (list :function (concatenate (quote string) prefix slot-name)))))))))

(defun %liveness-form-definitions (form)
  "Names defined by a supported top-level form as (NAMESPACE NAME ROOT-MARKER)."
  (let* ((operator (and (consp form) (%liveness-symbol-name (first form))))
         (name (and (consp form) (second form)))
         (operator operator)
         (simple-name (%designator-name name)))
    (cond
      ((member operator '("DEFUN" "DEFMACRO" "DEFGENERIC") :test #'string=)
       (list (list :function simple-name nil)))
      ((member operator '("DEFVAR" "DEFPARAMETER" "DEFCONSTANT") :test #'string=)
       (list (list :value simple-name nil)))
      ((equal operator "DEFINE-FLOW")
       (list (list :function (%designator-name (first name)) nil)))
      ((equal operator "DEFINE-WRITE-COMMAND")
       (list (list :function
                   (format nil "PREPARE-~A/K"
                           (substitute #\- #\. (string-upcase name)))
                   (list :write-command name))))
      ((equal operator "DEFINE-FORMAT-WRITE-COMMANDS")
       (when (stringp name)
         (loop for verb in '("set" "delete" "merge" "patch" "fmt")
               for command = (format nil "~A.~A" (string-downcase name) verb)
               collect (list :function
                             (format nil "PREPARE-~A/K"
                                     (substitute #\- #\. (string-upcase command)))
                             (list :format-write-command command)))))
      ((equal operator "DEFINE-DATA-TABLE")
       (list (list :value simple-name nil)))
      ((equal operator "DEFSTRUCT")
       (let* ((names (%liveness-struct-defined-names form))
              (struct-name (second (first names))))
         (loop for (namespace symbol) in names
               when symbol
                 collect (list namespace symbol (list :generated-struct struct-name)))))
      ((member operator '("DEFCLASS" "DEFINE-CONDITION") :test #'string=)
       (append (list (list :type simple-name nil))
               (loop for symbol in (%liveness-slot-function-names (fourth form))
                     collect (list :function symbol nil))))
      (t nil))))
(defun %liveness-data-function-designators (record form code)
  "Resolve each function designator from the same :CALC-FUNCTIONS entry."
  (declare (ignore code))
  (when (and (consp form)
             (string= (%liveness-symbol-name (first form)) "DEFINE-DATA-TABLE")
             (eq (third form) :calc-functions))
    (loop for entry in (fourth form)
          for designator = (third entry)
          for origin = (and (symbolp designator)
                            (get designator :u05-qualified-reference))
          for home = (and (symbolp designator) (symbol-package designator))
          when (and (symbolp designator)
                    (not (keywordp designator))
                    (or origin home))
            collect
              (or origin
                  (if (eq home *reader-package*)
                      (list (getf record :package)
                            (%liveness-symbol-name designator))
                      (list (package-name home)
                            (%liveness-symbol-name designator)))))))

(defun %liveness-definitions (records)
  "Definitions from production files and the roots registered for dynamic dispatch."
  (let ((definitions nil))
    (labels ((command-data-row-p (form command)
                                 (or (and (consp form)
                                          (eq (first form) :name)
                                          (stringp (second form))
                                          (string-equal (second form) command))
                                     (and (consp form)
                                          (some (lambda (part)
                                                  (command-data-row-p part command))
                                                form))))
             (preparer-dispatch-p (form)
                                  (or (and (consp form)
                                           (equal (%liveness-symbol-name (first form)) "GETHASH")
                                           (symbolp (second form))
                                           (equal (%liveness-symbol-name (second form)) "NAME")
                                           (symbolp (third form))
                                           (equal (%liveness-symbol-name (third form)) "*PREPARERS*"))
                                      (and (consp form)
                                           (some (lambda (part) (preparer-dispatch-p part)) form))))
             (command-data-present-p (command)
                                     (loop for record in records
                                           thereis
                                           (and (string= (getf record :relative)
                                                         "data/application/edit/command-spec-data.lisp")
                                                (loop for position in
                                                      (%liveness-top-level-forms (getf record :code))
                                                      for form = (%liveness-read-form-at (getf record :text) position)
                                                      thereis (command-data-row-p form command)))))
             (preparer-dispatch-present-p ()
                                          (loop for record in records
                                                thereis
                                                (and (string= (getf record :relative)
                                                              "packages/feature/edit/src/application/commands.lisp")
                                                     (loop for position in
                                                           (%liveness-top-level-forms (getf record :code))
                                                           for form =
                                                           (%liveness-read-form-at (getf record :text) position)
                                                           thereis (preparer-dispatch-p form)))))
             (data-function-root-p (package name)
                                   (loop for record in records
                                         thereis
                                         (and (eq (getf record :kind) :data)
                                              (let* ((code (getf record :code))
                                                     (text (getf record :text))
                                                     (starts (%liveness-top-level-forms code))
                                                     (ends (append (rest starts) (list (length code)))))
                                                (loop for start in starts
                                                      for end in ends
                                                      for form = (%liveness-read-form-at text start)
                                                      for form-code = (subseq code start end)
                                                      thereis
                                                      (member (list package name)
                                                              (%liveness-data-function-designators
                                                               record form form-code)
                                                              :test #'equal))))))
             (definition-root-p (package namespace name marker)
               (or (and (eq namespace :function) (data-function-root-p package name))
                   (%liveness-documented-api-root-p package namespace name)
                   (and (consp marker)
                        (eq (first marker) :write-command)
                        (string= package "AITOOLS.EDIT.APPLICATION")
                        (command-data-present-p (second marker))
                        (preparer-dispatch-present-p))
                   (and (consp marker)
                        (eq (first marker) :format-write-command)
                        (string= package "AITOOLS.EDIT.APPLICATION")
                        (command-data-present-p (second marker))
                        (preparer-dispatch-present-p)))))
      (dolist (record records)
        (when (eq (getf record :kind) :production)
          (let* ((text (getf record :text))
                 (package (getf record :package)))
            (dolist (start (%liveness-top-level-forms (getf record :code)))
              (let ((form (%liveness-read-form-at text start)))
                (dolist (entry (%liveness-form-definitions form))
                  (destructuring-bind (namespace name marker) entry
                    (when name
                      (push (list :name name :namespace namespace :package package
                            :file (getf record :relative)
                            :line (%line-at text start)
                            :root (not (null (definition-root-p package namespace name marker)))
                            :group (and (consp marker)
                                        (eq (first marker) :generated-struct)
                                        (list package (getf record :relative)
                                              start (second marker))))
                            definitions)))))))))
      (nreverse definitions))))

(defun %liveness-form-find-symbols (form record packages)
  "FIND-SYMBOL calls resolved to the Common Lisp function outside quoted data."
  (let ((calls nil))
    (labels ((name-of (object)
               (and (symbolp object)
                    (not (keywordp object))
                    (%liveness-symbol-name object)))
             (common-lisp-package-p (package)
               (member package '("CL" "COMMON-LISP") :test #'string-equal))
             (source-unqualified-p (object)
               (or (get object :u05-source-name)
                   (eq (symbol-package object) *reader-package*)))
             (cl-find-symbol-p (object)
               (let ((origin (and (symbolp object)
                                  (get object :u05-qualified-reference))))
                 (and (equal (name-of object) "FIND-SYMBOL")
                      (or (and origin
                               (common-lisp-package-p (first origin))
                               (equal (second origin) "FIND-SYMBOL"))
                          (and (symbolp object)
                               (source-unqualified-p object)
                               (let* ((own (getf record :package))
                                      (info (and own (gethash own packages)))
                                      (imports (getf info :imports))
                                      (import (find "FIND-SYMBOL" imports
                                                    :key #'second :test #'string=)))
                                 (and info
                                      (or (and import
                                               (common-lisp-package-p (first import)))
                                          (and (null import)
                                               (not (member "FIND-SYMBOL"
                                                            (getf info :shadows)
                                                            :test #'string=))
                                               (or (member "CL" (getf info :uses)
                                                           :key #'first :test #'string-equal)
                                                   (member "COMMON-LISP" (getf info :uses)
                                                           :key #'first :test #'string-equal)
                                                   (not (getf info :uses-declared-p))))))))))))
             (lexically-bound-p (object functions)
               (some (lambda (binding)
                       (and (cl-find-symbol-p binding)
                            (equal (name-of binding) (name-of object))))
                     functions))
             (walk-sequence (forms functions)
               (dolist (form forms) (walk form functions)))
             (walk (object functions)
               (when (consp object)
                 (let ((head (first object))
                       (operator (name-of (first object))))
                   (unless (equal operator "QUOTE")
                     (when (and (cl-find-symbol-p head)
                                (not (lexically-bound-p head functions)))
                       (push (list (get head :u05-source-position) object) calls))
                     (if (member operator '("FLET" "LABELS") :test #'equal)
                         (let* ((definitions (second object))
                                (names (mapcar #'first definitions))
                                (local-functions
                                  (remove-if-not #'cl-find-symbol-p names))
                                (definition-functions
                                  (if (equal operator "LABELS")
                                      (append local-functions functions)
                                      functions)))
                           (dolist (definition definitions)
                             (walk-sequence (cddr definition) definition-functions))
                           (walk-sequence (cddr object)
                                          (append local-functions functions)))
                         (progn
                           (walk (car object) functions)
                           (walk (cdr object) functions))))))))
      (walk form nil)
      (nreverse calls))))

(defun %liveness-reference-p (form)
  "References in FORM as (NAMESPACE NAME PACKAGE), excluding local and quoted data."
  (let ((references '()))
    (labels ((name-of (object)
               (and (symbolp object) (not (keywordp object))
                    (%liveness-symbol-name object)))
             (package-of (object)
  (let ((origin (and (symbolp object)
                     (get object :u05-qualified-reference)))
        (source-name (and (symbolp object) (get object :u05-source-name)))
        (package (and (symbolp object) (symbol-package object))))
    (cond
      (origin (first origin))
      (source-name :local)
      ((null package) :uninterned)
      ((eq package *reader-package*) :local)
      (t (package-name package)))))
             (emit (namespace object)
               (let ((name (name-of object)))
                 (when (and name (not (member name '("NIL" "T") :test #'string=)))
                   (push (list namespace name (package-of object)) references))))
             (bound-p (object bindings)
               (and (eq (package-of object) :local)
                    (member (name-of object) bindings :test #'equal)))
             (walk-sequence (forms values functions)
               (loop for tail = forms then (cdr tail)
                     while (consp tail)
                     do (walk (car tail) values functions)
                     finally
                       (unless (null tail)
                         (walk tail values functions))))
             (lambda-vars (lambda-list values functions method-p)
  (let ((names '()) (mode :required))
    (when (listp lambda-list)
      (dolist (item lambda-list)
        (if (and (symbolp item)
                 (let ((name (name-of item)))
                   (and name (plusp (length name))
                        (char= (char name 0) #\&))))
            (setf mode (intern (subseq (name-of item) 1) :keyword))
            (let* ((variable (if (consp item)
                                 (if (consp (first item))
                                     (second (first item))
                                     (first item))
                                 item))
                   (name (name-of variable)))
              (when (and method-p (consp item) (second item))
                (walk-type (second item)))
              (when (and (not method-p) (consp item)
                         (member mode '(:optional :key :aux)))
                (walk (second item) (append values names) functions))
              (when name (pushnew name names :test #'equal))
              (when (and (consp item) (third item)
                         (member mode '(:optional :key)))
                (when (name-of (third item))
                  (pushnew (name-of (third item)) names :test #'equal)))))))
    (nreverse names)))
             (walk-type (object)
               (cond
                 ((symbolp object)
                  (let ((name (name-of object)))
                    (unless (or (keywordp object)
                                (equal name "*")
                                (and name (plusp (length name))
                                     (char= (char name 0) #\&)))
                      (emit :type object))))
                 ((consp object)
                  (let ((operator (name-of (first object))))
                    (cond
                      ((member operator (quote ("AND" "OR" "NOT")) :test (function equal))
                       (mapc (function walk-type) (rest object)))
                      ((equal operator "SATISFIES")
                       (emit :function (second object)))
                      ((member operator (quote ("MEMBER" "EQL" "MOD"))
                               :test (function equal))
                       nil)
                      ((member operator
                               (quote ("ARRAY" "SIMPLE-ARRAY" "VECTOR" "SIMPLE-VECTOR"))
                               :test (function equal))
                       (emit :type (first object))
                       (let ((element-type (second object)))
                         (unless (or (null element-type)
                                     (equal (name-of element-type) "*"))
                           (walk-type element-type))))
                      (t
                       (emit :type (first object))
                       (dolist (part (rest object))
                         (let ((name (name-of part)))
                           (unless (or (numberp part)
                                       (keywordp part)
                                       (equal name "*")
                                       (and name (plusp (length name))
                                            (char= (char name 0) #\&)))
                             (walk-type part))))))))))
             (quoted-type (object)
               (when (and (consp object)
                          (equal (name-of (first object)) "QUOTE")
                          (symbolp (second object)))
                 (emit :type (second object))))
             (walk-slot-options (slot values functions)
               (when (consp slot)
                 (loop for options on (rest slot) by #'cddr
                       while (cdr options)
                       do (case (first options)
                            (:initform (walk (second options) values functions))
                            (:type (walk-type (second options)))))))
             (walk-class (object values functions)
               (dolist (superclass (third object))
                 (walk-type superclass))
               (dolist (slot (fourth object))
                 (walk-slot-options slot values functions))
               (walk-sequence (nthcdr 4 object) values functions))
             (walk-struct (object values functions)
               (let* ((spec (second object))
                      (options (if (consp spec) (rest spec) nil))
                      (slots (if (stringp (third object))
                                 (cdddr object) (cddr object))))
                 (dolist (option options)
                   (when (consp option)
                     (cond
                       ((eq (first option) :include)
                        (walk-type (second option)))
                       ((and (eq (first option) :constructor) (third option))
                        (lambda-vars (third option) values functions nil)))))
                 (dolist (slot slots)
                   (walk-slot-options slot values functions))))
             (walk (object values functions)
               (cond
                 ((symbolp object)
                  (unless (or (keywordp object) (bound-p object values))
                    (emit :value object)))
                 ((and (consp object) (eq (first object) :u05-quasiquote))
 (labels ((quasiquote-template (template depth visible-p)
            (cond
              ((consp template)
               (let ((marker (first template)))
                 (cond
                   ((eq marker :u05-quasiquote)
                    (quasiquote-template (second template) (1+ depth) nil))
                   ((member marker '(:u05-unquote :u05-unquote-splicing))
                    (if (= depth 1)
                        (progn
                          (walk (second template) values functions)
                          nil)
                        (quasiquote-template (second template) (1- depth) nil)))
                   (visible-p
                    (cons (quasiquote-template (car template) depth t)
                          (quasiquote-template (cdr template) depth t)))
                   (t
                    (quasiquote-template (car template) depth nil)
                    (quasiquote-template (cdr template) depth nil)
                    nil))))
              (visible-p template)
              (t nil))))
   (walk (quasiquote-template (second object) 1 t) values functions)))
                 ((consp object)
                  (let* ((head (first object))
                         (operator (name-of head))
                         (args (rest object)))
                    (cond
                      ((member operator
                         (quote ("QUOTE" "DEFPACKAGE" "IN-PACKAGE"
                                 "DEFINE-DATA-TABLE" "DECLARE" "DECLAIM" "THE" "CHECK-TYPE"))
                         :test (function equal))
                       (cond
                         ((member operator (quote ("DECLARE" "DECLAIM"))
                                  :test (function equal))
                          (dolist (declaration args)
                            (when (and (consp declaration)
                                       (member (name-of (first declaration))
                                               (quote ("TYPE" "FTYPE"))
                                               :test (function equal)))
                              (walk-type (second declaration)))))
                         ((equal operator "THE")
                          (walk-type (second object))
                          (walk (third object) values functions))
                         ((equal operator "CHECK-TYPE")
                          (walk (second object) values functions)
                          (walk-type (third object)))))
                      ((equal operator "FUNCTION")
                       (let ((target (second object)))
                         (if (symbolp target)
                             (unless (bound-p target functions)
                               (emit :function target))
                             (walk target values functions))))
                      ((equal operator "LAMBDA")
                       (let ((locals (lambda-vars (second object) values functions nil)))
                         (walk-sequence (cddr object) (append values locals) functions)))
                      ((member operator '("DEFUN" "DEFMACRO" "DEFGENERIC")
                               :test #'equal)
                       (let* ((function-name (second object))
                              (locals (lambda-vars (third object) values functions nil)))
                         (walk-sequence (cdddr object) (append values locals)
                                        (cons (name-of function-name) functions))))
                      ((equal operator "DEFMETHOD")
                       (let* ((generic (second object))
                              (lambda-list (first (cddr object)))
                              (locals (lambda-vars lambda-list values functions t)))
                         (emit :function generic)
                         (walk-sequence (cdddr object) (append values locals) functions)))
                      ((member operator '("DEFCLASS" "DEFINE-CONDITION") :test #'equal)
                       (walk-class object values functions))
                      ((equal operator "DEFSTRUCT")
                       (walk-struct object values functions))
                      ((equal operator "DEFINE-FLOW")
                       (emit :function head)
                       (let* ((signature (second object))
                              (function-name (and (consp signature) (first signature)))
                              (locals (lambda-vars (rest signature) values functions nil)))
                         (walk-sequence (cddr object) (append values locals)
                                        (cons (name-of function-name) functions))))
                      ((equal operator "DEFINE-WRITE-COMMAND")
                       (emit :function head)
                       (let ((locals (lambda-vars (third object) values functions nil)))
                         (walk-sequence (cdddr object) (append values locals) functions)))
                      ((equal operator "DEFINE-INSPECT-COMMAND")
                       (emit :function head)
                       (let ((builder (fourth object)))
                         (when (and (consp builder)
                                    (equal (name-of (first builder)) "QUOTE"))
                           (emit :function (second builder)))
                         (walk-sequence args values functions)))
                      ((member operator '("LET" "LET*" "MULTIPLE-VALUE-BIND") :test #'equal)
                       (if (equal operator "MULTIPLE-VALUE-BIND")
                           (let ((locals (remove nil (mapcar #'name-of (second object)))))
                             (walk (third object) values functions)
                             (walk-sequence (cdddr object) (append values locals) functions))
                           (let ((locals '())
                                 (sequential (equal operator "LET*")))
                             (dolist (binding (second object))
                               (let ((variable
                                      (if (consp binding)
                                          (first binding)
                                          binding))
                                     (initial (and (consp binding) (second binding))))
                                 (when initial
                                   (walk initial
                                         (if sequential
                                             (append values locals)
                                             values)
                                         functions))
                                 (when (name-of variable)
                                   (pushnew (name-of variable) locals :test #'equal))))
                             (walk-sequence (cddr object) (append values locals) functions))))
                      ((member operator '("DOLIST" "DOTIMES" "DESTRUCTURING-BIND") :test #'equal)
 (if (equal operator "DESTRUCTURING-BIND")
     (let ((names '()) (initializers '()))
       (labels ((bind-pattern (pattern)
                  (cond
                    ((symbolp pattern)
                     (let ((name (name-of pattern)))
                       (when (and name
                                  (not (keywordp pattern))
                                  (not (member name
                                               '("&WHOLE" "&ENVIRONMENT" "&OPTIONAL"
                                                 "&REST" "&BODY" "&KEY" "&ALLOW-OTHER-KEYS"
                                                 "&AUX")
                                               :test #'equal)))
                         (pushnew name names :test #'equal))))
                    ((consp pattern) (parse-pattern pattern))))
                (parse-pattern (lambda-list)
                  (let ((mode :required))
                    (loop while (consp lambda-list)
                          for item = (pop lambda-list)
                          for name = (name-of item)
                          do (cond
                               ((member name '("&WHOLE" "&ENVIRONMENT") :test #'equal)
                                (setf mode :whole-next))
                               ((equal name "&OPTIONAL") (setf mode :optional))
                               ((member name '("&REST" "&BODY") :test #'equal)
                                (setf mode :rest-next))
                               ((equal name "&KEY") (setf mode :key))
                               ((equal name "&AUX") (setf mode :aux))
                               ((equal name "&ALLOW-OTHER-KEYS") nil)
                               ((member mode '(:whole-next :rest-next))
                                (bind-pattern item)
                                (setf mode :required))
                               ((member mode '(:optional :key :aux))
                                (let* ((designator (and (consp item) (first item)))
                                       (variable (if (and (eq mode :key)
                                                          (consp designator))
                                                     (second designator)
                                                     designator))
                                       (initial (and (consp item) (second item)))
                                       (supplied (and (consp item) (third item))))
                                  (when initial
                                    (push (cons initial (copy-list names)) initializers))
                                  (bind-pattern (or variable item))
                                  (when supplied (bind-pattern supplied))))
                               (t (bind-pattern item))))
                    (when lambda-list (bind-pattern lambda-list)))))
         (parse-pattern (second object))
         (walk (third object) values functions)
         (dolist (initializer (nreverse initializers))
           (walk (car initializer) (append values (cdr initializer)) functions))
         (walk-sequence (cdddr object) (append values (nreverse names)) functions)))
     (let* ((binding (second object))
            (variable (first binding))
            (locals (and (name-of variable) (list (name-of variable)))))
       (walk (second binding) values functions)
       (walk-sequence (cddr object) (append values locals) functions))))
                      ((member operator '("FLET" "LABELS") :test #'equal)
                       (let* ((definitions (second object))
                              (names (mapcar (lambda (definition)
                                               (name-of (first definition)))
                                             definitions))
                              (local-functions (if (equal operator "LABELS")
                                                   (append names functions)
                                                   functions)))
                         (dolist (definition definitions)
                           (let ((locals (lambda-vars (second definition) values
                                                     local-functions nil)))
                             (walk-sequence (cddr definition) (append values locals)
                                            local-functions)))
                         (walk-sequence (cddr object) values
                                        (append names functions))))
                      ((equal operator "LOOP")
                       (if (and args (consp (first args)))
                           (walk-sequence args values functions)
                           (let ((locals '()) (expressions '()))
                             (labels ((variable-names (pattern)
                                        (cond
                                          ((symbolp pattern)
                                           (let ((name (name-of pattern)))
                                             (when (and name
                                                        (not (member name '("NIL" "T") :test #'equal))
                                                        (or (zerop (length name))
                                                            (char/= (char name 0) #\&)))
                                               (list name))))
                                          ((consp pattern)
                                           (append (variable-names (car pattern))
                                                   (variable-names (cdr pattern))))
                                          (t nil)))
                                      (scan-clauses (clauses)
                                        (loop for tail on clauses
                                              for marker = (name-of (first tail))
                                              do (cond
                                                   ((member marker '("FOR" "AS" "WITH" "INTO")
                                                            :test #'equal)
                                                    (setf locals
                                                          (append (variable-names (second tail))
                                                                  locals)))
                                                   ((member marker
                                                            '("IN" "ON" "ACROSS" "=" "THEN" "FROM"
                                                              "UPFROM" "DOWNFROM" "TO" "UPTO" "DOWNTO"
                                                              "ABOVE" "BELOW" "BY" "REPEAT" "WHILE"
                                                              "UNTIL" "ALWAYS" "NEVER" "THEREIS" "WHEN"
                                                              "UNLESS" "IF" "ELSEIF" "ELSE" "COLLECT"
                                                              "COLLECTING" "APPEND" "APPENDING" "NCONC"
                                                              "NCONCING" "COUNT" "COUNTING" "SUM" "SUMMING"
                                                              "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING"
                                                              "DO" "DOING" "INITIALLY" "FINALLY" "RETURN"
                                                              "RETURNING" "OF")
                                                            :test #'equal)
                                                    (when (cdr tail)
                                                      (push (second tail) expressions)
                                                      (when (member marker '("DO" "DOING") :test #'equal)
                                                        (loop for body-tail on (cddr tail)
                                                              for body-form = (first body-tail)
                                                              while (and (consp body-tail)
                                                                         (not (member (name-of body-form)
                                                                                      '("FOR" "AS" "WITH" "INTO" "IN" "ON"
                                                                                        "ACROSS" "=" "THEN" "FROM" "UPFROM"
                                                                                        "DOWNFROM" "TO" "UPTO" "DOWNTO" "ABOVE"
                                                                                        "BELOW" "BY" "REPEAT" "WHILE" "UNTIL"
                                                                                        "ALWAYS" "NEVER" "THEREIS" "WHEN" "UNLESS"
                                                                                        "IF" "ELSEIF" "ELSE" "COLLECT" "COLLECTING"
                                                                                        "APPEND" "APPENDING" "NCONC" "NCONCING"
                                                                                        "COUNT" "COUNTING" "SUM" "SUMMING"
                                                                                        "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING"
                                                                                        "DO" "DOING" "INITIALLY" "FINALLY" "RETURN"
                                                                                        "RETURNING" "OF" "AND" "END")
                                                                                      :test #'equal)))
                                                              do (push body-form expressions)))))))))
                               (scan-clauses args)
                               (walk-sequence (nreverse expressions)
                                              (append values locals)
                                              functions)))))
                      ((member operator '("DEFVAR" "DEFPARAMETER" "DEFCONSTANT")
                               :test #'equal)
                       (walk-sequence (cddr object) values functions))
                      ((member operator '("MAKE-INSTANCE" "MAKE-CONDITION"
                                          "FIND-CLASS" "CHANGE-CLASS") :test #'equal)
                       (emit :function head)
                       (let ((class (if (equal operator "CHANGE-CLASS")
                                        (third object) (second object))))
                         (if (and (consp class)
                                  (equal (name-of (first class)) "QUOTE"))
                             (emit :type (second class))
                             (when (and (symbolp class) (not (keywordp class)))
                               (emit :type class)))
                         (walk-sequence (if (equal operator "CHANGE-CLASS")
                                            (cdddr object) (cddr object))
                                        values functions)))
                      ((member operator '("TYPEP" "SUBTYPEP") :test #'equal)
                       (emit :function head)
                       (walk (second object) values functions)
                       (if (and (consp (third object))
                                (equal (name-of (first (third object))) "QUOTE"))
                           (emit :type (second (third object)))
                           (walk-type (third object)))
                       (walk-sequence (cdddr object) values functions))
                      ((member operator '("TYPECASE" "ETYPECASE" "CTYPECASE")
                               :test #'equal)
                       (emit :function head)
                       (walk (second object) values functions)
                       (dolist (clause (cddr object))
                         (walk-type (first clause))
                         (walk-sequence (rest clause) values functions)))
                      ((equal operator "HANDLER-CASE")
                       (walk (second object) values functions)
                       (dolist (clause (cddr object))
                         (when (consp clause)
                           (walk-type (first clause))
                           (let* ((lambda-list (second clause))
                                  (variable (and (consp lambda-list)
                                                 (first lambda-list)))
                                  (local (and variable (name-of variable))))
                             (walk-sequence (cddr clause)
                                            (if local (cons local values) values)
                                            functions)))))
                      ((member operator '("ERROR" "CERROR" "SIGNAL" "WARN")
                               :test #'equal)
                       (emit :function head)
                       (when (and (consp (second object))
                                  (equal (name-of (first (second object))) "QUOTE"))
                         (emit :type (second (second object))))
                       (walk-sequence (cddr object) values functions))
                      ((equal operator "FIND-SYMBOL") (unless (bound-p head functions) (emit :function head)))
                      ((equal operator "SYMBOL-FUNCTION")
                       (let ((designator (second object)))
                         (when (and (consp designator)
                                    (equal (name-of (first designator)) "QUOTE"))
                           (emit :function (second designator)))
                         (walk-sequence (cddr object) values functions)))
                      ((equal operator "SYMBOL-VALUE")
                       (let ((designator (second object)))
                         (when (and (consp designator)
                                    (equal (name-of (first designator)) "QUOTE"))
                           (emit :value (second designator)))
                         (walk-sequence (cddr object) values functions)))
                      (t
                       (if (equal operator "COND")
                           (progn
                             (emit :function head)
                             (dolist (clause args)
                               (when (consp clause)
                                 (walk (first clause) values functions)
                                 (walk-sequence (rest clause) values functions))))
                           (progn
                             (when (and (symbolp head) (not (keywordp head))
                                        (not (bound-p head functions)))
                               (emit :function head))
                             (if (equal operator "WITH-LOCK-HELD")
                                 (progn
                                   (walk-sequence (second object) values functions)
                                   (walk-sequence (cddr object) values functions))
                                 (walk-sequence args values functions)))))))))))
      (walk form nil nil)
      (nreverse references))))

(defun %liveness-all-package-metadata (records)
  "Package metadata from production, test, and Common Lisp declarations."
  (let ((packages (%liveness-package-metadata records))
        (test-records
          (loop for record in records
                when (eq (getf record :kind) :test)
                  collect (let ((copy (copy-list record)))
                            (setf (getf copy :kind) :production)
                            copy))))
    (maphash (lambda (name info) (setf (gethash name packages) info))
             (%liveness-package-metadata test-records))
    (let ((common-lisp (find-package "COMMON-LISP")))
      (when common-lisp
        (let ((info
                (list :name "COMMON-LISP"
                      :file nil
                      :line 0
                      :exports
                      (loop for symbol being the external-symbols of common-lisp
                            collect (string-upcase (symbol-name symbol)))
                      :uses nil
                      :uses-declared-p t
                      :imports nil
                      :nicknames '(("CL" . "COMMON-LISP")))))
          (setf (gethash "COMMON-LISP" packages) info
                (gethash "CL" packages) info))))
    packages))

(defun %liveness-reference-index (records packages)
  "Exact production/test references and unresolved dynamic lookup diagnostics."
  (let ((references (make-hash-table :test #'equal))
        (local-references (make-hash-table :test #'equal))
        (diagnostics '()))
    (labels ((mark (kind package namespace symbol)
               (when (and package namespace symbol)
                 (setf (gethash (list kind package namespace symbol) references) t)))
             (mark-symbol (kind package symbol)
               (dolist (namespace '(:function :value :type))
                 (mark-local kind (list :package package) namespace symbol nil)))
             (alias (record-or-package package)
  (let* ((owner (if (stringp record-or-package)
                    record-or-package
                    (getf record-or-package :package)))
         (nicknames (getf (gethash owner packages) :nicknames)))
    (or (cdr (assoc package nicknames :test #'string=))
        package)))
             (exports (package)
               (or (getf (gethash package packages) :exports)
                   (let ((found (find-package package)))
                     (when found
                       (loop for symbol being the external-symbols of found
                             collect (string-upcase (symbol-name symbol)))))))
             (mark-local (kind record namespace symbol &optional (local-p t) visited)
  (let* ((own (if (stringp record) record (getf record :package)))
         (info (and own (gethash own packages)))
         (seen (or visited (make-hash-table :test #'equal)))
         (key (list kind own namespace symbol))
         (imports (remove-if-not
                   (lambda (import) (string= (second import) symbol))
                   (getf info :imports)))
         ;; DEFPACKAGE defaults to COMMON-LISP only when no :USE clause is present.
         (uses (if (getf info :uses-declared-p)
                   (getf info :uses)
                   (list (list "COMMON-LISP" nil)))))
    (when (and own local-p)
      (setf (gethash (list kind own symbol) local-references) t))
    (when own
      (unless (gethash key seen)
        (setf (gethash key seen) t)
        (unless (and (null imports)
                     (not (member symbol (getf info :shadows)
                                  :test (function string=)))
                     (some (lambda (use)
                             (member symbol
                                     (exports (alias own (first use)))
                                     :test (function string=)))
                           uses))
          (mark kind own namespace symbol))
        (if imports
            (dolist (import imports)
              (setf (gethash (list kind own symbol) local-references) t)
              (mark-local kind
                          (list :package (alias own (first import)))
                          namespace (second import) t seen))
            (unless (member symbol (getf info :shadows) :test #'string=)
              (dolist (use uses)
                (let ((package (alias own (first use))))
                  (when (member symbol (exports package) :test #'string=)
                    (setf (gethash (list kind own symbol) local-references) t)
                    (mark-local kind (list :package package) namespace symbol t seen))))))))))
             (record-reference (kind record namespace symbol package)
  (cond
    ((eq package :local)
     (mark-local kind record namespace symbol))
    ((stringp package)
     (mark-local kind (list :package (alias record package))
                 namespace symbol nil))
    ((eq package :uninterned) nil)
    (t nil)))
             (mark-package-syntax (kind record form)
               (when
                   (and (consp form)
                        (symbolp (first form))
                        (eq (symbol-package (first form)) *reader-package*)
                        (member (string-upcase (symbol-name (first form)))
                                '("DEFUN" "DEFMACRO"
                                          "DEFGENERIC"
                                          "DEFMETHOD"
                                          "DEFCLASS"
                                          "DEFINE-CONDITION"
                                          "DEFSTRUCT"
                                          "DEFVAR"
                                          "DEFPARAMETER"
                                          "DEFCONSTANT")
                                :test
                                #'string=))
                 (mark-local kind
                             record
                             :function
                             (string-upcase (symbol-name (first form))))))
             (scan-record (record)
                          (let* ((kind (getf record :kind))
                                 (reference-kind (if (eq kind :test) :test :production))
                                 (code (getf record :code))
                                 (text (getf record :text))
                                 (starts (%liveness-top-level-forms code))
                                 (ends (append (rest starts) (list (length code)))))
                            (loop for start in starts
                                  for end in ends
                                  for form = (%liveness-read-form-at text start)
                                  for form-code = (subseq code start end)
                                  for refs = (%liveness-reference-p form)
                                  do (when (eq kind :data)
                                       (scan-data-table record form form-code))
                                  (dolist (reference refs)
                                    (destructuring-bind (namespace symbol package) reference
                                      (record-reference reference-kind record namespace symbol package)))
                                  (progn
                                    (lookup-diagnostic record start form)
                                    (mark-package-syntax reference-kind record form))
                                  (when (and (eq kind :production)
                                             (string= (getf record :relative) "aitools.asd")
                                             (asdf-entry-point-p form))
                                    (mark :production "AITOOLS/CLI" :function "IMAGE-ENTRY-POINT"))
                                  (dolist (call (%liveness-form-find-symbols form record packages))
                                    (scan-find-symbol record (or (first call) start) (second call))))))
             (literal-package (object)
                              (or (literal-name object)
                                  (and (consp object)
                                       (symbolp (first object))
                                       (string= (%liveness-symbol-name (first object)) "FIND-PACKAGE")
                                       (literal-name (second object)))))
             (literal-name (object)
                           (cond
                            ((stringp object) object)
                            ((and (symbolp object) (keywordp object))
                             (%liveness-symbol-name object))
                            ((and (consp object)
                                  (symbolp (first object))
                                  (string= (%liveness-symbol-name (first object)) "QUOTE")
                                  (symbolp (second object)))
                             (%liveness-symbol-name (second object)))
                            (t nil)))
             (lookup-diagnostic (record position message-or-form)
               (when (eq (getf record :kind) :production)
                 (if (stringp message-or-form)
                     (push (format nil "~A:~D: ~A"
                                   (getf record :relative)
                                   (%line-at (getf record :text) position)
                                   message-or-form)
                           diagnostics)
                     (labels ((static-builder-p (builder allow-lambda-p)
                                (and (consp builder)
                                     (symbolp (first builder))
                                     (member (%liveness-symbol-name (first builder))
                                             (if allow-lambda-p
                                                 (list "FUNCTION" "QUOTE" "LAMBDA")
                                                 (list "FUNCTION" "QUOTE"))
                                             :test (function string=))))
                              (form-position (operator cursor)
                                (or (loop for candidate in
                                            (%liveness-form-positions
                                             (getf record :code)
                                             operator)
                                          when (>= candidate cursor)
                                            return candidate)
                                    position))
                              (scan (object cursor context depth)
                                (if (not (consp object))
                                    cursor
                                    (let* ((head (first object))
                                           (operator (and (symbolp head)
                                                          (%liveness-symbol-name head)))
                                           (next-position cursor))
                                      (cond
                                        ((eq head :u05-quasiquote)
                                         (case context
                                           (:active
                                            (scan (second object) cursor :template 1))
                                           (:template
                                            (scan (second object) cursor :template
                                                  (1+ depth)))
                                           (otherwise
                                            (scan (second object) cursor :inactive 0))))
                                        ((member head
                                                 (list :u05-unquote :u05-unquote-splicing))
                                         (case context
                                           (:template
                                            (if (= depth 1)
                                                (scan (second object) cursor :active 0)
                                                (scan (second object) cursor :template
                                                      (1- depth))))
                                           (otherwise
                                            (scan (second object) cursor :inactive 0))))
                                        (t
                                         (when (and operator
                                                    (member operator
                                                            (list "DEFINE-COMMAND"
                                                                  "DEFINE-INSPECT-COMMAND")
                                                            :test (function string=)))
                                           (let ((at (form-position operator cursor)))
                                             (setf next-position (1+ at))
                                             (when (eq context :active)
                                               (cond
                                                 ((string= operator "DEFINE-COMMAND")
                                                  (let* ((options (third object))
                                                         (builder
                                                           (loop for tail = options then (cdr tail)
                                                                 while (consp tail)
                                                                 when (eq (first tail)
                                                                          :command-builder)
                                                                   return (second tail))))
                                                    (unless (static-builder-p builder t)
                                                      (lookup-diagnostic
                                                       record at
                                                       "unresolved command builder"))))
                                                 (t
                                                  (unless (static-builder-p
                                                           (fourth object) nil)
                                                    (lookup-diagnostic
                                                     record at
                                                     "unresolved inspect command builder")))))))
                                         (if (and operator (string= operator "QUOTE"))
                                             (setf next-position
                                                   (scan (second object)
                                                         next-position
                                                         (if (eq context :active)
                                                             :inactive
                                                             context)
                                                         depth))
                                             (loop for tail = object then (cdr tail)
                                                   while (consp tail)
                                                   do (setf next-position
                                                            (scan (car tail)
                                                                  next-position
                                                                  context
                                                                  depth))))
                                         next-position))))))
                       (scan message-or-form position :active 0)))))
             (scan-find-symbol (record position form)
  (let* ((name (and (consp form) (literal-name (second form))))
         (package (if (cddr form)
                      (literal-package (third form))
                      nil)))
    (if (and name package)
        (mark-symbol (if (eq (getf record :kind) :test) :test :production)
                     (alias record package)
                     name)
        (when (eq (getf record :kind) :production)
          (lookup-diagnostic record position
                             "unresolved dynamic find-symbol")))))
             (scan-data-table (record form code)
  (dolist (designator (%liveness-data-function-designators record form code))
    (when designator
      (mark :production
            (alias record (first designator))
            :function (second designator)))))
             (asdf-entry-point-p (form)
               (and (consp form)
                    (or (loop for tail = form then (cdr tail)
                              while (consp tail)
                              thereis (and (eq (car tail) :entry-point)
                                           (stringp (cadr tail))
                                           (string-equal (cadr tail)
                                                         "aitools/cli::image-entry-point")))
                        (loop for tail = form then (cdr tail)
                              while (consp tail)
                              thereis (and (consp (car tail))
                                           (asdf-entry-point-p (car tail))))))))
      (dolist (record records)
        (scan-record record))
      (values references local-references (sort diagnostics #'string<)))))

(defun %liveness-package-token-used-p (records package symbol)
  "Whether production code in PACKAGE refers to SYMBOL without qualification."
  (some (lambda (record)
          (and (eq (getf record :kind) :production)
               (string= (getf record :package) package)
               (member symbol (getf record :tokens) :test #'string=)))
        records))

(defun %liveness-production-package-p (info)
  "True for package metadata declared under production source paths."
  (let ((file (getf info :file)))
    (and file
         (or (and (>= (length file) 9) (string= file "packages/" :end1 9))
             (and (>= (length file) 4) (string= file "src/" :end1 4))))))
(defun %liveness-documented-api-root-p (package namespace symbol)
  "Architecture documents these public APIs without in-tree callers."
  (and (eq namespace :function)
       (member (list package symbol)
               (quote (;; architecture.md:320 documents the public diff-generation API.
                       ("AITOOLS.KERNEL.DOMAIN" "GENERATE-UNIFIED-DIFF")
                       ;; architecture.md:251 documents the transaction lock macros.
                       ("AITOOLS.STORE.APPLICATION" "WITH-TX-LOCK")
                       ("AITOOLS.STORE.APPLICATION" "WITH-WORKSPACE-LOCK")
                       ;; architecture.md:250 documents the text boundary macro.
                       ("AITOOLS.TEXT.INFRASTRUCTURE" "WITH-TEXT-BOUNDARIES")
                       ;; architecture.md:249 documents the workspace boundary macro.
                       ("AITOOLS.WORKSPACE.INFRASTRUCTURE" "WITH-WORKSPACE-BOUNDARIES")))
               :test (function equal))))

(defun scan-dead-code-tree (root)
  "Sorted dead definitions and package violations under ROOT.
Exported definitions are public roots; test-only exports remain violations."
  (let* ((records (progn (clrhash *liveness-reader-view-cache*) (%liveness-files root)))
         (packages (%liveness-all-package-metadata records))
         (definitions (%liveness-definitions records))
         (violations '()))
    (multiple-value-bind (references local-references dynamic-lookups)
        (%liveness-reference-index records packages)
      (let ((live-structs (make-hash-table :test #'equal))
            (reported-structs (make-hash-table :test #'equal)))
        (labels ((referenced-p (definition kind)
                   (gethash (list kind
                                  (getf definition :package)
                                  (getf definition :namespace)
                                  (getf definition :name))
                            references))
                 (exported-p (definition)
                   (member (getf definition :name)
                           (getf (gethash (getf definition :package) packages)
                                 :exports)
                           :test #'string=))
                 (rooted-p (definition)
                   (or (getf definition :root)
                       (exported-p definition)))
                 (unused-definition (definition)
                   (format nil "~A:~D: unused definition ~A:~A"
                           (getf definition :file) (getf definition :line)
                           (getf definition :package) (getf definition :name))))
          (dolist (definition definitions)
            (let ((group (getf definition :group)))
              (when group
                (let ((package (getf definition :package))
                      (type-name (fourth group)))
                  (when (or (rooted-p definition)
                            (referenced-p definition :production))
                    (setf (gethash group live-structs) t
                          (gethash (list :production package :type type-name)
                                   references) t))
                  (when (referenced-p definition :test)
                    (setf (gethash group live-structs) t
                          (gethash (list :test package :type type-name)
                                   references) t))))))
          (dolist (definition definitions)
            (let* ((group (getf definition :group))
                   (namespace (getf definition :namespace)))
              (cond
                ((and group (eq namespace :type))
                 (unless (or (gethash group live-structs)
                             (gethash group reported-structs))
                   (setf (gethash group reported-structs) t)
                   (push (unused-definition definition) violations)))
                (group
                 (when (and (gethash group live-structs)
                            (not (rooted-p definition))
                            (not (referenced-p definition :production))
                            (not (referenced-p definition :test)))
                   (push (unused-definition definition) violations)))
                (t
                 (unless (or (rooted-p definition)
                             (referenced-p definition :production)
                             (referenced-p definition :test))
                   (push (unused-definition definition) violations))))))
          (setf violations (append dynamic-lookups violations))
          (maphash
           (lambda (package info)
             (when (%liveness-production-package-p info)
               (dolist (symbol (getf info :exports))
                 (when (and (not (%liveness-documented-api-root-p package :function symbol))
                            (some (lambda (namespace)
                                    (gethash (list :test package namespace symbol)
                                             references))
                                  '(:function :value :type))
                            (not (some (lambda (namespace)
                                         (gethash (list :production package namespace symbol)
                                                  references))
                                       '(:function :value :type))))
                   (push (format nil
                                 "~A:~D: exported symbol ~A:~A is referenced only from tests"
                                 (getf info :file) (getf info :line) package symbol)
                         violations)))
               (dolist (import (getf info :imports))
                 (unless (or (member (second import) (getf info :exports)
                                     :test #'string=)
                             (gethash (list :production package (second import))
                                      local-references))
                   (push (format nil "~A:~D: unused ~(~S~) ~A:~A"
                                 (getf info :file) (third import) (fourth import)
                                 (first import) (second import))
                         violations)))
               (dolist (use (getf info :uses))
                 (let ((used-package (gethash (first use) packages)))
                   (unless (some (lambda (symbol)
                                   (gethash (list :production package symbol)
                                            local-references))
                                 (getf used-package :exports))
                     (push (format nil "~A:~D: unused :use ~A"
                                   (getf info :file) (second use) (first use))
                           violations))))))
           packages)
          (sort (remove-duplicates violations :test #'string=) #'string<))))))
