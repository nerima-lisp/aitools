(in-package #:aitools.integration.structure-test)

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
