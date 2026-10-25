(in-package #:aitools.integration.structure-test)

(defvar *liveness-index-references* nil)

(defvar *liveness-index-local-references* nil)

(defun %liveness-index-mark (kind package namespace symbol)
  (when (and package namespace symbol)
    (setf (gethash (list kind package namespace symbol)
                   *liveness-index-references*)
          t)))

(defun %liveness-index-mark-symbol (kind package symbol packages)
  (dolist (namespace '(:function :value :type))
    (%liveness-index-mark-local kind (list :package package) namespace symbol
                                packages nil)))

(defun %liveness-index-alias (record-or-package package packages)
  (let* ((owner (if (stringp record-or-package)
                    record-or-package
                    (getf record-or-package :package)))
         (nicknames (getf (gethash owner packages) :nicknames)))
    (or (cdr (assoc package nicknames :test #'string=)) package)))

(defun %liveness-index-exports (package packages)
  (or (getf (gethash package packages) :exports)
      (let ((found (find-package package)))
        (when found
          (loop for symbol being the external-symbols of found
                collect (string-upcase (symbol-name symbol)))))))

(defun %liveness-index-mark-local (kind record namespace symbol packages
                                  &optional (local-p t) visited)
  (let* ((own (if (stringp record) record (getf record :package)))
         (info (and own (gethash own packages)))
         (seen (or visited (make-hash-table :test #'equal)))
         (key (list kind own namespace symbol))
         (imports (remove-if-not
                   (lambda (import) (string= (second import) symbol))
                   (getf info :imports)))
         (uses (if (getf info :uses-declared-p)
                   (getf info :uses)
                   (list (list "COMMON-LISP" nil)))))
    (when (and own local-p)
      (setf (gethash (list kind own symbol)
                     *liveness-index-local-references*)
            t))
    (when own
      (unless (gethash key seen)
        (setf (gethash key seen) t)
        (unless (and (null imports)
                     (not (member symbol (getf info :shadows)
                                  :test #'string=))
                     (some (lambda (use)
                             (member symbol
                                     (%liveness-index-exports
                                      (%liveness-index-alias own (first use)
                                                             packages)
                                      packages)
                                     :test #'string=))
                           uses))
          (%liveness-index-mark kind own namespace symbol))
        (if imports
            (dolist (import imports)
              (setf (gethash (list kind own symbol)
                             *liveness-index-local-references*)
                    t)
              (%liveness-index-mark-local
               kind (list :package
                          (%liveness-index-alias own (first import) packages))
               namespace (second import) packages t seen))
            (unless (member symbol (getf info :shadows) :test #'string=)
              (dolist (use uses)
                (let ((package (%liveness-index-alias own (first use) packages)))
                  (when (member symbol (%liveness-index-exports package packages)
                                :test #'string=)
                    (setf (gethash (list kind own symbol)
                                   *liveness-index-local-references*)
                          t)
                    (%liveness-index-mark-local
                     kind (list :package package) namespace symbol packages t
                     seen))))))))))

(defun %liveness-index-record-reference (kind record namespace symbol package
                                         packages)
  (cond
    ((eq package :local)
     (%liveness-index-mark-local kind record namespace symbol packages))
    ((stringp package)
     (%liveness-index-mark-local
      kind (list :package (%liveness-index-alias record package packages))
      namespace symbol packages nil))
    ((eq package :uninterned) nil)
    (t nil)))

(defun %liveness-index-mark-package-syntax (kind record form packages)
  (when (and (consp form)
             (symbolp (first form))
             (eq (symbol-package (first form)) *reader-package*)
             (member (string-upcase (symbol-name (first form)))
                     '("DEFUN" "DEFMACRO" "DEFGENERIC" "DEFMETHOD"
                       "DEFCLASS" "DEFINE-CONDITION" "DEFSTRUCT" "DEFVAR"
                       "DEFPARAMETER" "DEFCONSTANT")
                     :test #'string=))
    (%liveness-index-mark-local
     kind record :function (string-upcase (symbol-name (first form))) packages)))

(defun %liveness-index-literal-name (object)
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

(defun %liveness-index-literal-package (object)
  (or (%liveness-index-literal-name object)
      (and (consp object)
           (symbolp (first object))
           (string= (%liveness-symbol-name (first object)) "FIND-PACKAGE")
           (%liveness-index-literal-name (second object)))))

(defun %liveness-index-scan-find-symbol (record position form packages)
  (let* ((name (and (consp form)
                    (%liveness-index-literal-name (second form))))
         (package (if (cddr form)
                      (%liveness-index-literal-package (third form))
                      nil)))
    (if (and name package)
        (%liveness-index-mark-symbol
         (if (eq (getf record :kind) :test) :test :production)
         (%liveness-index-alias record package packages) name packages)
        (when (eq (getf record :kind) :production)
          (%liveness-index-lookup-diagnostic
           record position "unresolved dynamic find-symbol")))))

(defun %liveness-index-scan-data-table (record form code packages)
  (dolist (designator (%liveness-data-function-designators record form code))
    (when designator
      (%liveness-index-mark
       :production
       (%liveness-index-alias record (first designator) packages)
       :function (second designator)))))

(defun %liveness-index-asdf-entry-point-p (form)
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
                              (%liveness-index-asdf-entry-point-p
                               (car tail)))))))

(defun %liveness-index-scan-record (record packages)
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
               (%liveness-index-scan-data-table record form form-code packages))
             (dolist (reference refs)
               (destructuring-bind (namespace symbol package) reference
                 (%liveness-index-record-reference
                  reference-kind record namespace symbol package packages)))
             (%liveness-index-lookup-diagnostic record start form)
             (%liveness-index-mark-package-syntax
              reference-kind record form packages)
             (when (and (eq kind :production)
                        (string= (getf record :relative) "aitools.asd")
                        (%liveness-index-asdf-entry-point-p form))
               (%liveness-index-mark :production "AITOOLS/CLI" :function
                                     "IMAGE-ENTRY-POINT"))
             (dolist (call (%liveness-form-find-symbols form record packages))
               (%liveness-index-scan-find-symbol
                record (or (first call) start) (second call) packages)))))

(defun %liveness-reference-index (records packages)
  "Exact production/test references and unresolved dynamic lookup diagnostics."
  (let ((*liveness-index-references* (make-hash-table :test #'equal))
        (*liveness-index-local-references* (make-hash-table :test #'equal))
        (*liveness-index-diagnostics* nil))
    (dolist (record records)
      (%liveness-index-scan-record record packages))
    (values *liveness-index-references*
            *liveness-index-local-references*
            (sort *liveness-index-diagnostics* #'string<))))
