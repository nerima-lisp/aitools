(in-package #:aitools.integration.structure-test)
(defvar *loaded-context-structure-data* nil)
(defun %load-context-structure-data-files (directory)
  (loop for pathname in
          (sort (directory (merge-pathnames "*.lisp" directory))
                #'string< :key #'namestring)
        collect
        (let* ((context-data
                 (let ((*loaded-context-structure-data* nil))
                   (load pathname)
                   *loaded-context-structure-data*))
               (expected-context (pathname-name pathname)))
          (unless (and (listp context-data)
                       (stringp (getf context-data :context))
                       (string= (getf context-data :context) expected-context))
            (error "Context structure data in ~A must name its context ~A."
                   pathname expected-context))
          context-data)))

(defparameter *context-symbol-allowances*
  (loop for context-data in
        (%load-context-structure-data-files
         (merge-pathnames
          "structure-context-allowances/"
          (make-pathname :name nil :type nil :defaults *load-truename*)))
        append (getf context-data :symbol-allowances)))

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
