(in-package #:aitools.util.test)

(defun %util-source-directory ()
  (merge-pathnames "src/"
                   (asdf:system-source-directory "aitools/feature/util")))

(defun %util-lisp-files (directory)
  (append
   (remove-if-not (lambda (pathname)
                    (string-equal (or (pathname-type pathname) "") "lisp"))
                  (uiop:directory-files directory))
   (mapcan #'%util-lisp-files (uiop:subdirectories directory))))

(defun %util-production-files ()
  (cons (merge-pathnames "util.asd"
                         (asdf:system-source-directory "aitools/feature/util"))
        (%util-lisp-files (%util-source-directory))))

(defun %util-boundary-keys-in-form (form)
  (when (consp form)
    (append
     (when (and (eq (first form) 'cl-boundary-kit:boundary-context-require)
                (keywordp (third form)))
       (list (third form)))
     (%util-boundary-keys-in-form (car form))
     (%util-boundary-keys-in-form (cdr form)))))

(defun %util-required-boundary-keys ()
  (let ((keys '())
        (*package* (find-package '#:aitools.util.application))
        (*read-eval* nil)
        (eof (gensym "EOF")))
    (dolist (file (%util-lisp-files
                   (merge-pathnames "application/" (%util-source-directory))))
      (with-open-file (stream file)
        (loop for form = (read stream nil eof)
              until (eq form eof)
              do (setf keys
                       (nconc keys (%util-boundary-keys-in-form form))))))
    (remove-duplicates keys :test #'eq)))

(describe "aitools util boundary contract"
  (it "removes legacy util boundary symbols and files"
    (let* ((source-files (%util-production-files))
           (legacy-symbols '("util-ports" "make-util-ports"
                             "make-util-ports-from-boundaries"
                             "make-production-util-ports"))
           (legacy-symbol-files
             (loop for file in source-files
                   when (some (lambda (symbol)
                                (search symbol (uiop:read-file-string file)
                                        :test #'char-equal))
                              legacy-symbols)
                     collect file))
           (legacy-files
             (remove-if-not (lambda (file)
                              (string-equal (or (pathname-name file) "") "ports"))
                            source-files)))
      (expect source-files :to-be-truthy)
      (expect legacy-symbol-files :to-equal nil)
      (expect legacy-files :to-equal nil)))

  (it "registers each capability required by util application"
    (let* ((required-keys (%util-required-boundary-keys))
           (expected-keys
             (list :util-random-octets :util-uuid-v4 :util-unix-ms
                   :util-read-file-octets :util-read-stdin-octets
                   :util-workspace-host :util-open-store))
           (workspace-host (cl-boundary-kit:make-boundary-context))
           (open-store (function identity))
           (boundaries
             (aitools.util.composition::make-util-dependencies
              :workspace-host workspace-host :open-store open-store)))
      (expect required-keys :to-be-truthy)
      (expect (length required-keys) :to-be (length expected-keys))
      (expect (every (lambda (key) (member key required-keys)) expected-keys)
              :to-be t)
      (expect (every (lambda (key) (member key expected-keys)) required-keys)
              :to-be t)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :util-workspace-host)
              :to-equal workspace-host)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :util-open-store)
              :to-equal open-store)
      (dolist (key (remove :util-workspace-host
                           (remove :util-open-store expected-keys)))
        (expect (functionp
                 (cl-boundary-kit:boundary-context-require boundaries key))
                :to-be t)))))
