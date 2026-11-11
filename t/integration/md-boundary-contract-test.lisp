(in-package #:aitools.md.test)
(defun %md-source-directory ()
  (merge-pathnames "src/"
                   (asdf:system-source-directory "aitools/feature/md")))
(defun %md-lisp-files (directory)
  (append
   (remove-if-not (lambda (pathname)
                    (string-equal (or (pathname-type pathname) "") "lisp"))
                  (uiop:directory-files directory))
   (mapcan #'%md-lisp-files (uiop:subdirectories directory))))
(defun %md-boundary-keys-in-form (form)
  (when (consp form)
    (append
     (when (and (eq (first form) 'cl-boundary-kit:boundary-context-require)
                (keywordp (third form)))
       (list (third form)))
     (%md-boundary-keys-in-form (car form))
     (%md-boundary-keys-in-form (cdr form)))))
(defun %md-required-boundary-keys ()
  (let ((keys '())
        (*package* (find-package '#:aitools.md.application))
        (*read-eval* nil)
        (eof (gensym "EOF")))
    (dolist (file (%md-lisp-files
                   (merge-pathnames "application/" (%md-source-directory))))
      (with-open-file (stream file)
        (loop for form = (read stream nil eof)
              until (eq form eof)
              do (setf keys (nconc keys (%md-boundary-keys-in-form form))))))
    (remove-duplicates keys :test #'eq)))
(describe "aitools md boundary contract"
  (it "contains no legacy MD adapter symbol or adapter production file"
    (let* ((source-files (%md-lisp-files (%md-source-directory)))
           (legacy-adapter-symbol-files
             (loop for file in source-files
                   when (search (concatenate (quote string) "md-" "port" "s")
                                (uiop:read-file-string file)
                                :test #'char-equal)
                     collect file))
           (legacy-adapter-files
             (remove-if-not (lambda (file)
                              (string-equal (or (pathname-name file) "")
                                            (concatenate (quote string) "port" "s")))
                            source-files)))
      (expect source-files :to-be-truthy)
      (expect legacy-adapter-symbol-files :to-equal nil)
      (expect legacy-adapter-files :to-equal nil)))
  (it "registers every capability required by application flows"
    (let* ((required-keys (%md-required-boundary-keys))
           (expected-keys '(:md-workspace-host :md-text-context :md-open-store
                            :md-read-stdin-octets :md-parse-markdown :md-splice-section))
           (workspace-host
             (aitools.workspace.infrastructure:make-host-workspace-boundaries))
           (text-context (aitools.text.infrastructure:make-host-text-source))
           (open-store (function aitools.store.infrastructure:make-posix-store))
           (boundaries
             (aitools.md.composition::make-md-dependencies
              :workspace-host workspace-host
              :text-source text-context
              :open-store open-store)))
      (expect required-keys :to-be-truthy)
      (expect (every (lambda (key) (member key required-keys)) expected-keys)
              :to-be t)
      (expect (every (lambda (key) (member key expected-keys)) required-keys)
              :to-be t)
      (expect
       (every (lambda (key)
                (cl-boundary-kit:boundary-context-require boundaries key))
              required-keys)
       :to-be t)
      (expect (cl-boundary-kit:boundary-context-require boundaries :md-workspace-host)
              :to-equal workspace-host)
      (expect (cl-boundary-kit:boundary-context-require boundaries :md-text-context)
              :to-equal text-context)
      (expect (cl-boundary-kit:boundary-context-require boundaries :md-open-store)
              :to-equal open-store)
      (dolist (key '(:md-read-stdin-octets :md-parse-markdown :md-open-store :md-splice-section))
        (expect (functionp
                 (cl-boundary-kit:boundary-context-require boundaries key))
                :to-be t)))))
