(in-package #:aitools.inspect.test)

(defun %inspect-source-directory ()
  (merge-pathnames "src/"
                   (asdf:system-source-directory "aitools/feature/inspect")))

(defun %inspect-lisp-files (directory)
  (append
   (remove-if-not (lambda (pathname)
                    (string-equal (or (pathname-type pathname) "") "lisp"))
                  (uiop:directory-files directory))
   (mapcan (function %inspect-lisp-files) (uiop:subdirectories directory))))

(defun %inspect-boundary-keys-in-form (form)
  (when (consp form)
    (append
     (when (and (eq (first form) 'cl-boundary-kit:boundary-context-require)
                (keywordp (third form)))
       (list (third form)))
     (%inspect-boundary-keys-in-form (car form))
     (%inspect-boundary-keys-in-form (cdr form)))))

(defun %inspect-required-boundary-keys ()
  (let ((keys '())
        (*package* (find-package '#:aitools.inspect.application))
        (*read-eval* nil)
        (eof (gensym "EOF")))
    (dolist (file (%inspect-lisp-files
                   (merge-pathnames "application/" (%inspect-source-directory))))
      (with-open-file (stream file)
        (loop for form = (read stream nil eof)
              until (eq form eof)
              do (setf keys
                       (nconc keys (%inspect-boundary-keys-in-form form))))))
    (remove-duplicates keys :test #'eq)))

(defun %inspect-same-keys-p (left right)
  (and (= (length left) (length right))
       (every (lambda (key) (member key right)) left)))

(describe "inspect boundary contract"
  (it "contains no legacy inspect adapter symbol or production adapter file"
    (let* ((source-files (%inspect-lisp-files (%inspect-source-directory)))
           (legacy-adapter-symbol-files
             (loop for file in source-files
                   when (search (concatenate (quote string) "inspect-" "port" "s")
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
  (it "registers every inspect and text boundary required by application flows"
    (let* ((required-keys (%inspect-required-boundary-keys))
           (inspect-keys (list :inspect-workspace-host :inspect-text-context
                               :inspect-open-store :inspect-state-directory))
           (text-keys (list :text-read-octets :text-read-range
                            :text-file-size :text-call-with-chunks))
           (expected-keys (append inspect-keys text-keys))
           (workspace-host
             (aitools.workspace.infrastructure:make-host-workspace-boundaries))
           (text-context (aitools.text.infrastructure:make-host-text-source))
           (open-store (function aitools.store.infrastructure:make-posix-store))
           (state-directory-function (lambda () nil))
           (boundaries
             (aitools.inspect.composition::make-inspect-dependencies
              :workspace-host workspace-host
              :text-source text-context
              :open-store open-store
              :state-directory-function state-directory-function)))
      (expect (%inspect-same-keys-p required-keys expected-keys) :to-be t)
      (expect
       (every (lambda (key)
                (cl-boundary-kit:boundary-context-require boundaries key))
              inspect-keys)
       :to-be t)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :inspect-workspace-host)
              :to-equal workspace-host)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :inspect-text-context)
              :to-equal text-context)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :inspect-open-store)
              :to-equal open-store)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :inspect-state-directory)
              :to-equal state-directory-function)
      (expect
       (every (lambda (key)
                (functionp
                 (cl-boundary-kit:boundary-context-require text-context key)))
              text-keys)
       :to-be t))))
