(in-package #:aitools.edit.test)

(defun %edit-source-directory ()
  (merge-pathnames "src/"
                   (asdf:system-source-directory "aitools/feature/edit")))

(defun %edit-lisp-files (directory)
  (append
   (remove-if-not (lambda (pathname)
                    (string-equal (or (pathname-type pathname) "") "lisp"))
                  (uiop:directory-files directory))
   (mapcan (function %edit-lisp-files) (uiop:subdirectories directory))))

(defun %edit-boundary-keys-in-form (form)
  (when (consp form)
    (append
     (when (and (eq (first form) (quote cl-boundary-kit:boundary-context-require))
                (keywordp (third form))
                (search "EDIT-" (symbol-name (third form))))
       (list (third form)))
     (%edit-boundary-keys-in-form (car form))
     (%edit-boundary-keys-in-form (cdr form)))))

(defun %edit-required-boundary-keys ()
  (let ((keys (list))
        (*package* (find-package (quote #:aitools.edit.application)))
        (*read-eval* nil)
        (eof (gensym "EOF")))
    (dolist (file (%edit-lisp-files
                   (merge-pathnames "application/" (%edit-source-directory))))
      (with-open-file (stream file)
        (loop for form = (read stream nil eof)
              until (eq form eof)
              do (setf keys
                       (nconc keys (%edit-boundary-keys-in-form form))))))
    (remove-duplicates keys :test (function eq))))

(describe "aitools edit boundary contract"
  (it "contains no old edit ports symbol or ports production file"
    (let* ((source-files (%edit-lisp-files (%edit-source-directory)))
           (legacy-symbol-files
             (loop for file in source-files
                   when (search "edit-ports" (uiop:read-file-string file)
                                :test (function char-equal))
                     collect file))
           (legacy-port-files
             (remove-if-not (lambda (file)
                              (string-equal (or (pathname-name file) "") "ports"))
                            source-files)))
      (expect source-files :to-be-truthy)
      (expect legacy-symbol-files :to-equal nil)
      (expect legacy-port-files :to-equal nil)))
  (it "registers every capability required by edit application"
    (let* ((required-keys (%edit-required-boundary-keys))
           (expected-keys
             (list :edit-workspace-host :edit-open-store :edit-text-source
                   :edit-read-stdin-octets :edit-unix-now :edit-serialize-document))
           (workspace-host (aitools.workspace.infrastructure:make-host-workspace-boundaries))
           (text-source (aitools.text.infrastructure:make-host-text-source))
           (open-store (function identity))
           (boundaries
             (aitools.edit.composition::make-edit-dependencies
              :workspace-host workspace-host
              :open-store open-store
              :text-source text-source)))
      (expect required-keys :to-be-truthy)
      (expect (every (lambda (key) (member key required-keys)) expected-keys)
              :to-be t)
      (expect (every (lambda (key) (member key expected-keys)) required-keys)
              :to-be t)
      (expect (every (lambda (key)
                       (cl-boundary-kit:boundary-context-require boundaries key))
                     required-keys)
              :to-be t)
      (expect (cl-boundary-kit:boundary-context-require boundaries :edit-workspace-host)
              :to-equal workspace-host)
      (expect (cl-boundary-kit:boundary-context-require boundaries :edit-open-store)
              :to-equal open-store)
      (expect (cl-boundary-kit:boundary-context-require boundaries :edit-text-source)
              :to-equal text-source))))

(describe "aitools edit shared write boundaries"
  (it "provides only the workspace and store capabilities for shared writes"
    (let* ((workspace-host :workspace)
           (open-store (function identity))
           (boundaries
             (aitools.edit.application:make-edit-write-boundaries
              workspace-host open-store)))
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :edit-workspace-host)
              :to-be workspace-host)
      (expect (cl-boundary-kit:boundary-context-require boundaries :edit-open-store)
              :to-be open-store)
      (dolist (key (list :edit-text-source :edit-read-stdin-octets
                         :edit-unix-now :edit-serialize-document))
        (expect (cl-boundary-kit:boundary-context-present-p boundaries key)
                :to-be nil)))))
