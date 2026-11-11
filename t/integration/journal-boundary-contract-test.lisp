(in-package #:aitools.journal.test)
(defun %journal-source-directory ()
  (merge-pathnames "src/"
                   (asdf:system-source-directory "aitools/feature/journal")))
(defun %journal-lisp-files (directory)
  (append
   (remove-if-not (lambda (pathname)
                    (string-equal (or (pathname-type pathname) "") "lisp"))
                  (uiop:directory-files directory))
   (mapcan #'%journal-lisp-files (uiop:subdirectories directory))))
(defun %journal-boundary-keys-in-form (form)
  (when (consp form)
    (append
     (when (and (eq (first form) 'cl-boundary-kit:boundary-context-require)
                (keywordp (third form)))
       (list (third form)))
     (%journal-boundary-keys-in-form (car form))
     (%journal-boundary-keys-in-form (cdr form)))))
(defun %journal-required-boundary-keys ()
  (let ((keys '())
        (*package* (find-package 'aitools.journal.application))
        (*read-eval* nil)
        (eof (gensym "EOF")))
    (dolist (file (%journal-lisp-files
                   (merge-pathnames "application/" (%journal-source-directory))))
      (with-open-file (stream file)
        (loop for form = (read stream nil eof)
              until (eq form eof)
              do (setf keys (nconc keys (%journal-boundary-keys-in-form form))))))
    (remove-duplicates keys :test #'eq)))
(describe "aitools journal boundary contract"
  (it "contains no legacy journal adapter symbol or adapter production file"
    (let* ((source-files (%journal-lisp-files (%journal-source-directory)))
           (legacy-adapter-symbol-files
             (loop for file in source-files
                   when (search (concatenate (quote string) "journal-" "port" "s")
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
    (let* ((required-keys (%journal-required-boundary-keys))
           (expected-keys '(:journal-workspace-host :journal-open-store))
           (workspace-host
             (aitools.workspace.infrastructure:make-host-workspace-boundaries))
           (open-store (function aitools.store.infrastructure:make-posix-store))
           (boundaries
             (aitools.journal.composition::make-journal-dependencies
              :workspace-host workspace-host
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
      (expect
       (cl-boundary-kit:boundary-context-require boundaries :journal-workspace-host)
       :to-equal workspace-host)
      (expect
       (cl-boundary-kit:boundary-context-require boundaries :journal-open-store)
       :to-equal open-store)
      (expect
       (functionp
        (cl-boundary-kit:boundary-context-require boundaries :journal-open-store))
       :to-be t))))
