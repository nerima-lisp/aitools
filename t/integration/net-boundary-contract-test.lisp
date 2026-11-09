(in-package #:aitools.net.test)
(defun %net-source-directory ()
  (merge-pathnames "src/"
                   (asdf:system-source-directory "aitools/feature/net")))
(defun %net-lisp-files (directory)
  (append
   (remove-if-not (lambda (pathname)
                    (string-equal (or (pathname-type pathname) "") "lisp"))
                  (uiop:directory-files directory))
   (mapcan #'%net-lisp-files (uiop:subdirectories directory))))
(defun %net-boundary-keys-in-form (form)
  (when (consp form)
    (append
     (when (and (eq (first form) 'cl-boundary-kit:boundary-context-require)
                (keywordp (third form)))
       (list (third form)))
     (%net-boundary-keys-in-form (car form))
     (%net-boundary-keys-in-form (cdr form)))))
(defun %net-required-boundary-keys ()
  (let ((keys '())
        (*package* (find-package '#:aitools.net.application))
        (*read-eval* nil)
        (eof (gensym "EOF")))
    (dolist (file (%net-lisp-files
                   (merge-pathnames "application/" (%net-source-directory))))
      (with-open-file (stream file)
        (loop for form = (read stream nil eof)
              until (eq form eof)
              do (setf keys (nconc keys (%net-boundary-keys-in-form form))))))
    (remove-duplicates keys :test #'eq)))
(describe "aitools net boundary contract"
  (it "contains no legacy ports symbol in production source"
    (let* ((source-files (%net-lisp-files (%net-source-directory)))
           (legacy-files
             (loop for file in source-files
                   when (search "net-ports"
                                (uiop:read-file-string file)
                                :test #'char-equal)
                     collect file)))
      (expect source-files :to-be-truthy)
      (expect legacy-files :to-equal nil)))
  (it "registers each capability required by application flows"
    (let* ((required-keys (%net-required-boundary-keys))
           (boundaries
             (aitools.net.composition::make-net-dependencies)))
      (expect required-keys :to-be-truthy)
      (expect
       (every (lambda (key)
                (functionp
                 (cl-boundary-kit:boundary-context-require boundaries key)))
              required-keys)
       :to-be t))))
