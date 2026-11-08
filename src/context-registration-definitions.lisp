(in-package #:aitools/cli)

(defvar *context-command-registrations*
  nil)

(defun %record-context-command-registration (context register-function
                                                     dependency-function)
  (let ((registration
         (assoc context *context-command-registrations* :test #'equal)))
    (if registration
        (setf (cdr registration) (list register-function dependency-function))
        (setf *context-command-registrations* (append
                                               *context-command-registrations*
                                               (list
                                                (list context
                                                      register-function
                                                      dependency-function)))))))

(defun context-command-registrations ()
  (copy-tree *context-command-registrations*))

(defmacro define-context-command-registration (context register-function
                                                       dependency-function)
  `(eval-when (:load-toplevel :execute)
     (%record-context-command-registration ',context
                                           ,register-function
                                           ,dependency-function)))
