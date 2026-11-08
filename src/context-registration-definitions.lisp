(in-package #:aitools/cli)

(defvar *context-command-registrations*
  nil)

(defun %record-context-command-registration (context provider)
  (let ((registration
         (assoc context *context-command-registrations* :test #'equal)))
    (if registration
        (setf (cdr registration) (list provider))
        (setf *context-command-registrations* (append
                                               *context-command-registrations*
                                               (list (list context provider)))))))

(defun context-command-registrations ()
  (mapcar (lambda (registration)
            (cons (first registration)
                  (funcall (second registration))))
          *context-command-registrations*))

(defmacro define-context-command-registration (context register-function
                                                       dependency-function)
  `(eval-when (:load-toplevel :execute)
     (%record-context-command-registration ',context
                                           (lambda ()
                                             (list ,register-function
                                                   ,dependency-function)))))
