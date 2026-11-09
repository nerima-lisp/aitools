(in-package #:aitools.net.composition)
(defun make-net-dependencies (&rest arguments)
  (declare (ignore arguments))
  (let* ((resolve-host (aitools.net.infrastructure:make-os-resolver))
         (http-send (aitools.net.infrastructure:make-http-requester resolve-host))
         (boundaries (cl-boundary-kit:make-boundary-context)))
    (setf boundaries
          (cl-boundary-kit:boundary-context-with
           boundaries :net-http-send http-send))
    (setf boundaries
          (cl-boundary-kit:boundary-context-with
           boundaries :net-resolve resolve-host))
    boundaries))
(aitools/cli:define-context-command-registration
 :net
 #'aitools.net.presentation:register-net-commands
 #'make-net-dependencies)
