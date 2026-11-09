(in-package #:aitools.net.composition)
(defun make-net-dependencies (&rest arguments)
  (declare (ignore arguments))
  (let ((resolve-host (aitools.net.infrastructure:make-os-resolver)))
    (aitools.net.application:make-net-ports
     :http-request
     (aitools.net.infrastructure:make-http-requester resolve-host)
     :resolve-host resolve-host)))
(aitools/cli:define-context-command-registration
 :net
 #'aitools.net.presentation:register-net-commands
 #'make-net-dependencies)
