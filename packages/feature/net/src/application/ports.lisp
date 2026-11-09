(in-package #:aitools.net.application)
(defstruct (net-ports
             (:constructor make-net-ports (&key http-request resolve-host))
             (:copier nil))
  (http-request nil :type function :read-only t)
  (resolve-host nil :type function :read-only t))
