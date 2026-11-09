(in-package #:cl-user)
(defpackage #:aitools.net.application
  (:use #:cl)
  (:export #:net-ports
           #:make-net-ports
           #:net-ports-http-request
           #:net-ports-resolve-host
           #:get/k
           #:head/k
           #:resolve/k))
