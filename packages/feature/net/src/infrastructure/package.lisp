(in-package #:cl-user)
(defpackage #:aitools.net.infrastructure
  (:use #:cl)
  (:import-from #:aitools.net.domain
                #:make-net-address
                #:net-error)
  (:export #:make-os-resolver
           #:make-http-requester))
