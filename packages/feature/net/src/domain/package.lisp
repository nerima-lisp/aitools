(in-package #:cl-user)
(defpackage #:aitools.net.domain
  (:use #:cl)
  (:export #:net-error
           #:net-error-code
           #:net-error-detail
           #:net-address
           #:net-address-p
           #:make-net-address
           #:net-address-type
           #:net-address-value
           #:net-address-family
           #:net-address-octets))
