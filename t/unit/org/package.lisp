(in-package #:cl-user)

(defpackage #:aitools.org.test
  (:use #:cl #:aitools.org.infrastructure)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it-property #:gen-string #:expect))
