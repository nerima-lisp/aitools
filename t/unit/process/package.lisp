(in-package #:cl-user)

(defpackage #:aitools.process.test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:it-property #:gen-integer #:expect #:signals)
  (:import-from #:aitools.test.support #:json-alist-value #:string-bytes))
