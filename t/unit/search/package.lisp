;;;; t/unit/search/package.lisp
(in-package #:cl-user)
(defpackage #:aitools.search.test
  (:use #:cl #:aitools.search.domain #:aitools.search.application)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave
                #:it #:it-each #:it-property #:gen-integer
                #:expect #:fail)
  (:import-from #:aitools.test.support #:json-alist-value #:string-bytes))
