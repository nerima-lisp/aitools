;;;; t/unit/inspect/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.inspect.test
  (:use #:cl #:aitools.inspect.domain #:aitools.inspect.application)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:expect #:expect-not #:signals #:fail)
  (:import-from #:aitools.test.support #:string-bytes #:json-alist #:json-alist-value))
