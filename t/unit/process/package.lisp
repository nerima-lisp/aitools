;;;; t/unit/process/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.process.test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:expect #:expect-not #:signals #:fail)
  (:import-from #:aitools.test.support #:json-alist #:json-alist-value #:string-bytes))
