;;;; t/unit/vcs/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.vcs.test
  (:use #:cl #:aitools.vcs.domain #:aitools.vcs.application)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:expect #:expect-not #:signals #:fail)
  (:import-from #:aitools.test.support #:json-alist-value #:string-bytes))
