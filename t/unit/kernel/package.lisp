;;;; t/unit/kernel/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.kernel.test
  (:use #:cl #:aitools.kernel.domain)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:expect #:expect-not #:signals #:fail)
  (:import-from #:aitools.test.support #:string-bytes))
