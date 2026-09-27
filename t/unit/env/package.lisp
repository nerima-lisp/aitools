;;;; t/unit/env/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.env.test
  (:use #:cl #:aitools.env.domain #:aitools.env.application)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:expect #:expect-not #:signals #:fail))
