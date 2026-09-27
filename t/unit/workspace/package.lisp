;;;; t/unit/workspace/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.workspace.test
  (:use #:cl #:aitools.workspace.domain #:aitools.workspace.application)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:expect #:expect-not #:signals #:fail)
  (:import-from #:aitools.test.support #:string-bytes))
