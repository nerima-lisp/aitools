(in-package #:cl-user)
(defpackage #:aitools.net.test
  (:use #:cl #:aitools.net.domain #:aitools.net.infrastructure)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:expect #:expect-not #:signals #:fail))