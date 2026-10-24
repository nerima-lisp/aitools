(in-package #:cl-user)

(defpackage #:aitools.process.integration-test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect #:describe-skip-if)
  (:import-from #:aitools.test.support #:call-with-process-workspace))

(in-package #:aitools.process.integration-test)

