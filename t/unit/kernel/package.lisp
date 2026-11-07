(in-package #:cl-user)

(defpackage #:aitools.kernel.test
  (:use #:cl #:aitools.kernel.domain)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:it-property #:gen-integer #:gen-list #:gen-string #:expect #:expect-not #:signals #:fail)
  (:import-from #:aitools.test.support #:string-bytes))
