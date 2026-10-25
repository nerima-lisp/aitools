(in-package #:cl-user)

(defpackage #:aitools.protocol.test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:it-property #:gen-string
                #:expect #:signals #:fail)
  (:import-from #:aitools.test.support #:json-alist #:json-alist-value)
  (:import-from #:aitools.data #:*protocol-error-codes*)
  (:import-from #:aitools.protocol.domain
                #:make-ok-envelope #:make-error-envelope #:json-object-from-alist
                #:error-code-exit-code #:redact-secrets #:secret-key-name-p #:shell-quote #:command-line
                #:json-null #:json-or-null #:repair #:schema-repair
                #:repairs-for-unknown-name
                #:make-command-schema #:command-schema-name)
  (:import-from #:aitools.protocol.application
                #:normalize-command-continuations
                #:call-with-command-result/k #:command-result-kind #:command-result-fields
                #:redact-json-value #:render-command-summary #:render-command-detail))
