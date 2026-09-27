;;;; t/unit/protocol/package.lisp
(in-package #:cl-user)

(defpackage #:aitools.protocol.test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:it-property #:gen-string
                #:expect #:expect-not #:signals #:fail)
  (:import-from #:aitools.test.support #:json-alist #:json-alist-value)
  (:import-from #:aitools.protocol.domain
                #:make-ok-envelope #:make-error-envelope #:json-object-from-alist
                #:error-code-exit-code #:error-code-known-p
                #:redact-secrets #:secret-key-name-p #:shell-quote #:command-line
                #:top-level-command-p #:group-command-p #:repairs-for-unknown-name
                #:make-command-schema #:command-schema-name)
  (:import-from #:aitools.protocol.application
                #:call-with-command-result/k #:command-result-kind #:command-result-fields
                #:redact-json-value #:render-command-summary #:render-command-detail
                #:unknown-command-error))
