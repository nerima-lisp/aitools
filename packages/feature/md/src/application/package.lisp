(in-package #:cl-user)

(defpackage #:aitools.md.application
            (:use #:cl)
            (:import-from #:aitools.protocol.domain #:json-object #:json-null #:repair)
            (:export #:+md-max-input-bytes+
                     #:md-ports
                     #:make-md-ports
                     #:md-ports-workspace-host
                     #:md-ports-text-source
                     #:md-ports-open-store
                     #:md-ports-read-stdin-octets
                     #:md-ports-parse-markdown
                     #:md-ports-splice-section
                     #:md-headings/k
                     #:md-read/k
                     #:md-replace/k))
