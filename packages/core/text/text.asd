(in-package #:asdf-user)

(defsystem "aitools/core/text"
           :description "Text, line, encoding, and archive support."
           :depends-on ("aitools/data" "aitools/core/kernel" "cl-codec-kit"
              "cl-boundary-kit" "cl-deflate-kit" "cl-date-kit")
           :pathname "src/"
           :components
           ((:module "domain"
                     :components ((:file "package") (:file "binary") (:file "layout")
                                  (:file "line-index") (:file "utf8") (:file "charset")
                                  (:file "encoding-guess") (:file "mime")
                                  (:file "normalize") (:file "lines") (:file "language")
                                  (:file "codec-conditions") (:file "archive-model")
                                  (:file "codec-crc32") (:file "codec-deflate")
                                  (:file "codec-gzip") (:file "codec-zip")
                                  (:file "codec-zip-write") (:file "codec-tar-pax")
                                  (:file "codec-tar") (:file "codec-tar-write")
                                  (:file "archive-data") (:file "archive-format")))
            (:module "application"
                     :components ((:file "package") (:file "source")))
            (:module "infrastructure" :components ((:file "package") (:file "host-source")))))
(defsystem "aitools/test/text"
           :description "text context tests."
           :depends-on ("aitools/test/base" "aitools/core/text")
           :pathname "../../../t/"
           :serial t
           :components ((:file "unit/text/package")
                        (:file "unit/text/layout-test")
                        (:file "unit/text/charset-test")
                        (:file "unit/text/guess-test")
                        (:file "unit/text/codec-test-support")
                        (:file "unit/text/codec-test")
                        (:file "unit/text/codec-gzip-test")
                        (:file "unit/text/archive-test")
                        (:file "unit/text/archive-zip-test")
                        (:file "unit/text/archive-zip-signature-test")
                        (:file "unit/text/archive-zip-date-test")
                        (:file "unit/text/archive-tar-test")
                        (:file "unit/text/archive-safety-test")
                        (:file "unit/text/archive-metadata-test")
                        (:file "unit/text/source-test")
                        (:file "unit/text/source-range-test")
                        (:file "integration/text-archive-fixtures")
                        (:file "integration/text-archive-interop-test")
                        (:file "integration/text-host-source-test")
                        (:file "integration/text-host-source-range-test")))
