(in-package #:asdf-user)

(defsystem "aitools/core/text"
  :description "Text, line, encoding, and archive support."
  :depends-on ("aitools/core/kernel" "cl-codec-kit" "cl-boundary-kit")
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
                 (:file "codec-tar") (:file "archive-data")
                 (:file "archive-format")))
   (:module "application"
    :components ((:file "package") (:file "source")))
   (:module "infrastructure"
    :components ((:file "package") (:file "host-source")
                 (:file "boundaries")))))
