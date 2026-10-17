(in-package #:asdf-user)

(defsystem "aitools/feature/util"
  :description "Utility codecs, calculations, UUIDs, and random strings."
  :depends-on ("cl-codec-kit" "cl-host-kit")
  :pathname "src/"
  :components
  ((:module "domain"
    :components ((:file "package") (:file "codec") (:file "text-stats")
                 (:file "calc") (:file "uuid") (:file "random-string")))
   (:module "application"
    :components ((:file "package") (:file "ports") (:file "input")
                 (:file "flows")))
   (:module "infrastructure"
    :components ((:file "package") (:file "os-random") (:file "ports")))))
