(in-package #:asdf-user)

(defsystem "aitools/feature/md"
  :description "Markdown section commands."
  :depends-on ("aitools/core/kernel" "aitools/core/protocol"
                                     "aitools/core/text"
                                     "aitools/core/workspace"
                                     "aitools/core/store"
                                     "aitools/feature/edit"
                                     "cl-markdown-kit"
                                     "cl-parser-kit"
                                     "cl-codec-kit")
  :pathname "src/"
  :components ((:module "domain" :serial t :components ((:file "package") (:file "document")))
               (:module "application"
                        :depends-on
                        ("domain")
                        :serial
                        t
                        :components
                        ((:file "package") (:file "ports")
                         (:file "read-document")
                         (:file "read-flow")
                         (:file "replace-plan")
                         (:file "replace-flow")))
               (:module "infrastructure"
                        :depends-on
                        ("application")
                        :serial
                        t
                        :components
                        ((:file "package") (:file "markdown-parser") (:file "ports")))))

(defsystem "aitools/feature/md/presentation"
  :description "Markdown section CLI commands."
  :depends-on ("aitools/cli/foundation" "aitools/feature/md" "cl-cli")
  :pathname "src/"
  :components ((:module "presentation"
                        :serial
                        t
                        :components
                        ((:file "package") (:file "commands") (:file "command-registration")))
               (:module "composition"
                        :depends-on
                        ("presentation")
                        :serial
                        t
                        :components
                        ((:file "package") (:file "context-registration")))))

(defsystem "aitools/test/md"
  :description "Markdown context tests."
  :depends-on ("aitools/test/base" "aitools/feature/md" "aitools/feature/md/presentation" "aitools/cli")
  :pathname "../../../"
  :components ((:module "t"
                        :pathname
                        "t/"
                        :serial
                        t
                        :components
                        ((:file "unit/md-package") (:file "unit/md-domain-test")
                         (:file "unit/md-parser-test")
                         (:module "integration"
                                  :pathname
                                  "integration/"
                                  :serial
                                  t
                                  :components
                                  ((:file "md-command-test") (:file "md-write-test")))
                         (:module "e2e" :pathname "e2e/" :serial t :components ((:file "md-cli-test")))))))
