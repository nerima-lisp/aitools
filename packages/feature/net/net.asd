(in-package #:asdf-user)
(defsystem "aitools/feature/net"
  :description "Read-only HTTP and OS resolver commands."
  :depends-on ("aitools/core/kernel"
               "aitools/core/protocol"
               "aitools/core/text"
               "cl-boundary-kit"
               "cl-http-kit/client"
               "cl-http-kit/tls")
  :pathname "src/"
  :components
  ((:module "domain" :serial t
    :components ((:file "package")
                 (:file "model")))
   (:module "application" :depends-on ("domain") :serial t
    :components ((:file "package")
                 (:file "http-url-redaction")
                 (:file "http-flow")
                 (:file "resolve-flow")))
   (:module "infrastructure" :depends-on ("domain" "application") :serial t
    :components ((:file "package")
                 (:file "http-address-policy")
                 (:file "resolver")
                 (:file "http-transport")
                 (:file "http-client")))))
(defsystem "aitools/feature/net/presentation"
  :description "Net CLI commands."
  :depends-on ("aitools/cli/foundation"
               "aitools/feature/net"
               "aitools/data"
               "cl-cli")
  :pathname "src/"
  :components
  ((:module "presentation" :serial t
    :components ((:file "package")
                 (:file "commands")))
   (:module "composition" :depends-on ("presentation") :serial t
    :components ((:file "package")
                 (:file "context-registration")))))
(defsystem "aitools/test/net"
  :description "Network context tests."
  :depends-on ("aitools/test/base"
               "aitools/feature/net"
               "aitools/feature/net/presentation"
               "aitools/cli")
  :pathname "../../../"
  :components
  ((:module "t" :pathname "t/" :serial t
    :components ((:file "unit/net/package")
                 (:file "unit/net/resolver-test")
                 (:file "unit/net/resolver-worker-test")
                 (:file "unit/net/resolver-edge-test")
                 (:file "unit/net/resolve-flow-test")
                 (:file "unit/net/http-client-test")
                 (:file "unit/net/http-client-edge-test")
                 (:file "unit/net/http-transport-edge-test")
                 (:file "unit/net/http-address-policy-test")
                 (:file "unit/net/http-address-policy-edge-test")
                 (:file "unit/net/http-ssrf-edge-test")
                 (:file "unit/net/http-redirect-test")
                 (:file "unit/net/flows-test")
                 (:file "unit/net/http-flow-edge-test")
                 (:file "unit/net/commands-test")
                 (:file "integration/net-boundary-contract-test")
                 (:file "integration/net-http-transport-test")))))
