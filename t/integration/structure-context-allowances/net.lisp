(in-package #:aitools.integration.structure-test)

(setf *loaded-context-structure-data*
      '(:context "net"
        :symbol-allowances
        (
          ("net" (:composition) "CL-BOUNDARY-KIT" "BOUNDARY-CONTEXT-WITH"
               "Registers net capabilities in the composition boundary context.")
          ("net" (:composition) "CL-BOUNDARY-KIT" "MAKE-BOUNDARY-CONTEXT"
               "Creates the boundary context for net capabilities.")
          ("net" (:infrastructure) "HTTP-KIT" "HTTP-HEADER-CONTENT"
               "Reads response header values through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT" "HTTP-HEADER-NAME"
               "Reads response header names through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT" "HTTP-HEADER-VALUES"
               "Reads response headers through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT" "HTTP-REQUEST-URI"
               "Reads the request URI through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT" "HTTP-RESPONSE-BODY"
               "Reads the response body through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT" "HTTP-RESPONSE-HEADERS"
               "Reads response headers through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT" "HTTP-RESPONSE-STATUS"
               "Reads the response status through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT" "HTTP-SIZE-LIMIT-EXCEEDED"
               "Maps the cl-http-kit 0.5.0 response size limit condition.")
          ("net" (:infrastructure) "HTTP-KIT" "HTTP-TIMEOUT"
               "Maps the cl-http-kit 0.5.0 transport timeout condition.")
          ("net" (:infrastructure) "HTTP-KIT" "HTTP-UNSUPPORTED-FEATURE"
               "Maps unsupported cl-http-kit 0.5.0 transport features.")
          ("net" (:infrastructure) "HTTP-KIT" "HTTP-URI-AUTHORITY"
               "Validates URL authority through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT" "HTTP-URI-HOST"
               "Reads URL hosts through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT" "HTTP-URI-P"
               "Recognizes parsed URLs through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT" "HTTP-URI-PORT"
               "Reads URL ports through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT" "HTTP-URI-SCHEME"
               "Reads URL schemes through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT" "HTTP-URI-STRING"
               "Formats request URLs through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT" "MAKE-HTTP-HEADER"
               "Constructs request headers through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT" "PARSE-HTTP-URI"
               "Parses HTTP URLs through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT/CLIENT" "HTTP-CLIENT-SEND"
               "Sends bounded HTTP requests through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT/CLIENT" "MAKE-HTTP-CLIENT"
               "Builds the bounded cl-http-kit 0.5.0 client.")
          ("net" (:infrastructure) "HTTP-KIT/CLIENT" "MAKE-HTTP-CONTENT-DECODERS"
               "Selects supported content decoders from cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT/CLIENT" "MAKE-HTTP-REDIRECT-POLICY"
               "Disables the cl-http-kit 0.5.0 redirect handler.")
          ("net" (:infrastructure) "HTTP-KIT/CLIENT" "MAKE-HTTP-RETRY-POLICY"
               "Disables automatic retries in the cl-http-kit 0.5.0 client.")
          ("net" (:infrastructure) "HTTP-KIT/CLIENT" "RESOLVE-HTTP-URI"
               "Resolves redirect references through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "HTTP-KIT/NETWORK" "+NETWORK-IPV4-ADDRESS-TYPE+"
               "Selects the pinned numeric IPv4 address opener.")
          ("net" (:infrastructure) "HTTP-KIT/NETWORK" "+NETWORK-IPV6-ADDRESS-TYPE+"
               "Selects the pinned numeric IPv6 address opener.")
          ("net" (:infrastructure) "HTTP-KIT/TLS" "MAKE-HTTP-TLS-UPGRADER"
               "Builds the required-verification TLS layer through cl-http-kit 0.5.0.")
          ("net" (:infrastructure) "SB-INTROSPECT" "FUNCTION-LAMBDA-LIST"
               "Checks the required arity of the pinned numeric address opener.")
          ("net" (:infrastructure) "SB-THREAD" "JOIN-THREAD"
               "Waits for resolver workers only until the request deadline.")
          ("net" (:infrastructure) "SB-THREAD" "MAKE-SEMAPHORE"
               "Limits outstanding OS resolver workers.")
          ("net" (:infrastructure) "SB-THREAD" "MAKE-THREAD"
               "Runs blocking OS resolution outside the deadline caller.")
          ("net" (:infrastructure) "SB-THREAD" "SIGNAL-SEMAPHORE"
               "Releases the OS resolver worker limit when lookups finish.")
          ("net" (:infrastructure) "SB-THREAD" "WAIT-ON-SEMAPHORE"
               "Bounds outstanding OS resolver work by the request deadline.")
        )
))
