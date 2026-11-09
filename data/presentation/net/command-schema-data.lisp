(in-package #:aitools.data)

(defparameter *net-command-schemas*
  '((:name "net.get"
     :summary "Fetch an HTTP resource with GET."
     :description "Accepts absolute HTTP or HTTPS URLs without userinfo. TLS certificate verification is always required. Follows at most five redirects and refuses HTTPS-to-HTTP downgrade. Public means an address accepted by the net IPv4/IPv6 address filter; loopback, private, link-local, multicast, reserved, and special-use ranges are non-public. An initial URL may target a non-public address. Redirect policy is fixed for the entire chain by the original DNS answers: if all initial answers are non-public, redirects to public or non-public addresses are allowed. If any initial answer is public, the initial request uses only public answers and every redirect hop must resolve entirely to public addresses. Known IPv4-embedded IPv6 forms 64:ff9b::/96, ::ffff:0:0/96, ::/96, 2002::/16, and 2001:0::/32 are classified by their embedded IPv4 address; loopback, private, link-local, and other non-public embedded IPv4 values are non-public for redirect checks. 64:ff9b:1::/48 is non-public. Public native IPv6 is allowed. Network-specific NAT64 prefixes defined by [RFC 6052](https://www.rfc-editor.org/rfc/rfc6052.html) are not detected because Pref64 discovery via [RFC 7050](https://www.rfc-editor.org/rfc/rfc7050.html) is not implemented. An IPv6 address synthesized from a non-public IPv4 address under such a prefix may be misclassified as public and used by an initial request or pass the redirect target filter; this remains a residual risk. Response bodies stay in memory with a 16 MiB hard limit. Authentication headers are not accepted. Body text is masked by the shared secret redactor; secret-named response headers and Authorization, Proxy-Authorization, Cookie, and Set-Cookie values are masked. Secret query parameters in continuation URLs are also masked. Partial body output uses read-style line limits and includes continuation commands."
     :args ((:name "url" :kind "positional" :type "string" :required t :description "Absolute HTTP or HTTPS URL.")
            (:name "--timeout" :type "duration" :default "30s" :description "Total request deadline.")
            (:name "--max-lines" :type "integer" :default 80 :description "Maximum response body lines returned; each output line is capped at 16384 bytes.")
            (:name "--range" :type "string" :description "1-based response body line range, START:END or START:.")
            (:name "--line-offset" :type "integer" :description "Non-negative 0-based Unicode character offset on the first selected line."))
     :output-fields ((:name "status" :description "HTTP response status code.")
                     (:name "headers" :description "Response headers with secret values masked.")
                     (:name "start_line" :description "Number of the first returned body line.")
                     (:name "line_offset" :description "0-based Unicode character offset for the first returned line; present when supplied.")
                     (:name "lines" :description "Returned response body lines, without line terminators.")
                     (:name "total_lines" :description "Response body line count.")
                     (:name "truncated" :description "Whether a line or byte limit cut the displayed body (partial, exit 3).")
                     (:name "encoding_errors" :description "Invalid UTF-8 sequences replaced in returned lines.")
                     (:name "next_commands" :description "Commands to continue each clipped line or fetch the next bounded body range, present for partial output."))
     :error-codes ("argument.invalid" "environment.timeout" "environment.io"
                   "environment.unavailable" "refusal.too-large" "internal.unexpected"))
    (:name "net.head"
     :summary "Fetch HTTP response headers with HEAD."
     :description "Accepts absolute HTTP or HTTPS URLs without userinfo. TLS certificate verification is always required. Follows at most five redirects and refuses HTTPS-to-HTTP downgrade. The net get public-address and redirect policy also applies. Authentication headers are not accepted. Secret-named response headers and Authorization, Proxy-Authorization, Cookie, and Set-Cookie values are masked."
     :args ((:name "url" :kind "positional" :type "string" :required t :description "Absolute HTTP or HTTPS URL.")
            (:name "--timeout" :type "duration" :default "30s" :description "Total request deadline."))
     :output-fields ((:name "status" :description "HTTP response status code.")
                     (:name "headers" :description "Response headers with secret values masked."))
     :error-codes ("argument.invalid" "environment.timeout" "environment.io"
                   "environment.unavailable" "internal.unexpected"))
    (:name "net.resolve"
     :summary "Resolve A and AAAA addresses with the operating system resolver."
     :description "Resolve A and AAAA addresses with the operating system resolver."
     :args ((:name "host" :kind "positional" :type "string" :required t :description "DNS hostname or numeric address.")
            (:name "--timeout" :type "duration" :default "30s" :description "Total resolver deadline."))
     :output-fields ((:name "records" :description "Address results as [{name, type, value}]; type is A or AAAA."))
     :error-codes ("argument.invalid" "environment.timeout" "environment.io"
                   "environment.unavailable" "internal.unexpected"))))

(export '(*net-command-schemas*))
