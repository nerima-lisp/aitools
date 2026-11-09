# The net context

[Command index](index.md#command-index).

### `net get` {#net-get}

Fetch an HTTP resource with GET.

Accepts absolute HTTP or HTTPS URLs without userinfo. TLS certificate verification is always required. Follows at most five redirects and refuses HTTPS-to-HTTP downgrade. Public means an address accepted by the net IPv4/IPv6 address filter; loopback, private, link-local, multicast, reserved, and special-use ranges are non-public. An initial URL may target a non-public address. Redirect policy is fixed for the entire chain by the original DNS answers: if all initial answers are non-public, redirects to public or non-public addresses are allowed. If any initial answer is public, the initial request uses only public answers and every redirect hop must resolve entirely to public addresses. Known IPv4-embedded IPv6 forms 64:ff9b::/96, ::ffff:0:0/96, ::/96, 2002::/16, and 2001:0::/32 are classified by their embedded IPv4 address; loopback, private, link-local, and other non-public embedded IPv4 values are non-public for redirect checks. 64:ff9b:1::/48 is non-public. Public native IPv6 is allowed. Network-specific NAT64 prefixes defined by [RFC 6052](https://www.rfc-editor.org/rfc/rfc6052.html) are not detected because Pref64 discovery via [RFC 7050](https://www.rfc-editor.org/rfc/rfc7050.html) is not implemented. An IPv6 address synthesized from a non-public IPv4 address under such a prefix may be misclassified as public and used by an initial request or pass the redirect target filter; this remains a residual risk. Response bodies stay in memory with a 16 MiB hard limit. Authentication headers are not accepted. Body text is masked by the shared secret redactor; secret-named response headers and Authorization, Proxy-Authorization, Cookie, and Set-Cookie values are masked. Secret query parameters in continuation URLs are also masked. Partial body output uses read-style line limits and includes continuation commands.

| Argument | Type | Default | Description |
|---|---|---|---|
| `url` (positional, required) | string |  | Absolute HTTP or HTTPS URL. |
| `--timeout` | duration | `30s` | Total request deadline. |
| `--max-lines` | integer | `80` | Maximum response body lines returned; each output line is capped at 16384 bytes. |
| `--range` | string |  | 1-based response body line range, START:END or START:. |
| `--line-offset` | integer |  | Non-negative 0-based Unicode character offset on the first selected line. |

| Output field | Description |
|---|---|
| `status` | HTTP response status code. |
| `headers` | Response headers with secret values masked. |
| `start_line` | Number of the first returned body line. |
| `line_offset` | 0-based Unicode character offset for the first returned line; present when supplied. |
| `lines` | Returned response body lines, without line terminators. |
| `total_lines` | Response body line count. |
| `truncated` | Whether a line or byte limit cut the displayed body (partial, exit 3). |
| `encoding_errors` | Invalid UTF-8 sequences replaced in returned lines. |
| `next_commands` | Commands to continue each clipped line or fetch the next bounded body range, present for partial output. |

Errors: `argument.invalid`, `environment.timeout`, `environment.io`, `environment.unavailable`, `refusal.too-large`, `internal.unexpected`.

### `net head` {#net-head}

Fetch HTTP response headers with HEAD.

Accepts absolute HTTP or HTTPS URLs without userinfo. TLS certificate verification is always required. Follows at most five redirects and refuses HTTPS-to-HTTP downgrade. The net get public-address and redirect policy also applies. Authentication headers are not accepted. Secret-named response headers and Authorization, Proxy-Authorization, Cookie, and Set-Cookie values are masked.

| Argument | Type | Default | Description |
|---|---|---|---|
| `url` (positional, required) | string |  | Absolute HTTP or HTTPS URL. |
| `--timeout` | duration | `30s` | Total request deadline. |

| Output field | Description |
|---|---|
| `status` | HTTP response status code. |
| `headers` | Response headers with secret values masked. |

Errors: `argument.invalid`, `environment.timeout`, `environment.io`, `environment.unavailable`, `internal.unexpected`.

### `net resolve` {#net-resolve}

Resolve A and AAAA addresses with the operating system resolver.

| Argument | Type | Default | Description |
|---|---|---|---|
| `host` (positional, required) | string |  | DNS hostname or numeric address. |
| `--timeout` | duration | `30s` | Total resolver deadline. |

| Output field | Description |
|---|---|
| `records` | Address results as [{name, type, value}]; type is A or AAAA. |

Errors: `argument.invalid`, `environment.timeout`, `environment.io`, `environment.unavailable`, `internal.unexpected`.

