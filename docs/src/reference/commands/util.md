# The util context

[Command index](index.md#command-index).

### `util encode` {#util-encode}

Encode the input bytes as base64, URL percent-encoding, or hex.

| Argument | Type | Default | Description |
|---|---|---|---|
| `scheme`, required | enum: `base64`, `url`, `hex` |  | base64: RFC 4648 standard alphabet with padding. url: RFC 3986 percent-encoding (unreserved bytes kept). hex: lowercase pairs. |
| `--content` | string |  | Input text; commands that work on bytes use its UTF-8 encoding. |
| `--content-file` | path |  | Read the input as raw bytes from this file (at most 64 MiB). Reads are not limited to the workspace. |
| `--stdin` | flag |  | Read the input as UTF-8 text from standard input (at most 64 MiB). Standard input is never read otherwise. |

| Output field | Description |
|---|---|
| `output` | The encoded ASCII text. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `environment.io`.

### `util decode` {#util-decode}

Decode base64, URL percent-encoding, or hex; binary results are returned as hex, or written to a file with --to.

| Argument | Type | Default | Description |
|---|---|---|---|
| `scheme`, required | enum: `base64`, `url`, `hex` |  | base64: RFC 4648 standard alphabet with padding. url: RFC 3986 percent-encoding (unreserved bytes kept). hex: lowercase pairs. |
| `--content` | string |  | Input text; commands that work on bytes use its UTF-8 encoding. |
| `--content-file` | path |  | Read the input as raw bytes from this file (at most 64 MiB). Reads are not limited to the workspace. |
| `--stdin` | flag |  | Read the input as UTF-8 text from standard input (at most 64 MiB). Standard input is never read otherwise. |
| `--to` | path |  | Write the decoded bytes, as they are, to this path through the atomic write protocol (journaled; undo deletes the file). The path must not exist (refusal.exists) and must be inside the workspace or the mktemp area. |
| `--dry-run` | flag |  | With --to: validate and show the change without writing. |
| `--tx` | string |  | With --to: stage the write in this tx instead of the working tree. |

| Output field | Description |
|---|---|
| `bytes` | Decoded byte count. |
| `output` | The decoded bytes as text, when they are valid UTF-8 (not with --to). |
| `binary` | true when the decoded bytes are not valid UTF-8; `output` is then absent (not with --to). |
| `output_hex` | The decoded bytes as lowercase hex, present only with binary:true (not with --to). |
| `changes` | With --to: the standard write output, one created file. |
| `op_id` | With --to: the journal op (undo it to remove the file); tx and tx_op with --tx, dry_run with --dry-run. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `environment.io`, `input.syntax-error`, `refusal.exists`, `refusal.outside-workspace`, `refusal.redacted-input`, `refusal.not-a-file`, `environment.busy`.

### `util redact` {#util-redact}

Mask known secret formats in the input text.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--content` | string |  | Input text; commands that work on bytes use its UTF-8 encoding. |
| `--content-file` | path |  | Read the input as raw bytes from this file (at most 64 MiB). Reads are not limited to the workspace. |
| `--stdin` | flag |  | Read the input as UTF-8 text from standard input (at most 64 MiB). Standard input is never read otherwise. |

| Output field | Description |
|---|---|
| `text` | The input with each secret replaced by [REDACTED_SECRET]. Invalid UTF-8 in --content-file becomes U+FFFD. |
| `redactions` | Number of masked regions. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `environment.io`.

### `util tokens` {#util-tokens}

Measure the input: approximate tokens, characters, bytes, lines, words, longest line.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--content` | string |  | Input text; commands that work on bytes use its UTF-8 encoding. |
| `--content-file` | path |  | Read the input as raw bytes from this file (at most 64 MiB). Reads are not limited to the workspace. |
| `--stdin` | flag |  | Read the input as UTF-8 text from standard input (at most 64 MiB). Standard input is never read otherwise. |

| Output field | Description |
|---|---|
| `approx_tokens` | ceiling(chars / 4); not tied to any model's tokenizer. |
| `chars` | Unicode scalar values (invalid UTF-8 in --content-file counts one U+FFFD per invalid sequence). |
| `bytes` | Input size in bytes. |
| `lines` | Newline count, plus one for an unterminated last line. |
| `words` | Runs of non-whitespace separated by ASCII whitespace. |
| `max_line_chars` | Longest line in characters, excluding CR LF. |

Errors: `argument.invalid`, `input.not-found`, `input.not-utf8`, `environment.io`.

### `util calc` {#util-calc}

Evaluate integer, decimal, and rational arithmetic with arbitrary precision.

| Argument | Type | Default | Description |
|---|---|---|---|
| `expression` | string |  | Integers, decimals (1.5), + - * / % **, parentheses, min max abs floor ceil round. ** binds tighter than unary minus and is right-associative; its exponent must be an integer. % takes the divisor's sign. round rounds half away from zero. No variables, assignment, or other functions. At most 4096 characters, 64 nesting levels, and 65536 bits per intermediate value. Exactly one of expression or --stdin. An expression that starts with - must follow -- (aitools util calc -- -2**2). |
| `--stdin` | flag |  | Read the expression from standard input. |
| `--decimals` | integer | `10` | Fractional digits of result, 0 to 1000, rounded half away from zero; trailing zeros are dropped. |

| Output field | Description |
|---|---|
| `input` | The evaluated expression. |
| `result` | The value in decimal, as a string so big integers stay exact. |
| `exact` | The exact value as N/D; present only when the value is not an integer. |

Errors: `argument.invalid`, `input.syntax-error`, `input.not-utf8`, `environment.io`.

### `util uuid` {#util-uuid}

Generate RFC 9562 UUIDs from the OS cryptographic random source.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--kind` | enum: `v4`, `v7` | `v4` | v4: random. v7: Unix-millisecond timestamp plus a counter; values from one call strictly increase. |
| `--count` | integer | `1` | 1 to 1000. |

| Output field | Description |
|---|---|
| `values` | Lowercase 8-4-4-4-12 UUID strings. |

Errors: `argument.invalid`.

### `util random` {#util-random}

Generate uniformly random strings over hex, alnum, or base64url.

| Argument | Type | Default | Description |
|---|---|---|---|
| `--length` | integer | `32` | 1 to 4096. |
| `--alphabet` | enum: `hex`, `alnum`, `base64url` | `hex` | hex: 0-9a-f. alnum: A-Za-z0-9. base64url: A-Za-z0-9-_. |
| `--count` | integer | `1` | 1 to 1000. |

| Output field | Description |
|---|---|
| `values` | The generated strings. |

Errors: `argument.invalid`.

