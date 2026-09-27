;;;; data/presentation/util/command-schema-data.lisp
;;;;
;;;; Schema text for the `util` group, one entry
;;;; per command. AITOOLS.UTIL.PRESENTATION builds each COMMAND-SCHEMA from
;;;; this table; the cl-cli option definitions live beside the handlers. The
;;;; codec-scheme and random-alphabet choices come from the same
;;;; AITOOLS.DATA vocabularies the domain layer consumes, so the schema and
;;;; the running command cannot name different sets.
(in-package #:aitools.data)

(defparameter *util-input-args*
  '((:name "--content" :type "string"
     :description "Input text; commands that work on bytes use its UTF-8 encoding.")
    (:name "--content-file" :type "path"
     :description "Read the input as raw bytes from this file (at most 64 MiB). Reads are not limited to the workspace.")
    (:name "--stdin" :type "flag"
     :description "Read the input as UTF-8 text from standard input (at most 64 MiB). Standard input is never read otherwise."))
  "Exactly one of these is required.")

(defparameter *util-input-error-codes*
  '("argument.invalid" "input.not-found" "input.not-utf8" "environment.io"))

(defparameter *util-scheme-arg*
  `(:name "scheme" :type "enum" :required t :choices ,*util-codec-schemes*
    :description "base64: RFC 4648 standard alphabet with padding. url: RFC 3986 percent-encoding (unreserved bytes kept). hex: lowercase pairs."))

(defparameter *util-command-schemas*
  `((:name "encode"
     :summary "Encode the input bytes as base64, URL percent-encoding, or hex."
     :args ,(cons *util-scheme-arg* *util-input-args*)
     :output-fields ((:name "output" :description "The encoded ASCII text."))
     :error-codes ,*util-input-error-codes*)
    (:name "decode"
     :summary "Decode base64, URL percent-encoding, or hex; binary results are returned as hex, or written to a file with --to."
     :args ,(append (cons *util-scheme-arg* *util-input-args*)
                    '((:name "--to" :type "path"
                       :description "Write the decoded bytes, as they are, to this path through the atomic write protocol (journaled; undo deletes the file). The path must not exist (refusal.exists) and must be inside the workspace or the mktemp area.")
                      (:name "--dry-run" :type "flag" :description "With --to: validate and show the change without writing.")
                      (:name "--tx" :type "string" :description "With --to: stage the write in this tx instead of the working tree.")))
     :output-fields ((:name "bytes" :description "Decoded byte count.")
                     (:name "output" :description "The decoded bytes as text, when they are valid UTF-8 (not with --to).")
                     (:name "binary" :description "true when the decoded bytes are not valid UTF-8; `output` is then absent (not with --to).")
                     (:name "output_hex" :description "The decoded bytes as lowercase hex, present only with binary:true (not with --to).")
                     (:name "changes" :description "With --to: the standard write output, one created file.")
                     (:name "op_id" :description "With --to: the journal op (undo it to remove the file); tx and tx_op with --tx, dry_run with --dry-run."))
     :error-codes ,(append *util-input-error-codes*
                           '("input.syntax-error" "refusal.exists" "refusal.outside-workspace" "refusal.redacted-input"
                             "refusal.not-a-file" "environment.busy")))
    (:name "redact"
     :summary "Mask known secret formats in the input text."
     :args ,*util-input-args*
     :output-fields ((:name "text" :description "The input with each secret replaced by [REDACTED_SECRET]. Invalid UTF-8 in --content-file becomes U+FFFD.")
                     (:name "redactions" :description "Number of masked regions."))
     :error-codes ,*util-input-error-codes*)
    (:name "tokens"
     :summary "Measure the input: approximate tokens, characters, bytes, lines, words, longest line."
     :args ,*util-input-args*
     :output-fields ((:name "approx_tokens" :description "ceiling(chars / 4); not tied to any model's tokenizer.")
                     (:name "chars" :description "Unicode scalar values (invalid UTF-8 in --content-file counts one U+FFFD per invalid sequence).")
                     (:name "bytes" :description "Input size in bytes.")
                     (:name "lines" :description "Newline count, plus one for an unterminated last line.")
                     (:name "words" :description "Runs of non-whitespace separated by ASCII whitespace.")
                     (:name "max_line_chars" :description "Longest line in characters, excluding CR LF."))
     :error-codes ,*util-input-error-codes*)
    (:name "calc"
     :summary "Evaluate integer, decimal, and rational arithmetic with arbitrary precision."
     :args ((:name "expression" :type "string"
             :description "Integers, decimals (1.5), + - * / % **, parentheses, min max abs floor ceil round. ** binds tighter than unary minus and is right-associative; its exponent must be an integer. % takes the divisor's sign. round rounds half away from zero. No variables, assignment, or other functions. At most 4096 characters, 64 nesting levels, and 65536 bits per intermediate value. Exactly one of expression or --stdin. An expression that starts with - must follow -- (aitools util calc -- -2**2).")
            (:name "--stdin" :type "flag" :description "Read the expression from standard input.")
            (:name "--decimals" :type "integer" :default 10
             :description "Fractional digits of result, 0 to 1000, rounded half away from zero; trailing zeros are dropped."))
     :output-fields ((:name "input" :description "The evaluated expression.")
                     (:name "result" :description "The value in decimal, as a string so big integers stay exact.")
                     (:name "exact" :description "The exact value as N/D; present only when the value is not an integer."))
     :error-codes ("argument.invalid" "input.syntax-error" "input.not-utf8" "environment.io"))
    (:name "uuid"
     :summary "Generate RFC 9562 UUIDs from the OS cryptographic random source."
     :args ((:name "--kind" :type "enum" :default "v4" :choices ("v4" "v7")
             :description "v4: random. v7: Unix-millisecond timestamp plus a counter; values from one call strictly increase.")
            (:name "--count" :type "integer" :default 1 :description "1 to 1000."))
     :output-fields ((:name "values" :description "Lowercase 8-4-4-4-12 UUID strings."))
     :error-codes ("argument.invalid"))
    (:name "random"
     :summary "Generate uniformly random strings over hex, alnum, or base64url."
     :args ((:name "--length" :type "integer" :default 32 :description "1 to 4096.")
            (:name "--alphabet" :type "enum" :default "hex" :choices ,(mapcar #'car *util-random-alphabets*)
             :description "hex: 0-9a-f. alnum: A-Za-z0-9. base64url: A-Za-z0-9-_.")
            (:name "--count" :type "integer" :default 1 :description "1 to 1000."))
     :output-fields ((:name "values" :description "The generated strings."))
     :error-codes ("argument.invalid")))
  "One plist per `util` command: :NAME, :SUMMARY, :ARGS, :OUTPUT-FIELDS,
:ERROR-CODES, in COMMAND-SCHEMA terms. `util` commands carry no top-level
:DESCRIPTION.")

(export '(*util-input-args* *util-input-error-codes* *util-scheme-arg* *util-command-schemas*))
