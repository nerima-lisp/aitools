;;;;
;;;; Schema text for the read-only Org commands. AITOOLS.ORG.PRESENTATION
;;;; builds each COMMAND-SCHEMA from this table; CLI option declarations are
;;;; in packages/feature/org/src/presentation/commands.lisp.
(in-package #:aitools.data)

(defparameter *org-command-schemas*
  `((:name "org.outline"
     :summary "List Org headings and named or unnamed source and example blocks."
     :description ,(concatenate 'string
                                "Syntax diagnostics appear in `error.diagnostics` when available. "
                                "Source positions use one-based `line` and `col` fields. Errors include "
                                "at least one repair; "
                                "see [the error envelope](errors.md#the-error-envelope) for repair fields and "
                                "how to use the suggested command. Failed parses return no entries.")
     :args ((:name "path" :kind "positional" :type "string" :required t
             :description "Org file, relative to the working directory.")
            (:name "--limit" :type "integer"
             :description "Maximum entries; 0 returns none. Omit to return all entries."))
     :output-fields ((:name "entries" :description
                      ,(concatenate 'string
                                    "[{kind,name,level,line}] in document order. "
                                    "line is one-based and marks each entry's starting source line. "
                                    "kind is headline, src-block, or example-block. "
                                    "Headline name omits TODO keywords. A block name uses #+NAME, "
                                    "then source language for src-block, or empty for unnamed example-block. "
                                    "level is null for blocks."))
                     (:name "total" :description "All outline entries before --limit.")
                     (:name "truncated" :description "True only when total exceeds --limit."))
     :error-codes ("argument.invalid" "input.not-found" "input.not-utf8"
                   "input.unsupported-format" "input.syntax-error" "refusal.too-large"
                   "environment.io" "internal.unexpected"))
    (:name "org.query"
     :summary "Find Org headings that match an Org match expression."
     :description ,(concatenate 'string
                                "Match syntax errors appear in `error.diagnostics`. "
                                "Source positions use one-based `line` and `col` fields and include a `message`. "
                                "Every error includes at least one repair; see "
                                "[the error envelope](errors.md#the-error-envelope) for its fields and "
                                "how to use the suggested command.")
     :args ((:name "path" :kind "positional" :type "string" :required t
             :description "Org file, relative to the working directory.")
            (:name "--match" :type "string" :required t
             :description ,(concatenate 'string
                                        "Org match syntax covers tag, TODO, level, property, and planning predicates. "
                                        "`+` means AND; a vertical bar separates OR alternatives; for example, "
                                        "`work+LEVEL=2/TODO` combines tag, level, and TODO. "
                                        "Relative planning dates use `--at` as their reference date; for example, "
                                        "`DEADLINE<\"<today>\"`. This is not a title substring search."))
            (:name "--at" :type "string" :required t
             :description ,(concatenate 'string
                                        "Zone-less local datetime in `YYYY-MM-DDTHH:MM:SS` form; "
                                        "seconds must be `00` because the Org time context has minute precision."))
            (:name "--tz" :type "string" :required t
             :description "Resolvable IANA time zone name.")
            (:name "--dst-policy" :type "enum"
             :choices ("compatible" "earlier" "later" "strict")
             :default "compatible"
             :description ,(concatenate 'string
                                        "Resolve DST gaps and overlaps for `--at`. `compatible` chooses the earlier "
                                        "instant for overlaps and moves forward by the gap width. `earlier` chooses "
                                        "the earlier overlap instant and moves backward by the gap width. `later` "
                                        "chooses the later overlap instant and moves forward by the gap width. "
                                        "`strict` rejects gaps and overlaps. Relative planning predicates `SCHEDULED`, "
                                        "`DEADLINE`, and `CLOSED` use local wall time in `--tz`.")))
     :output-fields ((:name "matches" :description
                      ,(concatenate 'string
                                    "[{line,level,title,todo,tags,heading_path}] in document order. line is one-based; "
                                    "title omits its TODO keyword and tags; todo is a keyword or null; "
                                    "tags is a string array. "
                                    "heading_path is an array of `{title,index}` objects from root to match. "
                                    "Each index is the one-based ordinal among siblings with the same title and parent."))
                     (:name "count" :description "Number of matching headings."))
     :error-codes ("argument.invalid" "input.not-found" "input.not-utf8"
                   "input.unsupported-format" "input.syntax-error" "refusal.too-large"
                   "environment.io" "internal.unexpected"))
    (:name "org.export"
     :summary "Export an Org file as HTML, GFM, or ASCII text."
     :description ,(concatenate 'string
                                "Unsupported syntax fails the whole command without content. "
                                "HTTP, HTTPS, and mailto links are supported; other link types return "
                                "`input.unsupported-format`. "
                                "Source-block code and attached `#+RESULTS` blocks are emitted when `:exports` "
                                "is omitted or recognized, regardless of the selected mode. "
                                "Unknown, repeated, or value-less `:exports` declarations fail with "
                                "`input.unsupported-format`. "
                                "Errors include at least one repair; see "
                                "[the error envelope](errors.md#the-error-envelope) for its fields and "
                                "how to use the suggested command. "
                                "Exporter syntax errors include a source location when provided.")
     :args ((:name "path" :kind "positional" :type "string" :required t
             :description "Org file, relative to the working directory.")
            (:name "--backend" :type "enum" :choices ("html" "gfm" "ascii") :required t
             :description "Export backend."))
     :output-fields ((:name "backend" :description "The selected backend.")
                     (:name "content" :description "Complete export text. Present only on success."))
     :error-codes ("argument.invalid" "input.not-found" "input.not-utf8"
                   "input.unsupported-format" "input.syntax-error" "refusal.too-large"
                   "environment.io" "internal.unexpected")))
  "One plist per read-only Org command: :NAME, :SUMMARY, :DESCRIPTION,
:ARGS, :OUTPUT-FIELDS, and :ERROR-CODES in COMMAND-SCHEMA terms.")

(export '(*org-command-schemas*))
