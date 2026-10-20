;;;; data/presentation/org/command-schema-data.lisp
;;;;
;;;; Schema text for the read-only Org commands. AITOOLS.ORG.PRESENTATION
;;;; builds each COMMAND-SCHEMA from this table; CLI options live beside the
;;;; handlers.
(in-package #:aitools.data)

(defparameter *org-command-schemas*
  `((:name "org.outline"
     :summary "List Org headings and named or unnamed source and example blocks."
     :description ,(concatenate 'string
                                "Reads one UTF-8 Org file. Syntax errors include source diagnostics with "
                                "one-based line and column. All errors include a repair; failed parses "
                                "return no entries.")
     :args ((:name "path" :kind "positional" :type "string" :required t
             :description "Org file, relative to the working directory.")
            (:name "--limit" :type "integer"
             :description "Maximum entries; 0 returns none. Omit to return all entries."))
     :output-fields ((:name "entries" :description
                      ,(concatenate 'string
                                    "[{kind,name,level,line}] in document order. kind is headline, src-block, or "
                                    "example-block. Headline name omits TODO keywords. A block name uses #+NAME, "
                                    "then source language for src-block, or empty for unnamed example-block. "
                                    "level is null for blocks."))
                     (:name "total" :description "All outline entries before --limit.")
                     (:name "truncated" :description "True only when total exceeds --limit."))
     :error-codes ("argument.invalid" "input.not-found" "input.not-utf8"
                   "input.unsupported-format" "input.syntax-error" "refusal.too-large"
                   "environment.io" "internal.unexpected"))
    (:name "org.query"
     :summary "Find Org headings that match an Org match expression."
     :description ,(concatenate (quote string)
                                "MATCH uses cl-org-kit Org match syntax. --at and --tz are required. "
                                "--at is a zone-less local datetime in YYYY-MM-DDTHH:MM:SS form; seconds must be 00. "
                                "--tz is an IANA zone name; --dst-policy defaults to compatible. "
                                "Match syntax errors include source diagnostics with one-based line and col fields "
                                "and a message field. Every error has at least one repair.")
     :args ((:name "path" :kind "positional" :type "string" :required t
             :description "Org file, relative to the working directory.")
            (:name "--match" :type "string" :required t
             :description "Org match expression, not a title substring search.")
            (:name "--at" :type "string" :required t
             :description "Zone-less local datetime YYYY-MM-DDTHH:MM:SS; seconds must be 00.")
            (:name "--tz" :type "string" :required t
             :description "Resolvable IANA time zone name.")
            (:name "--dst-policy" :type "enum"
             :choices ("compatible" "earlier" "later" "strict")
             :default "compatible" :description "Resolve daylight-saving gaps and overlaps."))
     :output-fields ((:name "matches" :description
                      ,(concatenate 'string
                                    "[{line,level,title,todo,tags,heading_path}] in document order. line is one-based; "
                                    "title is raw text; todo is a keyword or null; tags is a string array. "
                                    "heading_path is [{title,index}] from the root with one-based sibling "
                                    "ordinals."))
                     (:name "count" :description "Number of matching headings."))
     :error-codes ("argument.invalid" "input.not-found" "input.not-utf8"
                   "input.unsupported-format" "input.syntax-error" "refusal.too-large"
                   "environment.io" "internal.unexpected"))
    (:name "org.export"
     :summary "Export an Org file as HTML, GFM, or ASCII text."
     :description ,(concatenate 'string
                                "Returns the complete export as a string in content. Unsupported syntax fails "
                                "the whole command without content. File, ID, and internal links return "
                                "input.unsupported-format until link resolution is available. Errors have a "
                                "nonempty repairs list; exporter syntax errors include a source location when "
                                "provided by the exporter.")
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
