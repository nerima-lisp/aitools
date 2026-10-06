;;;; packages/feature/inspect/src/domain/package.lisp
;;;;
;;;; Pure rules of the inspect context (the file reads, the read side of the
;;;; format groups, and `snapshot`): line decoding, selector resolution, the views
;;;; of `read`, the facts of `info`, `check`'s parsers, `diff`'s
;;;; comparisons, JSON pointers and queries, tables, archives, and snapshot
;;;; records. No I/O happens here.
(in-package #:cl-user)

(defpackage #:aitools.inspect.domain
  (:use #:cl)
  (:import-from #:aitools.protocol.domain
                #:json-object #:json-null #:json-or-null #:shell-quote)
  (:import-from #:aitools.kernel.domain
                #:selector-kind #:selector-range-start #:selector-range-end
                #:selector-symbol-name #:selector-symbol-kind
                #:selector-between-start #:selector-between-end #:selector-exclusive
                #:selector-match-pattern #:selector-invert
                #:generate-diff-hunks #:make-diff-hunk #:make-diff-line
                #:diff-hunk-old-start #:diff-hunk-old-count #:diff-hunk-new-start #:diff-hunk-new-count
                #:diff-hunk-lines #:diff-line-op #:diff-line-text #:diff-line-no-newline)
  (:import-from #:aitools.text.domain
                #:decode-utf8 #:utf8-bom-length #:detect-text-layout #:decode-octets/k
                #:split-text-lines #:decode-text-lines
                #:line-index-bounds
                #:language-name #:language-extent #:language-definitions
                #:language-line-comment #:language-block-comment #:language-for-path)
  ;; archive.lisp
  (:import-from #:aitools.text.domain
                #:read-zip-entries #:read-tar-entries #:gzip-decompress #:gzip-member-header
                #:archive-entry-data #:archive-entry-name #:archive-entry-kind #:archive-entry-size
                #:archive-entry-mode #:archive-entry-mtime
                #:archive-error #:archive-error-reason #:archive-limit-exceeded #:archive-limit-exceeded-limit)
  (:export
   ;; json-values.lisp
   #:json-false
   #:json-bool
   #:json-or-null
   #:json-null-value-p
   #:json-false-value-p
   #:json-object-from-pairs
   #:json-object-value-p
   #:json-object-pairs
   #:json-array-value-p
   #:json-object-get
   #:render-json
   #:parse-json-document/k
   #:json-type-name
   #:json-equal
   ;; lines.lisp
   #:octet-vector
   #:split-text-lines
   #:decode-text-lines
   #:decode-line-range
   #:decode-charset-lines/k
   ;; pattern.lisp
   #:compile-pattern/k
   #:call-with-regex-limit/k
   #:pattern-matches-p
   #:pattern-group-string
   ;; similarity.lisp
   #:edit-distance
   #:rank-similar
   #:shell-quote
   ;; lisp-scan.lisp
   #:lisp-dialect-for-language
   #:scan-lisp-delimiters
   #:lisp-balance-diagnostics
   #:sexp-end-line
   ;; outline.lisp
   #:definition
   #:definition-p
   #:definition-line
   #:definition-end-line
   #:definition-kind
   #:definition-name
   #:find-definitions
   ;; selection.lisp
   #:resolve-line-selector/k
   ;; read-render.lisp
   #:invisible-char-p
   #:escape-invisible
   #:hex-rows
   #:redact-hex-octets
   #:extract-strings
   #:make-strings-scan
   #:scan-strings-chunk
   ;; file-facts.lisp
   #:text-content-counts
   #:line-ending-name
   #:format-file-mode
   #:iso8601-from-unix
   #:check-format-for-path
   #:json-diagnostics
   ;; diff.lisp
   #:diff-key
   #:compare-lines
   #:hunk-line-counts
   #:compare-line-sets
   #:compare-path-lists
   ;; === json-query (json get/select/diff) ===
   #:parse-json-pointer
   #:format-json-pointer
   #:json-child
   #:resolve-json-pointer/k
   #:json-child-names
   #:json-value-length
   #:split-comparison
   #:parse-comparison-value
   #:comparison
   #:comparison-key
   #:comparison-operator
   #:comparison-value
   #:make-comparison/k
   #:comparison-holds-p
   #:json-value-less-p
   #:json-diff-ops
   ;; === end json-query ===
   ;; === table (table read/agg) ===
   #:table
   #:table-format
   #:table-columns
   #:table-types
   #:table-rows
   #:*table-formats*
   #:detect-table-format
   #:parse-table/k
   #:table-column-index
   #:table-comparison/k
   #:table-row-matches-p
   #:table-row-json
   #:table-group
   #:table-group-key
   #:table-group-count
   #:table-group-sum
   #:table-group-min
   #:table-group-max
   #:table-group-average
   #:table-group-distinct-count
   #:non-numeric-cells
   #:aggregate-table
   ;; === end table ===
   ;; === archive ===
   #:+archive-max-decompressed+
   #:+archive-max-entry+
   #:archive-item
   #:archive-item-name
   #:archive-item-kind
   #:archive-item-size
   #:archive
   #:archive-format
   #:archive-items
   #:archive-format-name
   #:open-archive/k
   #:archive-item-content/k
   #:archive-item-json
   #:find-archive-item
   ;; === end archive ===
   ;; === snapshot ===
   #:snapshot-file
   #:make-snapshot-file
   #:snapshot-file-path
   #:snapshot-file-size
   #:snapshot-file-mtime
   #:snapshot-file-hash
   #:snapshot
   #:make-snapshot
   #:snapshot-id
   #:snapshot-files
   #:snapshot-glob
   #:snapshot-lang
   #:snapshot-no-ignore
   #:snapshot-skip-larger-than
   #:snapshot-newer
   #:valid-snapshot-id-p
   #:snapshot-id-from-time
   #:encode-snapshot
   #:decode-snapshot/k
   #:compare-snapshot
   ;; === end snapshot ===
   ))
