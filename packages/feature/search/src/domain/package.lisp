;;;; packages/feature/search/src/domain/package.lisp
;;;;
;;;; Pure rules of the search context: the byte-level
;;;; matcher behind `search` (regex semantics over cl-regex-kit byte
;;;; regexes, no allocation for non-matching lines, a per-file literal
;;;; prefilter), block merging and match rendering, `find` filtering, sorting,
;;;; and trees, the `code` definition index over the text context's language
;;;; table, and `overview`'s git and language summaries. Nothing here performs
;;;; I/O.
(in-package #:cl-user)

(defpackage #:aitools.search.domain
  (:use #:cl)
  (:import-from #:aitools.protocol.domain
                #:json-object-from-alist #:json-null
                #:shell-quote #:command-line)
  (:import-from #:aitools.text.domain
                #:decode-utf8
                #:utf8-bom-length
                #:binary-octets-p
                #:count-lines
                #:language-name
                #:language-extent
                #:language-identifier
                #:language-definitions
                #:language-for-path)
  (:export
   ;; bytes.lisp
   #:octets
   #:line-content-end
   #:line-start-at
   #:next-line-start
   #:strip-bom
   #:octets-find
   ;; protocol shell-word helpers
   #:shell-quote
   #:command-line
   ;; matcher.lisp
   #:matcher
   #:matcher-p
   #:matcher-programs
   #:build-matcher/k
   #:call-with-regex-budget/k
   #:*regex-run-deadline*
   ;; results.lisp
   #:file-outcome
   #:file-outcome-p
   #:file-outcome-octets
   #:file-outcome-selected
   #:file-outcome-selected-count
   #:file-outcome-match-count
   #:file-outcome-matches
   #:search-file
   #:build-blocks
   #:render-match
   ;; find.lisp
   #:found-entry
   #:make-found-entry
   #:found-entry-path
   #:found-entry-kind
   #:found-entry-size
   #:found-entry-mode
   #:found-entry-mtime
   #:found-entry-json
   #:kind-name
   #:find-pattern-matches-p
   #:entry-depth
   #:sort-found-entries
   #:build-find-tree
   ;; code.lisp
   #:language-index
   #:language-index-for-path
   #:language-index-language
   #:definition
   #:definition-line
   #:definition-end-line
   #:definition-kind
   #:definition-name
   #:file-definitions
   #:line-definition
   #:map-word-occurrences
   ;; overview.lisp
   #:parse-head-file
   #:packed-ref-sha
   #:build-file-name-p
   #:make-language-tally
   #:tally-file
   #:language-tally-rows))
