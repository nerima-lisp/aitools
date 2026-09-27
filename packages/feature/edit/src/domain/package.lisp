;;;; packages/feature/edit/src/domain/package.lisp
;;;;
;;;; Pure rules of the edit context (text edits, file operations, and the
;;;; write side of the format groups):
;;;; the text document model that preserves BOM, line endings and the final
;;;; newline, `--old` matching and candidate ranking,
;;;; replacement templates, `transform` line operations, the JSON value model
;;;; with RFC 6901/6902/7386, table cells, `split` pieces, and archive
;;;; extraction plans. Nothing here performs I/O.
(in-package #:cl-user)

(defpackage #:aitools.edit.domain
  (:use #:cl)
  (:export
   ;; refusal.lisp
   #:edit-refusal
   #:edit-refusal-code
   #:edit-refusal-detail
   #:refuse
   ;; document.lisp
   #:octets
   #:text-document
   #:text-document-p
   #:text-document-lines
   #:text-document-terminators
   #:text-document-bom-p
   #:text-document-eol
   #:+lf+
   #:+crlf+
   #:make-text-document
   #:decode-text-document/k
   #:document-line-count
   #:document-line
   #:document-final-newline-p
   #:document-text
   #:document-logical-text
   #:document-line-offsets
   #:offset-line-index
   #:render-document
   #:content-lines
   #:document-replace-lines
   #:document-with-logical-text
   #:document-with-eol
   #:document-with-final-newline
   #:document-without-bom
   #:document-with-lines
   ;; old-match.lisp
   #:levenshtein-distance
   #:trim-whitespace
   #:leading-whitespace
   #:similar-windows
   #:find-old/k
   #:reindent-lines
   #:first-indent
   #:apply-old-edit
   ;; template.lisp
   #:+template-filters+
   #:parse-replacement-template
   #:perl-backreferences
   #:rewrite-perl-backreferences
   #:apply-filter
   #:expand-template
   #:template-regex-replacement
   ;; transform.lisp
   #:+line-transform-ops+
   #:+whole-file-transform-ops+
   #:version<
   #:seeded-shuffle
   #:transform-lines
   ;; json-doc.lisp
   #:json-obj
   #:json-obj-p
   #:make-json-obj
   #:json-obj-members
   #:json-num
   #:json-num-p
   #:make-json-num
   #:json-num-text
   #:parse-json-text
   #:parse-json-text/k
   #:json-string-literal
   #:serialize-json
   #:detect-json-indent
   #:parse-json-pointer
   #:format-json-pointer
   #:json-pointer-get
   #:json-add
   #:json-remove
   #:json-replace
   #:json-equal
   #:json-merge-patch
   #:json-apply-patch
   ;; regex.lisp
   #:compile-search-pattern/k
   #:call-with-regex-refusals
   #:regex-group-count
   #:replacer
   #:make-replacer
   #:reset-replacer
   #:replacer-required-literal
   #:octets-search
   #:literal-replacement
   #:replace-document
   #:parse-mtime
   #:iso-utc
   ;; table.lisp
   #:table-delimiter-for-path
   #:table-set-cell
   ;; split.lisp
   #:split-piece
   #:split-piece-start
   #:split-piece-end
   #:split-piece-start-line
   #:split-piece-lines
   #:split-by-lines
   #:split-by-bytes
   #:split-at-matches
   #:split-piece-name
   ;; archive-plan.lisp
   #:archive-format-for-path
   #:parse-archive-format
   #:extract-step
   #:extract-step-kind
   #:extract-step-path
   #:extract-step-data
   #:extract-step-mode
   #:extract-step-target
   #:plan-archive-extraction
   #:build-archive))
