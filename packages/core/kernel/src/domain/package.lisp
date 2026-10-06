;;;; packages/core/kernel/src/domain/package.lisp
;;;;
;;;; kernel is the only context with no application/infrastructure layer
;;;; (docs/src/reference/architecture.md): every value type here is a pure computation over
;;;; already-read bytes or strings, shared by every other context's domain
;;;; layer. Nothing in this package performs I/O.
(in-package #:cl-user)

(defpackage #:aitools.kernel.domain
  (:use #:cl)
  (:export
   ;; path.lisp
   #:workspace-path
   #:make-workspace-path
   #:workspace-path-absolute
   #:workspace-path-real
   #:workspace-path-relative
   #:workspace-path-p
   #:path-inside-p
   #:path-relative-to
   ;; selector.lisp
   #:selector
   #:make-old-selector
   #:make-range-selector
   #:make-symbol-selector
   #:make-between-selector
   #:make-match-selector
   #:selector-kind
   #:selector-p
   #:selector-old
   #:selector-range-start
   #:selector-range-end
   #:selector-symbol-name
   #:selector-symbol-kind
   #:selector-between-start
   #:selector-between-end
   #:selector-exclusive
   #:selector-match-pattern
   #:selector-invert
   #:parse-range-spec
   #:selector-basis
   #:selector-uniqueness
   #:selector-accepts-command-p
   #:*selector-catalog*
   ;; guard.lisp
   #:parse-expect-hash-argument
   #:expect-hash-entry
   #:make-expect-hash-entry
   #:expect-hash-entry-p
   #:expect-hash-entry-path
   #:expect-hash-entry-hash
   #:guard-required-p
   ;; duration.lisp
   #:duration
   #:duration-p
   #:parse-duration
   #:duration-milliseconds
   #:invalid-duration-error
   #:invalid-duration-error-text
   ;; time.lisp
   #:+unix-epoch-universal-time+
   #:universal-time-to-unix-seconds
   #:unix-seconds-to-universal-time
   ;; size.lisp
   #:size
   #:size-p
   #:parse-size
   #:size-bytes
   #:invalid-size-error
   #:invalid-size-error-text
   ;; digest.lisp
   #:sha256-hex
   #:sha1-hex
   #:md5-hex
   #:content-hash
   ;; token-estimate.lisp
   #:approx-token-count
   ;; unified-diff.lisp
   #:diff-line
   #:make-diff-line
   #:diff-line-p
   #:diff-line-op
   #:diff-line-text
   #:diff-line-no-newline
   #:diff-hunk
   #:make-diff-hunk
   #:diff-hunk-p
   #:diff-hunk-old-start
   #:diff-hunk-old-count
   #:diff-hunk-new-start
   #:diff-hunk-new-count
   #:diff-hunk-lines
   #:generate-diff-hunks
   #:generate-unified-diff
   #:render-hunks
   ;; unified-diff-patch.lisp
   #:split-diff-lines
   #:file-patch
   #:make-file-patch
   #:file-patch-p
   #:file-patch-old-path
   #:file-patch-new-path
   #:file-patch-hunks
   #:parse-unified-diff
   #:strip-path-components
   #:reverse-hunk
   #:apply-hunks/k
   ;; json.lisp
   #:parse-json-pointer
   #:format-json-pointer
   #:json-pointer-array-index
   #:json-equal
   #:iso8601-utc
   #:octal-mode))
