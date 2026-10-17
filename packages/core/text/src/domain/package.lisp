;;;; packages/core/text/src/domain/package.lisp
;;;;
;;;; Pure byte and text rules shared by every reading and writing context:
;;;; binary sniffing, BOM /
;;;; line-ending / final-newline detection, byte line indexing, UTF-8 and
;;;; legacy-charset codecs, encoding and MIME guessing, Unicode
;;;; normalization, the language table, and the byte-format codecs (deflate,
;;;; gzip, zip, tar) that both the inspect and edit contexts need.
;;;;
;;;; UTF-8 decoding lives here rather than in infrastructure because it is a
;;;; pure computation over cl-codec-kit, a pure kit the layer table allows
;;;; only in domain.
(in-package #:cl-user)

(defpackage #:aitools.text.domain
  (:use #:cl)
  (:export
   ;; binary.lisp
   #:+binary-sniff-length+
   #:binary-octets-p
   ;; layout.lisp
   #:utf8-bom-length
   #:line-ending-style
   #:final-newline-p
   #:text-layout
   #:text-layout-p
   #:text-layout-bom-p
   #:text-layout-line-ending
   #:text-layout-final-newline-p
   #:detect-text-layout
   ;; line-index.lisp
   #:map-lines
   #:do-lines
   #:count-lines
   #:line-index
   #:line-index-p
   #:build-line-index
   #:line-index-count
   #:line-index-bounds
   ;; utf8.lisp
   #:decode-utf8
   #:decode-utf8-strict/k
   #:utf8-valid-p
   #:encode-utf8
   ;; charset.lisp
   #:*supported-encodings*
   #:find-encoding
   #:encoding-name
   #:decode-octets/k
   #:encode-string/k
   ;; encoding-guess.lisp
   #:guess-encoding
   ;; mime.lisp
   #:guess-mime
   ;; normalize.lisp
   #:normalize-text
   ;; lines.lisp
   #:split-text-lines
   #:decode-text-lines
   ;; language.lisp
   #:language
   #:language-p
   #:language-name
   #:language-extensions
   #:language-filenames
   #:language-line-comment
   #:language-block-comment
   #:language-extent
   #:language-identifier
   #:language-definitions
   #:find-language
   #:language-for-path
   #:language-path-predicate
   #:language-names
   ;; codec-conditions.lisp
   #:archive-error
   #:archive-error-reason
   #:archive-limit-exceeded
   #:archive-limit-exceeded-limit
   #:archive-unsupported
   ;; archive-model.lisp
   #:archive-entry
   #:archive-entry-p
   #:archive-entry-format
   #:archive-entry-name
   #:archive-entry-kind
   #:archive-entry-size
   #:archive-entry-mode
   #:archive-entry-mtime
   #:archive-entry-link-target
   #:archive-member
   #:archive-member-p
   #:make-archive-member
   #:archive-member-name
   #:archive-member-kind
   #:archive-member-data
   #:archive-member-mode
   #:archive-member-mtime
   #:archive-member-link-target
   #:archive-entry-path-problem
   #:archive-link-target-problem
   ;; codec-crc32.lisp
   #:crc32
   ;; codec-deflate.lisp
   #:inflate
   #:deflate
   ;; codec-gzip.lisp
   #:gzip-member-header
   #:gzip-member-name
   #:gzip-decompress
   #:gzip-compress
   ;; codec-zip.lisp
   #:days-from-civil
   #:read-zip-entries
   #:write-zip
   ;; codec-tar.lisp
   #:read-tar-entries
   #:write-tar
   ;; archive-data.lisp
   #:archive-entry-data
   ;; archive-format.lisp
   #:detect-archive-format
   #:archive-format-name))
