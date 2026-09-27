;;;; packages/feature/vcs/src/domain/package.lisp
;;;;
;;;; Pure parsing and shaping for the read-only `git` group
;;;; (docs/src/reference/commands.md). cl-vcs-kit parses only porcelain-v2 status and numstat; the log,
;;;; blame, and patch formats it returns as raw text are parsed here, the
;;;; hunks into the kernel's DIFF-HUNK values (see diff.lisp).
(in-package #:cl-user)

(defpackage #:aitools.vcs.domain
  (:use #:cl)
  (:import-from #:aitools.protocol.domain #:json-object-from-alist #:json-null)
  (:export
   ;; render.lisp
   #:command-line
   #:epoch-seconds-to-iso8601
   ;; paths.lisp
   #:repository-relative-path
   #:path-from-directory
   ;; status.lisp
   #:status-fields
   ;; log.lisp
   #:*log-format*
   #:map-log-records
   #:log-item
   ;; blame.lisp
   #:map-blame-lines
   #:blame-line
   ;; diff.lisp
   #:split-patch-by-file
   #:diff-files/k
   ;; blob.lisp
   #:line-window/k
   #:split-object-spec))
