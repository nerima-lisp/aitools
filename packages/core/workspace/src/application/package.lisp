;;;; packages/core/workspace/src/application/package.lisp
;;;;
;;;; The workspace context's public boundary (other contexts reach
;;;; workspace only through this package). It defines the
;;;; WORKSPACE-HOST port and the continuation-passing flows for root
;;;; resolution, the write boundary, and the ignore-aware scan.
(in-package #:cl-user)

(defpackage #:aitools.workspace.application
  (:use #:cl #:aitools.workspace.domain)
  (:import-from #:aitools.kernel.domain
                #:make-workspace-path
                #:path-inside-p
                #:path-relative-to)
  (:export
   ;; re-exported domain values callers receive from the flows below
   #:workspace-entry
   #:workspace-entry-p
   #:make-workspace-entry
   #:workspace-entry-name
   #:workspace-entry-kind
   #:workspace-entry-size
   #:workspace-entry-mtime
   #:workspace-entry-mode
   #:workspace-root
   #:workspace-root-p
   #:workspace-root-path
   #:workspace-root-real
   #:workspace-root-source
   #:workspace-root-repository
   #:git-repository
   #:git-repository-p
   #:git-repository-top
   #:git-repository-git-dir
   #:git-repository-common-dir
   ;; host.lisp
   #:workspace-host
   #:workspace-host-p
   #:make-workspace-host
   #:host-list-directory
   #:host-stat
   #:host-read-link
   #:host-read-octets
   #:host-getenv
   #:host-home-directory
   #:host-current-directory
   #:host-call-with-ordered-mapper
   ;; real-path.lisp
   #:resolve-real-path
   #:*realpath-cache*
   ;; root.lisp
   #:find-git-repository
   #:call-with-resolved-root/k
   #:user-path-absolute
   #:resolve-user-path/k
   #:workspace-relative-path
   ;; boundary.lisp
   #:call-with-workspace-boundary/k
   ;; ignore-context.lisp
   #:ignore-context
   #:ignore-context-p
   #:ignore-context-source
   #:ignore-context-casefold
   #:load-ignore-context
   ;; scan.lisp
   #:workspace-overlay
   #:workspace-overlay-p
   #:make-workspace-overlay
   #:scan-entry
   #:scan-entry-p
   #:scan-entry-path
   #:scan-entry-absolute
   #:scan-entry-name
   #:scan-entry-kind
   #:scan-entry-size
   #:scan-entry-mtime
   #:scan-entry-mode
   #:scan-entry-tracked-p
   #:+default-skip-larger-than+
   #:call-with-workspace-scan/k
   #:workspace-path-ignored-p))
