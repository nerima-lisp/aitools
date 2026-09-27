;;;; packages/core/workspace/src/domain/package.lisp
;;;;
;;;; Pure rules for the workspace boundary and ignore
;;;; decisions: lexical path arithmetic, git's wildmatch, .gitignore
;;;; parsing and precedence, git config and index parsing, and the write
;;;; boundary verdict. Nothing here performs I/O; the application layer reads
;;;; bytes through the WORKSPACE-HOST port and hands them to these functions.
(in-package #:cl-user)

(defpackage #:aitools.workspace.domain
  (:use #:cl)
  (:import-from #:aitools.kernel.domain
                #:path-inside-p
                #:path-relative-to)
  (:export
   ;; path-syntax.lisp
   #:absolute-path-p
   #:normalize-path
   #:join-path
   #:path-components
   #:path-parent
   #:path-basename
   ;; entry.lisp
   #:workspace-entry
   #:make-workspace-entry
   #:workspace-entry-p
   #:workspace-entry-name
   #:workspace-entry-kind
   #:workspace-entry-size
   #:workspace-entry-mtime
   #:workspace-entry-mode
   #:entry-order-key
   #:sort-entries
   ;; wildmatch.lisp
   #:wildmatch
   ;; gitignore.lisp
   #:ignore-pattern
   #:ignore-pattern-p
   #:ignore-pattern-text
   #:ignore-pattern-negative-p
   #:ignore-pattern-directory-only-p
   #:ignore-pattern-basename-only-p
   #:ignore-list
   #:ignore-list-p
   #:ignore-list-base
   #:ignore-list-patterns
   #:ignore-list-source
   #:parse-ignore-lines
   #:parse-ignore-octets
   #:ignore-list-verdict
   #:ignore-stack-verdict
   ;; builtin-excludes.lisp
   #:builtin-ignore-list
   #:aitools-temporary-name-p
   #:git-metadata-name-p
   ;; glob.lisp
   #:glob-filter
   #:make-glob-filter
   #:glob-filter-accepts-p
   ;; git-config.lisp
   #:parse-git-config
   #:git-config-value
   #:git-config-values
   #:git-config-boolean
   #:expand-config-path
   #:decode-git-text
   ;; git-index.lisp
   #:git-index-error
   #:parse-git-index-paths
   #:sorted-paths-contains-p
   #:sorted-paths-have-prefix-p
   ;; repository.lisp
   #:parse-gitdir-file
   #:git-repository
   #:make-git-repository
   #:git-repository-p
   #:git-repository-top
   #:git-repository-git-dir
   #:git-repository-common-dir
   #:workspace-root
   #:make-workspace-root
   #:workspace-root-p
   #:workspace-root-path
   #:workspace-root-real
   #:workspace-root-source
   #:workspace-root-repository
   ;; boundary.lisp
   #:write-target-verdict))
