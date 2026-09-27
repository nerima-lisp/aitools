;;;; packages/core/store/src/application/package.lisp
;;;;
;;;; The store context's public application boundary: the workspace and tx
;;;; locks, the write protocol and recovery, the journal, blobs and
;;;; undo, and the tx overlay. Every entry point takes a STORE (the
;;;; filesystem port, the workspace root, and its state directory) and
;;;; workspace-relative path strings; no function here reads the process
;;;; environment or the current directory.
(in-package #:cl-user)

(defpackage #:aitools.store.application
  (:use #:cl #:aitools.store.domain)
  (:export
   ;; port.lisp
   #:store-io
   #:make-store-io
   #:copy-store-io
   #:store-io-p
   #:store-io-lstat
   #:store-io-read-file
   #:store-io-create-file
   #:store-io-append-file
   #:store-io-rename
   #:store-io-unlink
   #:store-io-rmdir
   #:store-io-mkdir
   #:store-io-chmod
   #:store-io-symlink
   #:store-io-set-mtime
   #:store-io-list-directory
   #:store-io-try-lock
   #:store-io-unlock
   #:store-io-sleep
   #:store-io-monotonic-ms
   #:store-io-now
   #:store-io-random-hex
   #:store-io-error
   #:store-io-error-operation
   #:store-io-error-path
   #:store-io-error-errno
   #:store-io-error-detail
   #:store-committed-error
   #:store-committed-error-op-id
   #:store
   #:make-store
   #:state-directory-for-root
   #:workspace-state-root
   #:store-p
   #:store-io-port
   #:store-root
   #:store-state-directory
   #:store-temporary
   #:*fault-hook*
   #:fault-point
   ;; lock.lisp
   #:+default-lock-timeout-ms+
   #:call-with-workspace-lock/k
   #:with-workspace-lock
   #:call-with-tx-lock/k
   #:with-tx-lock
   ;; files.lisp
   #:workspace-state
   #:workspace-mtime
   ;; blobs.lisp
   #:write-blob
   #:read-blob
   #:collect-garbage
   ;; journal.lisp
   #:read-journal
   #:map-journal-entries
   #:find-journal-entry
   ;; write-protocol.lisp
   #:commit-changes/k
   ;; recovery.lisp
   #:recover/k
   ;; undo.lisp
   #:undo-op/k
   ;; view.lisp
   #:store-view
   #:store-view-p
   #:store-view-store
   #:disk-view
   #:view-path-state
   #:view-path-kind
   #:view-path-mtime
   #:view-read-file
   #:view-directory-entries
   #:path-hash
   ;; tx.lisp
   #:tx-status
   #:tx-status-p
   #:tx-status-id
   #:tx-status-name
   #:tx-status-created
   #:tx-status-ops
   #:tx-status-paths
   #:tx-status-drift
   #:tx-status-stale-reads
   #:tx-begin/k
   #:tx-list
   #:tx-status/k
   #:call-with-tx-view/k
   #:tx-record-read/k
   #:tx-abort/k
   ;; tx-stage.lisp
   #:tx-stage/k
   #:tx-drop/k
   #:tx-rebase/k
   ;; tx-commit.lisp
   #:tx-diff/k
   #:tx-commit/k))
