;;;; packages/core/store/src/domain/package.lisp
;;;;
;;;; Pure models for the store context (the write protocol, journal, state layout, and
;;;; docs/src/reference/transactions.md): entry states, change requests and their planned results,
;;;; the intent record, journal entries with retention, the state directory
;;;; layout, and the tx index/ops/reads records. Nothing here performs I/O;
;;;; the planner takes lookup functions that the application layer backs with
;;;; the filesystem port or a tx overlay.
(in-package #:cl-user)

(defpackage #:aitools.store.domain
  (:use #:cl)
  (:import-from #:aitools.kernel.domain #:content-hash)
  (:import-from #:aitools.protocol.domain #:json-object)
  (:import-from #:aitools.text.domain #:decode-utf8-strict/k)
  (:export
   ;; errors
   #:store-format-error
   #:store-format-error-detail
   ;; octets and json helpers
   #:octets
   #:string-octets
   #:octets-string
   ;; entry-state.lisp
   #:entry-state
   #:entry-state-p
   #:entry-state-kind
   #:entry-state-hash
   #:entry-state-mode
   #:entry-state-target
   #:entry-state-mtime
   #:absent-state
   #:file-state
   #:directory-state
   #:symlink-state
   #:entry-state-absent-p
   #:entry-state-equal
   #:entry-state->json
   #:json->entry-state
   #:valid-blob-hash-p
   ;; layout.lisp
   #:state-home
   #:workspace-id
   #:workspace-state-directory
   #:join-path
   #:lock-file-path
   #:commit-directory
   #:intent-file-path
   #:journal-directory
   #:journal-file-path
   #:blobs-directory
   #:blob-file-path
   #:tx-root-directory
   #:tx-directory
   #:tmp-directory
   #:temp-file-name
   #:temp-file-name-p
   #:format-op-id
   #:format-tx-id
   #:valid-op-id-p
   #:valid-tx-id-p
   #:valid-relative-path-p
   #:parent-relative-path
   #:path-ancestors
   #:path-under-p
   #:git-metadata-path-p
   ;; change.lisp
   #:change-request
   #:change-request-p
   #:change-request-op
   #:change-request-path
   #:change-request-from
   #:change-request-content
   #:change-request-mode
   #:change-request-target
   #:change-request-mtime
   #:write-file-request
   #:delete-request
   #:move-request
   #:chmod-request
   #:symlink-request
   #:mkdir-request
   #:mtime-request
   #:change-result
   #:make-change-result
   #:change-result-p
   #:change-result-path
   #:change-result-action
   #:change-result-from
   #:change-result-before
   #:change-result-after
   #:change-result-source-before
   #:change-result-before-content
   #:change-result-after-content
   #:change-result-hash-before
   #:change-result-hash-after
   #:action-name
   #:parse-action-name
   #:conflict
   #:make-conflict
   #:conflict-p
   #:conflict-path
   #:conflict-kind
   #:conflict-base
   #:conflict-current
   #:conflict->json
   ;; plan.lisp
   #:plan-changes/k
   #:+default-file-mode+
   ;; intent.lisp
   #:intent-step
   #:make-intent-step
   #:intent-step-p
   #:intent-step-op
   #:intent-step-path
   #:intent-step-from
   #:intent-step-temp
   #:intent-step-mode
   #:intent-step-target
   #:intent-step-mtime
   #:intent-step-kind
   #:changes->steps
   #:change-keeps-content-p
   #:intent
   #:make-intent
   #:intent-p
   #:intent-op-id
   #:intent-steps
   #:intent-journal-entry
   #:intent-tx-id
   #:encode-intent-header
   #:encode-intent-checksum
   #:decode-intent
   ;; journal.lisp
   #:journal-entry
   #:make-journal-entry
   #:journal-entry-p
   #:journal-entry-op-id
   #:journal-entry-argv
   #:journal-entry-time
   #:journal-entry-changes
   #:journal-entry-undoes
   #:journal-entry-paths
   #:journal-entry->json
   #:json->journal-entry
   #:encode-journal
   #:decode-journal
   #:retention-removals
   #:+retained-generations+
   #:journal-referenced-blobs
   ;; tx-model.lisp
   #:tx-path
   #:make-tx-path
   #:tx-path-p
   #:tx-path-path
   #:tx-path-base
   #:tx-path-staged
   #:tx-index
   #:make-tx-index
   #:tx-index-p
   #:tx-index-last-tx-op
   #:tx-index-ops-generation
   #:tx-ops-file-name
   #:tx-index-paths
   #:tx-index-find
   #:tx-index-put
   #:tx-index-remove
   #:copy-tx-index-deep
   #:tx-op-record
   #:make-tx-op-record
   #:tx-op-record-p
   #:tx-op-record-tx-op
   #:tx-op-record-argv
   #:tx-op-record-paths
   #:tx-op-record-previous
   #:tx-op-record-after
   #:tx-op-record-replayable
   #:tx-op-record-time
   #:tx-meta
   #:make-tx-meta
   #:tx-meta-p
   #:tx-meta-id
   #:tx-meta-name
   #:tx-meta-created
   #:encode-tx-index
   #:decode-tx-index
   #:encode-tx-ops
   #:decode-tx-ops
   #:encode-tx-reads
   #:decode-tx-reads
   #:encode-tx-meta
   #:decode-tx-meta
   #:tx-index-referenced-blobs
   #:tx-ops-referenced-blobs
   #:tx-drift-paths
   #:tx-commit-conflicts
   #:tx-drop-index
   #:tx-staged-changed-p
   ;; changes-json.lisp
   #:+default-max-diff-lines+
   #:change-diff
   #:changes->json
   #:op-diff-command
   #:write-result-fields
   #:recovered->json))
