;;;; packages/feature/inspect/src/application/package.lisp
;;;;
;;;; The inspect context's flows (one per command, each calling exactly one
;;;; of the command-result continuations) and its public selector boundary
;;;; for the edit and vcs contexts (selector.lisp; the selector rules are in
;;;; docs/src/reference/commands.md, "Conventions shared by many commands").
(in-package #:cl-user)

(defpackage #:aitools.inspect.application
  (:use #:cl #:aitools.inspect.domain)
  (:import-from #:aitools.protocol.domain #:json-object #:json-null)
  (:import-from #:aitools.kernel.domain
                #:make-range-selector #:make-symbol-selector #:make-between-selector #:make-match-selector
                #:selector-kind #:selector-range-start #:selector-range-end #:selector-match-pattern
                #:selector-invert #:selector-accepts-command-p
                #:content-hash #:sha256-hex #:sha1-hex #:md5-hex #:approx-token-count
                #:parse-duration #:duration-milliseconds #:parse-size #:size-bytes #:parse-range-spec
                #:path-inside-p #:path-relative-to)
  (:import-from #:aitools.text.domain
                #:+binary-sniff-length+ #:binary-octets-p #:utf8-bom-length #:build-line-index
                #:line-index-count #:detect-text-layout #:text-layout-bom-p #:text-layout-line-ending
                #:text-layout-final-newline-p #:utf8-valid-p #:guess-encoding #:guess-mime
                #:find-encoding #:encoding-name #:*supported-encodings* #:language-for-path
                #:decode-utf8-strict/k)
  (:import-from #:aitools.text.application
                #:source-read-octets #:source-read-range #:source-file-size #:source-call-with-chunks)
  (:import-from #:aitools.workspace.domain
                #:normalize-path #:join-path #:path-basename #:aitools-temporary-name-p)
  (:import-from #:aitools.workspace.application
                #:workspace-root-path #:workspace-root-real #:workspace-entry-kind #:workspace-entry-size
                #:workspace-entry-mtime #:workspace-entry-mode #:workspace-entry-name
                #:host-current-directory #:host-stat #:host-list-directory #:host-read-link #:host-read-octets
                #:resolve-real-path
                #:call-with-resolved-root/k #:user-path-absolute #:call-with-workspace-scan/k #:workspace-path-ignored-p
                #:scan-entry-path #:scan-entry-kind #:scan-entry-size #:scan-entry-mtime #:scan-entry-absolute)
  (:import-from #:aitools.store.domain
                #:entry-state-kind #:entry-state-hash #:entry-state-mode #:entry-state-target #:entry-state-mtime #:entry-state-equal
                #:journal-entry-changes #:journal-entry-op-id #:journal-entry-argv
                #:change-result-path #:change-result-action #:change-result-from
                #:change-result-before #:change-result-after #:make-change-result #:action-name
                #:change-diff)
  (:import-from #:aitools.store.application
                #:+default-lock-timeout-ms+ #:call-with-tx-view/k #:tx-record-read/k
                #:path-hash
                #:view-path-state #:view-read-file #:view-directory-entries
                #:find-journal-entry #:read-journal #:read-blob #:workspace-state
                #:store-io-port #:store-io-read-file #:store-io-create-file #:store-io-rename
                #:store-io-mkdir #:store-io-list-directory #:store-io-lstat #:store-io-now
                #:store-io-random-hex)
  ;; snapshot-flows.lisp
  (:import-from #:aitools.store.application #:store-state-directory)
  (:import-from #:aitools.kernel.domain #:parse-size #:size-bytes)
  (:import-from #:aitools.text.domain #:language-path-predicate)
  (:import-from #:aitools.workspace.application #:+default-skip-larger-than+)
  (:export
   ;; ports.lisp
   #:inspect-ports
   #:inspect-ports-p
   #:make-inspect-ports
   ;; selector.lisp (public API for edit and vcs, see inspect-api.md)
   #:parse-selector-options/k
   #:resolve-selector/k
   ;; read-flow.lisp
   #:read-flow
   ;; info-flow.lisp
   #:info-flow
   ;; check-flow.lisp
   #:check-flow
   ;; diff-flow.lisp
   #:diff-flow
   ;; === json-query flows ===
   #:json-get-flow
   #:json-select-flow
   #:json-diff-flow
   ;; === end json-query flows ===
   ;; === table flows ===
   #:table-read-flow
   #:table-agg-flow
   ;; === end table flows ===
   ;; === archive flows ===
   #:archive-list-flow
   #:archive-read-flow
   ;; === end archive flows ===
   ;; === snapshot flows ===
   #:snapshot-create-flow
   #:snapshot-diff-flow
   ;; === end snapshot flows ===
   ))
