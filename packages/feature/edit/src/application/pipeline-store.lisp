(in-package #:aitools.edit.application)

(defun %dry-run-in-tx/k (store tx-id validate &key on-planned on-rejected on-not-found)
  (aitools.kernel.domain:flow-call
   (aitools.store.application:call-with-tx-view/k store tx-id)
   (:on-view (view)
     (funcall validate view
                       (lambda (requests)
                         (aitools.kernel.domain:flow-call
                          (aitools.store.domain:plan-changes/k
                           requests
                           :lookup-state
                           (lambda (path)
                             (aitools.store.application:view-path-state view path))
                           :lookup-content
                           (lambda (path)
                             (aitools.store.application:view-read-file view path))
                           :list-children
                           (lambda (path)
                             (mapcar (lambda (entry)
                                       (%child path (car entry)))
                                     (aitools.store.application:view-directory-entries view path))))
                          (:on-planned (results)
                            (funcall on-planned results))
                          (:on-rejected (code message)
                            (funcall on-rejected code message))))
                       on-rejected))
   (:on-not-found ()
     (funcall on-not-found))))

(defun run-write-command/k (ports write-plan &key root lock-timeout dry-run tx display-argv on-ok on-error)
  "Run WRITE-PLAN (a WRITE-PLAN) as one write command and call exactly one of
ON-OK (fields) with the write-output alist (`changes`, then `op_id`, or
`tx`/`tx_op` for --tx, or `dry_run`; the plan's extra fields; then
`next_commands` when a diff was cut) or ON-ERROR (code message &key
candidates diagnostics conflicts repairs), repairs always present.

ROOT is the global --root, LOCK-TIMEOUT the global --lock-timeout text,
DRY-RUN and TX the command's --dry-run and --tx, DISPLAY-ARGV the words the
user typed after the program name (for repair commands; defaults to the
recorded argv). A --dry-run writes nothing, so it needs no --expect-count:
it reports the count it selected as `expect_count` instead."
  (declare (type function on-ok on-error))
  (let* ((command (write-plan-command write-plan))
         (host (edit-ports-workspace-host ports))
         (first-target (first (write-plan-targets write-plan)))
         (display-path (and first-target (write-target-path first-target)))
         (argv (or display-argv
                   (let ((paths (mapcar #'write-target-path (write-plan-targets write-plan))))
                     (options-argv command (funcall (write-plan-record-positionals write-plan) paths)
                                   (%record-options write-plan paths)))))
         (command-line (command-line argv)))
    (labels ((fail (code message &key candidates diagnostics conflicts repairs (path display-path))
               (funcall on-error code message
                        :candidates candidates :diagnostics diagnostics :conflicts conflicts
                        :repairs (or repairs (default-repairs code command argv path)))))
      (%inspect-write-plan/k
       write-plan lock-timeout dry-run argv display-path
       (lambda (lock-timeout-ms hash-entries expect-count)
         (aitools.workspace.application:call-with-resolved-root/k
          host :root root
          :on-error (lambda (reason path)
                      (fail (if (eq reason :not-found) "input.not-found" "argument.invalid")
                            (format nil "workspace root ~A ~:[is not a directory~;does not exist~]" path (eq reason :not-found))
                            :repairs (list (repair "inspect-schema" "Pass an existing directory as --root." "aitools schema"))))
          :on-resolved
          (lambda (workspace-root)
            (%run-resolved ports write-plan workspace-root hash-entries
                           expect-count
                           lock-timeout-ms dry-run tx command-line on-ok #'fail))))
       (lambda (code message repairs)
         (fail code message :repairs repairs))))))

(defun %run-resolved (ports write-plan workspace-root hash-entries expect-count lock-timeout-ms dry-run tx
                      command-line on-ok fail)
  (let* ((host (edit-ports-workspace-host ports))
         (workspace-store (funcall (edit-ports-open-store ports)
                                   (aitools.workspace.application:workspace-root-real workspace-root)))
         ;; Real, like every target the boundary resolves: the state home may
         ;; sit behind a symlink ($XDG_STATE_HOME=/tmp/x with /tmp ->
         ;; /private/tmp), and a mktemp path is compared with it below.
         (temporary-root (let ((tmp (aitools.store.domain:tmp-directory
                                     (aitools.store.application:store-state-directory workspace-store))))
                           (or (aitools.workspace.application:resolve-real-path host tmp) tmp)))
         (targets (write-plan-targets write-plan))
         (command (write-plan-command write-plan)))
    (multiple-value-bind (paths area) (%resolve-targets host workspace-root targets temporary-root fail)
      (unless (or paths (null targets))
        (return-from %run-resolved nil))
      (when (and tx (eq area :temporary))
        (return-from %run-resolved (funcall fail "argument.invalid" "--tx cannot stage writes to the mktemp area")))
      (let* ((store (if (eq area :temporary)
                        (funcall (edit-ports-open-store ports) temporary-root)
                        workspace-store))
             (paths (if (eq area :temporary)
                        (mapcar (lambda (real) (aitools.kernel.domain:path-relative-to temporary-root real)) paths)
                        paths))
             (argv (options-argv command (funcall (write-plan-record-positionals write-plan) paths)
                                 (%record-options write-plan paths)))
             (extra '()))
        (labels ((hash-path (entry)
                   (%resolve-hash-path
                    entry targets paths area
                    (lambda (path)
                      (%resolve-unmatched-hash-path host workspace-root path temporary-root))))
                 (hash-view (view entry-area)
                   ;; An entry in the other area than the write's is read from
                   ;; that area's store on disk (the mktemp area is never staged
                   ;; in a tx), not looked up by its relative name in VIEW.
                   (if (or (null entry-area) (eq entry-area area))
                       view
                       (aitools.store.application:disk-view
                        (if (eq entry-area :temporary)
                            (funcall (edit-ports-open-store ports) temporary-root)
                            workspace-store))))
                 (validate (view commit reject)
                   ;; Commit step 2: everything below reads the state under the lock.
                   (let ((recheck (%resolve-targets host workspace-root targets temporary-root
                                                    (lambda (code message &rest keys)
                                                      (declare (ignore keys))
                                                      (return-from validate (funcall reject code message))))))
                     (when (and (eq area :inside) (not (equal recheck paths)))
                       (return-from validate
                         (funcall reject "refusal.target-changed" "a target path now resolves elsewhere"))))
                   (dolist (entry hash-entries)
                     (multiple-value-bind (path entry-area) (hash-path entry)
                       (let* ((expected (aitools.kernel.domain:expect-hash-entry-hash entry))
                              (actual (and path (view-hash (hash-view view entry-area) path host))))
                         (unless (equal expected actual)
                           (return-from validate
                             (funcall reject "refusal.target-changed"
                                      (format nil "~A changed: expected hash ~A, found ~A" path expected (or actual "none"))
                                      :conflicts (list (%hash-conflict path expected actual))))))))
                   (let ((context (make-write-context :view view :root workspace-root :host host :ports ports
                                                      :paths paths :command command :command-line command-line
                                                      :expect-count expect-count :temporary-root temporary-root
                                                      :hash-paths (mapcar #'hash-path hash-entries)
                                                      :dry-run dry-run :tx tx)))
                     (run-plan context
                               (write-plan-plan write-plan)
                               (lambda (requests more)
                                 (let ((redacted (%redacted-output-path view requests)))
                                   (if redacted
                                       (funcall reject "refusal.redacted-input"
                                                (format nil "the write would put ~A into ~A, which currently has none; ~A is an output mask, not real content"
                                                        +redaction-placeholder+ redacted +redaction-placeholder+))
                                       (progn (setf extra (append more (%selected-count-field write-plan context)))
                                              (funcall commit requests)))))
                               reject)))
                 (rejected (code message &rest keys)
                   (apply fail code message keys))
                 (busy ()
                   (funcall fail "environment.busy" "the workspace lock could not be acquired within --lock-timeout"))
                 (finish (results &rest keys)
                   (funcall on-ok (%with-extra-fields (apply #'aitools.store.domain:write-result-fields results keys)
                                                      extra))))
          (handler-case
            (cond
              ((and tx dry-run)
               (%dry-run-in-tx/k store tx #'validate
                                 :on-planned (lambda (results) (finish results :dry-run t))
                                 :on-rejected #'rejected
                                 :on-not-found (lambda () (%tx-not-found fail tx))))
              (tx
               (aitools.kernel.domain:flow-call
                (aitools.store.application:tx-stage/k
                 store
                 tx
                 argv
                 #'validate
                 :replayable (write-plan-replayable write-plan)
                 :lock-timeout-ms lock-timeout-ms)
                (:on-staged (tx-op results)
                  (finish results :tx tx :tx-op tx-op))
                (:on-rejected (code message &rest keys)
                  (apply #'rejected code message keys))
                (:on-not-found ()
                  (%tx-not-found fail tx))
                (:on-busy ()
                  (funcall #'busy))))
              (t
               (aitools.kernel.domain:flow-call
                (aitools.store.application:commit-changes/k
                 store
                 argv
                 (lambda (commit reject)
                   (validate (aitools.store.application:disk-view store)
                             commit
                             reject))
                 :lock-timeout-ms lock-timeout-ms
                 :dry-run dry-run)
                (:on-committed (op-id results)
                  (if dry-run
                      (finish results :dry-run t)
                      (finish results :op-id op-id)))
                (:on-rejected (code message &rest keys)
                  (apply #'rejected code message keys))
                (:on-busy ()
                  (funcall #'busy)))))
            ;; Outside VALIDATE, too (creating the state directories, taking
            ;; the lock), a store I/O failure is an I/O error, never an
            ;; unexpected one.
            (aitools.store.application:store-io-error (condition)
              (funcall fail "environment.io" (princ-to-string condition)))
            ;; A damaged store record (a hand-edited tx index, a
            ;; truncated state file) is an environment fault the tx tools
            ;; repair, not an internal error escaping as internal.unexpected.
            (aitools.store.domain:store-format-error (condition)
              (funcall fail "environment.io" (princ-to-string condition)
                       :repairs (%damaged-state-repairs tx)))))))))

(defun %tx-not-found (fail tx)
  (funcall fail "input.not-found" (format nil "no open tx ~A" tx)
           :repairs (list (repair "list-tx" "List the open transactions." "aitools tx status"))))

(defun %damaged-state-repairs (tx)
  "Repairs for a STORE-FORMAT-ERROR: inspect or discard the transaction
whose record is damaged, or list open transactions when the write had no tx."
  (if tx
      (list (repair "tx-status" "Inspect the transaction's recorded state."
                    (format nil "aitools tx status ~A" (aitools.protocol.domain:shell-quote tx)))
            (repair "tx-abort" "Discard the damaged transaction and its staged writes."
                    (format nil "aitools tx abort ~A" (aitools.protocol.domain:shell-quote tx))))
      (list (repair "tx-status" "List open transactions and their state." "aitools tx status"))))
