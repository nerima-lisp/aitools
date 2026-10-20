(in-package #:aitools.edit.application)

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
                                            (funcall reject "refusal.target-changed"
                                                     "a target path now resolves elsewhere"))))
                           (dolist (entry hash-entries)
                             (multiple-value-bind (path entry-area) (hash-path entry)
                               (let* ((expected (aitools.kernel.domain:expect-hash-entry-hash entry))
                                      (actual (and path (view-hash (hash-view view entry-area) path host))))
                                 (unless (equal expected actual)
                                   (return-from validate
                                                (funcall reject "refusal.target-changed"
                                                         (format nil "~A changed: expected hash ~A, found ~A" path
                                                                 expected (or actual
                                                                              "none"))
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
                                                        (format nil
                                                                (concatenate 'string
                                                                             "the write would put ~A into "
                                                                             "~A, which currently has "
                                                                             "none; ~A is an output mask, "
                                                                             "not real content"
                                                                )
                                                                +redaction-placeholder+ redacted
                                                                +redaction-placeholder+))
                                             (progn (setf extra (append more (%selected-count-field write-plan
                                                                                                    context)))
                                               (funcall commit requests)))))
                                       reject)))
                 (rejected (code message &rest keys)
                           (apply fail code message keys))
                 (busy ()
                       (funcall fail "environment.busy"
                                "the workspace lock could not be acquired within --lock-timeout"))
                 (finish (results &rest keys)
                         (funcall on-ok (%with-extra-fields (apply #'aitools.store.domain:write-result-fields results
                                                                   keys)
                                                            extra))))
          (handler-case
              (cond
               ((and tx dry-run)
                (%dry-run-in-tx/k store tx #'validate
                                  :on-planned (lambda (results) (finish results :dry-run t))
                                  :on-rejected #'rejected
                                  :on-not-found (lambda () (%tx-not-found fail tx))))
               (tx
                (aitools.store.application:tx-stage/k
                 store tx argv #'validate
                 :replayable (write-plan-replayable write-plan)
                 :lock-timeout-ms lock-timeout-ms
                 :on-staged (lambda (tx-op results) (finish results :tx tx :tx-op tx-op))
                 :on-rejected #'rejected
                 :on-not-found (lambda () (%tx-not-found fail tx))
                 :on-busy #'busy))
               (t
                (aitools.store.application:commit-changes/k
                 store argv
                 (lambda (commit reject) (validate (aitools.store.application:disk-view store) commit reject))
                 :lock-timeout-ms lock-timeout-ms
                 :dry-run dry-run
                 :on-committed (lambda (op-id results)
                                 (if dry-run (finish results :dry-run t) (finish results :op-id op-id)))
                 :on-rejected #'rejected
                 :on-busy #'busy)))
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
