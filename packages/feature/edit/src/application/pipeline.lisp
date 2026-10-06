;;;; packages/feature/edit/src/application/pipeline.lisp
;;;;
;;;; RUN-WRITE-COMMAND/K: the path every workspace-changing command takes
;;;; (guards, input, output, redaction refusal, the workspace boundary,
;;;; the store commit, and tx staging; see
;;;; docs/src/reference/commands.md and transactions.md). A command
;;;; describes its write as a WRITE-PLAN (write-plan.lisp: targets, inputs,
;;;; guards, and a PLAN function that turns the current state into change
;;;; requests); this file resolves the root, refuses redacted input and
;;;; missing guards, checks the boundary before and again after taking the
;;;; lock (the checks themselves are in write-guards.lisp), compares
;;;; --expect-hash against the state the write sees (the disk, or the tx
;;;; overlay), and hands the requests to the store's commit, its tx
;;;; staging, or a dry-run plan. It renders the write output with the
;;;; store's WRITE-RESULT-FIELDS, so every write command answers alike.
(in-package #:aitools.edit.application)

;;; -------------------------------------------------------------- the run

(defun %extend-change (change per-change)
  (let* ((members (aitools.protocol.domain:json-object-members change))
         (more (cdr (assoc (cdr (assoc "path" members :test #'string=)) per-change :test #'equal))))
    (if more (aitools.protocol.domain:json-object-from-alist (append members more)) change)))

(defun %record-options (write-plan paths)
  (let ((options (write-plan-record-options write-plan)))
    (if (functionp options) (funcall options paths) options)))

(defun %with-extra-fields (fields extra)
  "FIELDS (from WRITE-RESULT-FIELDS) with the plan's EXTRA string-keyed
fields inserted before `next_commands`. Two keyword entries adjust
`changes` itself: (:PER-CHANGE . ((path . members) ...)) appends members to
that path's change, (:LIMIT-CHANGES . N) keeps the first N changes, and
(:TOTAL-CHANGES . T) adds `total`, the count before that limit."
  (let* ((per-change (cdr (assoc :per-change extra)))
         (limit (cdr (assoc :limit-changes extra)))
         (total (and (assoc :total-changes extra)
                     (list (cons "total" (length (cdr (assoc "changes" fields :test #'string=)))))))
         (plain (append (remove-if (lambda (pair) (keywordp (car pair))) extra) total))
         (fields (mapcar (lambda (pair)
                           (if (string= (car pair) "changes")
                               (let ((changes (mapcar (lambda (change) (%extend-change change per-change)) (cdr pair))))
                                 (cons "changes" (if limit (subseq changes 0 (min limit (length changes))) changes)))
                               pair))
                         fields))
         (next (assoc "next_commands" fields :test #'string=)))
    (append (remove next fields) plain (and next (list next)))))

(defun %catching-refusals (reject thunk)
  "Call THUNK; turn the domain's and the kits' input refusals into REJECT."
  (handler-case (call-with-regex-refusals thunk)
    (aitools.edit.domain:edit-refusal (condition)
      (funcall reject (aitools.edit.domain:edit-refusal-code condition)
               (aitools.edit.domain:edit-refusal-detail condition)))
    (aitools.text.domain:archive-unsupported (condition)
      (funcall reject "input.unsupported-format" (princ-to-string condition)))
    (aitools.text.domain:archive-error (condition)
      (funcall reject "input.syntax-error" (princ-to-string condition)))
    (aitools.store.application:store-io-error (condition)
      (funcall reject "environment.io" (princ-to-string condition)))))

(defun run-plan (context plan commit reject)
  "Call PLAN (from a WRITE-PLAN) against CONTEXT. COMMIT is called with the
requests and the extra fields the plan passed; REJECT as in a store
VALIDATE continuation."
  (declare (type function plan commit reject))
  (%catching-refusals reject
                      (lambda ()
                        (funcall plan context
                                 (lambda (requests &optional extra) (funcall commit requests extra))
                                 reject))))

(defun %dry-run-in-tx/k (store tx-id validate &key on-planned on-rejected on-not-found)
  (aitools.store.application:call-with-tx-view/k
   store tx-id
   :on-view (lambda (view)
              (funcall validate view
                       (lambda (requests)
                         (aitools.store.domain:plan-changes/k
                          requests
                          :lookup-state (lambda (path) (aitools.store.application:view-path-state view path))
                          :lookup-content (lambda (path) (aitools.store.application:view-read-file view path))
                          :list-children (lambda (path)
                                           (mapcar (lambda (entry) (%child path (car entry)))
                                                   (aitools.store.application:view-directory-entries view path)))
                          :on-planned on-planned
                          :on-rejected on-rejected))
                       on-rejected))
   :on-not-found on-not-found))

(defun %inspect-write-plan (write-plan lock-timeout dry-run argv display-path)
  "Return the static inspection of WRITE-PLAN, or its error details.
The result is (:OK LOCK-TIMEOUT-MS HASH-ENTRIES EXPECT-COUNT), or
(:ERROR CODE MESSAGE REPAIRS). No workspace state is read here."
  (multiple-value-bind (lock-timeout-ms lock-timeout-valid) (%lock-timeout-ms lock-timeout)
    (multiple-value-bind (hash-entries hash-error) (%parse-expect-hashes (write-plan-expect-hashes write-plan))
      (let ((expect-count (write-plan-expect-count write-plan)))
        (cond
          ((not lock-timeout-valid)
           (list :error "argument.invalid"
                 (format nil "--lock-timeout ~S is not a duration (<n>ms|s|m|h|d)" lock-timeout)
                 nil))
          (hash-error (list :error "argument.invalid" hash-error nil))
          ((and expect-count (null (parse-count expect-count)))
           (list :error "argument.invalid"
                 (format nil "--expect-count ~S is not a non-negative integer" expect-count)
                 nil))
          (t
           (dolist (requirement (write-plan-guard-requirements write-plan))
             (ecase (first requirement)
               (:expect-hash
                (let ((path (second requirement)))
                  ;; A bare --expect-hash is the first target's (VALIDATE hashes
                  ;; it against that one); any other file of a multi-file write
                  ;; needs its own PATH=HASH.
                  (unless (find-if (lambda (entry)
                                     (let ((entry-path (aitools.kernel.domain:expect-hash-entry-path entry)))
                                       (or (null path) (equal path (or entry-path display-path)))))
                                   hash-entries)
                    (return-from %inspect-write-plan
                      (list :error "argument.invalid"
                            (format nil "this write needs --expect-hash ~:[<hash>~;~:*~A=<hash>~]"
                                    (and (rest (write-plan-targets write-plan)) path))
                            (list (repair "get-hash" "Read the current hash, then pass it as --expect-hash."
                                          (format nil "aitools info ~A"
                                                  (aitools.protocol.domain:shell-quote (or path display-path))))))))))
               (:expect-count
                (unless (or expect-count dry-run)
                  (return-from %inspect-write-plan
                    (list :error "argument.invalid" "this write needs --expect-count N"
                          (list (repair "count" "Count the selection without writing, then pass that --expect-count."
                                        (dry-run-command-line argv)))))))))
           (let ((redacted (find-if #'%contains-placeholder-p (write-plan-inputs write-plan))))
             (when redacted
               (return-from %inspect-write-plan
                 (list :error "refusal.redacted-input"
                       (format nil "the input contains ~A, an output mask rather than real content"
                               +redaction-placeholder+)
                       nil))))
           (list :ok lock-timeout-ms hash-entries
                 ;; Keep this second parse: the original caller parsed the
                 ;; validated text again when entering the resolved phase.
                 (and expect-count (parse-count expect-count)))))))))

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
      (let ((inspection (%inspect-write-plan write-plan lock-timeout dry-run argv display-path)))
        (if (eq (first inspection) :error)
            (destructuring-bind (_ code message repairs) inspection
              (declare (ignore _))
              (return-from run-write-command/k
                (fail code message :repairs repairs)))
            (destructuring-bind (_ lock-timeout-ms hash-entries expect-count) inspection
              (declare (ignore _))
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
                              lock-timeout-ms dry-run tx command-line on-ok #'fail)))))))))

(defun %selected-count-field (write-plan context)
  "`expect_count` for a --dry-run of a write that --expect-count guards: the
count the plan selected, the value a real run passes as --expect-count."
  (let ((count (write-context-selected-count context)))
    (and (write-context-dry-run context)
         count
         (find :expect-count (write-plan-guard-requirements write-plan) :key #'first)
         (list (cons "expect_count" count)))))

(defun %resolve-targets (host workspace-root targets temporary-root fail)
  "(values relative-paths area) with AREA :INSIDE or :TEMPORARY, or NIL after
calling FAIL."
  ;; One realpath cache for this pass over the targets: the mktemp area and
  ;; each shared parent directory resolve once rather than once per target.
  ;; Fresh per call, so the recheck under the lock still reads the disk.
  (let ((aitools.workspace.application:*realpath-cache* (make-hash-table :test 'equal))
        (paths '()) (areas '()))
    (dolist (target targets)
      (unless (%boundary/k host workspace-root
                           (%target-absolute host workspace-root (write-target-path target) (write-target-base target))
                           temporary-root (write-target-follow target)
                           (lambda (real relative verdict)
                             (declare (ignore real))
                             (push relative paths)
                             (pushnew verdict areas)
                             t)
                           (lambda (verdict lexical real)
                             (declare (ignore real))
                             (funcall fail "refusal.outside-workspace" (%outside-message verdict lexical)
                                      :path (write-target-path target))
                             nil))
        (return-from %resolve-targets nil)))
    (when (rest areas)
      (funcall fail "argument.invalid" "one write cannot mix the workspace and the mktemp area")
      (return-from %resolve-targets nil))
    (values (nreverse paths) (or (first areas) :inside))))

(defun %resolve-hash-path (entry targets paths area on-unmatched)
  "Resolve ENTRY to its path and area using already resolved TARGETS.
ON-UNMATCHED resolves a hash entry that does not name one of TARGETS."
  (let* ((path (aitools.kernel.domain:expect-hash-entry-path entry))
         (target-index (and path (position path targets :key #'write-target-path :test #'string=))))
    (cond
      ((null path) (values (first paths) area))
      (target-index (values (nth target-index paths) area))
      (t (funcall on-unmatched path)))))

(defun %resolve-unmatched-hash-path (host workspace-root path temporary-root)
  (block resolved
    (%boundary/k host workspace-root (aitools.workspace.application:user-path-absolute host path)
                 temporary-root t
                 (lambda (real relative verdict)
                   (return-from resolved
                     (if (eq verdict :temporary)
                         (values (aitools.kernel.domain:path-relative-to temporary-root real) :temporary)
                         (values relative :inside))))
                 (lambda (&rest ignore)
                   (declare (ignore ignore))
                   (return-from resolved path)))))

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
