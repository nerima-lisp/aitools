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

(defun %inspect-write-plan/k (write-plan lock-timeout dry-run argv display-path on-ok on-error)
  "Inspect WRITE-PLAN without reading workspace state and call one continuation."
  (multiple-value-bind (lock-timeout-ms lock-timeout-valid) (%lock-timeout-ms lock-timeout)
    (multiple-value-bind (hash-entries hash-error) (%parse-expect-hashes (write-plan-expect-hashes write-plan))
      (let* ((expect-count-text (write-plan-expect-count write-plan))
             (expect-count (and expect-count-text (parse-count expect-count-text))))
        (cond
          ((not lock-timeout-valid)
           (funcall on-error
                    "argument.invalid"
                    (format nil "--lock-timeout ~S is not a duration (<n>ms|s|m|h|d)" lock-timeout)
                    nil))
          (hash-error (funcall on-error "argument.invalid" hash-error nil))
          ((and expect-count-text (null expect-count))
           (funcall on-error
                    "argument.invalid"
                    (format nil "--expect-count ~S is not a non-negative integer" expect-count-text)
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
                    (return-from %inspect-write-plan/k
                      (funcall on-error
                               "argument.invalid"
                               (format nil "this write needs --expect-hash ~:[<hash>~;~:*~A=<hash>~]"
                                       (and (rest (write-plan-targets write-plan)) path))
                               (list (repair "get-hash" "Read the current hash, then pass that as --expect-hash."
                                             (format nil "aitools info ~A"
                                                     (aitools.protocol.domain:shell-quote (or path display-path))))))))))
               (:expect-count
                (unless (or expect-count dry-run)
                  (return-from %inspect-write-plan/k
                    (funcall on-error
                             "argument.invalid" "this write needs --expect-count N"
                             (list (repair "count" "Count the selection without writing, then pass that --expect-count."
                                           (dry-run-command-line argv)))))))))
           (let ((redacted (find-if #'%contains-placeholder-p (write-plan-inputs write-plan))))
             (when redacted
               (return-from %inspect-write-plan/k
                 (funcall on-error
                          "refusal.redacted-input"
                          (format nil "the input contains ~A, an output mask rather than real content"
                                  +redaction-placeholder+)
                          nil))))
           (funcall on-ok lock-timeout-ms hash-entries expect-count)))))))

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
A bare --expect-hash names the first target. A PATH=HASH naming a target uses
that target's resolved path, so a symlink remains the write target. Other
paths are resolved as ordinary writes by ON-UNMATCHED: they use :INSIDE or
:TEMPORARY when they resolve to either area, and otherwise retain the input
path with a NIL area."
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
