(in-package #:aitools.edit.application)

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
