(in-package #:aitools.edit.application)

(defun %selected-count-field (write-plan context)
  "`expect_count` for a --dry-run of a write that --expect-count guards: the
count the plan selected, the value a real run passes as --expect-count."
  (let ((count (write-context-selected-count context)))
    (and (write-context-dry-run context)
         count
         (find :expect-count
               (write-plan-guard-requirements write-plan)
               :key
               #'first)
         (list (cons "expect_count" count)))))

(aitools.kernel.domain:define-flow
 (%resolve-targets host
                   workspace-root
                   targets
                   temporary-root
                   &key
                   on-resolved
                   on-error)
 "Resolve the targets to relative paths and their shared area."
 (let ((aitools.workspace.application:*realpath-cache*
        (make-hash-table :test 'equal))
       (paths '())
       (areas '())
       (failure-code nil)
       (failure-message nil)
       (failure-path nil))
   (dolist (target targets)
     (unless failure-code
       (aitools.kernel.domain:flow-call
        (%boundary/k host
                     workspace-root
                     (%target-absolute host
                                       workspace-root
                                       (write-target-path target)
                                       (write-target-base target))
                     temporary-root
                     (write-target-follow target))
        (:on-inside (real relative verdict)
                    (declare (ignore real))
                    (push relative paths)
                    (pushnew verdict areas))
        (:on-outside (verdict lexical real)
                     (declare (ignore real))
                     (setf failure-code "refusal.outside-workspace"
                           failure-message (%outside-message verdict lexical)
                           failure-path (write-target-path target))))))
   (cond
     (failure-code
      (aitools.kernel.domain:flow-continue :on-error
                                           failure-code
                                           failure-message
                                           :path
                                           failure-path))
     ((rest areas)
      (aitools.kernel.domain:flow-continue :on-error
                                           "argument.invalid"
                                           "one write cannot mix the workspace and the mktemp area"))
     (t
      (aitools.kernel.domain:flow-continue :on-resolved
                                           (nreverse paths)
                                           (or (first areas) :inside))))))

(aitools.kernel.domain:define-flow
 (%resolve-unmatched-hash-path host
                               workspace-root
                               path
                               temporary-root
                               &key
                               on-resolved)
 (aitools.kernel.domain:flow-call
  (%boundary/k host
               workspace-root
               (aitools.workspace.application:user-path-absolute host path)
               temporary-root
               t)
  (:on-inside (real relative verdict)
              (aitools.kernel.domain:flow-continue :on-resolved
                                                   (if (eq verdict :temporary)
                                                       (aitools.kernel.domain:path-relative-to
                                                        temporary-root
                                                        real)
                                                       relative)
                                                   (if (eq verdict :temporary)
                                                       :temporary
                                                       :inside)))
  (:on-outside (verdict lexical real)
               (declare (ignore verdict lexical real))
               (aitools.kernel.domain:flow-continue :on-resolved path nil))))

(defun %resolve-hash-path (entry targets
                                 paths
                                 area
                                 host
                                 workspace-root
                                 temporary-root)
  "Resolve ENTRY against the already-resolved TARGETS. A bare --expect-hash names
its first target. A path naming a target uses its resolved path, so a symlink
remains the write target. Other paths are resolved as ordinary writes."
  (let* ((path (aitools.kernel.domain:expect-hash-entry-path entry))
         (target-index
          (and path
               (position path targets :key #'write-target-path :test #'string=))))
    (cond
      ((null path) (values (first paths) area))
      (target-index (values (nth target-index paths) area))
      (t
       (let ((resolved-path nil)
             (resolved-area nil))
         (aitools.kernel.domain:flow-call
          (%resolve-unmatched-hash-path host workspace-root path temporary-root)
          (:on-resolved (resolved resolved-in)
                        (setf resolved-path resolved
                              resolved-area resolved-in)))
         (values resolved-path resolved-area))))))
