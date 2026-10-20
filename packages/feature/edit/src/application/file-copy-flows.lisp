(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow
 (%copy-tree-requests/k context source destination max-bytes &key on-error on-requests)
 "`copy --recursive` of directory SOURCE to DESTINATION: ON-REQUESTS
(requests files skipped). Symlinks whose target leaves the workspace are
skipped; the total file size is bounded by MAX-BYTES."
 (let ((requests (list (aitools.store.domain:mkdir-request destination)))
       (files 0)
       (bytes 0)
       (skipped '())
       (failure nil)
       (view (write-context-view context)))
   (dolist (entry (%walk-tree context source))
     (unless failure
       (let* ((path (car entry))
              (kind (cdr entry))
              (target (%replace-rebase path source destination))
              (state (aitools.store.application:view-path-state view path)))
         ;; A nested .git or a symlinked parent outside the root must be refused.
         (aitools.kernel.domain:flow-call
          (resolve-extra-path/k context target :base :root)
          (:on-outside (message)
                       (setf failure (list "refusal.outside-workspace" message)))
          (:on-inside (relative)
                      (declare (ignore relative))))

         (unless failure
           (case kind
             (:directory
              (push (aitools.store.domain:mkdir-request target) requests))
             (:file
              (let ((content (aitools.store.application:view-read-file view path)))
                (incf bytes (length content))
                (incf files)
                (push
                 (aitools.store.domain:write-file-request
                  target content
                  :mode (aitools.store.domain:entry-state-mode state))
                 requests)))
             (:symlink
              (let* ((link-target (aitools.store.domain:entry-state-target state))
                     (parent
                      (aitools.workspace.domain:path-parent
                       (concatenate 'string "/" target)))
                     (resolved-link (%copy-link-target link-target parent)))
                (aitools.kernel.domain:flow-call
                 (resolve-extra-path/k context resolved-link)
                 (:on-inside (resolved)
                             (declare (ignore resolved))
                             (push (aitools.store.domain:symlink-request target link-target)
                                   requests))
                 (:on-outside (message)
                              (declare (ignore message))
                              (push (json-object "path" path
                                                 "reason" "symlink-outside-workspace")
                                    skipped))))))
           (when (> bytes max-bytes)
             (setf failure
                   (list "refusal.too-large"
                         (format nil "copying ~A exceeds --max-bytes ~D"
                                 source max-bytes)))))))) (if failure
         (destructuring-bind (code message) failure (flow-continue :on-error code message))
       (aitools.kernel.domain:flow-continue
        :on-requests
        (nreverse requests)
        files
        (nreverse skipped)))))

(defun %copy-link-target (link-target parent)
  (if (aitools.workspace.domain:absolute-path-p link-target)
      link-target
      (aitools.workspace.domain:normalize-path
       (aitools.workspace.domain:join-path
        (subseq (or parent "/") 1)
        link-target))))
