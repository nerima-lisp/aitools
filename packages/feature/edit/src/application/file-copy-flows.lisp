(in-package #:aitools.edit.application)

(defun %walk-tree (context directory function)
  "Call FUNCTION (relative-path kind) for every entry below DIRECTORY in the
write's view, parents before children, in path order."
  (dolist (entry (aitools.store.application:view-directory-entries (write-context-view context) directory))
    (let ((path (%child directory (car entry))))
      (funcall function path (cdr entry))
      (when (eq (cdr entry) :directory)
        (%walk-tree context path function)))))

(defun %replace-rebase (path from to)
  "PATH (below FROM, \"\" for the workspace root) moved below TO."
  (if (zerop (length from))
      (%child to path)
      (concatenate 'string to (subseq path (length from)))))

(defun %copy-tree-requests/k (context source destination max-bytes reject on-requests)
  "`copy --recursive` of directory SOURCE to DESTINATION: ON-REQUESTS
(requests files skipped). Symlinks whose target leaves the workspace are
skipped; the total file size is bounded by MAX-BYTES."
  (declare (type function reject on-requests))
  (let ((requests (list (aitools.store.domain:mkdir-request destination)))
        (files 0) (bytes 0) (skipped '())
        (view (write-context-view context)))
    (%walk-tree
     context source
     (lambda (path kind)
       (let ((target (%replace-rebase path source destination))
             (state (aitools.store.application:view-path-state view path)))
         ;; A nested .git (or a child a symlinked parent would take
         ;; outside the root) must be refused, not copied in, the same as
         ;; every other write.
         (resolve-extra-path/k
          context target :base :root
          :on-outside (lambda (message)
                        (return-from %copy-tree-requests/k
                          (funcall reject "refusal.outside-workspace" message)))
          :on-inside (lambda (relative) (declare (ignore relative))))
         (ecase kind
           (:directory (push (aitools.store.domain:mkdir-request target) requests))
           (:file
            (let ((content (aitools.store.application:view-read-file view path)))
              (incf bytes (length content))
              (when (> bytes max-bytes)
                (return-from %copy-tree-requests/k
                  (funcall reject "refusal.too-large"
                           (format nil "copying ~A exceeds --max-bytes ~D" source max-bytes))))
              (incf files)
              (push (aitools.store.domain:write-file-request target content
                                                            :mode (aitools.store.domain:entry-state-mode state))
                    requests)))
           (:symlink
            (let* ((link-target (aitools.store.domain:entry-state-target state))
                   (parent (aitools.workspace.domain:path-parent (concatenate 'string "/" target))))
              (resolve-extra-path/k
               context (if (aitools.workspace.domain:absolute-path-p link-target)
                           link-target
                           (aitools.workspace.domain:normalize-path
                            (aitools.workspace.domain:join-path (subseq (or parent "/") 1) link-target)))
               :on-inside (lambda (resolved)
                            (declare (ignore resolved))
                            (push (aitools.store.domain:symlink-request target link-target) requests))
               :on-outside (lambda (message)
                             (declare (ignore message))
                             (push (json-object "path" path "reason" "symlink-outside-workspace") skipped)))))))))
    (funcall on-requests (nreverse requests) files (nreverse skipped))))
