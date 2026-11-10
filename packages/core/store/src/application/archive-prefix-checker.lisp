(in-package #:aitools.store.application)

(defun make-archive-symlink-prefix-checker
    (view boundary archive-symlink-paths &key deletions-visible)
  (let* ((symlink-root (make-hash-table :test 'equal))
         (staged-root (make-hash-table :test 'equal))
         (index (store-view-index view))
         (store (store-view-store view))
         (io (store-boundaries store))
         (open-root (cl-boundary-kit:boundary-context-get io :open-search-root))
         (open-entry (cl-boundary-kit:boundary-context-get io :open-search-entry))
         (close-directory (cl-boundary-kit:boundary-context-get io :close-search-directory))
         (boundary-components
           (remove-if (lambda (component)
                        (or (zerop (length component))
                            (string= component ".")))
                      (aitools.workspace.domain:path-components
                       (aitools.workspace.domain:normalize-path boundary))))
         (root-fd nil)
         (root-identity nil)
         (template-fd nil)
         (template nil)
         (ready nil))
    (unless (and (functionp open-root)
                 (functionp open-entry)
                 (functionp close-directory))
      (error 'store-io-error :operation "open" :path boundary
                             :detail "directory cursor ports are unavailable"))
    (when index
      (loop for entry being the hash-values of (tx-index-paths index)
            for path = (tx-path-path entry)
            for kind = (entry-state-kind (tx-path-staged entry))
            do (progn
                 (%archive-stage-add-path staged-root path kind)
                 (when (eq kind :symlink)
                   (%archive-prefix-add-path symlink-root path)))))
    (let ((archive-node symlink-root))
      (dolist (component boundary-components)
        (setf archive-node (%archive-prefix-node archive-node component)))
      (dolist (path archive-symlink-paths)
        (%archive-prefix-add-path archive-node path)))
    (unwind-protect
         (progn
           (multiple-value-setq (root-fd root-identity)
             (funcall open-root (store-root store)))
           (multiple-value-bind (kind fd identity)
               (funcall open-entry root-fd "." ".")
             (setf template-fd fd)
             (unless (and (eq kind :directory)
                          (equal identity root-identity))
               (error 'store-io-error :operation "open" :path boundary
                                      :detail "workspace root identity changed"))
             (setf template
                   (make-%archive-prefix-cursor
                    :current-fd template-fd
                    :current-identity identity
                    :folded-node symlink-root
                    :stage-node staged-root
                    :symlink-p nil
                    :disk-live-p t
                    :stack nil))
             (setf template-fd nil))
           (dolist (component boundary-components)
             (%archive-prefix-enter template component open-entry
                                    close-directory deletions-visible))
           (let ((fd root-fd))
             (setf root-fd nil)
             (funcall close-directory fd))
           (prog1
               (lambda (action &optional cursor component)
                 (ecase action
                   (:start
                    (%archive-prefix-start template open-entry close-directory))
                   (:finish
                    (%archive-prefix-finish-cursor (or cursor template)
                                                   close-directory))
                   (:enter
                    (%archive-prefix-enter cursor component open-entry
                                           close-directory deletions-visible))
                   (:parent
                    (%archive-prefix-parent cursor open-entry
                                            close-directory))))
             (setf ready t)))
      (unless ready
        (when template
          (%archive-prefix-finish-cursor template close-directory))
        (when template-fd
          (funcall close-directory template-fd))
        (when root-fd
          (funcall close-directory root-fd))))))
