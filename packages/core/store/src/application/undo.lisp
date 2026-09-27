;;;; packages/core/store/src/application/undo.lisp
;;;;
;;;; Undo: under the workspace lock, confirm every path of the op is
;;;; still in its `after` state, then write the `before` states back through
;;;; the write protocol as a new op whose journal entry records `undoes`. Undoing that
;;;; op again is a redo, since its `before` is the original `after`.
(in-package #:aitools.store.application)

(defun %undo-conflicts (store entry)
  "CONFLICTs (kind :write, base = the op's recorded `after`) for every path
that changed since ENTRY, including a new entry inside a directory the op
created (removing that directory would delete someone else's file)."
  (let ((conflicts '())
        (touched (journal-entry-paths entry))
        (targets (mapcar #'change-result-path (journal-entry-changes entry))))
    (flet ((check (path expected)
             (let ((current (if (entry-state-mtime expected)
                                (%state-with-mtime store path)
                                (workspace-state store path))))
               (unless (entry-state-equal expected current)
                 (push (make-conflict :path path :kind :write :base expected :current current)
                       conflicts)))))
      (dolist (change (journal-entry-changes entry))
        (let ((path (change-result-path change))
              (after (change-result-after change)))
          (check path after)
          (when (and (change-result-from change)
                     (not (member (change-result-from change) targets :test #'string=)))
            (check (change-result-from change) (absent-state)))
          (when (and (eq (change-result-action change) :created)
                     (eq (entry-state-kind after) :directory)
                     (eq (%kind-at store (%workspace-path store path)) :directory))
            (dolist (child (%list-children store path))
              (unless (member child touched :test #'string=)
                (push (make-conflict :path child :kind :write :base (absent-state)
                                     :current (workspace-state store child))
                      conflicts)))))))
    (sort conflicts #'string< :key #'conflict-path)))

(defun %restore-request (store path state)
  (ecase (entry-state-kind state)
    (:absent (delete-request path))
    (:file (write-file-request path (read-blob store (entry-state-hash state)) :mode (entry-state-mode state)))
    (:directory (mkdir-request path))
    (:symlink (symlink-request path (entry-state-target state)))))

(defun %inverse-requests (store entry)
  "Requests returning ENTRY's paths to their `before` states, in reverse
order of the original changes (so children go before the directories that
contain them). A `touch` of an existing file (CHANGE-KEEPS-CONTENT-P)
is undone in place, by setting the recorded mtime back.

A file deletion immediately superseded by a move onto the same path is
dropped: the rename replaces the file by itself. This is the redo of an
undone overwriting move, whose entry holds `moved src<-dst` and `created
dst`; deleting dst first and then moving onto it would be two changes to
one path in one op."
  (let ((requests
          (loop for change in (reverse (journal-entry-changes entry))
                for path = (change-result-path change)
                for before = (change-result-before change)
                append (ecase (change-result-action change)
                         (:created (list (delete-request path)))
                         (:modified (list (if (change-keeps-content-p change)
                                          (mtime-request path (entry-state-mtime before) :mode (entry-state-mode before))
                                          (%restore-request store path before))))
                         ((:deleted :linked) (list (%restore-request store path before)))
                         (:moved (cons (move-request path (change-result-from change))
                                       (unless (entry-state-absent-p before)
                                         (list (%restore-request store path before)))))
                         (:mode-changed (list (chmod-request path (entry-state-mode before))))))))
    (loop for (request . rest) on requests
          unless (and (eq (change-request-op request) :delete)
                      (member (%kind-at store (%workspace-path store (change-request-path request)))
                              '(:file :symlink))
                      (find-if (lambda (later)
                                 (and (eq (change-request-op later) :move)
                                      (string= (change-request-path later) (change-request-path request))))
                               rest))
            collect request)))

(defun undo-op/k (store op-id argv &key (lock-timeout-ms +default-lock-timeout-ms+) dry-run
                                     on-committed on-rejected on-busy)
  "Undo OP-ID. Continuations as for COMMIT-CHANGES/K; the new op's
journal entry has `undoes` = OP-ID. Rejections:
  input.not-found          no such op in the journal
  refusal.target-changed   some path is no longer in its `after` state;
                           :CONFLICTS lists them and nothing is written"
  (commit-changes/k store argv
                    (lambda (commit reject)
                      (let ((entry (find-journal-entry store op-id)))
                        (if (null entry)
                            (funcall reject "input.not-found" (format nil "no op ~A in the journal" op-id))
                            (let ((conflicts (%undo-conflicts store entry)))
                              (if conflicts
                                  (funcall reject "refusal.target-changed"
                                           (format nil "~D path~:P changed after ~A" (length conflicts) op-id)
                                           :conflicts conflicts)
                                  (funcall commit (%inverse-requests store entry)))))))
                    :lock-timeout-ms lock-timeout-ms
                    :dry-run dry-run
                    :undoes op-id
                    :on-committed on-committed
                    :on-rejected on-rejected
                    :on-busy on-busy))
