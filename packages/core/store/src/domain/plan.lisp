;;;;
;;;; The planner turns CHANGE-REQUESTs into CHANGE-RESULTs against a view of
;;;; the current state, which the caller supplies as lookup functions (the
;;;; disk under the workspace lock for a direct write, a tx overlay for a transaction).
;;;; Requests are applied in order to a virtual state so that later requests
;;;; see earlier ones: parents auto-created by one write exist for the next,
;;;; a directory whose children an undo deletes first is empty by the time
;;;; its own deletion is planned.
;;;;
;;;; The planner is the store's own last line of defence, not the command's
;;;; validation: callers check selectors, guards and `--overwrite` first. It
;;;; still refuses anything the write protocol cannot apply atomically.
(in-package #:aitools.store.domain)

(defconstant +default-file-mode+ #o644
  "Mode of a newly created regular file when the request names none.")

(aitools.kernel.domain:define-flow
    (plan-changes/k requests &key lookup-state lookup-content list-children (lookup-mtime (constantly nil))
                                   on-planned on-rejected)
  "Plan REQUESTS (a list of CHANGE-REQUEST) and call exactly one of:

- ON-PLANNED (results): RESULTS is a list of CHANGE-RESULT in application
  order, including a `created` directory result for every missing parent.
  A `mkdir` of an existing directory yields no result.
- ON-REJECTED (code message): CODE is a spec error.code string.

LOOKUP-STATE (path) returns the ENTRY-STATE of a workspace-relative path;
LOOKUP-CONTENT (path) returns the bytes of a regular file; LIST-CHILDREN
(path) returns the relative paths of a directory's existing entries;
LOOKUP-MTIME (path) returns a regular file's modification time in Unix
seconds (or NIL), recorded as the `before` of an :MTIME request so undo can
restore it.

An :MTIME request is a `modified` change whose content is unchanged: its
before and after states differ only in mtime (and mode, when it names one)."
  (declare (type function lookup-state lookup-content list-children lookup-mtime on-planned on-rejected))
  (let ((overrides (make-hash-table :test 'equal))
        (roles (make-hash-table :test 'equal))
        (moved-directories '())
        (results '()))
    (labels ((reject (code control &rest arguments)
               (list :rejected code (apply #'format nil control arguments)))
             (state (path)
               (if (zerop (length path))
                   (directory-state)
                   (multiple-value-bind (state found) (gethash path overrides)
                     (if found state (funcall lookup-state path)))))
             (children (path)
               (let ((names (remove-if (lambda (child) (entry-state-absent-p (state child)))
                                       (funcall list-children path))))
                 (loop for child being the hash-keys of overrides using (hash-value state)
                       when (and (string= (parent-relative-path child) path)
                                 (not (entry-state-absent-p state)))
                         do (pushnew child names :test #'string=))
                 names))
             (check-path (path)
               (if (not (valid-relative-path-p path))
                   (reject "argument.invalid" "~S is not a workspace-relative path" path)
                   (loop for moved in moved-directories
                         when (path-under-p moved path)
                           return (reject "argument.invalid"
                                          "~A is inside a directory this operation moves" path))))
             (claim (path role op)
               ;; A path is the target of at most one request, except that
               ;; a move's source may afterwards be written or relinked
               ;; (undo of a move that overwrote its destination needs
               ;; exactly that). Only those two ops: their intent steps
               ;; run in the phase after moves, which is what keeps the
               ;; move step's roll-forward idempotent.
               (let ((previous (gethash path roles)))
                 (if (or (eq previous :target)
                         (and previous (eq role :source))
                         (and (eq previous :source) (not (member op '(:write :symlink)))))
                     (reject "argument.invalid" "~A is changed twice in one operation" path)
                     (progn
                       (setf (gethash path roles) role)
                       nil))))
             (record (result new-state)
               (push result results)
               (setf (gethash (change-result-path result) overrides) new-state)
               nil)
             (ensure-parents (path)
               (loop for ancestor in (path-ancestors path)
                     for existing = (state ancestor)
                     do (ecase (entry-state-kind existing)
                          (:directory)
                          (:absent
                           (record (make-change-result :path ancestor :action :created
                                                       :before existing :after (directory-state))
                                   (directory-state)))
                          ((:file :symlink)
                           (return (reject "refusal.not-a-file" "~A is not a directory" ancestor))))))
             (content-of (path state)
               (when (eq (entry-state-kind state) :file)
                 (coerce (funcall lookup-content path) 'octets)))
             (plan-request (request)
               (let ((path (change-request-path request)))
                 (or (check-path path)
                     (ecase (change-request-op request)
                       (:write
                        (or (claim path :target (change-request-op request))
                            (ensure-parents path)
                            (let* ((before (state path))
                                   (content (change-request-content request))
                                   (mode (or (change-request-mode request)
                                             (and (eq (entry-state-kind before) :file)
                                                  (entry-state-mode before))
                                             +default-file-mode+))
                                   (after (file-state (content-hash content) mode
                                                      (change-request-mtime request))))
                              (if (eq (entry-state-kind before) :directory)
                                  (reject "refusal.not-a-file" "~A is a directory" path)
                                  (record (make-change-result
                                           :path path
                                           :action (if (entry-state-absent-p before)
                                                       :created :modified)
                                           :before before :after after
                                           :before-content (content-of path before)
                                           :after-content content)
                                          after)))))
                       (:delete
                        (or (claim path :target (change-request-op request))
                            (let ((before (state path)))
                              (or (ecase (entry-state-kind before)
                                    (:absent (reject "input.not-found" "~A does not exist" path))
                                    (:directory
                                     (when (children path)
                                       (reject "refusal.not-a-file"
                                               "~A is a non-empty directory" path)))
                                    ((:file :symlink) nil))
                                  (record (make-change-result
                                           :path path :action :deleted
                                           :before before :after (absent-state)
                                           :before-content (content-of path before))
                                          (absent-state))))))
 (:move
                        (let ((from (change-request-from request)))
                          (or (check-path from)
                              (when (string= from path)
                                (reject "argument.invalid" "~A is moved onto itself" path))
                              (let ((source (state from)))
                                (or (when (entry-state-absent-p source)
                                      (reject "input.not-found" "~A does not exist" from))
                                    (when (and (eq (entry-state-kind source) :directory)
                                               (path-under-p from path))
                                      (reject "argument.invalid" "~A cannot move inside itself" from))
                                    (claim from :source :move)
                                    (claim path :target (change-request-op request))
                                    (ensure-parents path)
                                    (let ((before (state path)))
                                      (if (or (entry-state-absent-p before)
                                              (and (member (entry-state-kind before) '(:file :symlink))
                                                   (member (entry-state-kind source) '(:file :symlink))))
                                          (let ((after-content (content-of from source)))
                                            (record (make-change-result
                                                     :path path :action :moved :from from
                                                     :before before :after source :source-before source
                                                     :before-content (content-of path before)
                                                     :after-content after-content)
                                                    source)
                                            (setf (gethash from overrides) (absent-state))
                                            (when (eq (entry-state-kind source) :directory)
                                              (push from moved-directories)
                                              (push path moved-directories))
                                            nil)
                                          (reject "refusal.exists" "~A already exists" path))))))))
 (:chmod
                        (or (claim path :target (change-request-op request))
                            (let ((before (state path))
                                  (mode (change-request-mode request)))
                              (ecase (entry-state-kind before)
                                (:absent (reject "input.not-found" "~A does not exist" path))
                                (:symlink (reject "refusal.not-a-file" "~A is a symlink" path))
                                (:file
                                 (let ((after (file-state (entry-state-hash before) mode)))
                                   (record (make-change-result :path path :action :mode-changed
                                                               :before before :after after)
                                           after)))
                                (:directory
                                 (let ((after (directory-state mode)))
                                   (record (make-change-result :path path :action :mode-changed
                                                               :before before :after after)
                                           after)))))))
 (:symlink
                        (or (claim path :target (change-request-op request))
                            (let ((target (change-request-target request)))
                              (or (unless (and target (plusp (length target))
                                               (not (find (code-char 0) target)))
                                    (reject "argument.invalid" "symlink target for ~A is empty" path))
                                  (ensure-parents path)
                                  (let ((before (state path))
                                        (after (symlink-state target)))
                                    (if (eq (entry-state-kind before) :directory)
                                        (reject "refusal.exists" "~A is a directory" path)
                                        (record (make-change-result
                                                 :path path :action :linked
                                                 :before before :after after
                                                 :before-content (content-of path before))
                                                after)))))))
 (:mtime (or (claim path :target (change-request-op request))
                            (let ((before (state path)))
                              (ecase (entry-state-kind before)
                                (:absent
                                 (reject "input.not-found" "~A does not exist" path))
                                ((:directory :symlink)
                                 (reject "refusal.not-a-file" "~A is not a regular file" path))
                                (:file
                                 (let ((before (file-state
                                                (entry-state-hash before)
                                                (entry-state-mode before)
                                                (or (entry-state-mtime before)
                                                    (funcall lookup-mtime path))))
                                       (after (file-state
                                               (entry-state-hash before)
                                               (or (change-request-mode request)
                                                   (entry-state-mode before))
                                               (change-request-mtime request)))
                                       (content (content-of path before)))
                                   (record (make-change-result
                                            :path path :action :modified
                                            :before before :after after
                                            :before-content content :after-content content)
                                           after)))))))
 (:mkdir
                        (or (claim path :target (change-request-op request))
                            (let ((before (state path)))
                              (ecase (entry-state-kind before)
                                (:directory nil)
                                (:absent
                                 (or (ensure-parents path)
                                     (let ((after (directory-state
                                                   (change-request-mode request))))
                                       (record (make-change-result
                                                :path path :action :created
                                                :before before :after after)
                                              after))))
                                ((:file :symlink)
                                 (reject "refusal.exists" "~A already exists" path)))))))))))
        (let ((rejection (loop for request in requests
                             for outcome = (plan-request request)
                             when outcome return outcome)))
          (if rejection
              (aitools.kernel.domain:flow-continue
               :on-rejected (second rejection) (third rejection))
              (aitools.kernel.domain:flow-continue :on-planned (reverse results)))))))
