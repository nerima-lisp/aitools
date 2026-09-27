;;;; t/integration/journal-flows-test.lisp
;;;;
;;;; `history` and `undo`
;;;; through the journal flows against a real temporary workspace and store:
;;;; output shapes, restoring bytes, mode and existence, undo of undo,
;;;; conflicts, the required op_id, dry runs, diff truncation, and the lock.
(in-package #:aitools.journal.test)

(defun wait-until-present (store relative)
  (loop repeat 500
        until (not (eq (disk-text store relative) :absent))
        do (sleep 0.01)))

(defun hold-lock-in-child (store hold-seconds)
  "Fork a child that takes the workspace lock, creates `locked`, and holds
the lock for HOLD-SECONDS. Returns the pid once the lock is held."
  (let ((pid (call-in-child-process
              (lambda ()
                (aitools.store.application:with-workspace-lock (store 1000)
                  (put-file store "locked" "1")
                  (sleep hold-seconds))))))
    (wait-until-present store "locked")
    pid))

(describe "aitools.journal history"
  (it "lists ops newest first with command, paths and time"
    (with-temp-store (store)
      (let ((first (write-op store "a.txt" "1"))
            (second (write-op store "b.txt" "2")))
        (multiple-value-bind (kind fields) (run #'history-flow store)
          (expect kind :to-be :ok)
          (expect (mapcar #'car fields) :to-equal '("items" "total" "truncated"))
          (expect (mapcar (lambda (item) (member-value item "op_id")) (field fields "items"))
                  :to-equal (list second first))
          (let ((item (first (field fields "items"))))
            (expect (mapcar #'car (json-alist item)) :to-equal '("op_id" "command" "paths" "time"))
            (expect (member-value item "command") :to-equal "aitools write b.txt")
            (expect (member-value item "paths") :to-equal '("b.txt")))
          (expect (field fields "total") :to-be 2)
          (expect (field fields "truncated") :to-be json-kit:+json-false+)))))

  (it "filters by a path or a directory above it"
    (with-temp-store (store)
      (let ((in-dir (write-op store "dir/a.txt" "1")))
        (write-op store "other.txt" "2")
        (dolist (path (list "dir/a.txt" "dir" (disk-path store "dir")))
          (multiple-value-bind (kind fields) (run #'history-flow store :path path)
            (expect kind :to-be :ok)
            (expect (mapcar (lambda (item) (member-value item "op_id")) (field fields "items"))
                    :to-equal (list in-dir)))))))

  (it "is partial past --limit and names the command listing everything"
    (with-temp-store (store)
      (dotimes (i 3) (write-op store "a.txt" (princ-to-string i)))
      (multiple-value-bind (kind fields) (run #'history-flow store :limit 2)
        (expect kind :to-be :partial)
        (expect (length (field fields "items")) :to-be 2)
        (expect (field fields "total") :to-be 3)
        (expect (field fields "truncated") :to-be t)
        (expect (field fields "next_commands")
                :to-equal (list (format nil "aitools --root ~A history --limit 3" (aitools.store.application:store-root store)))))))

  (it "refuses a path outside the workspace"
    (with-temp-store (store)
      (multiple-value-bind (kind error) (run #'history-flow store :path "/")
        (expect kind :to-be :error)
        (expect (getf error :code) :to-equal "argument.invalid")
        (expect (repair-commands error) :not :to-equal '())))))

(defun undo (store op-id &rest keys)
  (apply #'run #'undo-flow store op-id keys))

(describe "aitools.journal undo"
  (it "restores bytes and mode and reports changes with op_id and undoes"
    (with-temp-store (store)
      (put-file store "a.txt" (format nil "one~%two~%") :mode #o600)
      (let ((op (commit-write store (list (aitools.store.domain:write-file-request "a.txt" (bytes "new") :mode #o755)))))
        (multiple-value-bind (kind fields) (undo store op)
          (expect kind :to-be :ok)
          (expect (mapcar #'car fields) :to-equal '("changes" "op_id" "undoes"))
          (expect (field fields "undoes") :to-equal op)
          (expect (aitools.store.domain:valid-op-id-p (field fields "op_id")) :to-be-truthy)
          (let ((change (first (field fields "changes"))))
            (expect (member-value change "path") :to-equal "a.txt")
            (expect (member-value change "action") :to-equal "modified")
            (expect (member-value change "diff") :to-equal (format nil "@@ -1,1 +1,2 @@~%-new~%\\ No newline at end of file~%+one~%+two~%"))))
        (expect (disk-text store "a.txt") :to-equal (format nil "one~%two~%"))
        (expect (disk-mode store "a.txt") :to-be #o600))))

  (it "restores existence: a created file is removed, a deleted one comes back with its bytes and mode"
    (with-temp-store (store)
      (put-file store "old.bin" (coerce #(0 1 2 255) 'aitools.store.domain:octets) :mode #o640)
      (let ((op (commit-write store (list (aitools.store.domain:write-file-request "new/n.txt" (bytes "n"))
                                          (aitools.store.domain:delete-request "old.bin")))))
        (multiple-value-bind (kind fields) (undo store op)
          (expect kind :to-be :ok)
          (expect (mapcar (lambda (change) (list (member-value change "path") (member-value change "action")))
                          (field fields "changes"))
                  :to-equal '(("old.bin" "created") ("new/n.txt" "deleted") ("new" "deleted")))
          ;; binary bytes: no diff
          (expect (member-value (first (field fields "changes")) "diff" :none) :to-be :none))
        (expect (disk-text store "new") :to-be :absent)
        (expect (aitools.store.domain:entry-state-hash (disk-state store "old.bin"))
                :to-equal (aitools.kernel.domain:content-hash (coerce #(0 1 2 255) 'aitools.store.domain:octets)))
        (expect (disk-mode store "old.bin") :to-be #o640))))

  (it "moves a moved file back"
    (with-temp-store (store)
      (put-file store "from.txt" "x")
      (let ((op (commit-write store (list (aitools.store.domain:move-request "from.txt" "to.txt")))))
        (multiple-value-bind (kind fields) (undo store op)
          (expect kind :to-be :ok)
          (let ((change (first (field fields "changes"))))
            (expect (member-value change "action") :to-equal "moved")
            (expect (member-value change "from") :to-equal "to.txt")))
        (expect (disk-text store "from.txt") :to-equal "x")
        (expect (disk-text store "to.txt") :to-be :absent))))

  (it "redoes when the undo itself is undone"
    (with-temp-store (store)
      (put-file store "a.txt" "before")
      (let* ((op (write-op store "a.txt" "after"))
             (undo-op (field (nth-value 1 (undo store op)) "op_id")))
        (expect (disk-text store "a.txt") :to-equal "before")
        (multiple-value-bind (kind fields) (undo store undo-op)
          (expect kind :to-be :ok)
          (expect (field fields "undoes") :to-equal undo-op))
        (expect (disk-text store "a.txt") :to-equal "after")
        ;; history marks both undo entries
        (let ((items (field (nth-value 1 (run #'history-flow store)) "items")))
          (expect (mapcar (lambda (item) (member-value item "undoes" nil)) items)
                  :to-equal (list undo-op op nil))))))

  (it "refuses with exit-2 conflicts when a path changed since, writing nothing"
    (with-temp-store (store)
      (let ((op (commit-write store (list (aitools.store.domain:write-file-request "a.txt" (bytes "1"))
                                          (aitools.store.domain:write-file-request "b.txt" (bytes "1"))))))
        (put-file store "b.txt" "someone else")
        (multiple-value-bind (kind error) (undo store op)
          (expect kind :to-be :error)
          (expect (getf error :code) :to-equal "refusal.target-changed")
          (expect (aitools.protocol.domain:error-code-exit-code (getf error :code)) :to-be 2)
          (expect (mapcar #'json-alist (getf error :conflicts))
                  :to-equal (list (list (cons "path" "b.txt") (cons "kind" "write")
                                        (cons "base" (aitools.kernel.domain:content-hash (bytes "1")))
                                        (cons "current" (aitools.kernel.domain:content-hash (bytes "someone else"))))))
          (expect (some (lambda (command) (search "history b.txt" command)) (repair-commands error)) :to-be-truthy))
        (expect (disk-text store "a.txt") :to-equal "1")
        (expect (disk-text store "b.txt") :to-equal "someone else"))))

  (it "requires the op_id and repairs with aitools history"
    (with-temp-store (store)
      (multiple-value-bind (kind error) (undo store nil)
        (expect kind :to-be :error)
        (expect (getf error :code) :to-equal "argument.invalid")
        (expect (repair-commands error)
                :to-equal (list (format nil "aitools --root ~A history" (aitools.store.application:store-root store)))))
      (dolist (op-id '("latest" "op-20260101T000000Z-0000000f"))
        (multiple-value-bind (kind error) (undo store op-id)
          (expect kind :to-be :error)
          (expect (getf error :code) :to-equal "input.not-found")
          (expect (search "history" (first (repair-commands error))) :to-be-truthy)))))

  (it "changes nothing with --dry-run and has no op_id"
    (with-temp-store (store)
      (put-file store "a.txt" "before")
      (let ((op (write-op store "a.txt" "after")))
        (multiple-value-bind (kind fields) (undo store op :dry-run t)
          (expect kind :to-be :ok)
          (expect (mapcar #'car fields) :to-equal '("changes" "dry_run" "undoes"))
          (expect (member-value (first (field fields "changes")) "action") :to-equal "modified"))
        (expect (disk-text store "a.txt") :to-equal "after")
        (expect (field (nth-value 1 (run #'history-flow store)) "total") :to-be 1))))

  (it "cuts long diffs, stays exit 0, and points at aitools diff --op"
    (with-temp-store (store)
      (let ((op (write-op store "big.txt" (format nil "~{~D~%~}" (loop for i below 50 collect i)))))
        (multiple-value-bind (kind fields) (undo store op :max-diff-lines 5)
          (expect kind :to-be :ok)
          (expect (member-value (first (field fields "changes")) "diff_truncated") :to-be t)
          (expect (mapcar #'car fields) :to-equal '("changes" "op_id" "undoes" "next_commands"))
          (expect (field fields "next_commands")
                  :to-equal (list (format nil "aitools diff --op ~A" (field fields "op_id"))))))))

  (it "answers environment.busy with the same command as the repair"
    (with-temp-store (store)
      (let* ((op (write-op store "a.txt" "1"))
             (pid (hold-lock-in-child store 3)))
        (unwind-protect
             (let ((result (aitools.protocol.application:call-with-command-result/k
                            (lambda (&rest continuations)
                              (apply #'undo-flow (ports-for store) (context-for store :lock-timeout "100ms")
                                     op continuations)))))
               (let ((error (aitools.protocol.application:command-result-fields result)))
                 (expect (getf error :code) :to-equal "environment.busy")
                 (expect (repair-commands error)
                         :to-equal (list (format nil "aitools --root ~A --lock-timeout 100ms undo ~A"
                                                 (aitools.store.application:store-root store) op)))))
          (kill-child pid))
        (expect (disk-text store "a.txt") :to-equal "1")))))
