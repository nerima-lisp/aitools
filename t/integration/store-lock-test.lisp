;;;; t/integration/store-lock-test.lisp
;;;;
;;;; The workspace lock between two real processes:
;;;; a forked child holds the lock while the test process tries to write.
(in-package #:aitools.store.test)

(defun wait-until-exists (store relative &key (timeout-ms 10000))
  (loop repeat (ceiling timeout-ms 10)
        until (not (eq (entry-state-kind (workspace-state store relative)) :absent))
        do (sleep 0.01))
  (expect (entry-state-kind (workspace-state store relative)) :not :to-be :absent))

(defun hold-lock-in-child (store hold-seconds)
  "Fork a child that takes the workspace lock, creates `locked` in the
workspace, and holds the lock for HOLD-SECONDS. Returns the pid once the
lock is held."
  (let ((pid (call-in-child-process
              (lambda ()
                (with-workspace-lock (store 1000)
                  (put-file store "locked" "1")
                  (sleep hold-seconds))))))
    (wait-until-exists store "locked")
    pid))

(describe "aitools.store workspace lock"
  (it "answers busy when the lock is not released within --lock-timeout"
    (with-temp-store (store)
      (let ((pid (hold-lock-in-child store 3)))
        (expect (commit-changes/k store '("write")
                                  (lambda (commit reject)
                                    (declare (ignore reject))
                                    (funcall commit (list (write-file-request "x" (bytes "x")))))
                                  :lock-timeout-ms 200
                                  :on-committed (lambda (&rest args) (declare (ignore args)) :committed)
                                  :on-rejected (lambda (&rest args) (declare (ignore args)) :rejected)
                                  :on-busy (lambda () :busy))
                :to-be :busy)
        (expect (disk-text store "x") :to-be :absent)
        (kill-child pid))))

  (it "waits for the holder and then writes"
    (with-temp-store (store)
      (let* ((pid (hold-lock-in-child store 0.5))
             (start (get-internal-real-time))
             (outcome (commit-changes/k store '("write")
                                        (lambda (commit reject)
                                          (declare (ignore reject))
                                          (funcall commit (list (write-file-request "x" (bytes "x")))))
                                        :lock-timeout-ms 10000
                                        :on-committed (lambda (&rest args) (declare (ignore args)) :committed)
                                        :on-rejected (lambda (&rest args) (declare (ignore args)) :rejected)
                                        :on-busy (lambda () :busy)))
             (waited (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
        (expect outcome :to-be :committed)
        (expect waited :to-be-greater-than 0.1)
        (expect (multiple-value-list (wait-for-child pid)) :to-equal '(:exited 0))
        (expect (disk-text store "x") :to-equal "x"))))

  (it "is released by the OS when the holder is killed"
    (with-temp-store (store)
      (let ((pid (hold-lock-in-child store 60)))
        (expect (multiple-value-list (kill-child pid)) :to-equal (list :signalled sb-posix:sigkill))
        (expect (with-workspace-lock (store 2000 :on-timeout :busy) :acquired) :to-be :acquired))))

  (it "serialises two processes' writes to the journal"
    (with-temp-store (store)
      (let ((pids (loop for n from 0 below 2
                        collect (let ((n n))
                                  (call-in-child-process
                                   (lambda ()
                                     (loop for i from 0 below 5
                                           do (commit store (list (write-file-request (format nil "p~D-~D" n i)
                                                                                      (bytes "x")))))))))))
        (dolist (pid pids)
          (expect (multiple-value-list (wait-for-child pid)) :to-equal '(:exited 0))))
      (expect (length (read-journal store)) :to-be 10))))

(describe "aitools.store tx lock and the flock primitive"
  (it "runs WITH-TX-LOCK's body, or its not-found form for an unknown tx"
    (with-temp-store (store)
      (let ((tx (begin store)))
        (expect (with-tx-lock (store tx 1000 :on-timeout :busy :on-not-found :missing) :held) :to-be :held)
        (expect (with-tx-lock (store (format-tx-id 3964000000 "00000000") 1000 :on-timeout :busy :on-not-found :missing)
                  :held)
                :to-be :missing))))

  (it "answers busy for a tx whose lock another process holds"
    (with-temp-store (store)
      (let* ((tx (begin store))
             (pid (call-in-child-process
                   (lambda ()
                     (with-tx-lock (store tx 1000)
                       (put-file store "locked" "1")
                       (sleep 3))))))
        (wait-until-exists store "locked")
        (expect (with-tx-lock (store tx 100 :on-timeout :busy :on-not-found :missing) :held) :to-be :busy)
        (kill-child pid))))

  (it "takes an exclusive flock once per open file description"
    (with-temp-store (store)
      (let* ((io (store-io-port store))
             (path (disk-path store "lockfile"))
             (first (funcall (store-io-try-lock io) path :create t)))
        (expect (integerp first) :to-be t)
        (expect (funcall (store-io-try-lock io) path) :to-be nil)
        (funcall (store-io-unlock io) first)
        (let ((again (funcall (store-io-try-lock io) path)))
          (expect (integerp again) :to-be t)
          (funcall (store-io-unlock io) again))
        (expect (handler-case (funcall (store-io-try-lock io) (disk-path store "absent-lock"))
                  (store-io-error (condition) (store-io-error-operation condition)))
                :to-equal "open")))))
