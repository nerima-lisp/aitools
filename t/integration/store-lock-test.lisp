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
    (call-with-temp-store (lambda (store) (let ((pid (hold-lock-in-child store 3)))
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
        (kill-child pid))) :io nil))
  (it "answers busy when the lock is not released within --lock-timeout"
    (call-with-temp-store (lambda (store) (let ((pid (hold-lock-in-child store 3)))
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
        (kill-child pid))) :io nil))
  (it "accepts a lock acquired after the deadline"
    (call-with-temp-store
     (lambda (plain)
       (let* ((base-boundaries (store-boundaries plain))
              (real-try-lock (store-operation base-boundaries :try-lock))
              (real-unlock (store-operation base-boundaries :unlock))
              (clock (cl-boundary-kit:make-fake-clock :start 0 :monotonic-start 0))
              (acquired-handle nil)
              (unlocked-handles nil)
              (lock-path nil)
              (boundaries
                (cl-boundary-kit:boundary-context-with
                 base-boundaries
                 :try-lock
                 (lambda (path &rest keys)
                   (setf lock-path path)
                   (let ((handle (apply real-try-lock path keys)))
                     (setf acquired-handle handle)
                     (when handle
                       (cl-boundary-kit:advance-fake-clock
                        clock 0 :monotonic-delta
                        (* 101 (/ internal-time-units-per-second 1000))))
                     handle))
                 :unlock
                 (lambda (handle)
                   (push handle unlocked-handles)
                   (funcall real-unlock handle))))
              (store (make-store
                      boundaries
                      (store-root plain)
                      (subseq (store-state-directory plain)
                              0
                              (position #\/ (store-state-directory plain) :from-end t))
                      :clock clock
                      :sleeper (cl-boundary-kit:make-sleeper)))
              (outcome nil))
         (unwind-protect
              (progn
                (setf outcome
                      (with-workspace-lock (store 100 :on-timeout :busy)
                        :acquired))
                (progn
                  (let ((e (macroexpand-1 '(aitools.store.application:with-workspace-lock (store 100 :on-timeout :busy) :acquired))))
                    (expect (list outcome (first e) (mapcar (function first) (cddr e)))
                            :to-equal '(:acquired aitools.kernel.domain:flow-call (:on-acquired :on-timeout)))))
                (expect acquired-handle :to-be-truthy)
                (expect unlocked-handles :to-equal (list acquired-handle))
                (let ((again (funcall real-try-lock lock-path)))
                  (expect (integerp again) :to-be t)
                  (when (integerp again) (funcall real-unlock again))))
           (when (and acquired-handle
                      (not (member acquired-handle unlocked-handles :test (function eql))))
             (funcall real-unlock acquired-handle))))) :io nil))

  (it "waits for the holder and then writes"
    (call-with-temp-store (lambda (store) (let* ((pid (hold-lock-in-child store 0.5))
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
        (expect (disk-text store "x") :to-equal "x"))) :io nil))

  (it "is released by the OS when the holder is killed"
    (call-with-temp-store (lambda (store) (let ((pid (hold-lock-in-child store 60)))
        (expect (multiple-value-list (kill-child pid)) :to-equal (list :signalled sb-posix:sigkill))
        (expect (with-workspace-lock (store 2000 :on-timeout :busy) :acquired) :to-be :acquired))) :io nil))

  (it "serialises two processes' writes to the journal"
    (call-with-temp-store (lambda (store) (let ((pids (loop for n from 0 below 2
                        collect (let ((n n))
                                  (call-in-child-process
                                   (lambda ()
                                     (loop for i from 0 below 5
                                           do (commit store (list (write-file-request (format nil "p~D-~D" n i)
                                                                                      (bytes "x")))))))))))
        (dolist (pid pids)
          (expect (multiple-value-list (wait-for-child pid)) :to-equal '(:exited 0))))
      (expect (length (read-journal store)) :to-be 10)) :io nil)))

(describe "aitools.store tx lock and the flock primitive"
  (it "runs WITH-TX-LOCK's body, or its not-found form for an unknown tx"
    (call-with-temp-store (lambda (store) (let ((tx (begin store)))
        (progn
          (let ((e (macroexpand-1 '(aitools.store.application:with-tx-lock (store tx 1000 :on-timeout :busy :on-not-found :missing) :held))))
            (expect (list (first e) (mapcar (function first) (cddr e))
                          (with-tx-lock (store tx 1000 :on-timeout :busy :on-not-found :missing) :held))
                    :to-equal '(aitools.kernel.domain:flow-call (:on-acquired :on-timeout :on-not-found) :held))))
        (expect (with-tx-lock (store (format-tx-id 3964000000 "00000000") 1000 :on-timeout :busy :on-not-found :missing)
                  :held)
                :to-be :missing))) :io nil))

  (it "answers busy for a tx whose lock another process holds"
    (call-with-temp-store (lambda (store) (let* ((tx (begin store))
             (pid (call-in-child-process
                   (lambda ()
                     (with-tx-lock (store tx 1000)
                       (put-file store "locked" "1")
                       (sleep 3))))))
        (wait-until-exists store "locked")
        (expect (with-tx-lock (store tx 100 :on-timeout :busy :on-not-found :missing) :held) :to-be :busy)
        (kill-child pid))) :io nil))

  (it "takes an exclusive flock once per open file description"
    (call-with-temp-store (lambda (store) (let* ((io (store-boundaries store))
             (path (disk-path store "lockfile"))
             (first (funcall (store-operation io :try-lock) path :create t)))
        (expect (integerp first) :to-be t)
        (expect (funcall (store-operation io :try-lock) path) :to-be nil)
        (funcall (store-operation io :unlock) first)
        (let ((again (funcall (store-operation io :try-lock) path)))
          (expect (integerp again) :to-be t)
          (funcall (store-operation io :unlock) again))
        (expect (handler-case (funcall (store-operation io :try-lock) (disk-path store "absent-lock"))
                  (store-io-error (condition) (store-io-error-operation condition)))
                :to-equal "open"))) :io nil)))

(describe "aitools.store zero-timeout lock acquisition"
  (it "attempts acquisition immediately"
    (call-with-temp-store (lambda (store) (expect (with-workspace-lock (store 0 :on-timeout :busy) :acquired)
              :to-be :acquired)) :io nil)))

(describe "aitools.store recovery lock timeout"
  (it "returns busy without changing prepared state while another process holds the lock"
    (call-with-temp-store
     (lambda (store)
       (put-file store "a.txt" "a-old")
       (expect
        (call-with-fault-at
         :after-prepare
         (lambda ()
           (commit store (list (write-file-request "a.txt" (bytes "a-new"))))))
        :to-be-truthy)
       (let ((pid (hold-lock-in-child store 3)))
         (unwind-protect
              (let ((intents (intent-files store))
                    (temps (temp-files store)))
                (expect (length intents) :to-be 1)
                (expect temps :to-be-truthy)
                (expect
                 (recover/k store
                            :lock-timeout-ms 200
                            :on-rolled-forward (lambda (op-id) (declare (ignore op-id)))
                            :on-discarded (lambda (op-id) (declare (ignore op-id)))
                            :on-none (lambda () :none)
                            :on-busy (lambda () :busy))
                 :to-be :busy)
                (expect (intent-files store) :to-equal intents)
                (expect (temp-files store) :to-equal temps)
                (expect (disk-text store "a.txt") :to-equal "a-old"))
           (kill-child pid))))
     :io nil)))
