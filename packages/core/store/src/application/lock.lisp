;;;;
;;;; The workspace lock and the per-tx lock: an exclusive
;;;; flock on a lock file, held for the dynamic extent of ON-ACQUIRED.
;;;; flock has no timed wait. Resilience-kit retries non-blocking attempts
;;;; with delays doubling from 5 ms to 100 ms using the store's monotonic clock
;;;; and `--lock-timeout`.
;;;; The OS releases the lock when the process dies, so a crashed writer never
;;;; leaves a stale lock.
;;;;
;;;; Locks are reentrant within one dynamic extent (*HELD-LOCKS*): `tx
;;;; commit` holds the workspace lock and then runs the same write-protocol code that
;;;; `edit` enters without it. A second flock through a new file description
;;;; in the same process would otherwise wait on itself.
(in-package #:aitools.store.application)

(defconstant +default-lock-timeout-ms+ 10000
  "The `--lock-timeout` default, 10s.")

(defvar *held-locks* '()
  "Lock file paths this thread holds in the current dynamic extent.")

(defun %ensure-directory (store path)
  "mkdir -p for a state-directory path, creating each missing directory 0700
(the state holds workspace content and argv). An existing symlink counts as
the directory, as for mkdir -p: the state home may be one ($XDG_STATE_HOME
pointing at a symlinked directory)."
  (flet ((present-p () (member (%io store lstat path) '(:directory :symlink))))
    (unless (present-p)
      (let ((parent (subseq path 0 (or (position #\/ path :from-end t) 0))))
        (when (plusp (length parent))
          (%ensure-directory store parent)))
      (handler-case (%io store mkdir path :mode #o700)
        (store-io-error (condition)
          (unless (present-p)
            (error condition)))))))

(defun %tighten-directory (store path)
  "Make PATH, a directory aitools owns, private when an earlier version or
another umask left it readable by others."
  (multiple-value-bind (kind mode) (%io store lstat path)
    (when (and (eq kind :directory) (logtest mode #o077))
      (%io store chmod path #o700))))

(defun %ensure-state-directories (store)
  (let ((state (store-state-directory store)))
    (dolist (path (list (commit-directory state) (journal-directory state)
                        (blobs-directory state) (tx-root-directory state)))
      (%ensure-directory store path))
    (%tighten-directory store (workspace-state-root state))
    (%tighten-directory store state)))

(defun %lock-retry-attempt-limit (timeout-ms)
  (loop with remaining = timeout-ms
        with interval = 5
        with attempts = 1
        repeat 5
        while (plusp remaining)
        do (decf remaining (min interval remaining))
           (setf interval (min 100 (* interval 2)))
           (incf attempts)
        finally (return (+ attempts
                           (if (plusp remaining)
                               (ceiling remaining 100)
                               0)))))

(aitools.kernel.domain:define-flow
  (%call-with-file-lock/k store path timeout-ms create &key on-acquired on-timeout)
  (declare (type function on-acquired on-timeout))
  (if (member path *held-locks* :test (function string=))
      (aitools.kernel.domain:flow-continue :on-acquired)
      (let ((handle
              (if (<= timeout-ms 0)
                  (%io store try-lock path :create create)
                  (let* ((retry-start-ms (%io store monotonic-ms))
                         (policy
                           (resilience-kit:make-retry-policy
                            :max-attempts (%lock-retry-attempt-limit timeout-ms)
                            :initial-delay 0d0
                            :multiplier 1d0
                            :max-delay 0.1d0
                            :jitter :none
                            :retry-safe-p t
                            :result-classifier
                            (lambda (result attempt)
                              (when (null result)
                                (let* ((elapsed (- (%io store monotonic-ms)
                                                   retry-start-ms))
                                       (remaining (- timeout-ms elapsed)))
                                  (when (plusp remaining)
                                    (/ (min (min 100
                                                 (ash 5 (min 5 (1- attempt))))
                                            remaining)
                                       1000d0))))))))
                    (%io store call-with-retry
                         policy
                         (lambda () (%io store try-lock path :create create))
                         (lambda () (%io store monotonic-ms))
                         (lambda (milliseconds)
                           (%io store sleep milliseconds)))))))
        (if handle
            (unwind-protect
                 (let ((*held-locks* (cons path *held-locks*)))
                   (aitools.kernel.domain:flow-continue :on-acquired))
              (%io store unlock handle))
            (aitools.kernel.domain:flow-continue :on-timeout)))))


(aitools.kernel.domain:define-flow
  (call-with-workspace-lock/k store timeout-ms &key on-acquired on-timeout)
  "Hold the workspace lock around ON-ACQUIRED and return its value.
Call ON-TIMEOUT when retry exhaustion or the deadline prevents acquisition
(environment.busy)."
  (declare (type function on-acquired on-timeout))
  (%ensure-state-directories store)
  (aitools.kernel.domain:flow-call
      (%call-with-file-lock/k
       store
       (lock-file-path (store-state-directory store))
       timeout-ms
       t)
    (:on-acquired ()
      (aitools.kernel.domain:flow-continue :on-acquired))
    (:on-timeout ()
      (aitools.kernel.domain:flow-continue :on-timeout))))

(defmacro with-workspace-lock
    ((store timeout-ms &key on-timeout) &body body)
  `(aitools.kernel.domain:flow-call
       (call-with-workspace-lock/k ,store ,timeout-ms)
     (:on-acquired ()
       ,@body)
     (:on-timeout ()
       ,on-timeout)))

(aitools.kernel.domain:define-flow
  (call-with-tx-lock/k store tx-id timeout-ms &key on-acquired on-timeout on-not-found)
  "Hold TX-ID lock around ON-ACQUIRED.
Call ON-NOT-FOUND when TX-ID is absent before or after waiting; another process
may commit or abort TX-ID while this call waits."
  (declare (type function on-acquired on-timeout on-not-found))
  (flet ((exists-p ()
           (and (valid-tx-id-p tx-id)
                (eq (%io store
                         lstat
                         (join-path
                          (tx-directory (store-state-directory store) tx-id)
                          "index.json"))
                    :file))))
    (if (not (exists-p))
        (aitools.kernel.domain:flow-continue :on-not-found)
        (aitools.kernel.domain:flow-call
            (%call-with-file-lock/k
             store
             (join-path
              (tx-directory (store-state-directory store) tx-id)
              "lock")
             timeout-ms
             nil)
          (:on-acquired ()
            (if (exists-p)
                (aitools.kernel.domain:flow-continue :on-acquired)
                (aitools.kernel.domain:flow-continue :on-not-found)))
          (:on-timeout ()
            (aitools.kernel.domain:flow-continue :on-timeout))))))

(defmacro with-tx-lock
    ((store tx-id timeout-ms &key on-timeout on-not-found) &body body)
  `(aitools.kernel.domain:flow-call
       (call-with-tx-lock/k ,store ,tx-id ,timeout-ms)
     (:on-acquired ()
       ,@body)
     (:on-timeout ()
       ,on-timeout)
     (:on-not-found ()
       ,on-not-found)))
