;;;; packages/core/store/src/application/lock.lisp
;;;;
;;;; The workspace lock and the per-tx lock: an exclusive
;;;; flock on a lock file, held for the dynamic extent of ON-ACQUIRED.
;;;; flock has no timed wait, so acquisition polls a non-blocking attempt,
;;;; sleeping 5 ms, then doubling up to 100 ms between attempts, until
;;;; `--lock-timeout` expires. The OS releases the lock when the process
;;;; dies, so a crashed writer never leaves a stale lock.
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

(defun %call-with-file-lock/k (store path timeout-ms create on-acquired on-timeout)
  (declare (type function on-acquired on-timeout))
  (when (member path *held-locks* :test #'string=)
    (return-from %call-with-file-lock/k (funcall on-acquired)))
  (let* ((deadline (+ (%io store monotonic-ms) timeout-ms))
         (interval 5))
    (loop
      (let ((handle (%io store try-lock path :create create)))
        (when handle
          (return
            (unwind-protect
                 (let ((*held-locks* (cons path *held-locks*)))
                   (funcall on-acquired))
              (%io store unlock handle))))
        (let ((remaining (- deadline (%io store monotonic-ms))))
          (when (<= remaining 0)
            (return (funcall on-timeout)))
          (%io store sleep (min interval remaining))
          (setf interval (min 100 (* 2 interval))))))))

(defun call-with-workspace-lock/k (store timeout-ms &key on-acquired on-timeout)
  "Hold the workspace lock around ON-ACQUIRED (no arguments) and return its
value, or call ON-TIMEOUT (no arguments) when TIMEOUT-MS milliseconds pass
without acquiring it (`environment.busy`)."
  (declare (type function on-acquired on-timeout))
  (%ensure-state-directories store)
  (%call-with-file-lock/k store (lock-file-path (store-state-directory store))
                          timeout-ms t on-acquired on-timeout))

(defmacro with-workspace-lock ((store timeout-ms &key on-timeout) &body body)
  (let ((acquired (gensym "ACQUIRED")) (timeout (gensym "TIMEOUT")))
    `(flet ((,acquired () ,@body)
            (,timeout () ,on-timeout))
       (declare (dynamic-extent #',acquired #',timeout))
       (call-with-workspace-lock/k ,store ,timeout-ms :on-acquired #',acquired :on-timeout #',timeout))))

(defun call-with-tx-lock/k (store tx-id timeout-ms &key on-acquired on-timeout on-not-found)
  "Hold TX-ID's lock around ON-ACQUIRED. ON-NOT-FOUND (no arguments) when the
tx does not exist, before or after waiting (another process may commit or
abort it while this one waits)."
  (declare (type function on-acquired on-timeout on-not-found))
  (flet ((exists-p ()
           (and (valid-tx-id-p tx-id)
                (eq (%io store lstat (join-path (tx-directory (store-state-directory store) tx-id) "index.json"))
                    :file))))
    (if (not (exists-p))
        (funcall on-not-found)
        (%call-with-file-lock/k store (join-path (tx-directory (store-state-directory store) tx-id) "lock")
                                timeout-ms nil
                                (lambda () (if (exists-p) (funcall on-acquired) (funcall on-not-found)))
                                on-timeout))))

(defmacro with-tx-lock ((store tx-id timeout-ms &key on-timeout on-not-found) &body body)
  (let ((acquired (gensym "ACQUIRED")) (timeout (gensym "TIMEOUT")) (missing (gensym "MISSING")))
    `(flet ((,acquired () ,@body)
            (,timeout () ,on-timeout)
            (,missing () ,on-not-found))
       (declare (dynamic-extent #',acquired #',timeout #',missing))
       (call-with-tx-lock/k ,store ,tx-id ,timeout-ms
                            :on-acquired #',acquired :on-timeout #',timeout :on-not-found #',missing))))
