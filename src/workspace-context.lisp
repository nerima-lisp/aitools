;;;; src/workspace-context.lisp
;;;;
;;;; Per-invocation workspace facts the composition root owns: the resolved
;;;; real root (from the global `--root`), the per-workspace state directory derived
;;;; from it, and recovery of interrupted operations, which runs before every command.
;;;; Ports are built once in BUILD-APP, before argv is parsed, so the state
;;;; directory reaches them as a function that reads this invocation's root.
(in-package #:aitools/cli)

(defvar *invocation-workspace-root* nil
  "The real root path of the workspace the current invocation acts on, or
NIL when none could be resolved (e.g. `--root` names a missing directory).")

(defun current-state-directory ()
  "The state directory `<state>/<workspace-id>` for the current invocation, or NIL when
no workspace root was resolved."
  (when *invocation-workspace-root*
    (aitools.store.application:state-directory-for-root
     *invocation-workspace-root*
     :xdg-state-home (uiop:getenv "XDG_STATE_HOME")
     :home (uiop:getenv "HOME"))))

(defun %resolve-invocation-root (invocation)
  (aitools.workspace.application:call-with-resolved-root/k
   (aitools.workspace.infrastructure:make-host-workspace-host)
   :root (option-value invocation :root)
   :on-resolved (lambda (root)
                  (namestring (aitools.workspace.application:workspace-root-real root)))
   :on-error (lambda (reason path)
               (declare (ignore reason path))
               nil)))

(defun %lock-timeout-ms (invocation)
  "The global `--lock-timeout` in milliseconds, the store default when absent,
or :INVALID when the value is not a duration."
  (let ((text (option-value invocation :lock-timeout)))
    (if (null text)
        aitools.store.application:+default-lock-timeout-ms+
        (handler-case (aitools.kernel.domain:duration-milliseconds
                       (aitools.kernel.domain:parse-duration text))
          (aitools.kernel.domain:invalid-duration-error () :invalid)))))

(defun %recover-invocation-workspace (root timeout)
  "Run startup recovery for the store at ROOT. Returns :BUSY when the lock
could not be taken, a (RECOVERY-ERROR . CONDITION) cons when recovery raised a
STORE-IO-ERROR or STORE-FORMAT-ERROR (a workspace whose recovery cannot
complete must not fail every command with INTERNAL.UNEXPECTED), or the list of
(op-id . action) conses otherwise. A normal entry's CAR is a string op-id, so
the :RECOVERY-ERROR keyword marker cannot collide with it."
  (block recover
    (handler-case
        (aitools.store.application:recover/k
         (aitools.store.infrastructure:make-posix-store root)
         :lock-timeout-ms timeout
         :on-rolled-forward (lambda (op-id) (declare (ignore op-id)))
         :on-discarded (lambda (op-id) (declare (ignore op-id)))
         :on-none (lambda () nil)
         :on-busy (lambda () (return-from recover :busy)))
      ((or aitools.store.application:store-io-error aitools.store.domain:store-format-error) (condition)
        (cons :recovery-error condition)))))

(defun call-with-invocation-workspace/k (invocation &key on-ready on-busy on-invalid-timeout
                                                      on-recovery-error)
  "Resolve the invocation's workspace, run startup recovery for it, and call
exactly one continuation: ON-READY (recovered) with *INVOCATION-WORKSPACE-ROOT*
bound, where RECOVERED is a list of (:op-id ID :action ACTION) plists;
ON-BUSY () when the recovery lock could not be taken; ON-INVALID-TIMEOUT
(text) when `--lock-timeout` is not a duration; ON-RECOVERY-ERROR (condition
root) when recovery itself raised a store I/O or format error. The
recovery outcome is captured before ON-READY runs, so a store error raised by
the command's own handler reaches the caller unchanged rather than being
mistaken for a recovery failure."
  (declare (type function on-ready on-busy on-invalid-timeout on-recovery-error))
  (let ((timeout (%lock-timeout-ms invocation))
        (root (%resolve-invocation-root invocation)))
    (if (eq timeout :invalid)
        (funcall on-invalid-timeout (option-value invocation :lock-timeout))
        (let ((*invocation-workspace-root* root))
          (if (null root)
              (funcall on-ready nil)
              (let ((outcome (%recover-invocation-workspace root timeout)))
                (cond
                  ((eq outcome :busy) (funcall on-busy))
                  ((and (consp outcome) (eq (car outcome) :recovery-error))
                   (funcall on-recovery-error (cdr outcome) root))
                  (t (funcall on-ready
                              (mapcar (lambda (entry) (list :op-id (car entry) :action (cdr entry)))
                                      outcome))))))))))
