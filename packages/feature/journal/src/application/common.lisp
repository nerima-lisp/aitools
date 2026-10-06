;;;; packages/feature/journal/src/application/common.lisp
;;;;
;;;; Pieces every journal flow shares: opening the store of the resolved
;;;; workspace, the global-option words repeated in repair commands, and the
;;;; mapping of store outcomes (busy, I/O failure, damaged state record,
;;;; conflicts) onto error codes with repairs.
(in-package #:aitools.journal.application)

(defun %globals (context)
  "The invocation's global options as command-line words, so a repair
command acts on the same workspace with the same lock timeout."
  (append (when (journal-context-root context)
            (list "--root" (journal-context-root context)))
          (when (journal-context-lock-timeout context)
            (list "--lock-timeout" (journal-context-lock-timeout context)))))

(defun %aitools (context &rest words)
  (apply #'command-line "aitools" (%globals context) words))

(defun %command-words (command-name)
  "The dotted dispatch name COMMAND-NAME as the words an agent types
(`tx.diff` -> (\"tx\" \"diff\")), so a schema repair shows the typed form and
never leaks the internal dotted name."
  (loop with start = 0
        for dot = (position #\. command-name :start start)
        collect (subseq command-name start dot)
        while dot do (setf start (1+ dot))))

(defun %schema-repair (command-name)
  (aitools.protocol.domain:schema-repair
   (command-line "aitools" "schema" (%command-words command-name))))

(defun %history-repair (context)
  (repair "list-ops" "List the journal's operations, newest first, to find the op_id."
          (%aitools context "history")))

(defun %tx-list-repair (context)
  (repair "list-tx" "List the open transactions." (%aitools context "tx" "status")))

(defun %lock-timeout-ms (context)
  "The `--lock-timeout` in milliseconds, the store default when absent.
Dispatch rejects a non-duration --lock-timeout before any journal flow runs
(src/dispatch.lisp), so TEXT here is a valid duration or NIL."
  (let ((text (journal-context-lock-timeout context)))
    (if (null text)
        aitools.store.application:+default-lock-timeout-ms+
        (aitools.kernel.domain:duration-milliseconds (aitools.kernel.domain:parse-duration text)))))

(defun %call-with-store (ports context command-name on-error continuation)
  "Resolve the workspace root and call CONTINUATION with (STORE ROOT
LOCK-TIMEOUT-MS), ROOT being the workspace context's WORKSPACE-ROOT. A store
I/O failure or a damaged state record escaping CONTINUATION is reported as
`environment.io`."
  (declare (type function on-error continuation))
  (let ((host (journal-ports-workspace-host ports))
        (timeout (%lock-timeout-ms context)))
    (aitools.workspace.application:call-with-resolved-root/k
     host
     :root (journal-context-root context)
     :on-error (lambda (reason path)
                 (funcall on-error "argument.invalid"
                          (format nil "workspace root ~A: ~(~A~)" path reason)
                          :repairs (list (%schema-repair command-name))))
     :on-resolved
     (lambda (root)
       (handler-case
           (funcall continuation
                    (funcall (journal-ports-open-store ports)
                             (aitools.workspace.application:workspace-root-real root))
                    root timeout)
         ;; A post-commit I/O failure: the op passed the store's commit point
         ;; (its intent is recorded under op-id) and only a later write was
         ;; interrupted. This is NOT a "nothing written" rejection; recovery
         ;; rolls the op forward on the next command once the path is usable,
         ;; so surface the op_id and the recovery status. This clause must
         ;; precede store-io-error, its superclass.
         (aitools.store.application:store-committed-error (condition)
           (funcall on-error "environment.io" (princ-to-string condition)
                    :diagnostics (list (json-object "op_id" (aitools.store.application:store-committed-error-op-id condition)
                                                    "recovery" "pending"))
                    :repairs (list (repair "recover"
                                           "Re-run once the path is writable: aitools completes a committed-but-interrupted write on its next command. List the recorded op meanwhile."
                                           (%aitools context "history")))))
         (aitools.store.application:store-io-error (condition)
           (funcall on-error "environment.io" (princ-to-string condition)
                    :repairs (list (%schema-repair command-name))))
         (aitools.store.domain:store-format-error (condition)
           (funcall on-error "environment.io" (princ-to-string condition)
                    :repairs (list (%schema-repair command-name)))))))))

(defun %report-busy (on-error context &rest words)
  (declare (type function on-error))
  (funcall on-error "environment.busy"
           "the workspace or tx lock could not be acquired within --lock-timeout"
           :repairs (list (repair "retry" "Run the same command again once the other writer is done."
                                  (apply #'%aitools context words)))))

(defun %conflicts-json (conflicts)
  (mapcar #'aitools.store.domain:conflict->json conflicts))

(defun %report-rejection (on-error command-name code message)
  "A store rejection with no command-specific repair: the planner's refusals
and `environment.io`. Only refusal.target-changed carries conflicts, and
every caller answers that code itself."
  (declare (type function on-error))
  (funcall on-error code message
           :repairs (list (%schema-repair command-name))))

(defun %fields-with (fields &rest extra)
  "FIELDS (a write-result alist) with the EXTRA alist entries inserted before its
`next_commands`, which stays last."
  (let ((next (assoc "next_commands" fields :test #'string=)))
    (append (remove next fields) extra (and next (list next)))))
