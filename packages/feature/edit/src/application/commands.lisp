;;;; packages/feature/edit/src/application/commands.lisp
;;;;
;;;; The entry points presentation and `tx rebase` use: RUN-EDIT-COMMAND runs
;;;; a command from its parsed positionals and options through the write
;;;; pipeline; REPLAY-EDIT-OP re-runs a recorded op against a rebuilt tx
;;;; view, registered with the journal context for every command whose
;;;; ops can be replayable.
(in-package #:aitools.edit.application)

(defun replay-edit-op (argv view commit reject)
  "The `tx rebase` replayer for every command of +REPLAYABLE-COMMANDS+: parse
the recorded ARGV and re-run the op's plan against VIEW."
  (declare (type function commit reject))
  (parse-recorded-argv
   argv
   :on-invalid (lambda (message) (funcall reject "argument.invalid" message))
   :on-parsed
   (lambda (name positionals options)
     (let ((preparer (gethash name *preparers*)))
       (if (not (member name +replayable-commands+ :test #'string=))
           (funcall reject "argument.invalid" (format nil "~A cannot be replayed" name))
           (funcall preparer nil nil positionals options
                    (lambda (plan)
                      (if (not (write-plan-replayable plan))
                          (funcall reject "refusal.target-changed" (format nil "~A is position-based and is not replayed" name))
                          (run-plan (make-write-context :view view
                                                        :paths (mapcar #'write-target-path (write-plan-targets plan))
                                                        :command name
                                                        :expect-count (parse-count (getf options :expect-count)))
                                    (write-plan-plan plan)
                                    (lambda (requests extra)
                                      (declare (ignore extra))
                                      (funcall commit requests))
                                    reject)))
                    (lambda (code message &rest keys)
                      (declare (ignore keys))
                      (funcall reject code message))))))))

(dolist (name +replayable-commands+)
  (aitools.journal.application:register-tx-replayer name #'replay-edit-op))
