;;;; packages/feature/journal/src/application/replayers.lisp
;;;;
;;;; `tx rebase` re-runs each drifted content-based op from its
;;;; recorded argv, which only the command that staged it can interpret. The
;;;; owners of write commands register one replayer per command here, at
;;;; load time; the journal context loads before them for that reason.
(in-package #:aitools.journal.application)

(defvar *tx-replayers* (make-hash-table :test 'equal)
  "Dispatch name (\"edit\", \"json.set\") -> replayer function.")

(defun register-tx-replayer (command-name function)
  "Make FUNCTION the `tx rebase` replayer for COMMAND-NAME, the command's
dispatch name. FUNCTION is called as (FUNCTION ARGV VIEW COMMIT REJECT):
ARGV is the argv given to TX-STAGE/K, VIEW a store view of the tx state
rebuilt so far, and it must call (COMMIT requests) or (REJECT code message
&rest keys) exactly once, as the staging VALIDATE did."
  (check-type command-name string)
  (check-type function function)
  (setf (gethash command-name *tx-replayers*) function)
  command-name)

(defun find-tx-replayer (argv)
  "The replayer for a recorded ARGV: the group form `<argv0>.<argv1>` first,
then `<argv0>`; a leading \"aitools\" word is skipped. NIL when none."
  (let ((words (if (equal (first argv) "aitools") (rest argv) argv)))
    (or (and (second words)
             (gethash (format nil "~A.~A" (first words) (second words)) *tx-replayers*))
        (and (first words)
             (gethash (first words) *tx-replayers*)))))
