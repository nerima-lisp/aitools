;;;; packages/feature/process/src/domain/process-outcome.lisp
;;;;
;;;; What a finished `run` child left behind, independent of how it was
;;;; launched, and `run`'s success fields built from it.
(in-package #:aitools.process.domain)

(defstruct (process-outcome (:copier nil))
  (exit-code nil :type (or null integer) :read-only t)
  (signal nil :type (or null integer) :read-only t)
  (timed-out nil :type boolean :read-only t)
  (duration-ms 0 :type (integer 0) :read-only t)
  (stdout "" :type string :read-only t)
  (stderr "" :type string :read-only t)
  ;; True when the capture buffer filled before the child stopped writing, so
  ;; later lines never reached STDOUT/STDERR at all.
  (stdout-capped nil :type boolean :read-only t)
  (stderr-capped nil :type boolean :read-only t)
  ;; Bytes written to the `--stdout-to` file, or NIL when stdout was captured.
  (stdout-bytes nil :type (or null (integer 0)) :read-only t))

(defun run-result-fields (outcome stdout-report stderr-report &key stdout-path)
  "`run`'s command-specific success fields, in envelope order, as an alist
ready for the command-result contract. `redactions` sums both streams. With
STDOUT-PATH (`--stdout-to`), `stdout` names the file instead of carrying
head and tail."
  (append
   (list (cons "exit_code" (json-or-null (process-outcome-exit-code outcome)))
         (cons "signal" (json-or-null (process-outcome-signal outcome)))
         (cons "timed_out" (json-boolean (process-outcome-timed-out outcome)))
         (cons "duration_ms" (process-outcome-duration-ms outcome))
         (cons "stdout" (if stdout-path
                            (json-object "path" stdout-path
                                         "bytes" (or (process-outcome-stdout-bytes outcome) 0))
                            (output-report-json stdout-report)))
         (cons "stderr" (output-report-json stderr-report))
         (cons "redactions" (+ (output-report-redactions stdout-report)
                               (output-report-redactions stderr-report))))
   (when (or (process-outcome-stdout-capped outcome) (process-outcome-stderr-capped outcome))
     (list (cons "capture_capped" (json-boolean t))))))
