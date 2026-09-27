;;;; src/entry-point.lisp -- process and delivery boundaries.
;;;;
;;;; Mirrors cl-sl's src/cli-entry-point.lisp shape: MAIN takes the process
;;;; argv and injectable DISPATCH-FUNCTION/QUIT-FUNCTION (for delivery
;;;; tests), IMAGE-ENTRY-POINT sets up process-level state and calls MAIN.
;;;; aitools has no raw terminal mode to restore on a signal, so it needs
;;;; none of cl-sl's signal-handling code.
(in-package #:aitools/cli)

(defun main (&key (argv (current-process-argv))
              (dispatch-function #'dispatch)
              (quit-function #'uiop:quit))
  "Dispatch ARGV (the full process argv, argv0 included -- PARSE-ARGV strips it
itself) against the app+registry baked into the image at build time
(*APPLICATION*, src/app.lisp), and exit with the resulting exit code. The
registry is reused rather than rebuilt per process start; all
per-invocation state (root, state directory, lock-timeout, environment) is still
resolved at dispatch time."
  (destructuring-bind (app registry) *application*
    (funcall quit-function (funcall dispatch-function app registry argv))))

(defun image-entry-point (&key (main-function #'main))
  "Initialize delivery state and delegate to MAIN-FUNCTION."
  (setf *default-pathname-defaults* (uiop:getcwd))
  (uiop:setup-temporary-directory)
  (funcall main-function))
