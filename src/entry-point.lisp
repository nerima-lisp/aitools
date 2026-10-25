;;;;
;;;; MAIN accepts process argv and injectable dispatch/quit functions for
;;;; delivery tests. IMAGE-ENTRY-POINT sets process defaults and calls MAIN.
;;;; The CLI sets no raw terminal mode that needs restoration on a signal.
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
  (setf *default-pathname-defaults* (uiop:getcwd))
  (uiop:setup-temporary-directory)
  (funcall main-function))
