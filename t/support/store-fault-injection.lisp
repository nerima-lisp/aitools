;;;; t/support/store-fault-injection.lisp
;;;;
;;;; The interruption-injecting adapter for the store context. It
;;;; binds AITOOLS.STORE.APPLICATION:*FAULT-HOOK* so that the Nth occurrence
;;;; of a named step boundary does one of:
;;;;
;;;;   :throw  unwind to CALL-WITH-FAULT-AT without signalling an error. The
;;;;           store's error-only cleanup is skipped, as it would be if the
;;;;           process died, while UNWIND-PROTECT still releases the flock,
;;;;           as the OS would. This is the in-process model of a crash.
;;;;   :error  signal INJECTED-FAULT, an ordinary error (an I/O failure).
;;;;   :exit   end the process at once with SB-EXT:EXIT :ABORT T (no unwind,
;;;;           no cleanup): real process death, for a forked child.
;;;;
;;;; Forked children (CALL-IN-CHILD-PROCESS) give a separate process that
;;;; already holds the loaded image; no second SBCL has to find and load the
;;;; system.
(in-package #:cl-user)

(defpackage #:aitools.store.test-support
  (:use #:cl)
  (:export
   #:injected-fault
   #:call-with-fault-at
   #:with-fault-at
   #:call-with-temp-store
   #:with-temp-store
   #:call-in-child-process
   #:wait-for-child
   #:kill-child))

(in-package #:aitools.store.test-support)

(define-condition injected-fault (error)
  ((point :initarg :point :reader injected-fault-point))
  (:report (lambda (condition stream)
             (format stream "injected fault at ~S" (injected-fault-point condition)))))

(defun call-with-fault-at (point thunk &key (nth 1) (mode :throw))
  "Call THUNK with the fault hook armed for the NTH occurrence of POINT.
Returns (values fired-p thunk-value)."
  (let ((seen 0))
    (flet ((hook (name &rest details)
             (declare (ignore details))
             (when (and (eq name point) (= (incf seen) nth))
               (ecase mode
                 (:throw (throw 'simulated-crash point))
                 (:error (error 'injected-fault :point point))
                 (:exit (sb-ext:exit :code 99 :abort t))))))
      (let ((value :crashed))
        (catch 'simulated-crash
          (let ((aitools.store.application:*fault-hook* #'hook))
            (setf value (funcall thunk))))
        (values (>= seen nth) value)))))

(defmacro with-fault-at ((point &rest options) &body body)
  `(call-with-fault-at ,point (lambda () ,@body) ,@options))

(defun %make-temp-directory ()
  (let ((path (sb-posix:mkdtemp (format nil "~A/aitools-store-XXXXXX"
                                        (string-right-trim "/" (or (sb-posix:getenv "TMPDIR") "/tmp"))))))
    ;; The store works on real paths (the store directory is keyed by the real root); on
    ;; Darwin /tmp and $TMPDIR are behind symlinks.
    (string-right-trim "/" (sb-ext:native-namestring (truename (sb-ext:parse-native-namestring
                                                                 (concatenate 'string path "/")))))))

(defun call-with-temp-store (function &key io)
  "Call FUNCTION with a STORE over a fresh workspace directory and a fresh
state home, both removed afterwards. IO defaults to the production adapter."
  (let ((base (%make-temp-directory)))
    (unwind-protect
         (let ((root (concatenate 'string base "/work"))
               (home (concatenate 'string base "/state")))
           (sb-posix:mkdir root #o755)
           (funcall function (aitools.store.application:make-store
                              (or io (aitools.store.infrastructure:make-posix-store-io)) root home)))
      (uiop:delete-directory-tree (sb-ext:parse-native-namestring (concatenate 'string base "/"))
                                  :validate (lambda (path) (search "aitools-store-" (namestring path)))
                                  :if-does-not-exist :ignore))))

(defmacro with-temp-store ((store &key io) &body body)
  `(call-with-temp-store (lambda (,store) ,@body) :io ,io))

(defun call-in-child-process (thunk)
  "Fork; the child runs THUNK and exits 0, or 1 when THUNK signals. Returns
the child's pid. The child never returns into the caller's code."
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (let ((pid (sb-posix:fork)))
    (if (zerop pid)
        (sb-ext:exit :code (handler-case (progn (funcall thunk) 0)
                             (error () 1))
                     :abort t)
        pid)))

(defun wait-for-child (pid)
  "Wait for PID; returns (values :exited code) or (values :signalled signal)."
  (multiple-value-bind (reaped status) (sb-posix:waitpid pid 0)
    (declare (ignore reaped))
    (if (sb-posix:wifexited status)
        (values :exited (sb-posix:wexitstatus status))
        (values :signalled (sb-posix:wtermsig status)))))

(defun kill-child (pid)
  (sb-posix:kill pid sb-posix:sigkill)
  (wait-for-child pid))
