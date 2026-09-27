;;;; packages/feature/process/src/infrastructure/runner.lisp
;;;;
;;;; The RUN-PROGRAM port over process-kit:RUN: argv without a shell, the
;;;; child in its own process group (so a timeout's SIGTERM -> SIGKILL reaches
;;;; everything it spawned), stdin at /dev/null (nothing reads stdin
;;;; implicitly), output decoded as UTF-8 with replacement.
(in-package #:aitools.process.infrastructure)

(defparameter +capture-limit-characters+ (* 64 1024 1024)
  "Per-stream capture ceiling. `--grep` wants the whole output, but an
unbounded child must not exhaust aitools's heap; past this the result says
`capture_capped`.")

(defun %milliseconds (seconds)
  (max 0 (round (* 1000 (or seconds 0)))))

(defun %outcome-from-result (result stdout-bytes)
  (aitools.process.domain:make-process-outcome
   :stdout-bytes stdout-bytes
   :exit-code (and (eq (process-kit:process-result-status result) :exited)
                   (process-kit:process-result-exit-code result))
   :signal (and (eq (process-kit:process-result-status result) :signaled)
                (process-kit:process-result-signal result))
   :timed-out (process-kit:process-result-timed-out-p result)
   :duration-ms (%milliseconds (process-kit:process-result-duration-seconds result))
   :stdout (or (process-kit:process-result-stdout result) "")
   :stderr (or (process-kit:process-result-stderr result) "")
   :stdout-capped (process-kit:process-result-stdout-truncated-p result)
   :stderr-capped (process-kit:process-result-stderr-truncated-p result)))

(defun %run-with-output (argv timeout-ms output on-unavailable)
  "process-kit:RUN of ARGV with stdout going to OUTPUT (:CAPTURE or a
stream). Returns the PROCESS-RESULT, or NIL after calling ON-UNAVAILABLE."
  (handler-case
      (process-kit:run (first argv) (rest argv)
                       :search t
                       :output output
                       :timeout (/ timeout-ms 1000)
                       :on-timeout :return
                       :max-output-characters +capture-limit-characters+
                       :external-format :utf-8
                       :decoding-error-policy :replace)
    (process-kit:process-launch-error (condition)
      (funcall on-unavailable (format nil "cannot start ~A: ~A" (first argv) condition) (first argv))
      nil)
    (process-kit:process-error (condition)
      (error 'aitools.process.application:process-port-error
             :message (format nil "running ~A failed: ~A" (first argv) condition)))))

(defun %run-program (argv timeout-ms &key stdout-path on-exited on-timed-out on-unavailable on-exists)
  (flet ((finish (result stdout-bytes)
           (when result
             (funcall (if (process-kit:process-result-timed-out-p result) on-timed-out on-exited)
                      (%outcome-from-result result stdout-bytes)))))
    (if (null stdout-path)
        (finish (%run-with-output argv timeout-ms :capture on-unavailable) nil)
        ;; :IF-EXISTS NIL is O_CREAT|O_EXCL: it refuses an existing file and a
        ;; final-component symlink alike, so the check and the create are one
        ;; step.
        (let ((stream (%port-io ("creating ~A" stdout-path)
                        (open stdout-path :direction :output :element-type '(unsigned-byte 8)
                                          :if-exists nil :if-does-not-exist :create))))
          (if (null stream)
              (funcall on-exists)
              (let ((result nil) (bytes 0))
                (unwind-protect
                     (setf result (%run-with-output argv timeout-ms stream on-unavailable)
                           bytes (file-length stream))
                  (close stream)
                  (unless result
                    (%remove-file stdout-path)))
                (finish result bytes)))))))
