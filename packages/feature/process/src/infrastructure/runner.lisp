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

(defun %environment-overlay (entries)
  (when entries
    (let ((environment (copy-list (sb-ext:posix-environ))))
      (dolist (entry entries environment)
        (let ((key (subseq entry 0 (position #\= entry))))
          (setf environment
                (remove-if (lambda (inherited)
                             (let ((separator (position #\= inherited)))
                               (and separator (string= key inherited :end2 separator))))
                           environment))
          (push entry environment))))))

(defclass %borrowed-input ()
  ((source :initarg :source :reader %input-source)
   (process :initarg :process :reader %input-process)))

(defclass %borrowed-character-input (%borrowed-input sb-gray:fundamental-character-input-stream) ())
(defclass %borrowed-binary-input (%borrowed-input sb-gray:fundamental-binary-input-stream) ())

(defmethod stream-element-type ((input %borrowed-binary-input))
  '(unsigned-byte 8))

(defmethod sb-gray:stream-read-char ((input %borrowed-character-input))
  (loop
    (unless (process-kit:process-alive-p (%input-process input)) (return :eof))
    (let ((character (read-char-no-hang (%input-source input) nil :eof)))
      (when character (return character)))
    (sleep 0.01)))

(defmethod sb-gray:stream-read-byte ((input %borrowed-binary-input))
  (let ((source (%input-source input)))
    (loop
      (unless (process-kit:process-alive-p (%input-process input)) (return :eof))
      (when (or (listen source)
                (not (typep source 'sb-sys:fd-stream))
                (sb-unix:unix-simple-poll (sb-sys:fd-stream-fd source) :input 10))
        (return (read-byte source nil :eof))))))

(defmethod sb-gray:stream-read-sequence ((input %borrowed-input) sequence
                                        &optional (start 0) end)
  ;; READ-SEQUENCE normally waits for the whole buffer. Return available input
  ;; promptly so an interactive child can answer before its caller sends EOF.
  (let ((read-one (if (typep input '%borrowed-character-input)
                      #'sb-gray:stream-read-char #'sb-gray:stream-read-byte)))
    (loop for index from start below (or end (length sequence))
          for element = (funcall read-one input)
          when (eq element :eof) return index
          do (setf (elt sequence index) element)
          unless (listen (%input-source input)) return (1+ index)
          finally (return (or end (length sequence))))))

(defun %run-borrowing-input (argv timeout-ms output input cwd environment)
  (let ((process (process-kit:spawn (first argv) (rest argv)
                                  :search t :input :stream
                                  :output (if (eq output :capture) :stream output)
                                  :error :stream :directory cwd
                                  :environment (or environment (copy-list (sb-ext:posix-environ)))
                                  :external-format :latin-1)))
    (unwind-protect
         (process-kit:communicate
          process
          :input (make-instance (if (subtypep (stream-element-type input) '(unsigned-byte 8))
                                    '%borrowed-binary-input '%borrowed-character-input)
                                :source input :process process)
          :timeout (/ timeout-ms 1000) :on-timeout :return
          :max-output-characters +capture-limit-characters+
          :external-format :utf-8 :decoding-error-policy :replace)
      (unless (process-kit:process-try-wait process)
        (process-kit:close-process process :terminate t))
      (process-kit:close-process-streams process))))

(defun %run-with-output (argv timeout-ms output on-unavailable input cwd environment)
  "Run ARGV with stdout going to OUTPUT (:CAPTURE or a
stream). Returns the PROCESS-RESULT, or NIL after calling ON-UNAVAILABLE."
  (handler-case
      (if (streamp input)
          (%run-borrowing-input argv timeout-ms output input cwd environment)
          (process-kit:run (first argv) (rest argv)
                       :search t
                       :output output
                       :input input
                       :directory cwd
                       :environment environment
                       :timeout (/ timeout-ms 1000)
                       :on-timeout :return
                       :max-output-characters +capture-limit-characters+
                       :external-format :utf-8
                       :decoding-error-policy :replace))
    (process-kit:process-launch-error (condition)
      (funcall on-unavailable (format nil "cannot start ~A: ~A" (first argv) condition) (first argv))
      nil)
    (process-kit:process-error (condition)
      (error 'aitools.process.application:process-port-error
             :message (format nil "running ~A failed: ~A" (first argv) condition)))))

(defun %run-program-with-input (argv timeout-ms input cwd environment
                               stdout-path on-exited on-timed-out on-unavailable on-exists)
  (flet ((finish (result stdout-bytes)
           (when result
             (funcall (if (process-kit:process-result-timed-out-p result) on-timed-out on-exited)
                      (%outcome-from-result result stdout-bytes)))))
    (if (null stdout-path)
        (finish (%run-with-output argv timeout-ms :capture on-unavailable input cwd environment) nil)
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
                     (setf result (%run-with-output argv timeout-ms stream on-unavailable input cwd environment)
                           bytes (file-length stream))
                  (close stream)
                  (unless result
                    (%remove-file stdout-path)))
                (finish result bytes)))))))

(defun %run-program (argv timeout-ms &key stdout-path stdin stdin-data stdin-file cwd env
                                        on-exited on-timed-out on-unavailable on-exists)
  (let ((environment (%environment-overlay env)))
    (flet ((run (input)
             (%run-program-with-input argv timeout-ms input cwd environment
                                      stdout-path on-exited on-timed-out on-unavailable on-exists)))
      (if stdin-file
          (%port-io ("reading ~A" stdin-file)
            ;; Opening first with O_NONBLOCK avoids a FIFO wait; checking the
            ;; same descriptor avoids a path replacement between stat and open.
            (let ((fd (sb-posix:open (uiop:native-namestring (merge-pathnames stdin-file))
                                     (logior sb-posix:o-rdonly sb-posix:o-nonblock)))
                  (input nil))
              (unwind-protect
                   (progn
                     (unless (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:fstat fd)))
                       (error 'aitools.process.application:process-port-error
                              :message (format nil "stdin file ~A is not a regular file" stdin-file)))
                     (setf input (sb-sys:make-fd-stream fd :input t :element-type '(unsigned-byte 8)))
                     (run input))
                (if input (close input) (sb-posix:close fd)))))
          (run (if stdin *standard-input* stdin-data))))))
