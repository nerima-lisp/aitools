;;;; packages/feature/process/src/infrastructure/host.lisp
;;;;
;;;; File, signal, socket, and clock adapters for PROCESS-PORTS. Every I/O
;;;; failure leaves as AITOOLS.PROCESS.APPLICATION:PROCESS-PORT-ERROR.
(in-package #:aitools.process.infrastructure)

(defmacro %port-io ((description &rest arguments) &body body)
  (let ((condition (gensym "CONDITION")))
    `(handler-case (progn ,@body)
       ((or file-error stream-error sb-posix:syscall-error) (,condition)
         (error 'aitools.process.application:process-port-error
                :message (format nil "~? failed: ~A" ,description (list ,@arguments) ,condition))))))

;;; ---------------------------------------------------------------- files

(defun %list-directory (directory)
  (%port-io ("listing ~A" directory)
    ;; DIRECTORY returns NIL for a directory it cannot read, which would make
    ;; `bg status` report no processes; opening it first surfaces the error.
    (sb-posix:closedir (sb-posix:opendir (uiop:native-namestring directory)))
    (mapcar #'file-namestring
            (remove-if (lambda (path) (null (pathname-name path)))
                       (directory (merge-pathnames uiop:*wild-file-for-directory* directory)
                                  :resolve-symlinks nil)))))

(defun %read-octets (path start end)
  (%port-io ("reading ~A" path)
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (let* ((end (min end (file-length in)))
             (octets (make-array (max 0 (- end start)) :element-type '(unsigned-byte 8))))
        (file-position in start)
        (read-sequence octets in)
        octets))))

(defun %file-size (path)
  (%port-io ("inspecting ~A" path)
    (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
      (and in (file-length in)))))

(defun %read-file-text (path)
  (let ((size (%file-size path)))
    (and size (aitools.process.domain:decode-output-octets (%read-octets path 0 size)))))

(defun %write-text (stream text)
  (write-sequence (sb-ext:string-to-octets text :external-format :utf-8) stream))

(defun %open-private-output-fd (path extra-flags)
  "Open PATH for writing as mode 0600 with O_WRONLY|O_NOFOLLOW plus EXTRA-FLAGS
(O_CREAT, O_EXCL, O_APPEND). O_NOFOLLOW refuses a symlink at PATH, so a bg log
or record is never written through one. Returns a byte stream owning the fd;
signals sb-posix:syscall-error on failure."
  (let ((fd (sb-posix:open (uiop:native-namestring path)
                           (logior extra-flags sb-posix:o-wronly sb-posix:o-nofollow)
                           #o600)))
    (sb-sys:make-fd-stream fd :output t :element-type '(unsigned-byte 8) :name (namestring path))))

(defun call-with-bg-log (path function)
  "Reopen the bg log at PATH for append and call FUNCTION with the owning byte
stream, closing the fd on any exit from FUNCTION. PATH was created 0600 when
the id was reserved; the reopen keeps O_APPEND|O_NOFOLLOW at 0600 so a
symlink swapped in at the path is refused rather than written through."
  (let ((log (%open-private-output-fd path (logior sb-posix:o-creat sb-posix:o-append))))
    (unwind-protect (funcall function log)
      (close log))))

(defmacro with-bg-log ((stream path) &body body)
  "Bind STREAM to the bg log at PATH, opened for append as a private resource
closed on any exit from BODY (the CALL-WITH-BG-LOG sugar)."
  `(call-with-bg-log ,path (lambda (,stream) ,@body)))

(defun %create-file-exclusive (path text)
  "Create PATH as a new 0600 regular file and write TEXT. Returns T, or NIL
when PATH already exists (a regular file or a symlink alike): O_CREAT|O_EXCL
makes the check and create one step and refuses a pre-existing symlink."
  (handler-case
      (let ((out (%open-private-output-fd path (logior sb-posix:o-creat sb-posix:o-excl))))
        (unwind-protect (progn (%write-text out text) t)
          (close out)))
    (sb-posix:syscall-error (condition)
      (if (= (sb-posix:syscall-errno condition) sb-posix:eexist)
          nil
          (error 'aitools.process.application:process-port-error
                 :message (format nil "creating ~A failed: ~A" path condition))))))

(defparameter +private-temp-attempts+ 8)

(defun %random-temp-suffix ()
  "An unpredictable name suffix from /dev/urandom, so a bg record's temp file
cannot be guessed and pre-created; falls back to pid and clock if unreadable."
  (handler-case
      (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
        (let ((bytes (make-array 8 :element-type '(unsigned-byte 8))))
          (read-sequence bytes in)
          (with-output-to-string (out)
            (loop for byte across bytes do (format out "~2,'0X" byte)))))
    (error () (format nil "~36R-~36R" (sb-posix:getpid) (get-internal-real-time)))))

(defun %replace-file (path text)
  "Write TEXT to a fresh private temp beside PATH and rename it over PATH, so a
concurrent reader sees the old record or the new one, never a partial one. The
temp is created O_CREAT|O_EXCL|O_NOFOLLOW 0600 under an unpredictable name, so a
symlink or a guessed name planted at the temp path is refused, not written."
  (%port-io ("writing ~A" path)
    (loop for attempt from 1
          for temporary = (make-pathname :name (format nil ".~A.tmp-~A" (pathname-name path)
                                                       (%random-temp-suffix))
                                         :defaults path)
          for out = (handler-case (%open-private-output-fd temporary (logior sb-posix:o-creat sb-posix:o-excl))
                      (sb-posix:syscall-error (condition)
                        (if (and (= (sb-posix:syscall-errno condition) sb-posix:eexist)
                                 (< attempt +private-temp-attempts+))
                            nil
                            (error condition))))
          when out
            do (unwind-protect (%write-text out text) (close out))
               (sb-posix:rename (uiop:native-namestring temporary) (uiop:native-namestring path))
               (return))))

(defun %remove-file (path)
  (%port-io ("removing ~A" path)
    (when (probe-file path)
      (delete-file path))))

;;; -------------------------------------------------------------- signals

(defvar *launched-supervisors* (make-hash-table)
  "PID -> process-kit handle of each bg supervisor this process launched and
has not yet seen terminate. The supervisor is this process's child, so only
this process can reap it.")

(defvar *launched-supervisors-lock* (cl-concurrent-kit:make-lock :name "aitools bg supervisors"))

(defun %remember-supervisor (handle)
  (cl-concurrent-kit:with-lock-held (*launched-supervisors-lock*)
    (setf (gethash (process-kit:process-id handle) *launched-supervisors*) handle)))

(defun %reap-launched-supervisor (pid)
  "Reap PID if it is a supervisor this process launched and it has exited.
On Linux kill(2) still reaches a zombie, so an unreaped supervisor keeps its
group reported alive for as long as this process runs (a `batch` that starts
and stops a bg process, or a test), and SBCL's asynchronous SIGCHLD reaping
is not guaranteed to have run by then."
  (let ((handle (cl-concurrent-kit:with-lock-held (*launched-supervisors-lock*)
                  (gethash pid *launched-supervisors*))))
    (when (and handle (process-kit:process-try-wait handle))
      (cl-concurrent-kit:with-lock-held (*launched-supervisors-lock*)
        (remhash pid *launched-supervisors*)))))

(defun %group-alive-p (pid)
  "True while process group PID still has a member this user may signal.
The supervisor can exit after its target while a descendant remains in the
supervisor's process group, so this probe must not require the supervisor PID
itself to remain a session leader. A PID reused by an unrelated process after
the whole group ended can still create a residual window because aitools has
no portable process start time to compare; the recorded process group is the
only identity available after the supervisor exits. A failed getsid falls
back to true: on Darwin a zombie leader still answers kill(pid, 0) while
getsid answers ESRCH, so nil would report a group with surviving descendants
as gone."
  (%reap-launched-supervisor pid)
  (let* ((group-alive (handler-case (progn (sb-posix:kill (- pid) 0) t)
                        (sb-posix:syscall-error () nil)))
         (leader-alive (handler-case (progn (sb-posix:kill pid 0) t)
                         (sb-posix:syscall-error () nil)))
         (alive (and group-alive
                     (or (not leader-alive)
                         (handler-case (= (sb-posix:getsid pid) pid)
                           (sb-posix:syscall-error () t))))))
    ;; Darwin already answers EPERM for a group whose last member is exiting
    ;; or a zombie, so the supervisor may still be unreaped after a "gone"
    ;; answer; reap it now if it is already waitable, else on a later probe.
    (unless alive
      (%reap-launched-supervisor pid))
    alive))

(defun %signal-group (pid signal)
  (and (%group-alive-p pid)
       (handler-case (progn (sb-posix:kill (- pid) signal) t)
         (sb-posix:syscall-error (condition)
           (if (= (sb-posix:syscall-errno condition) sb-posix:esrch)
               nil
               (error 'aitools.process.application:process-port-error
                      :message (format nil "signalling process group ~D failed: ~A" pid condition)))))))

;;; -------------------------------------------------------------- sockets

(defun %connectable-p (make-socket address port)
  (let ((socket (handler-case (funcall make-socket)
                  (sb-bsd-sockets:socket-error () nil))))
    (when socket
      (unwind-protect
           (handler-case (progn (sb-bsd-sockets:socket-connect socket address port) t)
             (sb-bsd-sockets:socket-error () nil))
        (sb-bsd-sockets:socket-close socket)))))

(defun %tcp-connectable-p (port)
  "True when something accepts TCP connections on PORT at 127.0.0.1 or ::1."
  (or (%connectable-p (lambda () (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
                      #(127 0 0 1) port)
      (%connectable-p (lambda () (make-instance 'sb-bsd-sockets:inet6-socket :type :stream :protocol :tcp))
                      (let ((loopback (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
                        (setf (aref loopback 15) 1)
                        loopback)
                      port)))

;;; ---------------------------------------------------------------- clock

(defun %monotonic-ms ()
  (values (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second)))

(defun %sleep-ms (milliseconds)
  (sleep (/ milliseconds 1000)))
