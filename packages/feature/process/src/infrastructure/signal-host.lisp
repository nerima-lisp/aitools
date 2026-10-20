(in-package #:aitools.process.infrastructure)

#+darwin
(defun %darwin-pid-bytes (pid)
  (let ((bytes (make-array 136 :element-type '(unsigned-byte 8) :initial-element 0)))
    (let ((count (sb-alien:alien-funcall
                  (sb-alien:extern-alien "proc_pidinfo"
                    (function sb-alien:int sb-alien:int sb-alien:int sb-alien:unsigned-long
                              (* sb-alien:unsigned-char) sb-alien:int))
                  pid 3 0 (sb-alien:sap-alien (sb-sys:vector-sap bytes)
                                             (* sb-alien:unsigned-char)) 136)))
      (and (= count 136) bytes))))

#+darwin
(defun %darwin-argv (pid)
  (let ((size (%darwin-query-argv-size pid)))
    (when (and size (<= 6 size (* 1024 1024)))
      (%darwin-parse-argv (%darwin-query-argv-bytes pid size)))))

#+darwin
(defun %host-process-info (pid)
  (when (and (integerp pid) (>= pid 2))
    (let ((bytes (%darwin-pid-bytes pid)))
      (when (and bytes (= pid (%le bytes 12 4)))
        (%darwin-process-identity pid bytes)))))

#+darwin
(defun %host-list-pids ()
  (let* ((capacity 65536)
         (bytes (make-array (* capacity 4) :element-type '(unsigned-byte 8))))
    (let ((count (sb-alien:alien-funcall
                  (sb-alien:extern-alien "proc_listallpids"
                    (function sb-alien:int (* sb-alien:unsigned-char) sb-alien:int))
                  (sb-alien:sap-alien (sb-sys:vector-sap bytes) (* sb-alien:unsigned-char))
                  (length bytes))))
      (unless (and (plusp count) (< count capacity))
        (error 'aitools.process.application:process-port-error
               :message "process list unavailable or truncated"))
      (loop for i below count for pid = (%le bytes (* i 4) 4)
            when (>= pid 2) collect pid))))

#+linux
(defun %linux-file-bytes (path)
  (handler-case
      (with-open-file (stream path :element-type '(unsigned-byte 8))
        ;; procfs reports zero length for these virtual files. Read through
        ;; EOF, with a bound so an unexpected source cannot consume the heap.
        (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer 0)))
          (loop for byte = (read-byte stream nil nil) while byte
                do (when (>= (length bytes) (* 1024 1024))
                     (return-from %linux-file-bytes nil))
                   (vector-push-extend byte bytes))
          bytes))
    (error () nil)))

#+linux
(defun %linux-text (path)
  (let ((bytes (%linux-file-bytes path)))
    (and bytes (sb-ext:octets-to-string bytes :external-format :utf-8))))

#+linux
(defun %linux-argv (bytes)
  (when (and bytes (plusp (length bytes)))
    (loop with start = 0 for end = (position 0 bytes :start start)
          while end
          collect (sb-ext:octets-to-string bytes :start start :end end :external-format :utf-8) into args
          do (setf start (1+ end))
          finally (return (%join-argv args)))))

#+linux
(defun %linux-process-info (pid)
  (let* ((base (format nil "/proc/~D/" pid))
         (status (%linux-text (concatenate 'string base "status")))
         (stat (%linux-text (concatenate 'string base "stat"))))
    (when (and status stat)
      (%linux-process-identity pid base
                               (%linux-status-uids status)
                               (%linux-stat-fields stat)))))

#+linux
(defun %host-process-info (pid)
  (when (and (integerp pid) (>= pid 2))
    (%linux-process-info pid)))

#+linux
(defun %linux-pid-name (name)
  (and (stringp name)
       (plusp (length name))
       (every #'digit-char-p name)
       (ignore-errors (parse-integer name))))

#+linux
(defun %host-list-pids ()
  (let ((directory (handler-case (sb-posix:opendir "/proc")
                     (error () nil)))
        (paths nil)
        (closed nil))
    (unless directory
      (error 'aitools.process.application:process-port-error
             :message "process list unavailable"))
    (unwind-protect
         (setf paths (handler-case (directory #p"/proc/*/")
                       (error () nil)))
      (setf closed (handler-case (progn (sb-posix:closedir directory) t)
                     (error () nil))))
    (unless (and closed paths)
      (error 'aitools.process.application:process-port-error
             :message "process list unavailable"))
    (loop for path in paths
          for name = (first (last (pathname-directory path)))
          for pid = (%linux-pid-name name)
          when (and pid (>= pid 2)) collect pid)))

(defun %safe-process-info (pid)
  (handler-case (%host-process-info pid)
    (error () nil)))

(defun %ancestor-p (pid)
  (loop with seen = (make-hash-table)
        for current = (sb-posix:getppid) then (let ((info (%safe-process-info current)))
                                             (and info (aitools.process.domain:process-identity-ppid info)))
        while (and current (>= current 2))
        when (= current pid) return t
        when (gethash current seen) return t
        do (setf (gethash current seen) t)
        finally (return (null current))))

(defun %safe-target-p (expected)
  (handler-case
      (let* ((pid (aitools.process.domain:process-identity-pid expected))
             (actual (and (integerp pid) (>= pid 2) (%safe-process-info pid))))
        ;; This check narrows the interval between observation and kill(2), but
        ;; cannot make the two syscalls atomic. A pidfd/process handle would be
        ;; needed to close PID reuse races completely on supported kernels.
        (and actual
             (stringp (aitools.process.domain:process-identity-command-line expected))
             (stringp (aitools.process.domain:process-identity-command-line actual))
             (aitools.process.domain:same-process-p expected actual)
             (= (aitools.process.domain:process-identity-uid actual) (sb-posix:getuid))
             (= (aitools.process.domain:process-identity-ruid actual) (sb-posix:getuid))
             (/= pid (sb-posix:getpid))
             (/= (aitools.process.domain:process-identity-pgid actual)
                 (sb-posix:getpgid 0))
             (not (%ancestor-p pid))))
    (sb-posix:syscall-error () nil)))

(defun %signal-process (expected signal)
  (let ((pid (aitools.process.domain:process-identity-pid expected)))
    ;; Recheck here, including for SIGKILL after grace. This is still not an
    ;; atomic identity-to-signal operation on platforms without process handles.
    (unless (%safe-target-p expected)
      (error 'aitools.process.application::process-target-changed
             :message (format nil "process ~D changed or is unsafe to signal" pid)))
    (handler-case (progn (sb-posix:kill pid signal) t)
      (sb-posix:syscall-error (condition)
        (if (= (sb-posix:syscall-errno condition) sb-posix:esrch)
            nil
            (error 'aitools.process.application:process-port-error
                   :message (format nil "signalling process ~D failed: ~A" pid condition)))))))
