(in-package #:aitools.process.infrastructure)

(defun %le (bytes offset width)
  (loop for index below width sum (ash (aref bytes (+ offset index)) (* 8 index))))

(defun %nul-field (bytes start)
  (let ((end (position 0 bytes :start start)))
    (when end
      (values (sb-ext:octets-to-string bytes :start start :end end :external-format :utf-8)
              (1+ end)))))

(defun %join-argv (argv)
  (format nil "~{~A~^ ~}" argv))

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
  (sb-alien:with-alien ((mib (array sb-alien:int 3))
                        (size sb-alien:unsigned-long))
    (setf (sb-alien:deref mib 0) 1
          (sb-alien:deref mib 1) 49
          (sb-alien:deref mib 2) pid
          size 0)
    (let ((sysctl (sb-alien:extern-alien "sysctl"
                    (function sb-alien:int (* (array sb-alien:int 3)) sb-alien:unsigned-int
                              (* sb-alien:unsigned-char) (* sb-alien:unsigned-long)
                              (* sb-alien:unsigned-char) sb-alien:unsigned-long))))
      (when (zerop (sb-alien:alien-funcall sysctl (sb-alien:addr mib) 3
                                             (sb-alien:sap-alien (sb-sys:int-sap 0) (* sb-alien:unsigned-char))
                                             (sb-alien:addr size)
                                             (sb-alien:sap-alien (sb-sys:int-sap 0) (* sb-alien:unsigned-char)) 0))
        (when (and (>= size 6) (<= size (* 1024 1024)))
          (let ((bytes (make-array size :element-type '(unsigned-byte 8) :initial-element 0)))
            (when (zerop (sb-alien:alien-funcall sysctl (sb-alien:addr mib) 3
                                                   (sb-alien:sap-alien (sb-sys:vector-sap bytes)
                                                                      (* sb-alien:unsigned-char))
                                                   (sb-alien:addr size)
                                                   (sb-alien:sap-alien (sb-sys:int-sap 0) (* sb-alien:unsigned-char)) 0))
              (let* ((argc (%le bytes 0 4))
                     (offset (nth-value 1 (%nul-field bytes 4))))
                (when (and offset (<= offset size) (plusp argc) (< argc 4096))
                  (loop while (and (< offset size) (zerop (aref bytes offset))) do (incf offset))
                  (let ((argv '()))
                    (loop repeat argc do
                      (unless (< offset size) (return-from %darwin-argv nil))
                      (multiple-value-bind (arg next) (%nul-field bytes offset)
                        (unless (and next (<= next size))
                          (return-from %darwin-argv nil))
                        (push arg argv)
                        (setf offset next)))
                    (%join-argv (nreverse argv))))))))))))

#+darwin
(defun %host-process-info (pid)
  (when (and (integerp pid) (>= pid 2))
    (let ((bytes (%darwin-pid-bytes pid)))
      (when (and bytes (= pid (%le bytes 12 4)))
        (let ((uid (%le bytes 20 4))
              (ruid (%le bytes 28 4)))
          (aitools.process.domain:make-process-identity
           :pid pid :ppid (%le bytes 16 4) :pgid (%le bytes 100 4)
           :uid uid :ruid ruid
           :start (format nil "~D.~6,'0D" (%le bytes 120 8) (%le bytes 128 8))
           ;; Do not inspect another user's argv. An empty command line is a
           ;; private sentinel for an identity that is not eligible to signal;
           ;; NIL means this user's argv could not be read.
           :command-line (if (and (= uid (sb-posix:getuid))
                                  (= ruid (sb-posix:getuid)))
                             (%darwin-argv pid)
                             "")))))))

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
(defun %host-process-info (pid)
  (when (and (integerp pid) (>= pid 2))
    (let* ((base (format nil "/proc/~D/" pid))
           (status (%linux-text (concatenate 'string base "status")))
           (stat (%linux-text (concatenate 'string base "stat"))))
      (when (and status stat)
        (let* ((uid-line (find-if (lambda (line) (and (>= (length line) 4)
                                                   (string= line "Uid:" :end1 4)))
                                  (uiop:split-string status :separator '(#\Newline))))
               (uids (and uid-line (mapcar #'parse-integer
                                           (remove "" (uiop:split-string (subseq uid-line 4)
                                                                          :separator '(#\Space #\Tab))
                                                   :test #'string=))))
               (close (position #\) stat :from-end t)))
          (when (and (>= (length uids) 2) close)
            (let ((fields (remove "" (uiop:split-string (subseq stat (+ close 2))
                                                       :separator '(#\Space)) :test #'string=)))
              (when (>= (length fields) 20)
                (let ((ruid (first uids))
                      (uid (second uids)))
                  (aitools.process.domain:make-process-identity
                   :pid pid :ppid (parse-integer (nth 1 fields))
                   :pgid (parse-integer (nth 2 fields))
                   :ruid ruid :uid uid
                   :start (parse-integer (nth 19 fields))
                   ;; Do not inspect another user's argv. An empty command
                   ;; line is a private sentinel for an ineligible identity;
                   ;; NIL means this user's argv could not be read.
                   :command-line (if (and (= uid (sb-posix:getuid))
                                          (= ruid (sb-posix:getuid)))
                                     (%linux-argv
                                      (%linux-file-bytes (concatenate 'string base "cmdline")))
                                     "")))))))))))

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
