;;;;
;;;; Env boundaries combine cl-boundary-kit objects with host adapters the kit
;;;; does not expose: process-kit for commands, sb-posix for directory,
;;;; link, uid, and permission checks, and direct statfs(2)/uname(2) calls.
;;;; sb-posix binds neither of the last two, and SBCL's SOFTWARE-VERSION is
;;;; cached on first use, which a saved image would carry over from its build
;;;; host.
(in-package #:aitools.env.infrastructure)

(defparameter *maximum-read-bytes* (* 16 1024 1024)
  "Largest file READ-FILE returns. /proc reports a length of 0 for its
files, so reads go to end of file instead of trusting FILE-LENGTH; this
bound keeps an unexpected huge file from being read whole.")

(defun %epoch-milliseconds ()
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (+ (* seconds 1000) (floor microseconds 1000))))

(defun %external-format (name)
  (ecase name
    (:latin-1 :latin-1)
    (:utf-8 '(:utf-8 :replacement #\Replacement_Character))))

(defun %read-file-to-end (path &key external-format)
  (with-open-file (stream path :direction :input
                               :external-format (%external-format (or external-format :utf-8)))
    (let ((buffer (make-string 8192)))
      (with-output-to-string (out)
        (loop with total = 0
              for count = (read-sequence buffer stream)
              while (plusp count)
              do (incf total count)
                 (when (> total *maximum-read-bytes*)
                   (error 'file-error :pathname path))
                 (write-string buffer out :end count))))))

(defun %list-directory (path &key check-deadline)
  "Return directory entry names excluding dot entries. CHECK-DEADLINE runs between readdir calls."
  (handler-case
      (let ((directory (sb-posix:opendir path)))
        (unwind-protect
             (loop for entry = (progn
                                 (when check-deadline (funcall check-deadline))
                                 (sb-posix:readdir directory))
                   until (sb-alien:null-alien entry)
                   for name = (sb-posix:dirent-name entry)
                   unless (member name '("." "..") :test #'string=)
                     collect name)
          (sb-posix:closedir directory)))
    (sb-posix:syscall-error () nil)))

(defun %read-link (path)
  (handler-case (sb-posix:readlink path)
    (sb-posix:syscall-error () nil)))

(defun %regular-file-p (path)
  "The filesystem's path-exists-p: only regular files count, so a zone name
that is a directory (`America`) reads as absent rather than failing to read."
  (handler-case (= (logand (sb-posix:stat-mode (sb-posix:stat path)) sb-posix:s-ifmt) sb-posix:s-ifreg)
    (sb-posix:syscall-error () nil)))

(defun %executable-p (path)
  (and (%regular-file-p path)
       (handler-case (zerop (sb-posix:access path sb-posix:x-ok))
         (sb-posix:syscall-error () nil))))

(defun %run-program (program arguments &key timeout)
  (handler-case
      (let ((result (process-kit:run program arguments
                                     :timeout timeout :on-timeout :return
                                     :output :capture :error :capture)))
        (values (if (process-kit:process-result-timed-out-p result) :timeout :exited)
                (process-kit:process-result-exit-code result)
                (process-kit:process-result-stdout result)
                (process-kit:process-result-stderr result)))
    (process-kit:process-error () (values :not-started nil nil nil))))

(defun %file-system-space (path)
  "(VALUES TOTAL-BYTES AVAILABLE-BYTES) of the file system holding PATH via
statfs(2), or NIL. Offsets are those of struct statfs on 64-bit Darwin
(64-bit-inode layout: u32 f_bsize, i32 f_iosize, u64 f_blocks, f_bfree,
f_bavail) and 64-bit Linux (long f_type, f_bsize, f_blocks, f_bfree,
f_bavail). Darwin's struct is 2168 bytes, hence the 4 KiB buffer."
  (let ((buffer (sb-alien:make-alien (sb-alien:unsigned 8) 4096)))
    (unwind-protect
         (when (zerop (sb-alien:alien-funcall
                       (sb-alien:extern-alien #+(and darwin x86-64) "statfs$INODE64"
                                              #-(and darwin x86-64) "statfs"
                                              (function sb-alien:int sb-alien:c-string
                                                        (* (sb-alien:unsigned 8))))
                       path buffer))
           (let ((sap (sb-alien:alien-sap buffer)))
             (multiple-value-bind (block-size blocks available)
                 #+darwin (values (sb-sys:sap-ref-32 sap 0) (sb-sys:sap-ref-64 sap 8) (sb-sys:sap-ref-64 sap 24))
                 #-darwin (values (sb-sys:sap-ref-64 sap 8) (sb-sys:sap-ref-64 sap 16) (sb-sys:sap-ref-64 sap 32))
               (values (* block-size blocks) (* block-size available)))))
      (sb-alien:free-alien buffer))))

(defun %system-identity-from-uname-status (status read-fields)
  (if (zerop status)
      (funcall read-fields)
      (values (software-type) "" (string-downcase (machine-type)))))

(defun %system-identity ()
  "(VALUES SYSNAME RELEASE MACHINE) from uname(2). struct utsname holds
fixed char arrays: 256 bytes each on Darwin, 65 on Linux."
  (let ((field-size #+darwin 256 #-darwin 65)
        (buffer (sb-alien:make-alien (sb-alien:unsigned 8) 2048)))
    (unwind-protect
         (%system-identity-from-uname-status
          (sb-alien:alien-funcall
           (sb-alien:extern-alien "uname" (function sb-alien:int (* (sb-alien:unsigned 8))))
           buffer)
          (lambda ()
            (let ((sap (sb-alien:alien-sap buffer)))
              (flet ((field (index)
                       (let* ((start (* index field-size))
                              (octets (loop for offset from start below (+ start field-size)
                                            for octet = (sb-sys:sap-ref-8 sap offset)
                                            until (zerop octet)
                                            collect octet)))
                         (sb-ext:octets-to-string (coerce octets '(vector (unsigned-byte 8)))
                                                  :external-format :utf-8))))
                ;; utsname order: sysname, nodename, release, version, machine.
                (values (field 0) (field 2) (field 4))))))
      (sb-alien:free-alien buffer))))

(defun %workspace-root (&key check-deadline)
  "The workspace root without --root: the nearest ancestor of the working directory
holding `.git`, else the working directory. Does not start git."
  (let ((start (namestring (uiop:getcwd))))
    (loop for directory = (pathname start) then (uiop:pathname-parent-directory-pathname directory)
          for namestring = (progn
                             (when check-deadline (funcall check-deadline))
                             (namestring directory))
          when (handler-case (progn (sb-posix:stat (concatenate 'string namestring ".git")) t)
                 (sb-posix:syscall-error () nil))
            return namestring
          when (equal namestring "/")
            return start)))

(defun %cpu-counters-from-host-statistics (status count tick-at)
  (when (and (zerop status) (= count 4))
    (list (funcall tick-at 0) (funcall tick-at 3)
          (funcall tick-at 1) (funcall tick-at 2) 0 0 0 0)))

#+darwin
(defun %cpu-counters ()
  (let ((host (sb-alien:alien-funcall
               (sb-alien:extern-alien "mach_host_self" (function sb-alien:unsigned-int)))))
    (unwind-protect
         (sb-alien:with-alien ((ticks (array sb-alien:unsigned-int 4))
                              (count sb-alien:unsigned-int 4))
           ;; HOST_CPU_LOAD_INFO has four uint32 counters: user, system, idle, nice.
           (%cpu-counters-from-host-statistics
            (sb-alien:alien-funcall
             (sb-alien:extern-alien
              "host_statistics"
              (function sb-alien:int sb-alien:unsigned-int sb-alien:int
                        (* sb-alien:unsigned-int) (* sb-alien:unsigned-int)))
             host 3 (sb-alien:cast (sb-alien:addr ticks) (* sb-alien:unsigned-int))
             (sb-alien:addr count))
            count
            (lambda (index) (sb-alien:deref ticks index))))
      (sb-alien:alien-funcall
       (sb-alien:extern-alien "mach_port_deallocate"
                             (function sb-alien:int sb-alien:unsigned-int sb-alien:unsigned-int))
       (sb-alien:extern-alien "mach_task_self_" sb-alien:unsigned-int) host))))

(defun make-env-boundaries (&key clock environment filesystem host-info
                                         (sleep-seconds (function sleep))
                                         #+darwin (cpu-counters (function %cpu-counters))
                                         #+darwin (sysctl-mib (function %sysctl-mib))
                                         (run-program (function %run-program))
                                         (executable-p (function %executable-p))
                                         (list-directory (function %list-directory))
                                         (read-link (function %read-link))
                                         (file-system-space (function %file-system-space))
                                         (system-identity (function %system-identity))
                                         (user-id (function sb-posix:getuid))
                                         (workspace-root (function %workspace-root)))
  "Build an env context from boundary objects and host-specific operations."
  (let ((boundaries (cl-boundary-kit:make-boundary-context)))
    (dolist (capability
             (list (cons :env-clock clock)
                   (cons :env-environment environment)
                   (cons :env-filesystem filesystem)
                   (cons :env-host-info host-info)
                   (cons :env-sleep-seconds sleep-seconds)
                   #+darwin (cons :env-cpu-counters cpu-counters)
                   #+darwin (cons :env-sysctl-mib sysctl-mib)
                   (cons :env-run-program run-program)
                   (cons :env-executable-p executable-p)
                   (cons :env-list-directory list-directory)
                   (cons :env-read-link read-link)
                   (cons :env-file-system-space file-system-space)
                   (cons :env-system-identity system-identity)
                   (cons :env-user-id user-id)
                   (cons :env-workspace-root workspace-root)))
      (setf boundaries
            (cl-boundary-kit:boundary-context-with
             boundaries (car capability) (cdr capability))))
    boundaries))

(defun make-production-env-boundaries (&key state-directory-function with-span &allow-other-keys)
  "Return the binary env context; host access stays inside its capabilities."
  (declare (ignore state-directory-function))
  (labels ((span-outcome (values)
             (case (first values)
               (:timeout :timeout)
               (:not-started :unavailable)
               (otherwise :ok)))
           (instrumented-run-program (program arguments &key timeout)
             (if with-span
                 (funcall with-span :child_process "env.system-query"
                          (function %run-program)
                          (list program arguments :timeout timeout)
                          :outcome-function (function span-outcome))
                 (%run-program program arguments :timeout timeout))))
    (make-env-boundaries
     :clock (cl-boundary-kit:make-clock :now-fn (function %epoch-milliseconds))
     :environment (cl-boundary-kit:make-environment)
     :filesystem (cl-boundary-kit:make-filesystem
                  :read-file-fn (function %read-file-to-end)
                  :path-exists-p-fn (function %regular-file-p))
     :host-info (cl-boundary-kit:make-host-info)
     :run-program (function instrumented-run-program))))
