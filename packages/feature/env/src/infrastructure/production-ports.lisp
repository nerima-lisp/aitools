;;;; packages/feature/env/src/infrastructure/production-ports.lisp
;;;;
;;;; ENV-PORTS from cl-boundary-kit boundaries plus the host adapters the kit
;;;; has no boundary for: process-kit for commands, sb-posix for directory,
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

(defun %list-directory (path)
  "Entry names of directory PATH except `.` and `..`, or NIL when it cannot
be opened."
  (handler-case
      (let ((directory (sb-posix:opendir path)))
        (unwind-protect
             (loop for entry = (sb-posix:readdir directory)
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

(defun %run-program (program arguments timeout-seconds)
  (handler-case
      (let ((result (process-kit:run program arguments
                                     :timeout timeout-seconds :on-timeout :return
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

(defun %system-identity ()
  "(VALUES SYSNAME RELEASE MACHINE) from uname(2). struct utsname holds
fixed char arrays: 256 bytes each on Darwin, 65 on Linux."
  (let ((field-size #+darwin 256 #-darwin 65)
        (buffer (sb-alien:make-alien (sb-alien:unsigned 8) 2048)))
    (unwind-protect
         (if (zerop (sb-alien:alien-funcall
                     (sb-alien:extern-alien "uname" (function sb-alien:int (* (sb-alien:unsigned 8))))
                     buffer))
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
                 (values (field 0) (field 2) (field 4))))
             (values (software-type) "" (string-downcase (machine-type))))
      (sb-alien:free-alien buffer))))

(defun %workspace-root ()
  "The workspace root without --root: the nearest ancestor of the working directory
holding `.git`, else the working directory. Does not start git."
  (let ((start (namestring (uiop:getcwd))))
    (loop for directory = (pathname start) then (uiop:pathname-parent-directory-pathname directory)
          for namestring = (namestring directory)
          when (handler-case (progn (sb-posix:stat (concatenate 'string namestring ".git")) t)
                 (sb-posix:syscall-error () nil))
            return namestring
          when (equal namestring "/")
            return start)))

(defun make-env-ports-from-boundaries (&key clock environment filesystem host-info
                                         (run-program #'%run-program)
                                         (executable-p #'%executable-p)
                                         (list-directory #'%list-directory)
                                         (read-link #'%read-link)
                                         (file-system-space #'%file-system-space)
                                         (system-identity #'%system-identity)
                                         (user-id #'sb-posix:getuid)
                                         (workspace-root #'%workspace-root))
  "ENV-PORTS over cl-boundary-kit CLOCK (whose CLOCK-NOW is Unix epoch
milliseconds), ENVIRONMENT, FILESYSTEM, and HOST-INFO; the remaining
keywords default to the host adapters above and are replaced by fakes in
tests."
  (aitools.env.application:make-env-ports
   :now-milliseconds (lambda () (cl-boundary-kit:clock-now clock))
   :environment-variable (lambda (name) (cl-boundary-kit:environment-get environment name))
   :environment-variables (lambda () (cl-boundary-kit:environment-list environment))
   :read-file (lambda (path external-format)
                (when (cl-boundary-kit:filesystem-path-exists-p filesystem path)
                  (handler-case (cl-boundary-kit:filesystem-read-file filesystem path
                                                                     :external-format external-format)
                    (file-error () nil))))
   :list-directory list-directory
   :read-link read-link
   :executable-p executable-p
   :run-program run-program
   :file-system-space file-system-space
   :system-identity system-identity
   :user-id user-id
   :host-name (lambda () (cl-boundary-kit:host-info-hostname host-info))
   :user-name (lambda () (cl-boundary-kit:host-info-username host-info))
   :workspace-root workspace-root))

(defun make-production-env-ports (&key state-directory-function &allow-other-keys)
  "The ENV-PORTS the binary uses. Construction does no I/O; every adapter
reads the host only when a flow calls it. STATE-DIRECTORY-FUNCTION is part
of the composition root's shared constructor protocol; env keeps no state."
  (declare (ignore state-directory-function))
  (make-env-ports-from-boundaries
   :clock (cl-boundary-kit:make-clock :now-fn #'%epoch-milliseconds)
   :environment (cl-boundary-kit:make-environment)
   :filesystem (cl-boundary-kit:make-filesystem
                :read-file-fn #'%read-file-to-end
                :path-exists-p-fn #'%regular-file-p)
   :host-info (cl-boundary-kit:make-host-info)))
