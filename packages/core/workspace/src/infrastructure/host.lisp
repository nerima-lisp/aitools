;;;; packages/core/workspace/src/infrastructure/host.lisp
;;;;
;;;; The production WORKSPACE-HOST. Paths stay native strings all the way to
;;;; the system call (sb-posix), never CL pathnames, so names containing `*`,
;;;; `?`, `[`, or `\` work. Environment and home directory come from
;;;; host-kit.
(in-package #:aitools.workspace.infrastructure)

(defun %entry-from-stat (name stat)
  (let ((mode (sb-posix:stat-mode stat)))
    (make-workspace-entry
     :name name
     :kind (cond ((sb-posix:s-isreg mode) :file)
                 ((sb-posix:s-isdir mode) :directory)
                 ((sb-posix:s-islnk mode) :symlink)
                 (t :other))
     :size (max 0 (sb-posix:stat-size stat))
     :mtime (sb-posix:stat-mtime stat)
     :mode (logand mode #o7777))))

(defun %basename (path)
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path (1+ slash)) path)))

(defun %lstat-entry (path)
  (handler-case (%entry-from-stat (%basename path) (sb-posix:lstat path))
    (error () nil)))

(defun %list-directory (path)
  (let ((stream (handler-case (sb-posix:opendir path)
                  (error () (return-from %list-directory (values nil nil))))))
    (unwind-protect
         (let ((entries '()))
           (loop
             (let ((dirent (sb-posix:readdir stream)))
               (when (or (null dirent) (sb-alien:null-alien dirent)) (return))
               (let ((name (handler-case (sb-posix:dirent-name dirent) (error () nil))))
                 (when (and name (string/= name ".") (string/= name ".."))
                   (let ((entry (%lstat-entry (concatenate 'string (string-right-trim "/" path) "/" name))))
                     (when entry (push entry entries)))))))
           (values entries t))
      (sb-posix:closedir stream))))

(defun %read-link (path)
  (handler-case (sb-posix:readlink path)
    (error () nil)))

(defun read-regular-file-octets (path)
  "PATH's bytes when it is (or links to) a regular file, else NIL. Opened
O_NONBLOCK and type-checked with fstat on the open descriptor, so a FIFO or
device planted where an ignore or config file is expected neither blocks the
scan nor gets read."
  (let ((fd (handler-case (sb-posix:open path (logior sb-posix:o-rdonly sb-posix:o-nonblock))
              (error () (return-from read-regular-file-octets nil)))))
    (let ((stream nil))
      (unwind-protect
           (handler-case
               (when (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:fstat fd)))
                 (setf stream (sb-sys:make-fd-stream fd :input t :element-type '(unsigned-byte 8)
                                                        :buffering :full :auto-close nil))
                 (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8)))
                       (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
                   (loop for count = (read-sequence buffer stream)
                         while (plusp count)
                         do (loop for i from 0 below count do (vector-push-extend (aref buffer i) out)))
                   (coerce out '(simple-array (unsigned-byte 8) (*)))))
             (error () nil))
        ;; Closing the fd-stream closes FD itself; closing both would close a
        ;; descriptor number another thread may already have reused.
        (if stream (close stream) (sb-posix:close fd))))))

(defun %home-directory ()
  (string-right-trim "/" (sb-ext:native-namestring (host-kit:user-home-directory))))

(defun %current-directory ()
  (let ((cwd (sb-posix:getcwd)))
    (if (string= cwd "/") cwd (string-right-trim "/" cwd))))

(defun make-host-workspace-host (&key (getenv #'host-kit:getenv)
                                      (home-directory #'%home-directory)
                                      (current-directory #'%current-directory))
  "The production WORKSPACE-HOST over the real filesystem and environment,
with a CPU-sized ordered mapper (see CALL-WITH-ORDERED-MAPPER). The keyword
arguments replace the environment-facing functions, for callers (and
integration tests) that must not read the process environment."
  (make-workspace-host
   :list-directory #'%list-directory
   :stat #'%lstat-entry
   :read-link #'%read-link
   :read-octets #'read-regular-file-octets
   :getenv getenv
   :home-directory home-directory
   :current-directory current-directory
   :call-with-ordered-mapper #'call-with-ordered-mapper))
