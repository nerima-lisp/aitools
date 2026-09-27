;;;; packages/core/store/src/infrastructure/posix-file-io.lisp
;;;;
;;;; Whole-file reads and writes through file descriptors, and in-place mode
;;;; and modification-time changes that refuse a symlink or a regular file
;;;; with other hard links.
(in-package #:aitools.store.infrastructure)

;;; Reading and writing whole files.

(defun %fd-stream (fd direction)
  (sb-sys:make-fd-stream fd direction t :element-type '(unsigned-byte 8) :buffering :full))

(defun %read-file (path)
  (let ((fd (%syscall ("open" path)
              (sb-posix:open path (logior sb-posix:o-rdonly sb-posix:o-nofollow +o-cloexec+)))))
    (with-open-stream (stream (%fd-stream fd :input))
      (%stream-errors ("read" path)
        (let ((chunks '()) (total 0))
          (loop
            (let* ((buffer (make-array 65536 :element-type '(unsigned-byte 8)))
                   (count (read-sequence buffer stream)))
              (when (plusp count)
                (push (if (= count (length buffer)) buffer (subseq buffer 0 count)) chunks)
                (incf total count))
              (when (< count (length buffer))
                (return))))
          (let ((result (make-array total :element-type '(unsigned-byte 8)))
                (start 0))
            (dolist (chunk (nreverse chunks) result)
              (replace result chunk :start1 start)
              (incf start (length chunk)))))))))

(defun %write-and-close (path fd octets sync)
  (let ((stream (%fd-stream fd :output))
        (written nil))
    (unwind-protect
         (%stream-errors ("write" path)
           (write-sequence octets stream)
           (finish-output stream)
           (when sync
             (%syscall ("fsync" path) (sb-posix:fsync fd)))
           (setf written t))
      ;; After a failed write the buffer still holds the bytes; a plain close
      ;; would try to flush them again and signal a second stream error.
      (close stream :abort (not written)))))

(defun %create-file (root path octets &key mode sync)
  (let* ((mode (or mode #o600))
         (fd (%with-parent ((dirfd name) root path)
               ;; The mode is given to open itself, so the file is never more
               ;; permissive than MODE, even for a moment.
               (%alien-call ("create" path) "openat"
                            (function sb-alien:int sb-alien:int sb-alien:c-string sb-alien:int
                                      &optional sb-alien:unsigned-int)
                            dirfd name
                            (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl sb-posix:o-nofollow
                                    +o-cloexec+)
                            mode))))
    ;; open applies the umask; a requested mode (a file `chmod` set) must
    ;; hold exactly.
    (handler-case (sb-posix:fchmod fd mode)
      (sb-posix:syscall-error (condition)
        (sb-posix:close fd)
        (%io-error "fchmod" path condition)))
    (%write-and-close path fd octets sync)))

(defun %append-file (path octets &key sync)
  (let ((fd (%syscall ("open" path)
              (sb-posix:open path (logior sb-posix:o-wronly sb-posix:o-append sb-posix:o-nofollow +o-cloexec+)))))
    (%write-and-close path fd octets sync)))

;;; Changes in place: mode and modification time.

(defun %open-in-place (dirfd name path)
  "A read-only descriptor of the entry NAME in DIRFD, never through a
symlink, or NIL when its permissions deny opening it (EACCES)."
  (let ((fd (sb-alien:alien-funcall
             (sb-alien:extern-alien "openat" (function sb-alien:int sb-alien:int sb-alien:c-string sb-alien:int
                                                       &optional sb-alien:unsigned-int))
             dirfd name
             (logior sb-posix:o-rdonly sb-posix:o-nofollow sb-posix:o-nonblock +o-cloexec+)
             0)))
    (if (minusp fd)
        (let ((errno (sb-alien:get-errno)))
          (if (= errno sb-posix:eacces)
              nil
              (%errno-error "open" path errno)))
        fd)))

(defun %refuse-shared-inode (operation path mode links &key require-file)
  "A mode or time change applies to the inode, so it would reach every other
hard link of a regular file, including one outside the workspace."
  (let ((type (logand mode sb-posix:s-ifmt)))
    (cond ((= type sb-posix:s-iflnk)
           (error 'store-io-error :operation operation :path path :detail "is a symlink"))
          ((and require-file (/= type sb-posix:s-ifreg))
           (error 'store-io-error :operation operation :path path :detail "not a regular file"))
          ((and (= type sb-posix:s-ifreg) (> links 1))
           (error 'store-io-error :operation operation :path path
                                  :detail (format nil "has ~D hard links" links))))))

(defun %call-in-place (root path operation require-file on-fd on-name)
  "Call ON-FD (fd stat) with an open descriptor of PATH, or, when its
permissions deny opening it, ON-NAME (dirfd name stat) after checking it by
lstat. Either way PATH is checked by %REFUSE-SHARED-INODE first."
  (declare (type function on-fd on-name))
  (%with-parent ((dirfd name) root path)
    (let ((fd (%open-in-place dirfd name path)))
      (if fd
          (unwind-protect
               (let ((stat (%syscall ("fstat" path) (sb-posix:fstat fd))))
                 (%refuse-shared-inode operation path (sb-posix:stat-mode stat) (sb-posix:stat-nlink stat)
                                       :require-file require-file)
                 (funcall on-fd fd stat))
            (sb-posix:close fd))
          (let ((stat (%syscall ("lstat" path) (sb-posix:lstat path))))
            (%refuse-shared-inode operation path (sb-posix:stat-mode stat) (sb-posix:stat-nlink stat)
                                  :require-file require-file)
            (funcall on-name dirfd name stat))))))

(defun %chmod (root path mode)
  (%call-in-place root path "chmod" nil
                  (lambda (fd stat)
                    (declare (ignore stat))
                    (%syscall ("chmod" path) (sb-posix:fchmod fd mode)))
                  (lambda (dirfd name stat)
                    (declare (ignore stat))
                    (%alien-call ("chmod" path) "fchmodat"
                                 (function sb-alien:int sb-alien:int sb-alien:c-string sb-alien:unsigned-int
                                           sb-alien:int)
                                 dirfd name mode 0))))

(defun %set-mtime (root path unix-seconds)
  "Set the regular file PATH's modification time, keeping its access time."
  (flet ((set-times (atime set)
           (sb-alien:with-alien ((times (array sb-alien:long 4)))
             ;; struct timeval[2]: {atime, 0 usec}, {mtime, 0 usec}. Darwin's
             ;; tv_usec is an int followed by padding, so writing a long 0 over
             ;; both is the same on little-endian machines.
             (setf (sb-alien:deref times 0) atime
                   (sb-alien:deref times 1) 0
                   (sb-alien:deref times 2) unix-seconds
                   (sb-alien:deref times 3) 0)
             (funcall set (sb-alien:addr times)))))
    (%call-in-place root path "utimes" t
                    (lambda (fd stat)
                      (set-times (sb-posix:stat-atime stat)
                                 (lambda (times)
                                   (%alien-call ("utimes" path) "futimes"
                                                (function sb-alien:int sb-alien:int (* (array sb-alien:long 4)))
                                                fd times))))
                    (lambda (dirfd name stat)
                      (declare (ignore dirfd name))
                      (%syscall ("utimes" path) (sb-posix:utimes path (sb-posix:stat-atime stat) unix-seconds))))))
