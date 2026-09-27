;;;; packages/core/store/src/infrastructure/posix-syscall.lisp
;;;;
;;;; The syscall layer under the POSIX store adapter: the *at(2) constants
;;;; sb-posix lacks, the macros that turn errno and stream failures into
;;;; STORE-IO-ERROR, lstat, and the parent pinning walk that posix-io.lisp's
;;;; header describes.
(in-package #:aitools.store.infrastructure)

;; sb-posix binds neither the *at(2) calls nor these constants, whose values
;; differ between Darwin and Linux. O_CLOEXEC keeps a process started by
;; `run` or `bg start` from inheriting a descriptor, above all the lock's,
;; which would keep the workspace locked until that process exited.
(defconstant +at-fdcwd+ #+darwin -2 #-darwin -100)
(defconstant +at-removedir+ #+darwin #x80 #-darwin #x200)
(defconstant +o-cloexec+ #+darwin #x1000000 #-darwin #x80000)

(defun %errno-error (operation path errno)
  (error 'store-io-error :operation operation :path path :errno errno
                         :detail (sb-int:strerror errno)))

(defun %io-error (operation path condition)
  (%errno-error operation path (sb-posix:syscall-errno condition)))

(defmacro %syscall ((operation path) &body body)
  `(handler-case (progn ,@body)
     (sb-posix:syscall-error (condition)
       (%io-error ,operation ,path condition))))

(defmacro %alien-call ((operation path) name type &rest arguments)
  "Call the libc function NAME; a negative result signals STORE-IO-ERROR."
  (let ((result (gensym "RESULT")))
    `(let ((,result (sb-alien:alien-funcall (sb-alien:extern-alien ,name ,type) ,@arguments)))
       (when (minusp ,result)
         (%errno-error ,operation ,path (sb-alien:get-errno)))
       ,result)))

(defmacro %stream-errors ((operation path) &body body)
  "Report a stream error (ENOSPC, EIO) as the STORE-IO-ERROR every caller
handles, not as an SBCL stream condition."
  `(handler-case (progn ,@body)
     (stream-error (condition)
       (error 'store-io-error :operation ,operation :path ,path :detail (princ-to-string condition)))))

(defun %lstat (path)
  (handler-case
      (let* ((stat (sb-posix:lstat path))
             (mode (sb-posix:stat-mode stat))
             (type (logand mode sb-posix:s-ifmt))
             (mtime (sb-posix:stat-mtime stat))
             (links (sb-posix:stat-nlink stat)))
        (cond ((= type sb-posix:s-ifreg) (values :file (logand mode #o7777) nil mtime links))
              ((= type sb-posix:s-ifdir) (values :directory (logand mode #o7777) nil mtime links))
              ((= type sb-posix:s-iflnk) (values :symlink nil (sb-posix:readlink path) mtime links))
              (t (values :other (logand mode #o7777) nil mtime links))))
    (sb-posix:syscall-error (condition)
      (if (member (sb-posix:syscall-errno condition) (list sb-posix:enoent sb-posix:enotdir))
          (values :absent nil nil)
          (%io-error "lstat" path condition)))))

;;; Parent pinning.

(defun %relative-below (root path)
  "PATH relative to ROOT when PATH is strictly below it, else NIL."
  (let ((length (length root)))
    (and root
         (> (length path) (1+ length))
         (string= root path :end2 length)
         (char= (char path length) #\/)
         (subseq path (1+ length)))))

(defun %open-directory-at (dirfd name path &key (follow nil))
  (%alien-call ("open" path) "openat"
               (function sb-alien:int sb-alien:int sb-alien:c-string sb-alien:int &optional sb-alien:unsigned-int)
               dirfd name
               (logior sb-posix:o-rdonly sb-posix:o-directory +o-cloexec+ (if follow 0 sb-posix:o-nofollow))
               0))

(defun %call-with-parent (root path function)
  "Call FUNCTION with (dirfd name) naming PATH: a pinned descriptor of its
parent and its last component when PATH is below ROOT, else AT_FDCWD and
PATH itself."
  (declare (type function function))
  (let ((relative (and root (%relative-below root path))))
    (if (null relative)
        (funcall function +at-fdcwd+ path)
        (let ((components (loop with start = 0
                                for slash = (position #\/ relative :start start)
                                for component = (subseq relative start (or slash (length relative)))
                                unless (zerop (length component)) collect component
                                while slash
                                do (setf start (1+ slash))))
              (fd (%open-directory-at +at-fdcwd+ root root :follow t))
              (walked root))
          (unwind-protect
               (progn
                 (dolist (component (butlast components))
                   (setf walked (concatenate 'string walked "/" component))
                   (let ((next (%open-directory-at fd component walked)))
                     (rotatef fd next)
                     (sb-posix:close next)))
                 (funcall function fd (car (last components))))
            (sb-posix:close fd))))))

(defmacro %with-parent (((dirfd name) root path) &body body)
  `(%call-with-parent ,root ,path (lambda (,dirfd ,name) ,@body)))
