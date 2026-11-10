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
(defconstant +at-removedir+ #+darwin #x80 #-darwin #x200)
(progn (defconstant +o-cloexec+ #+darwin #x1000000 #-darwin #x80000) (defconstant +o-path+ #+linux #x200000 #-linux 0) (defconstant +o-search+ #+darwin #x40100000 #-darwin 0) (defconstant +o-symlink+ #+darwin #x200000 #-darwin 0) (defconstant +o-evtonly+ #+darwin #x8000 #-darwin 0))

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
             (links (sb-posix:stat-nlink stat))
             (identity (format nil "~D:~D" (sb-posix:stat-dev stat) (sb-posix:stat-ino stat))))
        (cond ((= type sb-posix:s-ifreg) (values :file (logand mode #o7777) nil mtime links identity))
              ((= type sb-posix:s-ifdir) (values :directory (logand mode #o7777) nil mtime links identity))
              ((= type sb-posix:s-iflnk) (values :symlink nil (sb-posix:readlink path) mtime links identity))
              (t (values :other (logand mode #o7777) nil mtime links identity))))
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

(progn
  (defun %open-directory-at (dirfd name path &key (follow nil))
    (%alien-call ("open" path) "openat"
                 (function sb-alien:int sb-alien:int sb-alien:c-string sb-alien:int &optional sb-alien:unsigned-int)
                 dirfd name
                 (logior sb-posix:o-rdonly sb-posix:o-directory +o-cloexec+ (if follow 0 sb-posix:o-nofollow))
                 0))
  (defun %openat (dirfd name flags path &optional (mode 0))
    (%alien-call ("open" path) "openat"
                 (function sb-alien:int sb-alien:int sb-alien:c-string sb-alien:int
                           &optional sb-alien:unsigned-int)
                 dirfd name flags mode))
  (defun %mode-kind (mode)
    (let ((type (logand mode sb-posix:s-ifmt)))
      (cond ((= type sb-posix:s-ifreg) :file)
            ((= type sb-posix:s-ifdir) :directory)
            ((= type sb-posix:s-iflnk) :symlink)
            (t :other))))
  (defun %descriptor-info (fd path)
    (let* ((stat (%syscall ("fstat" path) (sb-posix:fstat fd)))
           (kind (%mode-kind (sb-posix:stat-mode stat)))
           (identity (format nil "~D:~D" (sb-posix:stat-dev stat) (sb-posix:stat-ino stat))))
      (values kind identity)))
  (defun %search-directory-flags ()
    (logior +o-cloexec+ #+darwin +o-search+ #+linux +o-path+ #+linux sb-posix:o-directory))
  (defun %open-search-root (path)
    (let ((fd (%alien-call ("open" path) "open"
                           (function sb-alien:int sb-alien:c-string sb-alien:int &optional sb-alien:unsigned-int)
                           path (%search-directory-flags) 0)))
      (handler-case
          (multiple-value-bind (kind identity) (%descriptor-info fd path)
            (unless (eq kind :directory)
              (error 'aitools.store.application:store-io-error
                     :operation "open" :path path :detail "workspace root is not a directory"))
            (values fd identity))
        (error (condition)
          (sb-posix:close fd)
          (error condition)))))
  #+linux
  (defun %open-entry-linux (dirfd name path)
    (handler-case
        (let ((fd (%openat dirfd name (logior +o-path+ sb-posix:o-nofollow +o-cloexec+) path)))
          (handler-case
              (multiple-value-bind (kind identity) (%descriptor-info fd path)
                (if (eq kind :directory)
                    (values kind fd identity)
                    (progn (sb-posix:close fd) (values kind nil nil))))
            (error (condition)
              (sb-posix:close fd)
              (error condition))))
      (aitools.store.application:store-io-error (condition)
        (if (member (aitools.store.application:store-io-error-errno condition)
                    (list sb-posix:enoent sb-posix:enotdir))
            (values :absent nil nil)
            (error condition)))))
  #+darwin
  (defun %open-entry-darwin-metadata (dirfd name path)
    (handler-case
        (let ((fd (%openat dirfd name
                           (logior +o-symlink+ +o-evtonly+ sb-posix:o-nonblock +o-cloexec+)
                           path)))
          (handler-case
              (multiple-value-bind (kind identity) (%descriptor-info fd path)
                (if (eq kind :directory)
                    (values kind fd identity)
                    (progn (sb-posix:close fd) (values kind nil nil))))
            (error (condition)
              (sb-posix:close fd)
              (error condition))))
      (aitools.store.application:store-io-error (condition)
        (if (member (aitools.store.application:store-io-error-errno condition)
                    (list sb-posix:enoent sb-posix:enotdir))
            (values :absent nil nil)
            (error condition)))))
  #+darwin
  (defun %open-entry-darwin (dirfd name path)
    (handler-case
        (let ((fd (%openat dirfd name (logior +o-search+ sb-posix:o-nofollow +o-cloexec+) path)))
          (handler-case
              (multiple-value-bind (kind identity) (%descriptor-info fd path)
                (if (eq kind :directory)
                    (values kind fd identity)
                    (progn (sb-posix:close fd) (values kind nil nil))))
            (error (condition)
              (sb-posix:close fd)
              (error condition))))
      (aitools.store.application:store-io-error (condition)
        (let ((errno (aitools.store.application:store-io-error-errno condition)))
          (cond ((= errno sb-posix:enoent) (values :absent nil nil))
                ((= errno sb-posix:enotdir) (%open-entry-darwin-metadata dirfd name path))
                (t (error condition)))))))
  (defun %open-search-entry (dirfd name path)
    #+darwin (%open-entry-darwin dirfd name path)
    #+linux (%open-entry-linux dirfd name path))
  (defun %close-search-directory (fd)
    (sb-posix:close fd)))

(defun %call-with-parent (root path function)
  "Call FUNCTION with a pinned parent descriptor for PATH. Writes refuse a missing, invalid or unresolved workspace root."
  (declare (type function function))
  (unless (and (stringp root)
               (> (length root) 1)
               (char= (char root 0) #\/)
               (stringp path)
               (plusp (length path))
               (char= (char path 0) #\/))
    (error 'aitools.store.application:store-io-error
           :operation "open"
           :path (or path "")
           :detail "workspace root is missing or invalid"))
  (let ((fd (%open-search-root root)))
    (unwind-protect
         (let ((relative (%relative-below root path)))
           (cond ((string= root path)
                  (funcall function fd "."))
                 (relative
                  (let* ((components
                           (loop with start = 0
                                 for slash = (position #\/ relative :start start)
                                 for component = (subseq relative start (or slash (length relative)))
                                 unless (zerop (length component)) collect component
                                 while slash
                                 do (setf start (1+ slash))))
                         (walked root))
                    (when (or (null components)
                              (some (lambda (component)
                                      (or (string= component ".")
                                          (string= component "..")))
                                    components))
                      (error 'aitools.store.application:store-io-error
                             :operation "open"
                             :path path
                             :detail "path is not beneath the workspace root"))
                    (dolist (component (butlast components))
                      (let ((next (%open-directory-at fd component
                                                      (format nil "~A/~A" walked component))))
                        (sb-posix:close fd)
                        (setf fd next
                              walked (format nil "~A/~A" walked component))))
                    (funcall function fd (car (last components)))))
                 (t
                  (funcall function fd path))))
      (sb-posix:close fd))))

