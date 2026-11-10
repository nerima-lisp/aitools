;;;;
;;;; POSIX filesystem capabilities for workspace boundary contexts. Paths stay native strings all the way to
;;;; the system call (sb-posix), never CL pathnames, so names containing `*`,
;;;; `?`, `[`, or `\` work. Environment and home directory come from
;;;; cl-boundary-kit environment; the home directory comes from host-kit.
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
     :mode (logand mode #o7777)
     :device (sb-posix:stat-dev stat)
     :inode (sb-posix:stat-ino stat))))

(defun %basename (path)
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path (1+ slash)) path)))

(defun %lstat-entry (path)
  (handler-case (%entry-from-stat (%basename path) (sb-posix:lstat path))
    (error () nil)))

(defun %open-directory (path)
  (handler-case (sb-posix:opendir path)
    (error () nil)))

(defun %next-directory-name (stream name-reader)
  (let ((dirent (sb-posix:readdir stream)))
    (if (or (null dirent) (sb-alien:null-alien dirent))
        (values nil :end)
      (let ((name (handler-case (funcall name-reader dirent)
                    (error () nil))))
        (if name
            (values name :entry)
          (values nil :invalid))))))

(defun %directory-child-entry (path name)
  (if (member name '("." "..") :test (function string=))
      (values nil t)
    (let ((entry
           (%lstat-entry
            (concatenate (quote string)
                         (string-right-trim "/" path)
                         "/"
                         name))))
      (values entry (not (null entry))))))

(defun %record-directory-child (path name entries complete-p)
  (multiple-value-bind (entry child-complete-p)
      (%directory-child-entry path name)
    (when entry (push entry entries))
    (values entries (and complete-p child-complete-p))))

(defun %list-directory-entries (stream path &optional (name-reader (function sb-posix:dirent-name)))
  (let ((entries nil)
        (complete-p t))
    (loop
     (multiple-value-bind (name status)
         (%next-directory-name stream name-reader)
       (case status
         (:end (return))
         (:invalid (setf complete-p nil))
         (:entry
          (multiple-value-setq (entries complete-p)
                               (%record-directory-child path name entries complete-p))))))
    (values entries t complete-p)))

(defun %list-directory (path)
  (let ((stream (%open-directory path)))
    (unless stream
      (return-from %list-directory (values nil nil nil)))
    (unwind-protect
        (%list-directory-entries stream path)
      (sb-posix:closedir stream))))
(defun %read-link (path)
  (handler-case (sb-posix:readlink path)
    (error () nil)))

(defun %read-stream-octets (stream stat-size max-bytes &key (reader (function read-sequence)))
  "Read STREAM up to MAX-BYTES and return NIL, :TOO-LARGE on overflow."
  (check-type stat-size (integer 0))
  (check-type max-bytes (or null (integer 0)))
  (when (and max-bytes (> stat-size max-bytes))
    (return-from %read-stream-octets (values nil :too-large)))
  (let ((chunks nil)
        (total 0))
    (labels ((finish ()
               (let ((result
                       (make-array total :element-type (quote (unsigned-byte 8))))
                     (offset 0))
                 (dolist (chunk (nreverse chunks) result)
                   (replace result chunk :start1 offset)
                   (incf offset (length chunk))))))
      (loop
        (let* ((read-count
                 (if max-bytes
                     (min 65536 (1+ (- max-bytes total)))
                     65536))
               (buffer
                 (make-array read-count :element-type (quote (unsigned-byte 8))))
               (count (funcall reader buffer stream)))
          (when (and max-bytes (> (+ total count) max-bytes))
            (return-from %read-stream-octets (values nil :too-large)))
          (when (plusp count)
            (push (if (= count (length buffer)) buffer (subseq buffer 0 count))
                  chunks)
            (incf total count))
          (when (< count (length buffer))
            (return-from %read-stream-octets (values (finish) nil))))))))
(defun read-regular-file-octets (path &key max-bytes)
  "PATH bytes when regular, else NIL. Returns NIL and :TOO-LARGE when MAX-BYTES is exceeded."
  (check-type max-bytes (or null (integer 0)))
  (labels ((read-open-fd (fd)
             (let ((stream nil))
               (unwind-protect
                    (handler-case
                        (let* ((stat (sb-posix:fstat fd))
                               (mode (sb-posix:stat-mode stat))
                               (size (max 0 (sb-posix:stat-size stat))))
                          (when (sb-posix:s-isreg mode)
                            (if (and max-bytes (> size max-bytes))
                                (values nil :too-large)
                                (progn
                                  (setf stream
                                        (sb-sys:make-fd-stream fd :input t
                                                                 :element-type (quote (unsigned-byte 8))
                                                                 :buffering :full
                                                                 :auto-close nil))
                                  (%read-stream-octets stream size max-bytes)))))
                      (error () nil))
                 ;; Closing the fd-stream closes FD itself; closing both would
                 ;; close a descriptor number another thread may already have reused.
                 (if stream
                     (close stream)
                     (sb-posix:close fd))))))
    (let ((fd
            (handler-case
                (sb-posix:open path (logior sb-posix:o-rdonly sb-posix:o-nonblock))
              (error () nil))))
      (when fd (read-open-fd fd)))))

(defun %home-directory ()
  (string-right-trim "/" (sb-ext:native-namestring (host-kit:user-home-directory))))

(defun %current-directory ()
  (let ((cwd (sb-posix:getcwd)))
    (if (string= cwd "/") cwd (string-right-trim "/" cwd))))

(defun make-host-workspace-boundaries (&key (getenv (function host-kit:getenv))
                                           (home-directory (function %home-directory))
                                           (current-directory (function %current-directory)))
  "Build production environment, working-directory, and POSIX workspace boundaries."
  (make-workspace-boundaries
   :list-directory (function %list-directory)
   :stat (function %lstat-entry)
   :read-link (function %read-link)
   :read-octets (function read-regular-file-octets)
   :environment (cl-boundary-kit:make-environment :get-fn getenv)
   :working-directory
   (cl-boundary-kit:make-working-directory
    :get-fn (lambda ()
              (let ((directory (funcall current-directory)))
                (if (stringp directory)
                    directory
                    (sb-ext:native-namestring directory)))))
   :home-directory home-directory
   :call-with-ordered-mapper (function call-with-ordered-mapper)))
