;;;; packages/core/store/src/infrastructure/posix-io.lisp
;;;;
;;;; Every path is a native namestring handed straight to the syscall, never
;;;; parsed as a CL pathname (`*`, `?` and `[` are ordinary file name
;;;; characters here). Files the store opens get O_NOFOLLOW: every caller
;;;; has just lstat'ed the path as a regular file or is creating it, so a
;;;; symlink swapped in between is refused rather than followed.
;;;;
;;;; Parent pinning (against TOCTOU races in the write protocol): a STORE-IO made with a ROOT performs
;;;; every change to a path below ROOT relative to a descriptor of its parent
;;;; directory, reached from a descriptor of ROOT one component at a time
;;;; with O_DIRECTORY|O_NOFOLLOW. The store only ever writes real paths, so
;;;; a parent that is a symlink by then was swapped in after validation; the
;;;; walk fails (ELOOP or ENOTDIR) instead of following it, and the pinned
;;;; descriptor is the checked parent itself, whatever is renamed around it
;;;; afterwards. What stays path-based: ROOT itself (its parents are outside
;;;; the workspace), reads (lstat, read-file, list-directory), and the
;;;; chmod/mtime fallback for a file its owner cannot open (mode without
;;;; read permission), which checks the entry with lstat and then changes it
;;;; by name inside the pinned parent. There a swap of that one entry
;;;; between the check and the change is still possible.
;;;;
;;;; The syscall wrappers and the pinning walk are in posix-syscall.lisp, file
;;;; reads, writes and in-place changes in posix-file-io.lisp; this file adds
;;;; directory listing and locks and assembles the STORE-IO.
(in-package #:aitools.store.infrastructure)

(defconstant +lock-ex+ 2)
(defconstant +lock-nb+ 4)
(defconstant +lock-un+ 8)

;;; Directories and locks.

(defun %list-directory (path)
  (let ((directory (handler-case (sb-posix:opendir path)
                     (sb-posix:syscall-error (condition)
                       (if (= (sb-posix:syscall-errno condition) sb-posix:enoent)
                           (return-from %list-directory '())
                           (%io-error "opendir" path condition))))))
    (unwind-protect
         (loop for entry = (sb-posix:readdir directory)
               until (sb-alien:null-alien entry)
               for name = (sb-posix:dirent-name entry)
               unless (member name '("." "..") :test #'string=)
                 collect name)
      (sb-posix:closedir directory))))

(defun %flock (fd operation)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "flock" (function sb-alien:int sb-alien:int sb-alien:int))
   fd operation))

(defun %try-lock (path &key create)
  "The open file descriptor, holding an exclusive flock, or NIL when another
open file description holds it."
  (let ((fd (%syscall ("open" path)
              (sb-posix:open path (logior sb-posix:o-rdwr sb-posix:o-nofollow +o-cloexec+
                                          (if create sb-posix:o-creat 0))
                             #o600))))
    (loop
      (when (zerop (%flock fd (logior +lock-ex+ +lock-nb+)))
        (return fd))
      (let ((errno (sb-alien:get-errno)))
        (unless (= errno sb-posix:eintr)
          (sb-posix:close fd)
          (if (= errno sb-posix:ewouldblock)
              (return nil)
              (%errno-error "flock" path errno)))))))

(defun %unlock (fd)
  (%flock fd +lock-un+)
  (sb-posix:close fd))

(defun %random-hex (count)
  (with-open-file (stream "/dev/urandom" :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (ceiling count 2) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      (subseq (format nil "~(~{~2,'0X~}~)" (coerce bytes 'list)) 0 count))))

(defun %strip-trailing-slash (path)
  (if (and (> (length path) 1) (char= (char path (1- (length path))) #\/))
      (subseq path 0 (1- (length path)))
      path))

(defun %rename (root from to)
  "renameat(2) FROM onto TO. A failure reports TO as the path: FROM is almost
always the store's own temp file, already discarded by the time a repair
names the path, while TO is what blocks the rename. The detail names FROM."
  (%with-parent ((from-dirfd from-name) root from)
    (%with-parent ((to-dirfd to-name) root to)
      (let ((result (sb-alien:alien-funcall
                     (sb-alien:extern-alien "renameat" (function sb-alien:int sb-alien:int sb-alien:c-string
                                                                 sb-alien:int sb-alien:c-string))
                     from-dirfd from-name to-dirfd to-name)))
        (when (minusp result)
          (let ((errno (sb-alien:get-errno)))
            (error 'store-io-error :operation "rename" :path to :errno errno
                                   :detail (format nil "~A (renaming ~A)" (sb-int:strerror errno) from))))
        result))))

(defun make-posix-store-io (&key root
                                 (clock (cl-boundary-kit:make-clock))
                                 (sleeper (cl-boundary-kit:make-sleeper)))
  "The production STORE-IO. Changes to paths below ROOT (the workspace's real
root path, or NIL for none) go through pinned parent directories; see this
file's header. CLOCK supplies `now` (universal time, for journal times and
ids) and the monotonic time lock waits are measured in; SLEEPER performs the
waits between lock attempts."
  (let ((root (and root (%strip-trailing-slash root))))
    (make-store-io
     :lstat #'%lstat
     :read-file #'%read-file
     :create-file (lambda (path octets &rest keys) (apply #'%create-file root path octets keys))
     :append-file #'%append-file
     :rename (lambda (from to) (%rename root from to))
     :unlink (lambda (path)
               (%with-parent ((dirfd name) root path)
                 (%alien-call ("unlink" path) "unlinkat"
                              (function sb-alien:int sb-alien:int sb-alien:c-string sb-alien:int)
                              dirfd name 0)))
     :rmdir (lambda (path)
              (%with-parent ((dirfd name) root path)
                (%alien-call ("rmdir" path) "unlinkat"
                             (function sb-alien:int sb-alien:int sb-alien:c-string sb-alien:int)
                             dirfd name +at-removedir+)))
     :mkdir (lambda (path &key (mode #o777))
              (%with-parent ((dirfd name) root path)
                (%alien-call ("mkdir" path) "mkdirat"
                             (function sb-alien:int sb-alien:int sb-alien:c-string sb-alien:unsigned-int)
                             dirfd name mode)))
     :chmod (lambda (path mode) (%chmod root path mode))
     :symlink (lambda (target path)
                (%with-parent ((dirfd name) root path)
                  (%alien-call ("symlink" path) "symlinkat"
                               (function sb-alien:int sb-alien:c-string sb-alien:int sb-alien:c-string)
                               target dirfd name)))
     :set-mtime (lambda (path unix-seconds) (%set-mtime root path unix-seconds))
     :list-directory #'%list-directory
     :try-lock #'%try-lock
     :unlock #'%unlock
     :sleep (lambda (milliseconds) (cl-boundary-kit:sleeper-sleep sleeper (/ milliseconds 1000)))
     :monotonic-ms (lambda ()
                     (floor (* 1000 (cl-boundary-kit:clock-monotonic clock)) internal-time-units-per-second))
     :now (lambda () (cl-boundary-kit:clock-now clock))
     :random-hex #'%random-hex)))

(defun %real-directory (path)
  (ignore-errors
   (%strip-trailing-slash
    (sb-ext:native-namestring (truename (sb-ext:parse-native-namestring (concatenate 'string path "/")))))))

(defun %temporary-area-p (root state-home)
  "True when ROOT is `<STATE-HOME>/<workspace-id>/tmp`, a workspace's mktemp
area, compared with STATE-HOME as given and as its real path (ROOT is real)."
  (let* ((slash (position #\/ root :from-end t))
         (parent (subseq root 0 slash))
         (id-slash (position #\/ parent :from-end t)))
    (and (string= (subseq root (1+ slash)) "tmp")
         id-slash
         (< (1+ id-slash) (length parent))
         (let ((home (subseq parent 0 id-slash)))
           (or (string= home state-home)
               (equal home (%real-directory state-home)))))))

(defun make-posix-store (root &key (environment (cl-boundary-kit:make-environment)) io)
  "A STORE for the workspace whose real root path is ROOT, with its state
under the state directory resolved from ENVIRONMENT's XDG_STATE_HOME and
HOME. When ROOT is a workspace's mktemp area under that directory the store
is a temporary one (MAKE-STORE's TEMPORARY): its writes are not journaled.
IO defaults to the production adapter pinned at ROOT."
  (let ((root (%strip-trailing-slash root))
        (home (state-home (cl-boundary-kit:environment-get environment "XDG_STATE_HOME")
                          (cl-boundary-kit:environment-get environment "HOME"))))
    (make-store (or io (make-posix-store-io :root root)) root home
                :temporary (%temporary-area-p root home))))
