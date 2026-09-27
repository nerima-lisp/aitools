;;;; packages/core/store/src/application/files.lisp
;;;;
;;;; Compositions of STORE-IO primitives shared by the rest of the context:
;;;; reading a path's ENTRY-STATE, replacing a state file by rename, and
;;;; removing a directory tree under the state directory.
(in-package #:aitools.store.application)

(defun %workspace-path (store relative)
  (if (zerop (length relative))
      (store-root store)
      (join-path (store-root store) relative)))

(defun %state-at (store absolute)
  (multiple-value-bind (kind mode target) (%io store lstat absolute)
    (ecase kind
      (:absent (absent-state))
      (:file (file-state (aitools.kernel.domain:content-hash (%io store read-file absolute)) mode))
      (:directory (directory-state mode))
      (:symlink (symlink-state target))
      (:other (error 'store-io-error :operation "lstat" :path absolute
                                     :detail "not a regular file, directory, or symlink")))))

(defun workspace-state (store relative)
  "The ENTRY-STATE of workspace-relative RELATIVE on disk now (lstat: a
symlink is reported as itself, never followed)."
  (%state-at store (%workspace-path store relative)))

(defun workspace-mtime (store relative)
  "The modification time (Unix seconds) of workspace-relative RELATIVE on
disk now when it is a regular file, else NIL."
  (multiple-value-bind (kind mode target mtime) (%io store lstat (%workspace-path store relative))
    (declare (ignore mode target))
    (and (eq kind :file) mtime)))

(defun %kind-state-at (store absolute)
  "%STATE-AT without a regular file's content hash: an lstat, never a read."
  (multiple-value-bind (kind mode target) (%io store lstat absolute)
    (ecase kind
      (:absent (absent-state))
      (:file (file-state nil mode))
      (:directory (directory-state mode))
      (:symlink (symlink-state target))
      (:other (error 'store-io-error :operation "lstat" :path absolute
                                     :detail "not a regular file, directory, or symlink")))))

(defun workspace-kind (store relative)
  "The ENTRY-STATE of workspace-relative RELATIVE on disk now by lstat alone:
kind, mode, and a symlink's target, but no content hash (no read), for
callers that need only the kind or mode. Its :FILE hash is NIL; WORKSPACE-STATE
supplies the hash where a value is compared."
  (%kind-state-at store (%workspace-path store relative)))

(defun %state-with-mtime (store relative)
  "WORKSPACE-STATE with a regular file's mtime, for comparing against a state
that carries one."
  (let ((state (workspace-state store relative)))
    (if (eq (entry-state-kind state) :file)
        (file-state (entry-state-hash state) (entry-state-mode state) (workspace-mtime store relative))
        state)))

(defun %kind-at (store absolute)
  (values (%io store lstat absolute)))

(defun %read-file-if-exists (store absolute)
  (when (eq (%kind-at store absolute) :file)
    (%io store read-file absolute)))

(defun %read-text-if-exists (store absolute)
  (let ((octets (%read-file-if-exists store absolute)))
    (and octets (octets-string octets))))

(defun %discard-temp (store path)
  "Best-effort removal of a temp file or directory PATH left in place. Once an
atomic publish has renamed it away this finds nothing; a removal failure is
ignored, leaving a stray dot-prefixed name that every scan and the next
garbage collection skip."
  (handler-case
      (unless (eq (%kind-at store path) :absent)
        (%delete-tree store path))
    (store-io-error () nil)))

(defun call-with-temp-file (store path octets on-temp &key sync)
  "The store's atomic publish through a temp file: create PATH holding OCTETS
(fsynced when SYNC), call ON-TEMP with PATH so it can rename PATH over its
target, and remove PATH afterwards. A rename into place leaves nothing to
remove; a failure before or during the rename never leaves the temp behind."
  (declare (type function on-temp))
  (%io store create-file path octets :sync sync)
  (unwind-protect (funcall on-temp path)
    (%discard-temp store path)))

(defmacro with-temp-file ((var store path octets &key sync) &body body)
  (let ((on-temp (gensym "ON-TEMP")))
    `(flet ((,on-temp (,var) ,@body))
       (declare (dynamic-extent #',on-temp))
       (call-with-temp-file ,store ,path ,octets #',on-temp :sync ,sync))))

(defun call-with-temp-dir (store path on-dir &key (mode #o700))
  "The atomic publish of a directory built entry by entry before it is named:
create PATH (MODE 0700 by default), call ON-DIR with PATH so it can fill and
then rename PATH into place, and delete PATH's tree afterwards. A rename into
place leaves nothing to delete; a failure never leaves the temp tree behind."
  (declare (type function on-dir))
  (%io store mkdir path :mode mode)
  (unwind-protect (funcall on-dir path)
    (%discard-temp store path)))

(defmacro with-temp-dir ((var store path &key (mode #o700)) &body body)
  (let ((on-dir (gensym "ON-DIR")))
    `(flet ((,on-dir (,var) ,@body))
       (declare (dynamic-extent #',on-dir))
       (call-with-temp-dir ,store ,path #',on-dir :mode ,mode))))

(defun %replace-file (store absolute text)
  "Replace ABSOLUTE with TEXT through a temp file in the same directory and a
rename, so a reader or a crash sees the old or the new file, never a mix.
Not fsynced: fsync is limited to temp files, blobs, and intent records."
  (let* ((slash (position #\/ absolute :from-end t))
         (temp-path (format nil "~A/.~A.~A.tmp" (subseq absolute 0 slash) (subseq absolute (1+ slash))
                            (%io store random-hex 8))))
    (with-temp-file (temp store temp-path (string-octets text))
      (%io store rename temp absolute))))

(defun %ignoring-vanished (store absolute thunk)
  "Call THUNK; if it fails because ABSOLUTE no longer exists, that is the
outcome a removal wanted, so succeed."
  (handler-case (funcall thunk)
    (store-io-error (condition)
      (unless (eq (%kind-at store absolute) :absent)
        (error condition)))))

(defun %delete-tree (store absolute)
  (case (%kind-at store absolute)
    (:absent)
    (:directory
     (dolist (name (%io store list-directory absolute))
       (%delete-tree store (join-path absolute name)))
     (%ignoring-vanished store absolute (lambda () (%io store rmdir absolute))))
    (t (%ignoring-vanished store absolute (lambda () (%io store unlink absolute))))))

(defun %list-children (store relative)
  "Workspace-relative paths of RELATIVE's entries on disk, sorted."
  (let ((prefix (if (zerop (length relative)) "" (concatenate 'string relative "/"))))
    (sort (mapcar (lambda (name) (concatenate 'string prefix name))
                  (%io store list-directory (%workspace-path store relative)))
          #'string<)))

(defun %path-components (path)
  (loop with start = 0
        for slash = (position #\/ path :start start)
        for component = (subseq path start (or slash (length path)))
        unless (zerop (length component)) collect component
        while slash
        do (setf start (1+ slash))))

(defun %real-path (store absolute)
  "ABSOLUTE with every symlink resolved through LSTAT, the missing tail kept
as written; NIL for a symlink loop (more than 40 hops, Linux's MAXSYMLINKS)."
  (let ((pending (%path-components absolute))
        (resolved "")
        (hops 0))
    (loop while pending
          do (let ((component (pop pending)))
               (cond
                 ((string= component "."))
                 ((string= component "..")
                  (setf resolved (subseq resolved 0 (or (position #\/ resolved :from-end t) 0))))
                 (t
                  (let ((candidate (concatenate 'string resolved "/" component)))
                    (multiple-value-bind (kind mode target) (%io store lstat candidate)
                      (declare (ignore mode))
                      (if (eq kind :symlink)
                          (progn
                            (when (> (incf hops) 40)
                              (return-from %real-path nil))
                            (when (and (plusp (length target)) (char= (char target 0) #\/))
                              (setf resolved ""))
                            (setf pending (append (%path-components target) pending)))
                          (setf resolved candidate))))))))
    (if (zerop (length resolved)) "/" resolved)))

(defun %absolute-inside-p (directory path)
  (or (string= directory path)
      (string= directory "/")
      (and (> (length path) (length directory))
           (string= directory path :end2 (length directory))
           (char= (char path (length directory)) #\/))))

(defun %path-violation (store relative)
  "Why the workspace-relative RELATIVE must not be written, or NIL. The
store's own last check of the write boundary before it touches a path, for intent
records it did not write (recovery) as well as its own: no `.git`
component, no symlinked parent leading out of the root or into `.git`, and
nothing in the state directory unless this is the mktemp area's store."
  (if (not (valid-relative-path-p relative))
      "is not a workspace-relative path"
      (let* ((root (or (%real-path store (store-root store)) (store-root store)))
             (parent (%real-path store (%workspace-path store (parent-relative-path relative))))
             (name (subseq relative (1+ (or (position #\/ relative :from-end t) -1))))
             (target (and parent (join-path parent name))))
        (cond
          ((null parent) "cannot be resolved (symlink loop)")
          ((not (%absolute-inside-p root target)) "leads outside the workspace through a symlink")
          ((or (git-metadata-path-p relative)
               (and (string/= root target)
                    (git-metadata-path-p (subseq target (if (string= root "/") 1 (1+ (length root)))))))
           "is inside a .git directory")
          ((and (not (store-temporary store))
                (let ((state-root (workspace-state-root (store-state-directory store))))
                  (or (%absolute-inside-p state-root target)
                      (let ((real (%real-path store state-root)))
                        (and real (%absolute-inside-p real target))))))
           "is inside the aitools state directory")))))
