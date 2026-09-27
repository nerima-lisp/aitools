;;;; packages/core/store/src/application/view.lisp
;;;;
;;;; A STORE-VIEW is the workspace as a command sees it: the disk (DISK-VIEW),
;;;; or the disk with a tx's staged states laid over it (the tx read-through:
;;;; a staged path answers from the index and its blob, anything else from
;;;; the disk). Write validation, tx staging, and rebase replays all read
;;;; through a view, so one validation function serves `edit` and `edit --tx`.
(in-package #:aitools.store.application)

(defstruct (store-view (:constructor %make-store-view (store index)) (:copier nil))
  (store nil :type store :read-only t)
  ;; NIL for the plain disk.
  (index nil :type (or null tx-index) :read-only t))

(defun disk-view (store)
  (%make-store-view store nil))

(defun %view-entry (view path)
  (and (store-view-index view) (tx-index-find (store-view-index view) path)))

(defun view-path-state (view path)
  "ENTRY-STATE of PATH in VIEW. A staged file's hash is its blob name."
  (let ((entry (%view-entry view path)))
    (if entry
        (tx-path-staged entry)
        (workspace-state (store-view-store view) path))))

(defun view-path-kind (view path)
  "The ENTRY-STATE of PATH in VIEW with its kind, mode, and (for a symlink)
target, but WITHOUT the content hash of a regular file: an lstat, never a
read. A staged path answers from the tx index, as VIEW-PATH-STATE does; a
disk path is lstat'd alone. Use this where only kind or mode matters;
PATH-HASH or VIEW-PATH-STATE supply the hash where a value is compared."
  (let ((entry (%view-entry view path)))
    (if entry
        (tx-path-staged entry)
        (workspace-kind (store-view-store view) path))))

(defun view-path-mtime (view path)
  "PATH's modification time in VIEW (Unix seconds) when it is a regular file
there: a staged file's is the one a tx `touch` staged (NIL when none was),
any other path's is the disk's."
  (let ((entry (%view-entry view path)))
    (if entry
        (entry-state-mtime (tx-path-staged entry))
        (workspace-mtime (store-view-store view) path))))

(defun view-read-file (view path)
  "Bytes of PATH in VIEW when it is a regular file there, else NIL."
  (let ((entry (%view-entry view path))
        (store (store-view-store view)))
    (if entry
        (let ((staged (tx-path-staged entry)))
          (and (eq (entry-state-kind staged) :file)
               (read-blob store (entry-state-hash staged))))
        (%read-file-if-exists store (%workspace-path store path)))))

(defun %view-children (view path)
  "Relative paths of PATH's entries in VIEW, sorted, `.aitools-*.tmp` files
included (they occupy a directory as much as anything else)."
  (let* ((store (store-view-store view))
         (index (store-view-index view))
         (children (%list-children store path)))
    (when index
      (loop for entry being the hash-values of (tx-index-paths index)
            for child = (tx-path-path entry)
            when (string= (parent-relative-path child) path)
              do (if (entry-state-absent-p (tx-path-staged entry))
                     (setf children (remove child children :test #'string=))
                     (pushnew child children :test #'string=))))
    (sort children #'string<)))

(defun view-directory-entries (view path)
  "PATH's entries in VIEW as a sorted list of (name . kind), kind one of
:file :directory :symlink. This is the overlay a `--tx` scan merges into its
walk; the write protocol's temp files are left out, as every scan excludes them."
  (loop for child in (%view-children view path)
        for name = (subseq child (1+ (or (position #\/ child :from-end t) -1)))
        for kind = (entry-state-kind (view-path-kind view child))
        unless (or (temp-file-name-p name) (eq kind :absent))
          collect (cons name kind)))

(defun %view-plan/k (view requests on-planned on-rejected)
  (plan-changes/k requests
                  :lookup-state (lambda (path) (view-path-state view path))
                  :lookup-content (lambda (path) (view-read-file view path))
                  :list-children (lambda (path) (%view-children view path))
                  :lookup-mtime (lambda (path) (view-path-mtime view path))
                  :on-planned on-planned
                  :on-rejected on-rejected))

(defun %hash-view-relative (root real)
  "REAL relative to ROOT (\"\" for ROOT itself), or NIL when outside it."
  (and real (aitools.kernel.domain:path-inside-p root real)
       (let ((relative (aitools.kernel.domain:path-relative-to root real)))
         (if (string= relative ".") "" relative))))

(defun %hash-child-path (directory name)
  (if (zerop (length directory)) name (concatenate 'string directory "/" name)))

(defun path-hash (host root view absolute)
  "The change-detection `hash` of the path ABSOLUTE, one rule shared
by `info` and every write's --expect-hash:

- the content hash of the regular file the path leads to, symlinks followed;
- for a symlink that leads to no regular file (dangling, a directory, a
  loop), the content hash of its target text (the UTF-8 bytes readlink
  returns);
- NIL otherwise (a directory, an absent path).

ROOT is the real root the VIEW (a store view, or NIL for the disk) is
relative to. Symlinks resolve on disk; the resolved path, and the link
itself when its directory is inside ROOT, are then read through VIEW, as
`info --tx` reads them."
  (let* ((real (aitools.workspace.application:resolve-real-path host absolute))
         (relative (%hash-view-relative root real)))
    (flet ((viewed (path) (and view path (plusp (length path)) (view-path-state view path))))
      (or (and real
               (let ((state (viewed relative)))
                 (if state
                     (and (eq (entry-state-kind state) :file) (entry-state-hash state))
                     (let ((entry (aitools.workspace.application:host-stat host real)))
                       (and entry (eq (aitools.workspace.application:workspace-entry-kind entry) :file)
                            (let ((octets (aitools.workspace.application:host-read-octets host real)))
                              (and octets (aitools.kernel.domain:content-hash octets))))))))
          (let* ((parent (aitools.workspace.domain:path-parent absolute))
                 (parent-real (and parent (aitools.workspace.application:resolve-real-path host parent)))
                 (parent-relative (%hash-view-relative root parent-real))
                 (state (and parent-relative
                             (viewed (%hash-child-path parent-relative
                                                       (aitools.workspace.domain:path-basename absolute)))))
                 (target (if state
                             (and (eq (entry-state-kind state) :symlink) (entry-state-target state))
                             (let ((entry (aitools.workspace.application:host-stat host absolute)))
                               (and entry (eq (aitools.workspace.application:workspace-entry-kind entry) :symlink)
                                    (aitools.workspace.application:host-read-link host absolute))))))
            (and target (aitools.kernel.domain:content-hash
                         (aitools.text.domain:encode-utf8 target))))))))
