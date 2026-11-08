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

(defun view-path-state (view path &key max-bytes)
  "ENTRY-STATE of PATH in VIEW. A staged file's hash is its blob name."
  (let ((entry (%view-entry view path)))
    (if entry
        (tx-path-staged entry)
        (workspace-state (store-view-store view) path :max-bytes max-bytes))))

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

(defun view-read-file (view path &key max-bytes)
  "Bytes of PATH in VIEW when it is a regular file there, else NIL."
  (let ((entry (%view-entry view path))
        (store (store-view-store view)))
    (if entry
        (let ((staged (tx-path-staged entry)))
          (and (eq (entry-state-kind staged) :file)
               (read-blob store (entry-state-hash staged) :max-bytes max-bytes)))
        (%read-file-if-exists store (%workspace-path store path) :max-bytes max-bytes))))

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

(aitools.kernel.domain:define-flow
    (%view-plan/k view requests &key on-planned on-rejected)
  (aitools.kernel.domain:flow-call
      (plan-changes/k requests
                      :lookup-state
                      (lambda (path)
                        (view-path-state
                         view path
                         :max-bytes (%request-byte-limit requests path)))
                      :lookup-content
                      (lambda (path)
                        (view-read-file
                         view path
                         :max-bytes (%request-byte-limit requests path)))
                      :list-children (lambda (path) (%view-children view path))
                      :lookup-mtime (lambda (path) (view-path-mtime view path)))
    (:on-planned (results)
      (aitools.kernel.domain:flow-continue :on-planned results))
    (:on-rejected (code message)
      (aitools.kernel.domain:flow-continue :on-rejected code message))))

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

(defun %archive-symlink-component-key (component)
  (aitools.text.domain:normalize-text
   (sb-unicode:casefold
    (aitools.text.domain:normalize-text component :nfd))
   :nfd))

(defun %archive-prefix-node (node component)
  (let* ((key (%archive-symlink-component-key component))
         (child (gethash key node)))
    (or child
        (setf (gethash key node) (make-hash-table :test 'equal)))))

(progn
  (defun %archive-prefix-add-path (root prefix path)
    (let ((node root))
      (labels ((add-components (path)
                 (dolist (component (aitools.workspace.domain:path-components path))
                   (unless (string= component ".")
                     (setf node (%archive-prefix-node node component))))))
        (add-components prefix)
        (add-components path))
      (setf (gethash :symlink node) t)))
  (defun %archive-stage-node (node component)
    (let ((child (gethash component node)))
      (or child
          (setf (gethash component node) (make-hash-table :test 'equal)))))
  (defun %archive-stage-add-path (root path kind)
    (let ((node root))
      (dolist (component (aitools.workspace.domain:path-components path))
        (unless (string= component ".")
          (setf node (%archive-stage-node node component))))
      (setf (gethash :kind node) kind))))

(progn
  (defstruct (%archive-prefix-frame (:copier nil))
    folded-node
    stage-node
    (symlink-p nil :type boolean)
    (disk-live-p t :type boolean)
    parent-identity
    (moved-p nil :type boolean))
  (defstruct (%archive-prefix-cursor (:copier nil))
    (current-fd nil :type (or null integer))
    (current-identity nil :type (or null string))
    folded-node
    stage-node
    (symlink-p nil :type boolean)
    (disk-live-p t :type boolean)
    (stack nil)
    (finished-p nil :type boolean)))

(progn
(defun %archive-prefix-finish-cursor (cursor close-directory)
  (unless (%archive-prefix-cursor-finished-p cursor)
    (let ((fd (%archive-prefix-cursor-current-fd cursor)))
      (unwind-protect
           (when fd (funcall close-directory fd))
        (setf (%archive-prefix-cursor-current-fd cursor) nil
              (%archive-prefix-cursor-stack cursor) nil
              (%archive-prefix-cursor-finished-p cursor) t))))
  nil)
(defun %archive-prefix-start (template open-entry close-directory)
  (let ((fd nil)
        (cursor nil)
        (started nil))
    (unwind-protect
         (progn
           (multiple-value-bind (kind opened identity)
               (funcall open-entry (%archive-prefix-cursor-current-fd template) "." ".")
             (setf fd opened)
             (unless (and (eq kind :directory)
                          (equal identity (%archive-prefix-cursor-current-identity template)))
               (error 'store-io-error :operation "open" :path "."
                                      :detail "directory cursor identity changed"))
             (setf cursor
                   (make-%archive-prefix-cursor
                    :current-fd fd
                    :current-identity identity
                    :folded-node (%archive-prefix-cursor-folded-node template)
                    :stage-node (%archive-prefix-cursor-stage-node template)
                    :symlink-p (%archive-prefix-cursor-symlink-p template)
                    :disk-live-p (%archive-prefix-cursor-disk-live-p template)
                    :stack (%archive-prefix-cursor-stack template)))
             (setf fd nil))
           (setf started t)
           cursor)
      (unless started
        (if cursor
            (%archive-prefix-finish-cursor cursor close-directory)
            (when fd (funcall close-directory fd)))))))
(defun %archive-prefix-enter (cursor component open-entry close-directory deletions-visible)
  (let* ((folded-node (%archive-prefix-cursor-folded-node cursor))
         (stage-node (%archive-prefix-cursor-stage-node cursor))
         (folded-child
           (and folded-node
                (gethash (%archive-symlink-component-key component) folded-node)))
         (stage-child (and stage-node (gethash component stage-node)))
         (stage-present-p (and stage-child (nth-value 1 (gethash :kind stage-child))))
         (stage-kind (and stage-present-p (gethash :kind stage-child)))
         (frame
           (make-%archive-prefix-frame
            :folded-node folded-node
            :stage-node stage-node
            :symlink-p (%archive-prefix-cursor-symlink-p cursor)
            :disk-live-p (%archive-prefix-cursor-disk-live-p cursor)
            :parent-identity (%archive-prefix-cursor-current-identity cursor))))
    (push frame (%archive-prefix-cursor-stack cursor))
    (setf (%archive-prefix-cursor-folded-node cursor) folded-child
          (%archive-prefix-cursor-stage-node cursor) stage-child
          (%archive-prefix-cursor-symlink-p cursor)
          (or (%archive-prefix-cursor-symlink-p cursor)
              (and folded-child (gethash :symlink folded-child))
              (eq stage-kind :symlink)))
    (cond
      ((%archive-prefix-cursor-symlink-p cursor)
       (setf (%archive-prefix-cursor-disk-live-p cursor) nil))
      ((%archive-prefix-cursor-disk-live-p cursor)
       (multiple-value-bind (kind next-fd identity)
           (funcall open-entry
                    (%archive-prefix-cursor-current-fd cursor)
                    component
                    component)
         (cond
           ((eq kind :directory)
            (let ((previous-fd (%archive-prefix-cursor-current-fd cursor)))
              (setf (%archive-prefix-frame-moved-p frame) t
                    (%archive-prefix-cursor-current-fd cursor) next-fd
                    (%archive-prefix-cursor-current-identity cursor) identity)
              (funcall close-directory previous-fd)))
           ((eq kind :symlink)
            (setf (%archive-prefix-cursor-disk-live-p cursor) nil)
            (unless (and stage-present-p
                         (or (not deletions-visible)
                             (not (eq stage-kind :absent))))
              (setf (%archive-prefix-cursor-symlink-p cursor) t)))
           (t
            (setf (%archive-prefix-cursor-disk-live-p cursor) nil)))))
      (t
       (setf (%archive-prefix-cursor-disk-live-p cursor) nil)))
    (values cursor
            (if (%archive-prefix-cursor-symlink-p cursor) :symlink :clear))))
(defun %archive-prefix-parent (cursor open-entry close-directory)
  (let ((frame (car (%archive-prefix-cursor-stack cursor))))
    (when (or (null frame) (%archive-prefix-cursor-symlink-p cursor))
      (return-from %archive-prefix-parent (values cursor :outside)))
    (when (%archive-prefix-frame-moved-p frame)
      (multiple-value-bind (kind parent-fd identity)
          (funcall open-entry (%archive-prefix-cursor-current-fd cursor) ".." "..")
        (unless (and (eq kind :directory)
                     parent-fd
                     (equal identity (%archive-prefix-frame-parent-identity frame)))
          (when parent-fd (funcall close-directory parent-fd))
          (return-from %archive-prefix-parent (values cursor :outside)))
        (let ((current-fd (%archive-prefix-cursor-current-fd cursor)))
          (unwind-protect
               (progn
                 (funcall close-directory current-fd)
                 (setf (%archive-prefix-cursor-current-fd cursor) parent-fd
                       (%archive-prefix-cursor-current-identity cursor) identity
                       parent-fd nil))
            (when parent-fd (funcall close-directory parent-fd))))))
    (pop (%archive-prefix-cursor-stack cursor))
    (setf (%archive-prefix-cursor-folded-node cursor)
          (%archive-prefix-frame-folded-node frame)
          (%archive-prefix-cursor-stage-node cursor)
          (%archive-prefix-frame-stage-node frame)
          (%archive-prefix-cursor-symlink-p cursor)
          (%archive-prefix-frame-symlink-p frame)
          (%archive-prefix-cursor-disk-live-p cursor)
          (%archive-prefix-frame-disk-live-p frame))
    (values cursor :clear))))

(defun make-archive-symlink-prefix-checker
    (view boundary archive-symlink-paths &key deletions-visible)
  (let* ((symlink-root (make-hash-table :test 'equal))
         (staged-root (make-hash-table :test 'equal))
         (index (store-view-index view))
         (store (store-view-store view))
         (io (store-io-port store))
         (open-root (store-io-open-search-root io))
         (open-entry (store-io-open-search-entry io))
         (close-directory (store-io-close-search-directory io))
         (boundary-components
           (remove-if (lambda (component)
                        (or (zerop (length component))
                            (string= component ".")))
                      (aitools.workspace.domain:path-components
                       (aitools.workspace.domain:normalize-path boundary))))
         (root-fd nil)
         (root-identity nil)
         (template-fd nil)
         (template nil)
         (ready nil))
    (unless (and (functionp open-root)
                 (functionp open-entry)
                 (functionp close-directory))
      (error 'store-io-error :operation "open" :path boundary
                             :detail "directory cursor ports are unavailable"))
    (when index
      (loop for entry being the hash-values of (tx-index-paths index)
            for path = (tx-path-path entry)
            for kind = (entry-state-kind (tx-path-staged entry))
            do (progn
                 (%archive-stage-add-path staged-root path kind)
                 (when (eq kind :symlink)
                   (%archive-prefix-add-path symlink-root "" path)))))
    (dolist (path archive-symlink-paths)
      (%archive-prefix-add-path symlink-root boundary path))
    (unwind-protect
         (progn
           (multiple-value-setq (root-fd root-identity)
             (funcall open-root (store-root store)))
           (multiple-value-bind (kind fd identity)
               (funcall open-entry root-fd "." ".")
             (setf template-fd fd)
             (unless (and (eq kind :directory)
                          (equal identity root-identity))
               (error 'store-io-error :operation "open" :path boundary
                                      :detail "workspace root identity changed"))
             (setf template
                   (make-%archive-prefix-cursor
                    :current-fd template-fd
                    :current-identity identity
                    :folded-node symlink-root
                    :stage-node staged-root
                    :symlink-p nil
                    :disk-live-p t
                    :stack nil))
             (setf template-fd nil))
           (dolist (component boundary-components)
             (%archive-prefix-enter template component open-entry
                                    close-directory deletions-visible))
           (let ((fd root-fd))
             (setf root-fd nil)
             (funcall close-directory fd))
           (prog1
               (lambda (action &optional cursor component)
                 (ecase action
                   (:start
                    (%archive-prefix-start template open-entry close-directory))
                   (:finish
                    (%archive-prefix-finish-cursor (or cursor template)
                                                   close-directory))
                   (:enter
                    (%archive-prefix-enter cursor component open-entry
                                           close-directory deletions-visible))
                   (:parent
                    (%archive-prefix-parent cursor open-entry
                                            close-directory))))
             (setf ready t)))
      (unless ready
        (when template
          (%archive-prefix-finish-cursor template close-directory))
        (when template-fd
          (funcall close-directory template-fd))
        (when root-fd
          (funcall close-directory root-fd))))))
