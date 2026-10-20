;;;; packages/feature/search/src/application/session.lisp
;;;;
;;;; What every search-context flow shares: resolving the workspace root
;;;; (`--root`, the git top, or the working directory), the `--tx` read-through (a staged path is
;;;; read from the tx, directory listings get the tx's additions and
;;;; deletions, and the walk reads the tx's .gitignore), the common scan
;;;; options, and file reads with the binary check.
(in-package #:aitools.search.application)

(defstruct (session (:constructor %make-session (ports root overlay view staged)) (:copier nil))
  (ports nil :type search-ports :read-only t)
  (root nil :read-only t)
  (overlay nil :read-only t)
  ;; The tx's STORE-VIEW and its staged states (relative path ->
  ;; ENTRY-STATE), or NIL outside a tx.
  (view nil :read-only t)
  (staged nil :type (or null hash-table) :read-only t))

(defun %result-fields (fields &optional next-commands)
  (append fields (when next-commands
                   (list (cons "next_commands" next-commands)))))

(defun %host (session)
  (search-ports-workspace-host (session-ports session)))

(defun %root-path (session)
  (aitools.workspace.application:workspace-root-path (session-root session)))

(defun %ignore-source-name (source)
  (ecase source
    (:gitignore "gitignore")
    (:builtin "builtin")
    (:none "none")))

;;; Transaction overlay

(defun %staged-tables (staged)
  "(VALUES children implied): CHILDREN maps a directory's relative path to
the (name . state) pairs staged directly in it; IMPLIED maps a directory to
the names of subdirectories that must exist because something is staged
below them."
  (let ((children (make-hash-table :test 'equal))
        (implied (make-hash-table :test 'equal)))
    (maphash (lambda (path state)
               (let ((parent (or (aitools.workspace.domain:path-parent path) "")))
                 (push (cons (aitools.workspace.domain:path-basename path) state) (gethash parent children)))
               (unless (aitools.store.domain:entry-state-absent-p state)
                 (loop for directory = (aitools.workspace.domain:path-parent path)
                         then (aitools.workspace.domain:path-parent directory)
                       while (and directory (string/= directory ""))
                       do (pushnew (aitools.workspace.domain:path-basename directory)
                                   (gethash (or (aitools.workspace.domain:path-parent directory) "") implied)
                                   :test #'string=))))
             staged)
    (values children implied)))

(defun %staged-entry (view relative name state disk now)
  (let ((kind (aitools.store.domain:entry-state-kind state)))
    (aitools.workspace.application:make-workspace-entry
     :name name
     :kind kind
     :size (if (eq kind :file)
               (length (or (aitools.store.application:view-read-file view relative) #()))
               0)
     :mtime (if disk (aitools.workspace.application:workspace-entry-mtime disk) now)
     :mode (or (aitools.store.domain:entry-state-mode state)
               (and disk (aitools.workspace.application:workspace-entry-mode disk))
               #o644))))

(defun %make-overlay (view staged now)
  (multiple-value-bind (children implied) (%staged-tables staged)
    (aitools.workspace.application:make-workspace-overlay
     :list-directory
     (lambda (relative disk-entries)
       (let ((entries (copy-list disk-entries)))
         (flet ((disk-entry (name)
                  (find name entries :key #'aitools.workspace.application:workspace-entry-name :test #'string=)))
           (loop for (name . state) in (gethash relative children)
                 for disk = (disk-entry name)
                 do (setf entries (remove disk entries))
                    (unless (aitools.store.domain:entry-state-absent-p state)
                      (push (%staged-entry view (aitools.workspace.domain:join-path relative name) name state disk now)
                            entries)))
           (dolist (name (gethash relative implied))
             (unless (disk-entry name)
               (push (aitools.workspace.application:make-workspace-entry :name name :kind :directory
                                                                         :size 0 :mtime now :mode #o755)
                     entries))))
         entries))
     :read-octets
     (lambda (relative)
       (multiple-value-bind (state present) (gethash relative staged)
         (if present
             (values t (and (eq (aitools.store.domain:entry-state-kind state) :file)
                            (aitools.store.application:view-read-file view relative)))
             (values nil nil)))))))

;;; Session setup

(aitools.kernel.domain:define-flow (%unavailable command &key on-error)
  (aitools.kernel.domain:flow-continue :on-error "internal.unexpected"
   "the search context was built without its workspace host or text source"
   :repairs (list (repair "inspect-schema" "Show this command's arguments and rules."
                          (format nil "aitools schema ~A" command)))))

(defun %now (ports)
  (let ((clock (search-ports-unix-now ports)))
    (if clock (funcall clock) 0)))

(aitools.kernel.domain:define-flow
    (%open-tx/k ports workspace-root tx command &key on-session on-error)
  (let ((open-store (search-ports-open-store ports)))
    (if (null open-store)
        (aitools.kernel.domain:flow-call (%unavailable command)
          (:on-error (code message &key repairs)
            (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs)))
        (let ((store (funcall open-store (aitools.workspace.application:workspace-root-real workspace-root)))
              (staged (make-hash-table :test 'equal)))
          (flet ((missing ()
                   (aitools.kernel.domain:flow-continue :on-error
                    "input.not-found" (format nil "tx ~A does not exist" tx)
                    :repairs (list (repair "list-tx" "List the open transactions." "aitools tx status"))))
                 (with-view (view)
                   (aitools.kernel.domain:flow-continue :on-session
                    (%make-session ports workspace-root (%make-overlay view staged (%now ports))
                                   view staged))))
            (aitools.kernel.domain:flow-call
             (aitools.store.application:tx-status/k
              store
              tx)
             (:on-not-found ()
               (funcall #'missing))
             (:on-status (status)
               (dolist (entry (aitools.store.application:tx-status-paths status))
                 (setf (gethash
                        (aitools.store.domain:tx-path-path entry)
                        staged)
                       (aitools.store.domain:tx-path-staged entry)))
               (aitools.kernel.domain:flow-call
                (aitools.store.application:call-with-tx-view/k
                 store
                 tx)
                (:on-view (view)
                  (funcall #'with-view view))
                (:on-not-found ()
                  (funcall #'missing))))))))))

(aitools.kernel.domain:define-flow
    (call-with-session/k ports command &key root tx on-session on-error)
  "Resolve the workspace root by --root, git top, then working directory. Open
TX's read-through view when given and continue with the session or an error.
COMMAND names the command used to build repairs."
  (let ((host (search-ports-workspace-host ports)))
    (if (not (and host (search-ports-text-source ports)))
        (aitools.kernel.domain:flow-call (%unavailable command)
          (:on-error (code message &key repairs)
            (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs)))
        (aitools.workspace.application:call-with-resolved-root/k
         host :root root
         :on-error (lambda (reason path)
                     (aitools.kernel.domain:flow-continue :on-error
                      (if (eq reason :not-found) "input.not-found" "argument.invalid")
                      (format nil "workspace root ~A ~A" path
                              (if (eq reason :not-found) "does not exist" "is not a directory"))
                      :repairs
                      (list (repair "use-default-root" "Run from the workspace without --root."
                                    (format nil "aitools ~A" command)))))
         :on-resolved (lambda (workspace-root)
                        (if tx
                            (aitools.kernel.domain:flow-call
                                (%open-tx/k ports workspace-root tx command)
                              (:on-session (session)
                                (aitools.kernel.domain:flow-continue :on-session session))
                              (:on-error (code message &key repairs)
                                (aitools.kernel.domain:flow-continue
                                 :on-error code message :repairs repairs)))
                            (aitools.kernel.domain:flow-continue
                             :on-session (%make-session ports workspace-root nil nil nil))))))))

;;; Paths

(defun %absolute (session path)
  "PATH (absolute, or relative to the working directory) made absolute and
normalized."
  (aitools.workspace.application:user-path-absolute (%host session) path))

(defun %start-paths (session paths)
  "Absolute scan starts for PATHS; with none, the working directory when it
lies inside the workspace, else the whole workspace."
  (if paths
      (mapcar (lambda (path) (%absolute session path)) paths)
      (let ((cwd (aitools.workspace.domain:normalize-path
                  (aitools.workspace.application:host-current-directory (%host session)))))
        (if (%relative session cwd)
            (list cwd)
            nil))))

(defun %relative (session absolute)
  "ABSOLUTE relative to the workspace root, or NIL when it lies outside.
Compared as the scan compares its starts (WORKSPACE-RELATIVE-PATH): the
working directory is a real path even when --root names the root through a
symlink."
  (aitools.workspace.application:workspace-relative-path (%host session) (session-root session) absolute))

;;; Scan options

(aitools.kernel.domain:define-flow (%argument-error message command &key on-error)
  (aitools.kernel.domain:flow-continue :on-error "argument.invalid" message
   :repairs (list (repair "inspect-schema" "Show this command's arguments and rules."
                          (format nil "aitools schema ~A" command)))))

(aitools.kernel.domain:define-flow
    (scan-options/k session command &key glob lang no-ignore skip-larger-than newer on-options on-error)
  "Validate common scan options and call ON-OPTIONS with scan keywords or
ON-ERROR. SKIP-LARGER-THAN is a size string; NEWER is a path or duration."
  (aitools.workspace.application:call-with-scan-options/k
   (%host session)
   :path-absolute (lambda (path) (%absolute session path))
   :current-time (lambda () (%now (session-ports session)))
   :language-predicate #'aitools.text.domain:language-path-predicate
   :language-names (aitools.text.domain:language-names)
   :glob glob :lang lang :no-ignore no-ignore
   :skip-larger-than skip-larger-than :newer newer
   :overlay (session-overlay session)
   :on-error (lambda (kind value names)
               (case kind
                 (:unknown-language
                  (aitools.kernel.domain:flow-continue :on-error "argument.invalid"
                   (format nil "unknown language ~A; known: ~{~A~^, ~}" value names)
                   :repairs (list (repair "use-known-language" "Use one of the known language names."
                                          (format nil "aitools ~A --lang ~A" command (first names))))))
                 (:invalid-size
                  (aitools.kernel.domain:flow-call
                      (%argument-error (format nil "--skip-larger-than: not a size: ~A" value) command)
                    (:on-error (code message &key repairs)
                      (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs))))
                 (:invalid-newer
                  (aitools.kernel.domain:flow-call
                      (%argument-error
                       (format nil "--newer: ~A is neither an existing path nor a duration" value)
                       command)
                    (:on-error (code message &key repairs)
                      (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs))))))
   :on-options (lambda (options)
                 (aitools.kernel.domain:flow-continue :on-options options))))

(aitools.kernel.domain:define-flow
    (scan-error command reason path &key on-error)
  "Report a scan start rejected by workspace scan."
  (ecase reason
    (:outside-root
     (aitools.kernel.domain:flow-continue :on-error "argument.invalid"
      (format nil "~A is outside the workspace root" path)
      :repairs (list (repair "set-root" "Scan it as its own workspace."
                             (command-line (list "aitools" "--root" path command))))))
    (:not-found
     (aitools.kernel.domain:flow-continue :on-error "input.not-found"
      (format nil "~A does not exist" path)
      :repairs (list (repair "find-path" "Look for the path by name."
                             (command-line (list "aitools" "find"
                                                 (aitools.workspace.domain:path-basename path)))))))))
