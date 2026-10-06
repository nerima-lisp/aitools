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

(defun %finish (continuation fields &optional next-commands)
  "Pass FIELDS to CONTINUATION, with `next_commands` appended when given.
Secret masking and the `redactions` count are applied by the envelope
writer to everything it writes."
  (funcall continuation (append fields (when next-commands (list (cons "next_commands" next-commands))))))

(defun %host (session)
  (search-ports-workspace-host (session-ports session)))

(defun %root-path (session)
  (aitools.workspace.application:workspace-root-path (session-root session)))

(defun %ignore-source-name (source)
  (ecase source
    (:gitignore "gitignore")
    (:builtin "builtin")
    (:none "none")))

;;; ------------------------------------------------------------ the tx overlay

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

;;; ------------------------------------------------------------ session setup

(defun %unavailable (on-error command)
  (funcall on-error "internal.unexpected"
           "the search context was built without its workspace host or text source"
           :repairs (list (aitools.protocol.domain:repair "inspect-schema" "Show this command's arguments and rules."
                                   (format nil "aitools schema ~A" command)))))

(defun %now (ports)
  (let ((clock (search-ports-unix-now ports)))
    (if clock (funcall clock) 0)))

(defun %open-tx/k (ports workspace-root tx command on-session on-error)
  "Open TX's read-through view of WORKSPACE-ROOT and call ON-SESSION with a
session carrying the tx overlay, or ON-ERROR."
  (let ((open-store (search-ports-open-store ports)))
    (if (null open-store)
        (%unavailable on-error command)
        (let ((store (funcall open-store (aitools.workspace.application:workspace-root-real workspace-root)))
              (staged (make-hash-table :test 'equal)))
          (flet ((missing ()
                   (funcall on-error "input.not-found" (format nil "tx ~A does not exist" tx)
                            :repairs (list (aitools.protocol.domain:repair "list-tx" "List the open transactions." "aitools tx status"))))
                 (with-view (view)
                   (funcall on-session
                            (%make-session ports workspace-root (%make-overlay view staged (%now ports))
                                           view staged))))
            (aitools.store.application:tx-status/k
             store tx
             :on-not-found #'missing
             :on-status (lambda (status)
                          (dolist (entry (aitools.store.application:tx-status-paths status))
                            (setf (gethash (aitools.store.domain:tx-path-path entry) staged)
                                  (aitools.store.domain:tx-path-staged entry)))
                          (aitools.store.application:call-with-tx-view/k
                           store tx :on-view #'with-view :on-not-found #'missing))))))))

(defun call-with-session/k (ports command &key root tx on-session on-error)
  "Resolve the workspace root (the global `--root` ROOT, else git's top
level, else the working directory), open TX's read-through view when TX is
given, and call ON-SESSION (session), or ON-ERROR with an error code,
message, and repairs. COMMAND names the command for repairs."
  (declare (type function on-session on-error))
  (let ((host (search-ports-workspace-host ports)))
    (if (not (and host (search-ports-text-source ports)))
        (%unavailable on-error command)
        (aitools.workspace.application:call-with-resolved-root/k
         host
         :root root
         :on-error (lambda (reason path)
                     (funcall on-error (if (eq reason :not-found) "input.not-found" "argument.invalid")
                              (format nil "workspace root ~A ~A" path
                                      (if (eq reason :not-found) "does not exist" "is not a directory"))
                              :repairs (list (aitools.protocol.domain:repair "use-default-root" "Run from the workspace without --root."
                                                      (format nil "aitools ~A" command)))))
         :on-resolved (lambda (workspace-root)
                        (if tx
                            (%open-tx/k ports workspace-root tx command on-session on-error)
                            (funcall on-session (%make-session ports workspace-root nil nil nil))))))))

;;; ------------------------------------------------------------ paths

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

;;; ------------------------------------------------------------ file reads

(defconstant +read-chunk-size+ 65536)

(defun %read-disk-file/k (source absolute on-text on-binary on-missing)
  "Read ABSOLUTE through the text source in one open: the first chunk
decides binary-ness (the first 8 KiB) before anything more is read
(so a binary file costs one chunk), and a text file's chunks are joined into one buffer. This opens
the file once, where the source's CALL-WITH-TEXT-FILE/K opens it for the
size, the prefix, and the body separately."
  (let ((chunks '()) (total 0) (binary nil))
    (flet ((take (chunk)
             (if (and (null chunks) (aitools.text.domain:binary-octets-p chunk))
                 (progn (setf binary t) :stop)
                 (progn (push chunk chunks) (incf total (length chunk)) nil))))
      (declare (dynamic-extent #'take))
      (cond
        ((null (aitools.text.application:source-call-with-chunks source absolute +read-chunk-size+ #'take))
         (funcall on-missing))
        (binary (funcall on-binary))
        ((null chunks) (funcall on-text (make-array 0 :element-type '(unsigned-byte 8))))
        ((null (rest chunks)) (funcall on-text (first chunks)))
        (t (let ((octets (make-array total :element-type '(unsigned-byte 8))) (offset 0))
             (dolist (chunk (nreverse chunks))
               (replace octets chunk :start1 offset)
               (incf offset (length chunk)))
             (funcall on-text octets)))))))

(defun %read-file/k (session absolute relative &key on-text on-binary on-missing)
  "Read one file for text use and call exactly one continuation: ON-TEXT
(octets), ON-BINARY (), or ON-MISSING (). A path staged in the session's
tx is read from the tx; anything else from disk through the text source (%READ-DISK-FILE/K)."
  (declare (type function on-text on-binary on-missing))
  (let ((staged (session-staged session)))
    (multiple-value-bind (state present) (and staged relative (gethash relative staged))
      (if present
          (let ((octets (and (eq (aitools.store.domain:entry-state-kind state) :file)
                             (aitools.store.application:view-read-file (session-view session) relative))))
            (cond ((null octets) (funcall on-missing))
                  ((aitools.text.domain:binary-octets-p octets) (funcall on-binary))
                  (t (funcall on-text octets))))
          (%read-disk-file/k (search-ports-text-source (session-ports session)) absolute
                             on-text on-binary on-missing)))))

;;; ------------------------------------------------------------ scan options

(defun %argument-error (on-error message command)
  (funcall on-error "argument.invalid" message
           :repairs (list (aitools.protocol.domain:repair "inspect-schema" "Show this command's arguments and rules."
                                   (format nil "aitools schema ~A" command)))))

(defun scan-options/k (session command &key glob lang no-ignore skip-larger-than newer on-options on-error)
  "Validate the common scan options and call ON-OPTIONS with the
keyword arguments CALL-WITH-WORKSPACE-SCAN/K takes, or ON-ERROR.
SKIP-LARGER-THAN is a size string; NEWER a path or a duration."
  (declare (type function on-options on-error))
  (let ((predicate nil) (limit aitools.workspace.application:+default-skip-larger-than+) (since nil))
    (when lang
      (setf predicate (aitools.text.domain:language-path-predicate lang))
      (unless predicate
        (return-from scan-options/k
          (funcall on-error "argument.invalid"
                   (format nil "unknown language ~A; known: ~{~A~^, ~}" lang (aitools.text.domain:language-names))
                   :repairs (list (aitools.protocol.domain:repair "use-known-language" "Use one of the known language names."
                                           (format nil "aitools ~A --lang ~A" command
                                                   (first (aitools.text.domain:language-names)))))))))
    (when skip-larger-than
      (setf limit (handler-case (aitools.kernel.domain:size-bytes (aitools.kernel.domain:parse-size skip-larger-than))
                    (aitools.kernel.domain:invalid-size-error ()
                      (return-from scan-options/k
                        (%argument-error on-error (format nil "--skip-larger-than: not a size: ~A" skip-larger-than)
                                         command))))))
    (when newer
      (let ((entry (aitools.workspace.application:host-stat (%host session) (%absolute session newer))))
        (setf since
              (if entry
                  (aitools.workspace.application:workspace-entry-mtime entry)
                  (handler-case (- (%now (session-ports session))
                                   (floor (aitools.kernel.domain:duration-milliseconds
                                           (aitools.kernel.domain:parse-duration newer))
                                          1000))
                    (aitools.kernel.domain:invalid-duration-error ()
                      (return-from scan-options/k
                        (%argument-error on-error
                                         (format nil "--newer: ~A is neither an existing path nor a duration" newer)
                                         command))))))))
    (funcall on-options
             (list :glob glob :lang predicate :no-ignore no-ignore :skip-larger-than limit :newer since
                   :overlay (session-overlay session)))))

(defun scan-error (on-error command reason path)
  "Report a scan start the workspace scan rejected."
  (ecase reason
    (:outside-root
     (funcall on-error "argument.invalid" (format nil "~A is outside the workspace root" path)
              :repairs (list (aitools.protocol.domain:repair "set-root" "Scan it as its own workspace."
                                      (command-line (list "aitools" "--root" path command))))))
    (:not-found
     (funcall on-error "input.not-found" (format nil "~A does not exist" path)
              :repairs (list (aitools.protocol.domain:repair "find-path" "Look for the path by name."
                                      (command-line (list "aitools" "find"
                                                          (aitools.workspace.domain:path-basename path)))))))))
