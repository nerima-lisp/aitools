;;;; packages/feature/edit/src/application/file-flows.lisp
;;;;
;;;; File operations: move, copy, delete, mkdir, chmod,
;;;; link and touch. mktemp, which alone bypasses the journal, is in
;;;; mktemp.lisp.
(in-package #:aitools.edit.application)

(defun %state (context path)
  (aitools.store.application:view-path-state (write-context-view context) path))

(defun %kind (context path)
  (aitools.store.domain:entry-state-kind (%state context path)))

(defun %walk-tree (context directory function)
  "Call FUNCTION (relative-path kind) for every entry below DIRECTORY in the
write's view, parents before children, in path order."
  (dolist (entry (aitools.store.application:view-directory-entries (write-context-view context) directory))
    (let ((path (%child directory (car entry))))
      (funcall function path (cdr entry))
      (when (eq (cdr entry) :directory)
        (%walk-tree context path function)))))

(defun %replace-rebase (path from to)
  "PATH (below FROM, \"\" for the workspace root) moved below TO."
  (if (zerop (length from))
      (%child to path)
      (concatenate 'string to (subseq path (length from)))))

(defun %not-found (context path reject)
  (funcall reject "input.not-found" (format nil "~A does not exist" path)
           :candidates (path-candidates (write-context-view context) path)))

(defun %destination-check/k (context destination source-kind overwrite reject on-ok)
  "The destination rules for move/copy: absent is fine; an existing file
or symlink may be replaced with --overwrite and --expect-hash when the
source is not a directory; anything else is refusal.exists."
  (declare (type function reject on-ok))
  (let ((kind (%kind context destination)))
    (cond
      ((eq kind :absent) (funcall on-ok))
      ((and overwrite (member kind '(:file :symlink)) (not (eq source-kind :directory)))
       (require-expect-hash/k context destination reject on-ok))
      (t (funcall reject "refusal.exists"
                  (format nil "~A already exists~:[~; (directories are never merged)~]~:[~; (pass --overwrite with --expect-hash to replace a file)~]"
                          destination (eq source-kind :directory) (and (not overwrite) (member kind '(:file :symlink))))
                  :path destination)))))

;;; -------------------------------------------------------------------- move

(defun %per-path-move-requests (context source destination)
  "A directory move inside a tx, recorded per path: recreate every
entry below DESTINATION, then delete the originals, children first."
  (let ((creates (list (aitools.store.domain:mkdir-request destination)))
        (deletes (list (aitools.store.domain:delete-request source)))
        (view (write-context-view context)))
    (%walk-tree context source
                (lambda (path kind)
                  (let ((target (%replace-rebase path source destination))
                        (state (aitools.store.application:view-path-state view path)))
                    (push (aitools.store.domain:delete-request path) deletes)
                    (push (ecase kind
                            (:directory (aitools.store.domain:mkdir-request target))
                            (:file (aitools.store.domain:write-file-request
                                    target (aitools.store.application:view-read-file view path)
                                    :mode (aitools.store.domain:entry-state-mode state)))
                            (:symlink (aitools.store.domain:symlink-request
                                       target (aitools.store.domain:entry-state-target state))))
                          creates))))
    (append (nreverse creates) deletes)))

(define-write-command "move" (ports env positionals options on-plan fail)
  (if (/= (length positionals) 2)
      (funcall fail "argument.invalid" "move takes SRC and DST")
      (funcall on-plan
               (make-write-plan
                :command "move"
                :targets (list (make-write-target (first positionals) :follow nil)
                               (make-write-target (second positionals) :follow nil))
                :expect-hashes (getf options :expect-hash)
                :plan (lambda (context commit reject)
                        (let* ((source (context-path context 0))
                               (destination (context-path context 1))
                               (kind (%kind context source)))
                          (if (eq kind :absent)
                              (%not-found context source reject)
                              (%destination-check/k
                               context destination kind (getf options :overwrite) reject
                               (lambda ()
                                 (funcall commit
                                          (if (and (eq kind :directory) (write-context-tx context))
                                              (%per-path-move-requests context source destination)
                                              (list (aitools.store.domain:move-request source destination)))))))))
                :record-options options
                :record-positionals (lambda (paths) paths)))))

;;; -------------------------------------------------------------------- copy

(defun %copy-tree-requests/k (context source destination max-bytes reject on-requests)
  "`copy --recursive` of directory SOURCE to DESTINATION: ON-REQUESTS
(requests files skipped). Symlinks whose target leaves the workspace are
skipped; the total file size is bounded by MAX-BYTES."
  (declare (type function reject on-requests))
  (let ((requests (list (aitools.store.domain:mkdir-request destination)))
        (files 0) (bytes 0) (skipped '())
        (view (write-context-view context)))
    (%walk-tree
     context source
     (lambda (path kind)
       (let ((target (%replace-rebase path source destination))
             (state (aitools.store.application:view-path-state view path)))
         ;; A nested .git (or a child a symlinked parent would take
         ;; outside the root) must be refused, not copied in, the same as
         ;; every other write.
         (resolve-extra-path/k
          context target :base :root
          :on-outside (lambda (message)
                        (return-from %copy-tree-requests/k
                          (funcall reject "refusal.outside-workspace" message)))
          :on-inside (lambda (relative) (declare (ignore relative))))
         (ecase kind
           (:directory (push (aitools.store.domain:mkdir-request target) requests))
           (:file
            (let ((content (aitools.store.application:view-read-file view path)))
              (incf bytes (length content))
              (when (> bytes max-bytes)
                (return-from %copy-tree-requests/k
                  (funcall reject "refusal.too-large"
                           (format nil "copying ~A exceeds --max-bytes ~D" source max-bytes))))
              (incf files)
              (push (aitools.store.domain:write-file-request target content
                                                            :mode (aitools.store.domain:entry-state-mode state))
                    requests)))
           (:symlink
            (let* ((link-target (aitools.store.domain:entry-state-target state))
                   (parent (aitools.workspace.domain:path-parent (concatenate 'string "/" target))))
              (resolve-extra-path/k
               context (if (aitools.workspace.domain:absolute-path-p link-target)
                           link-target
                           (aitools.workspace.domain:normalize-path
                            (aitools.workspace.domain:join-path (subseq (or parent "/") 1) link-target)))
               :on-inside (lambda (resolved)
                            (declare (ignore resolved))
                            (push (aitools.store.domain:symlink-request target link-target) requests))
               :on-outside (lambda (message)
                             (declare (ignore message))
                             (push (json-object "path" path "reason" "symlink-outside-workspace") skipped)))))))))
    (funcall on-requests (nreverse requests) files (nreverse skipped))))

(define-write-command "copy" (ports env positionals options on-plan fail)
  (let ((max-bytes (handler-case (aitools.kernel.domain:size-bytes
                                  (aitools.kernel.domain:parse-size (or (getf options :max-bytes) "1GiB")))
                     (error () nil))))
    (cond
      ((/= (length positionals) 2) (funcall fail "argument.invalid" "copy takes SRC and DST"))
      ((null max-bytes) (funcall fail "argument.invalid" (format nil "--max-bytes ~S is not a size" (getf options :max-bytes))))
      (t
       (funcall on-plan
                (make-write-plan
                 :command "copy"
                 :targets (list (make-write-target (first positionals))
                                (make-write-target (second positionals) :follow nil))
                 :expect-hashes (getf options :expect-hash)
                 :plan (lambda (context commit reject)
                         (let* ((source (context-path context 0))
                                (destination (context-path context 1))
                                (state (%state context source))
                                (kind (aitools.store.domain:entry-state-kind state)))
                           (case kind
                             (:absent (%not-found context source reject))
                             (:directory
                              (if (not (getf options :recursive))
                                  (funcall reject "argument.invalid" (format nil "~A is a directory; copying it needs --recursive" source)
                                           :repairs (list (repair "recursive" "Copy the directory tree."
                                                                  (concatenate 'string (write-context-command-line context)
                                                                               " --recursive"))))
                                  (%destination-check/k
                                   context destination kind nil reject
                                   (lambda ()
                                     (%copy-tree-requests/k context source destination max-bytes reject
                                                            (lambda (requests files skipped)
                                                              (funcall commit requests
                                                                       (list (cons "files" files)
                                                                             (cons "skipped" skipped)))))))))
                             (:file
                              (%destination-check/k
                               context destination kind (getf options :overwrite) reject
                               (lambda ()
                                 (let ((content (aitools.store.application:view-read-file (write-context-view context) source)))
                                   (if (> (length content) max-bytes)
                                       (funcall reject "refusal.too-large" (format nil "~A exceeds --max-bytes ~D" source max-bytes))
                                       (funcall commit
                                                (list (aitools.store.domain:write-file-request
                                                               destination content :mode (aitools.store.domain:entry-state-mode state)))))))))
                             (t (funcall reject "refusal.not-a-file" (format nil "~A is a ~(~A~)" source kind))))))
                 :record-options options
                 :record-positionals (lambda (paths) paths)))))))

;;; ------------------------------------------------------- delete and mkdir

(defun %rm-requests/k (context recursive max-entries max-bytes commit reject)
  (let* ((view (write-context-view context))
         (host (write-context-host context))
         (root (write-context-root context))
         (pending (list (context-path context)))
         (requests '()) (entries 0) (bytes 0))
    (loop while pending
          for path = (pop pending)
          for absolute = (%target-absolute host root path :root)
          for kind = (aitools.store.domain:entry-state-kind
                      (aitools.store.application:view-path-kind view path))
          for file-bytes = nil
          do
             (when (or (zerop (length path))
                       (string= absolute "/")
                       (equal absolute (aitools.workspace.application:workspace-root-real root))
                       (equal absolute (aitools.workspace.application:resolve-real-path
                                        host (aitools.workspace.application:host-home-directory host)))
                       (loop for start = 0 then (1+ slash)
                             for slash = (position #\/ path :start start)
                             thereis (member (subseq path start slash) '(".git" ".jj")
                                             :test #'string-equal)
                             while slash))
               (return-from %rm-requests/k
                 (funcall reject "refusal.outside-workspace" "rm refuses roots, home, and version-control metadata")))
             ;; Check directories themselves as well as their parents: a metadata
             ;; directory can have an ordinary basename. Never follow a leaf link.
             (%boundary/k
              host root absolute (write-context-temporary-root context) (eq kind :directory)
              (lambda (real relative verdict)
                (declare (ignore real relative))
                (unless (eq verdict :inside)
                  (return-from %rm-requests/k
                    (funcall reject "refusal.outside-workspace" "rm requires a workspace target"))))
              (lambda (verdict lexical real)
                (declare (ignore lexical real))
                (return-from %rm-requests/k
                  (funcall reject "refusal.outside-workspace"
                           (format nil "rm refuses a protected target (~A)" verdict)))))
             (when (eq kind :absent)
               (return-from %rm-requests/k (%not-found context path reject)))
             (unless (member kind '(:file :directory :symlink))
               (return-from %rm-requests/k
                 (funcall reject "refusal.not-a-file" "rm accepts only files, directories, and symlinks")))
             (incf entries)
             (when (> entries max-entries)
               (return-from %rm-requests/k
                 (funcall reject "refusal.too-large" "rm exceeds --max-entries")))
             (when (eq kind :file)
               ;; A tx may replace a large disk file with a small staged blob.
               ;; Its view bytes, not the stale disk size, define the limit.
               (unless (write-context-tx context)
                 (let ((stat (aitools.workspace.application:host-stat host absolute)))
                   (when (and stat (> (+ bytes (aitools.workspace.domain:workspace-entry-size stat)) max-bytes))
                     (return-from %rm-requests/k
                       (funcall reject "refusal.too-large" "rm exceeds --max-bytes")))))
               (setf file-bytes (length (aitools.store.application:view-read-file
                                         view path :max-bytes (- max-bytes bytes))))
               (incf bytes file-bytes)
               (when (> bytes max-bytes)
                 (return-from %rm-requests/k
                   (funcall reject "refusal.too-large" "rm exceeds --max-bytes"))))
             ;; Preserve the observed bound through the store's locked replan.
             (push (aitools.store.domain:delete-request path :max-bytes file-bytes)
                   requests)
             (when (and recursive (eq kind :directory))
               (dolist (entry (aitools.store.application:view-directory-entries view path))
                 (push (%child path (car entry)) pending))))
    (check-expect-count/k
     context entries reject
     (lambda () (funcall commit requests (list (cons "entries" entries) (cons "bytes" bytes)))))))

(define-write-command "rm" (ports env positionals options on-plan fail)
  (let ((max-entries (parse-count (or (getf options :max-entries) "1000")))
        (max-bytes (handler-case
                       (aitools.kernel.domain:size-bytes
                        (aitools.kernel.domain:parse-size (or (getf options :max-bytes) "64MiB")))
                     (error () nil)))
        (recursive (getf options :recursive)))
    (cond
      ((/= (length positionals) 1) (funcall fail "argument.invalid" "rm takes exactly one path"))
      ((or (null max-entries) (zerop max-entries))
       (funcall fail "argument.invalid" "--max-entries must be a positive integer"))
      ((null max-bytes) (funcall fail "argument.invalid" "--max-bytes must be a size"))
      (t
       (funcall on-plan
                (make-write-plan
                 :command "rm"
                 :targets (list (make-write-target (first positionals) :follow nil))
                 :guard-requirements (when recursive '((:expect-count)))
                 :expect-count (getf options :expect-count)
                 :expect-hashes (getf options :expect-hash)
                 :plan (lambda (context commit reject)
                         (%rm-requests/k context recursive max-entries max-bytes commit reject))
                 :record-options options
                 :record-positionals (lambda (paths) paths)))))))

(define-write-command "delete" (ports env positionals options on-plan fail)
  (if (/= (length positionals) 1)
      (funcall fail "argument.invalid" "delete takes exactly one path")
      (funcall on-plan
               (make-write-plan
                :command "delete"
                :targets (list (make-write-target (first positionals) :follow nil))
                :expect-hashes (getf options :expect-hash)
                :plan (lambda (context commit reject)
                        (let ((path (context-path context)))
                          (if (eq (%kind context path) :absent)
                              (%not-found context path reject)
                              (funcall commit (list (aitools.store.domain:delete-request path))))))
                :record-options options
                :record-positionals (lambda (paths) paths)))))

(define-write-command "mkdir" (ports env positionals options on-plan fail)
  (if (/= (length positionals) 1)
      (funcall fail "argument.invalid" "mkdir takes exactly one path")
      (funcall on-plan
               (make-write-plan
                :command "mkdir"
                :targets (list (make-write-target (first positionals)))
                :plan (lambda (context commit reject)
                        (declare (ignore reject))
                        (funcall commit (list (aitools.store.domain:mkdir-request (context-path context)))))
                :record-options options
                :record-positionals (lambda (paths) paths)))))

;;; ------------------------------------------------------------------- chmod

(defun %parse-mode (text)
  "TEXT (1 to 4 octal digits) as a permission mode: the low 9 bits (#o777)
only. NIL when TEXT is not octal digits, :HIGH-BITS when it would set the
setuid, setgid or sticky bit, which chmod does not apply (a setuid file the
tool creates would be a privilege-escalation surface)."
  (and (<= 1 (length text) 4) (every (lambda (c) (char<= #\0 c #\7)) text)
       (let ((value (parse-integer text :radix 8)))
         (if (<= value #o777) value :high-bits))))

(define-write-command "chmod" (ports env positionals options on-plan fail)
  (let ((given (remove nil (list (and (getf options :exec) :exec) (and (getf options :no-exec) :no-exec)
                                 (and (getf options :mode) :mode)))))
    (cond
      ((/= (length positionals) 1) (funcall fail "argument.invalid" "chmod takes exactly one path"))
      ((/= (length given) 1) (funcall fail "argument.invalid" "chmod needs exactly one of --exec, --no-exec, --mode"))
      ((and (getf options :mode) (null (%parse-mode (getf options :mode))))
       (funcall fail "argument.invalid" (format nil "--mode ~S is not an octal mode such as 644" (getf options :mode))))
      ((and (getf options :mode) (eq (%parse-mode (getf options :mode)) :high-bits))
       (funcall fail "argument.invalid"
                (format nil "--mode ~S sets a setuid, setgid or sticky bit; only the low 9 permission bits (up to 777) are allowed"
                        (getf options :mode))))
      (t
       (funcall on-plan
                (make-write-plan
                 :command "chmod"
                 :targets (list (make-write-target (first positionals)))
                 :plan (lambda (context commit reject)
                         (let* ((path (context-path context))
                                (state (%state context path)))
                           (case (aitools.store.domain:entry-state-kind state)
                             (:absent (%not-found context path reject))
                             (:symlink (funcall reject "refusal.not-a-file" (format nil "~A is a symlink" path)))
                             (t
                              (let* ((previous (or (aitools.store.domain:entry-state-mode state) 0))
                                     (mode (ecase (first given)
                                             ;; `chmod +x`: add execute for user, group and other. A standard
                                              ;; umask never masks execute bits, so this matches `chmod +x`;
                                              ;; mirroring only the read bits gave 0750 where `chmod +x` gives
                                              ;; 0751 on a 0640 file.
                                              (:exec (logior previous #o111))
                                             (:no-exec (logand previous (lognot #o111)))
                                             (:mode (%parse-mode (getf options :mode))))))
                                (funcall commit
                                         (if (= mode previous) '() (list (aitools.store.domain:chmod-request path mode)))
                                         (list (cons "previous_mode" (format nil "~4,'0O" previous)))))))))
                 :record-options options
                 :record-positionals (lambda (paths) paths)))))))

;;; -------------------------------------------------------------------- link

(define-write-command "link" (ports env positionals options on-plan fail)
  (if (/= (length positionals) 2)
      (funcall fail "argument.invalid" "link takes TARGET and LINK")
      (let ((target (first positionals)))
        (funcall on-plan
                 (make-write-plan
                  :command "link"
                  :targets (list (make-write-target (second positionals) :follow nil))
                  :inputs (list target)
                  :expect-hashes (getf options :expect-hash)
                  :plan (lambda (context commit reject)
                          (let* ((path (context-path context))
                                 (parent (aitools.workspace.domain:path-parent (concatenate 'string "/" path)))
                                 (lexical (if (aitools.workspace.domain:absolute-path-p target)
                                              target
                                              (aitools.workspace.domain:normalize-path
                                               (aitools.workspace.domain:join-path (subseq (or parent "/") 1) target)))))
                            (resolve-extra-path/k
                             context lexical
                             :on-outside (lambda (message)
                                           (funcall reject "refusal.outside-workspace"
                                                    (format nil "the link would point outside the workspace: ~A" message)))
                             :on-inside
                             (lambda (resolved)
                               (declare (ignore resolved))
                               (flet ((link () (funcall commit (list (aitools.store.domain:symlink-request path target)))))
                                 (case (%kind context path)
                                   (:absent (link))
                                   (:symlink
                                    (if (getf options :overwrite)
                                        (require-expect-hash/k context path reject #'link)
                                        (funcall reject "refusal.exists"
                                                 (format nil "~A is already a symlink; pass --overwrite with --expect-hash to replace it" path))))
                                   (t (funcall reject "refusal.exists"
                                               (format nil "~A exists and is not a symlink; link --overwrite replaces only symlinks" path)))))))))
                  :record-options options
                  :record-positionals (lambda (paths) (list target (first paths))))))))

;;; ------------------------------------------------------------------- touch

(define-write-command "touch" (ports env positionals options on-plan fail)
  (let* ((now (funcall (edit-ports-unix-now ports)))
         (mtime (if (getf options :mtime) (parse-mtime (getf options :mtime)) now)))
    (cond
      ((/= (length positionals) 1) (funcall fail "argument.invalid" "touch takes exactly one path"))
      ((or (null mtime) (minusp mtime))
       (funcall fail "argument.invalid"
                (format nil "--mtime ~S is not Unix seconds, @seconds, or ISO 8601 at or after 1970" (getf options :mtime))))
      (t
       (funcall on-plan
                (make-write-plan
                 :command "touch"
                 :targets (list (make-write-target (first positionals)))
                 :plan (lambda (context commit reject)
                         (let* ((path (context-path context))
                                (fields (list (cons "mtime" (iso-utc mtime)))))
                           (case (%kind context path)
                             (:absent
                              (funcall commit (list (aitools.store.domain:write-file-request
                                                     path (make-array 0 :element-type '(unsigned-byte 8)) :mtime mtime))
                                       fields))
                             (:file
                              (funcall commit (list (aitools.store.domain:mtime-request path mtime)) fields))
                             (t (funcall reject "refusal.not-a-file" (format nil "~A is not a regular file" path))))))
                 :record-options options
                 :record-positionals (lambda (paths) paths)))))))
