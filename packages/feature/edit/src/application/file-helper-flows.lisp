(in-package #:aitools.edit.application)

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
                 (format nil
                         (concatenate 'string
                                      "~A already exists~:[~; "
                                      "(directories are never "
                                      "merged)~]~:[~; (pass "
                                      "--overwrite with "
                                      "--expect-hash to replace a "
                                      "file)~]"
                         )
                         destination (eq source-kind :directory) (and (not overwrite) (member kind '(:file :symlink))))
                 :path destination)))))

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
