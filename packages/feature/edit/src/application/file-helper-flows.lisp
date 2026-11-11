(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow
  (%not-found context path &key on-error)
  (aitools.kernel.domain:flow-continue
    :on-error
    "input.not-found"
    (format nil "~A does not exist" path)
    :candidates
    (path-candidates (write-context-view context) path)))
(defun %link-message (kind value)
  (format nil
          (ecase kind
            (:outside "the link would point outside the workspace: ~A")
            (:symlink
             "~A is already a symlink; pass --overwrite with --expect-hash to replace it")
            (:not-symlink
             "~A exists and is not a symlink; link --overwrite replaces only symlinks"))
          value))

(aitools.kernel.domain:define-flow
  (%destination-check/k context destination source-kind overwrite &key on-rejected on-ok)
  "The destination rules for move/copy: absent is fine; an existing file
or symlink may be replaced with --overwrite and --expect-hash when the
source is not a directory; anything else is refusal.exists."
  (let ((kind (%kind context destination)))
    (cond
      ((eq kind :absent)
       (aitools.kernel.domain:flow-continue
         :on-ok))
      ((and overwrite (member kind '(:file :symlink)) (not (eq source-kind :directory)))
       (aitools.kernel.domain:flow-call
         (require-expect-hash/k context destination)
         (:on-rejected (code message &key candidates diagnostics conflicts repairs path)
                       (declare (ignore path))
                       (flow-continue :on-rejected
                                      code
                                      message
                                      :candidates
                                      candidates
                                      :diagnostics
                                      diagnostics
                                      :conflicts
                                      conflicts
                                      :repairs
                                      repairs))
         (:on-ok ()
                 (aitools.kernel.domain:flow-continue
                   :on-ok))))
      (t
       (aitools.kernel.domain:flow-continue
         :on-rejected
         "refusal.exists"
         (format nil
                 (concatenate 'string
                              "~A already exists~:[~; (directories are never merged)~]~:[~; "
                              "(pass --overwrite with --expect-hash to replace a file)~]")
                 destination
                 (eq source-kind :directory)
                 (and (not overwrite) (member kind '(:file :symlink))))
         :path
         destination)))))

(defun %rm-protected-target-failure (host root path absolute)
  (when (or (zerop (length path))
            (string= absolute "/")
            (equal absolute
                   (aitools.workspace.application:workspace-root-real root))
            (equal absolute
                   (aitools.workspace.application:resolve-real-path
                    host
                    (aitools.workspace.application:host-home-directory host)))
            (loop for start = 0 then (1+ slash)
                  for slash = (position #\/ path :start start)
                  thereis (member (subseq path start slash)
                                  '(".git" ".jj")
                                  :test #'string-equal)
                  while slash))
    (list "refusal.outside-workspace"
          "rm refuses roots, home, and version-control metadata")))

(defun %rm-entry-limit-failure (entries max-entries)
  (and (> entries max-entries)
       (list "refusal.too-large" "rm exceeds --max-entries")))

(defun %rm-file-byte-budget (host context view path absolute bytes max-bytes)
  ;; A transaction may replace a large disk file with a smaller staged blob.
  ;; Its view bytes, not the stale disk size, define the limit.
  (let ((stat (and (not (write-context-tx context))
                   (aitools.workspace.application:host-stat host absolute))))
    (if (and stat
             (> (+ bytes
                   (aitools.workspace.domain:workspace-entry-size stat))
                max-bytes))
        (values
         (list "refusal.too-large" "rm exceeds --max-bytes")
         bytes
         nil)
        (let* ((file-bytes
                 (length
                  (aitools.store.application:view-read-file
                   view path :max-bytes (- max-bytes bytes))))
               (next-bytes (+ bytes file-bytes)))
          (values
           (when (> next-bytes max-bytes)
             (list "refusal.too-large" "rm exceeds --max-bytes"))
           next-bytes
           file-bytes)))))

(defun %rm-directory-children (recursive kind view path)
  (when (and recursive (eq kind :directory))
    (reverse
     (mapcar (lambda (entry) (%child path (car entry)))
             (aitools.store.application:view-directory-entries
              view path)))))

(aitools.kernel.domain:define-flow
  (%rm-boundary-failure/k host root absolute context kind &key on-result)
  (aitools.kernel.domain:flow-call
    (%boundary/k host
                 root
                 absolute
                 (write-context-temporary-root context)
                 (eq kind :directory))
    (:on-inside (real relative verdict)
      (declare (ignore real relative verdict))
      (flow-continue :on-result nil))
    (:on-outside (verdict lexical real)
      (declare (ignore lexical real))
      (flow-continue
        :on-result
        (list "refusal.outside-workspace"
              (format nil "rm refuses a protected target (~A)" verdict))))))

(aitools.kernel.domain:define-flow
  (%rm-absence-failure/k context path &key on-result)
  (aitools.kernel.domain:flow-call
    (%not-found context path)
    (:on-error
     (code message &key candidates diagnostics conflicts repairs path)
     (flow-continue
       :on-result
       (list code
             message
             :candidates candidates
             :diagnostics diagnostics
             :conflicts conflicts
             :repairs repairs
             :path path)))))

(aitools.kernel.domain:define-flow
 (%rm-requests/k context recursive max-entries max-bytes &key on-error on-committed)
 (let* ((view (write-context-view context))
        (host (write-context-host context))
        (root (write-context-root context))
        (pending (list (context-path context)))
        (requests '())
        (entries 0)
        (bytes 0)
        (failure nil))
   (loop while pending
         for path = (pop pending)
         for absolute = (%target-absolute host root path :root)
         for kind = (aitools.store.domain:entry-state-kind
                     (aitools.store.application:view-path-kind view path))
         for file-bytes = nil
         until failure
         do
         (setf failure (%rm-protected-target-failure host root path absolute))
         ;; Check directories themselves and their parents without following leaf links.
         (unless failure
           (aitools.kernel.domain:flow-call
            (%rm-boundary-failure/k host root absolute context kind)
            (:on-result (result) (setf failure result))))
         (when (and (null failure) (eq kind :absent))
           (aitools.kernel.domain:flow-call
            (%rm-absence-failure/k context path)
            (:on-result (result) (setf failure result))))
         (when (null failure)
           (incf entries)
           (setf failure
                 (%rm-entry-limit-failure entries max-entries))
           (when (and (null failure) (eq kind :file))
             (multiple-value-setq (failure bytes file-bytes)
                                  (%rm-file-byte-budget host context view path absolute bytes max-bytes)))
           (when (null failure)
             (push (aitools.store.domain:delete-request path :max-bytes file-bytes)
                   requests))
           (when (null failure)
             (setf pending
                   (append (%rm-directory-children recursive kind view path)
                           pending)))))
   (if failure
       (destructuring-bind (code message &key candidates diagnostics conflicts repairs path)
           failure
         (flow-continue
          :on-error
          code
          message
          :candidates
          candidates
          :diagnostics
          diagnostics
          :conflicts
          conflicts
          :repairs
          repairs
          :path
          path))
     (aitools.kernel.domain:flow-call
      (check-expect-count/k context entries)
      (:on-rejected (code message &key candidates diagnostics conflicts repairs path)
                    (flow-continue
                     :on-error
                     code
                     message
                     :candidates
                     candidates
                     :diagnostics
                     diagnostics
                     :conflicts
                     conflicts
                     :repairs
                     repairs
                     :path
                     path))
      (:on-ok ()
              (aitools.kernel.domain:flow-continue :on-committed
                                                   requests
                                                   (list (cons "entries" entries)
                                                         (cons "bytes" bytes)))))
     )))
