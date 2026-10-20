(in-package #:aitools.edit.application)

(defun %state (context path)
  (aitools.store.application:view-path-state (write-context-view context) path))

(defun %kind (context path)
  (aitools.store.domain:entry-state-kind (%state context path)))

;;; ------------------------------------------------------------------- chmod
(defun %parse-mode (text)
  "TEXT (1 to 4 octal digits) as a permission mode: the low 9 bits (#o777)
only. NIL when TEXT is not octal digits, :HIGH-BITS when it would set the
setuid, setgid or sticky bit, which chmod does not apply (a setuid file the
tool creates would be a privilege-escalation surface)."
  (and (<= 1 (length text) 4)
       (every
        (lambda (c)
          (char<= #\0 c #\7))
        text)
       (let ((value (parse-integer text :radix 8)))
         (if (<= value #o777)
             value
             :high-bits))))

(define-write-command "mkdir" (ports env positionals options on-plan fail)
  (if (/= (length positionals) 1)
      (funcall fail "argument.invalid" "mkdir takes exactly one path")
      (funcall on-plan
               (make-write-plan :command
                                "mkdir"
                                :targets
                                (list (make-write-target (first positionals)))
                                :plan
                                (lambda (context commit reject)
                                  (declare (ignore reject))
                                  (funcall commit
                                           (list
                                            (aitools.store.domain:mkdir-request
                                             (context-path context)))))
                                :record-options
                                options
                                :record-positionals
                                (lambda (paths)
                                  paths)))))

(define-write-command "chmod" (ports env positionals options on-plan fail)
                      (let ((given (remove nil (list (and (getf options :exec) :exec) (and (getf options :no-exec)
                                                                                           :no-exec)
                                                     (and (getf options :mode) :mode)))))
                        (cond
                         ((/= (length positionals) 1) (funcall fail "argument.invalid" "chmod takes exactly one path"))
                         ((/= (length given) 1) (funcall fail "argument.invalid"
                                                         "chmod needs exactly one of --exec, --no-exec, --mode"))
                         ((and (getf options :mode) (null (%parse-mode (getf options :mode))))
                          (funcall fail "argument.invalid" (format nil "--mode ~S is not an octal mode such as 644"
                                                                   (getf options :mode))))
                         ((and (getf options :mode) (eq (%parse-mode (getf options :mode)) :high-bits))
                          (funcall fail "argument.invalid"
                                   (format nil
                                           (concatenate 'string
                                                        "--mode ~S sets a setuid, "
                                                        "setgid or sticky bit; only "
                                                        "the low 9 permission bits "
                                                        "(up to 777) are allowed"
                                           )
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
                                                (:symlink (funcall reject "refusal.not-a-file" (format nil
                                                                                                       "~A is a symlink"
                                                                                                       path)))
                                                (t
                                                 (flet ((%chmod-request (path mode)
         (aitools.store.domain:chmod-request path mode)))
  (let* ((previous (or (aitools.store.domain:entry-state-mode state) 0))
                                                          (mode (ecase (first given)
                                                                  ;; `chmod +x`: add execute for user, group and
                                                                  ;; other. A standard umask never masks execute
                                                                  ;; bits, so this matches `chmod +x`;
                                                                  ;; mirroring only the read bits gave 0750 where
                                                                  ;; `chmod +x` gives 0751 on a 0640 file.
                                                                  (:exec (logior previous #o111))
                                                                  (:no-exec (logand previous (lognot #o111)))
                                                                  (:mode (%parse-mode (getf options :mode))))))
                                                     (funcall commit
                                                              (if (= mode previous) '() (list
                                                                                         (%chmod-request path mode)))
                                                              (list (cons "previous_mode" (format nil "~4,'0O"
                                                                                                  previous)))))
)))))
                                    :record-options options
                                    :record-positionals (lambda (paths) paths)))))))

(define-write-command "link" (ports env positionals options on-plan fail)
  (if (/= (length positionals) 2)
      (funcall fail "argument.invalid" "link takes TARGET and LINK")
      (let ((target (first positionals)))
        (funcall on-plan
                 (make-write-plan :command
                                  "link"
                                  :targets
                                  (list
                                   (make-write-target (second positionals) :follow nil))
                                  :inputs
                                  (list target)
                                  :expect-hashes
                                  (getf options :expect-hash)
                                  :plan
                                  (lambda (context commit reject)
                                    (let* ((path (context-path context))
                                           (parent
                                            (aitools.workspace.domain:path-parent
                                             (concatenate 'string "/" path)))
                                           (lexical
                                            (if (aitools.workspace.domain:absolute-path-p
                                                 target)
                                                target
                                                (aitools.workspace.domain:normalize-path
                                                 (aitools.workspace.domain:join-path
                                                  (subseq (or parent "/") 1)
                                                  target)))))
                                      (resolve-extra-path/k context
                                                            lexical
                                                            :on-outside
                                                            (lambda (message)
                                                              (funcall reject
                                                                       "refusal.outside-workspace"
                                                                       (format nil
                                                                               (concatenate 'string
  "the link would point outside the "
  "workspace: ~A")
                                                                               message)))
                                                            :on-inside
                                                            (lambda (resolved)
                                                              (declare (ignore resolved))
                                                              (flet ((link ()
                                                                       (funcall commit
                                                                                (list
                                                                                 (aitools.store.domain:symlink-request
                                                                                  path
                                                                                  target)))))
                                                                (case (%kind context path)
                                                                  (:absent (link))
                                                                  (:symlink
                                                                   (if (getf options
                                                                             :overwrite)
                                                                       (require-expect-hash/k
                                                                        context
                                                                        path
                                                                        reject
                                                                        #'link)
                                                                       (funcall reject
                                                                                "refusal.exists"
                                                                                (format
                                                                                 nil
                                                                                 (concatenate
                                                                                  'string
                                                                                  "~A is already a symlink; "
                                                                                  "pass --overwrite with "
                                                                                  "--expect-hash to replace it")
                                                                                 path))))
                                                                  (t
                                                                   (funcall reject
                                                                            "refusal.exists"
                                                                            (format nil
                                                                                    (concatenate
                                                                                     'string
                                                                                     "~A exists and is not a "
                                                                                     "symlink; link --overwrite "
                                                                                     "replaces only symlinks")
                                                                                    path)))))))))
                                  :record-options
                                  options
                                  :record-positionals
                                  (lambda (paths)
                                    (list target (first paths))))))))

(define-write-command "touch" (ports env positionals options on-plan fail)
  (let* ((now (funcall (edit-ports-unix-now ports)))
         (mtime
          (if (getf options :mtime)
              (parse-mtime (getf options :mtime))
              now)))
    (cond
      ((/= (length positionals) 1)
       (funcall fail "argument.invalid" "touch takes exactly one path"))
      ((or (null mtime) (minusp mtime))
       (funcall fail
                "argument.invalid"
                (format nil
                        "--mtime ~S is not Unix seconds, @seconds, or ISO 8601 at or after 1970"
                        (getf options :mtime))))
      (t
       (funcall on-plan
                (make-write-plan :command
                                 "touch"
                                 :targets
                                 (list (make-write-target (first positionals)))
                                 :plan
                                 (lambda (context commit reject)
                                   (let* ((path (context-path context))
                                          (fields (list (cons "mtime" (iso-utc mtime)))))
                                     (case (%kind context path)
                                       (:absent
                                        (funcall commit
                                                 (list
                                                  (aitools.store.domain:write-file-request
                                                   path
                                                   (make-array 0
                                                               :element-type
                                                               '(unsigned-byte 8))
                                                   :mtime
                                                   mtime))
                                                 fields))
                                       (:file
                                        (funcall commit
                                                 (list
                                                  (aitools.store.domain:mtime-request path
                                                                                      mtime))
                                                 fields))
                                       (t
                                        (funcall reject
                                                 "refusal.not-a-file"
                                                 (format nil
                                                         "~A is not a regular file"
                                                         path))))))
                                 :record-options
                                 options
                                 :record-positionals
                                 (lambda (paths)
                                   paths)))))))
