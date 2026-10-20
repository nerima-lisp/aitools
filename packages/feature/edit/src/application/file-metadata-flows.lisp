(in-package #:aitools.edit.application)

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
(define-write-command "chmod"
                      (ports env positionals options &key on-plan on-error)
                      (let ((given (remove nil (list (and (getf options :exec) :exec) (and
                                                                                       (getf options :no-exec) :no-exec)
                                                     (and (getf options :mode) :mode)))))
                        (cond
                         ((/= (length positionals) 1)
                         (flow-continue :on-error "argument.invalid" "chmod takes exactly one path"))
                         ((/= (length given) 1) (flow-continue :on-error "argument.invalid"
                                                               "chmod needs exactly one of --exec, --no-exec, --mode"))
                         ((and (getf options :mode) (null (%parse-mode (getf options :mode))))
                          (flow-continue :on-error "argument.invalid" (format nil
                                                                           "--mode ~S is not an octal mode such as 644"
                                                                           (getf options :mode))))
                         ((and (getf options :mode) (eq (%parse-mode (getf options :mode)) :high-bits))
                          (flow-continue :on-error "argument.invalid"
                                         (format nil
                                                 (concatenate 'string
                                                              "--mode ~S sets a setuid, setgid or sticky bit; only "
                                                              "the low 9 permission bits (up to 777) are allowed")
                                                 (getf options :mode))))
                         (t
                          (flow-continue :on-plan (make-write-plan
                                                   :command "chmod"
                                                   :targets (list (make-write-target (first positionals)))
                                                   :plan (lambda (context &key on-committed on-rejected)
                                                           (let* ((path (context-path context))
                                                                  (state (%state context path)))
                                                             (case (aitools.store.domain:entry-state-kind state)
                                                               (:absent
                                                                (aitools.kernel.domain:flow-call
                                                                 (%not-found context path)
                                                                 (:on-error (code message
                                                                            &key candidates diagnostics
                                                                            conflicts repairs path)
                                                                   (declare (ignore path))
                                                                   (funcall on-rejected
                                                                              code
                                                                              message
                                                                              :candidates candidates
                                                                              :diagnostics diagnostics
                                                                              :conflicts conflicts
                                                                              :repairs repairs))))
                                                               (:symlink (funcall on-rejected "refusal.not-a-file"
                                                                                  (format nil "~A is a symlink" path)))
                                                               (t
                                                                (let* ((previous
                                                                       (or
                                                                        (aitools.store.domain:entry-state-mode
                                                                         state)
                                                                        0))
                                                                       (mode (ecase (first given)
                                                                        ;; Umask preserves execute bits for every class.
                                                                               ;; 0640 becomes 0751, not 0750.
                                                                               (:exec (logior previous #o111))
                                                                             (:no-exec (logand previous (lognot #o111)))
                                                                           (:mode (%parse-mode (getf options :mode))))))
                                                                  (funcall on-committed
                                                                           (if (= mode previous)
                                                                               '()
                                                                             (list
                                                                               (aitools.store.domain:chmod-request
                                                                                path mode)))
                                                                           (list (cons "previous_mode"
                                                                                  (format nil "~4,'0O" previous)))))))))
                                                   :record-options options
                                                   :record-positionals (lambda (paths) paths)))))))

(define-write-command "link"
                      (ports env positionals options &key on-plan on-error)
                      (if (/= (length positionals) 2)
                          (flow-continue :on-error "argument.invalid" "link takes TARGET and LINK")
                          (let ((target (first positionals)))
                            (flow-continue :on-plan
                                           (make-write-plan :command
                                                            "link"
                                                            :targets
                                                            (list (make-write-target (second positionals) :follow nil))
                                                            :inputs
                                                            (list target)
                                                            :expect-hashes
                                                            (getf options :expect-hash)
                                                            :plan
                                                            (lambda (context &key on-committed on-rejected)
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
                                                                (aitools.kernel.domain:flow-call
                                                                  (resolve-extra-path/k context lexical)
                                                                  (:on-outside (message)
                                                                               (funcall on-rejected
                                                                                        "refusal.outside-workspace"
                                                                                        (%link-message
                                                                                                 :outside
                                                                                                 message)))
                                                                  (:on-inside (resolved)
                                                                              (declare (ignore resolved))
                                                                              (flet ((link ()
                                                                                       (funcall on-committed
                                                                                (list
                                                                                  (aitools.store.domain:symlink-request
                                                                                   path
                                                                                   target)))))
                                                                                (case (%kind context path)
                                                                                  (:absent (link))
                                                                                  (:symlink
                                                                                   (if (getf options :overwrite)
                                                                                       (aitools.kernel.domain:flow-call
                                                                                         (require-expect-hash/k context
                                                                                                                path)
                                                                                         (:on-rejected
                                                                                          (code message
                                                                                                &key
                                                                                                candidates
                                                                                                diagnostics
                                                                                                conflicts
                                                                                                repairs
                                                                                                path)
                                                                                          (declare (ignore path))
                                                                                          (funcall on-rejected
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
                                                                                         (:on-ok () (funcall #'link)))
                                                                                       (funcall on-rejected
                                                                                                "refusal.exists"
                                                                                                (%link-message
                                                                                                        :symlink
                                                                                                        path))))
                                                                                  (t
                                                                                   (funcall on-rejected
                                                                                            "refusal.exists"
                                                                                            (%link-message
                                                                                                    :not-symlink
                                                                                                    path)))))))))
                                                            :record-options
                                                            options
                                                            :record-positionals
                                                            (lambda (paths)
                                                              (list target (first paths))))))))

(define-write-command "touch"
                      (ports env positionals options &key on-plan on-error)
                      (let* ((now (funcall (edit-ports-unix-now ports)))
                             (mtime
                              (if (getf options :mtime)
                                  (parse-mtime (getf options :mtime))
                                  now)))
                        (cond
                          ((/= (length positionals) 1)
                           (flow-continue :on-error "argument.invalid" "touch takes exactly one path"))
                          ((or (null mtime) (minusp mtime))
                           (flow-continue :on-error
                                          "argument.invalid"
                                          (format nil
                                                  (format nil
                                                          (concatenate (quote string)
                                                          "--mtime ~S is not Unix seconds, @seconds, "
                                                          "or ISO 8601 at or after 1970")
                                                          (getf options :mtime))
                                                  (getf options :mtime))))
                          (t
                           (flow-continue :on-plan
                                          (make-write-plan :command
                                                           "touch"
                                                           :targets
                                                           (list (make-write-target (first positionals)))
                                                           :plan
                                                           (lambda (context &key on-committed on-rejected)
                                                             (let* ((path (context-path context))
                                                                    (fields (list (cons "mtime" (iso-utc mtime)))))
                                                               (case (%kind context path)
                                                                 (:absent
                                                                  (funcall on-committed
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
                                                                  (funcall on-committed
                                                                           (list
                                                                            (aitools.store.domain:mtime-request path
                                                                                                                mtime))
                                                                           fields))
                                                                 (t
                                                                  (funcall on-rejected
                                                                           "refusal.not-a-file"
                                                                       (format nil "~A is not a regular file" path))))))
                                                           :record-options
                                                           options
                                                           :record-positionals
                                                           (lambda (paths)
                                                             paths)))))))
