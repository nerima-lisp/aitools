;;;;
;;;; `archive extract` and `archive create`.
(in-package #:aitools.edit.application)

(defparameter +extract-changes-shown+
  200
  "`archive extract`'s `changes` lists at most this many entries; `total` counts all.")

(defparameter +extract-default-max-bytes+
  (* 256 1024 1024)
  "The default output cap for `archive extract` when --max-bytes is not
given. The former 1GiB default let one gzip/tar bomb allocate most of the
heap; the text-domain inflater enforces this bound (ARCHIVE-LIMIT-EXCEEDED)
per member and in total.")

(aitools.kernel.domain:define-flow
  (%archive-octets/k context env path &key on-error on-octets)
  "The archive PATH's bytes: through the write's view when it is inside the
workspace (so --tx sees staged archives), else from the host, since reads
are not bounded by the workspace boundary."
  (let ((view (write-context-view context)))
    (flet ((from-host ()
             (let* ((host (command-env-host env))
                    (absolute (aitools.workspace.application:user-path-absolute host path))
                    (octets (aitools.workspace.application:host-read-octets host absolute)))
               (if octets
                   (aitools.kernel.domain:flow-continue
                     :on-octets
                     (coerce octets 'octets))

                   (aitools.kernel.domain:flow-continue
                     :on-error
                     "input.not-found"
                     (format
                       nil
                       "archive ~A does not exist"
                       path))
)))
           ) (aitools.kernel.domain:flow-call
             (resolve-extra-path/k context path :base :cwd)
             (:on-inside (relative)
               (if (eq (aitools.store.domain:entry-state-kind (aitools.store.application:view-path-state view relative))
                       :file)
                   (aitools.kernel.domain:flow-continue
                     :on-octets
                     (coerce (aitools.store.application:view-read-file view relative) 'octets))

                   (from-host)))
             (:on-outside (message)
               (declare (ignore message))
               (from-host))))))

(aitools.kernel.domain:define-flow
  (%check-extract-paths/k context steps &key on-error on-ok)
  "Route every extracted path through the workspace boundary so an
entry named `.git/...` (or one a symlinked parent would take outside the
root) is refused rather than written. Calls ON-OK after every step is inside."
  (labels ((walk (remaining)
             (if (null remaining)
                 (aitools.kernel.domain:flow-continue
                   :on-ok)

                 (aitools.kernel.domain:flow-call
                   (resolve-extra-path/k context (extract-step-path (first remaining)) :base :root)
                   (:on-inside (relative)
                     (declare (ignore relative))
                     (walk (rest remaining)))
                   (:on-outside (message)
                     (aitools.kernel.domain:flow-continue
                       :on-error
                       "refusal.outside-workspace"
                       message)
                 ))
)))
    (walk steps)))

(defun %extract-step-request (step)
 (ecase (extract-step-kind step)
  (:directory (aitools.store.domain:mkdir-request (extract-step-path step)))
  (:file (aitools.store.domain:write-file-request
          (extract-step-path step) (extract-step-data step)
          ;; CWE-732: mask by #o755, never #o777, so a 0777
          ;; No 0666 entry extracts group or other-writable modes.
          ;; (or setuid/setgid/sticky) file. Deterministic, no
          ;; This avoids process-umask dependency, as with tar -x.
          ;; 022 for the common modes.
          :mode (logand (extract-step-mode step) #o755)))
  (:symlink (aitools.store.domain:symlink-request
             (extract-step-path step) (extract-step-target step)))))

(define-write-command "archive.extract" (ports env positionals options &key on-plan on-error)
  (let* ((path (first positionals))
         (destination (getf options :to))
         (max-bytes
          (if (getf options :max-bytes)
              (handler-case (aitools.kernel.domain:size-bytes
                             (aitools.kernel.domain:parse-size (getf options :max-bytes)))
                (error ()
                  nil))
              +extract-default-max-bytes+))
         (max-entries (parse-count (or (getf options :max-entries) "100000"))))
    (cond
      ((or (null path) (rest positionals))
       (flow-continue
         :on-error
         "argument.invalid"
         "archive extract takes exactly one archive PATH")
)
      ((null destination)
       (flow-continue
         :on-error
         "argument.invalid"
         "archive extract needs --to <dir>")
)
      ((null max-bytes)
       (flow-continue
         :on-error
         "argument.invalid"
         (format
           nil
           "--max-bytes ~S is not a size"
           (getf options :max-bytes)))
)
      ((null max-entries)
       (flow-continue
         :on-error
         "argument.invalid"
         "--max-entries must be a non-negative integer")
)
      (t
       (flow-continue
         :on-plan
         (make-write-plan :command
                          "archive.extract"
                          :targets
                          (list (make-write-target destination))
                          :plan
                          (lambda (context &key on-committed on-rejected)
                            (aitools.kernel.domain:flow-call
                              (%archive-octets/k context env path)
                              (:on-error (code message &key candidates diagnostics conflicts repairs path)
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
                              (:on-octets (octets)
                                (let ((format
                                        (aitools.text.domain:detect-archive-format octets path)))
                                  (if (null format)
                                      (funcall on-rejected
                                               "input.unsupported-format"
                                               (format
                                                 nil
                                                 "~A is not a zip, tar, tar.gz or gz archive"
                                                 path))
                                      (let* ((root (context-path context))
                                             (steps
                                              (plan-archive-extraction octets
                                                                       format
                                                                       root
                                                                       :archive-path
                                                                       path
                                                                       :selected
                                                                       (getf options :entry)
                                                                       :max-bytes
                                                                       max-bytes
                                                                       :max-entries
                                                                       max-entries
                                                                       :lookup-kind
                                                                       (lambda (relative)
                                                                         (aitools.store.domain:entry-state-kind
                                                                          (aitools.store.application:view-path-state
                                                                           (write-context-view context)
                                                                           relative))))))
                                        (aitools.kernel.domain:flow-call
                                          (%check-extract-paths/k context steps)
                                          (:on-error (code message &key candidates diagnostics conflicts repairs path)
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
                                          (:on-ok ()
                                            (let ((requests
                                                   (append
                                                    (and (plusp (length root))
                                                         (list (aitools.store.domain:mkdir-request root)))
                                                    (mapcar
                                                     (lambda (step)
                                                       (%extract-step-request step))
                                                     steps))))
                                              (funcall on-committed
                                                       requests
                                                       (list (cons :limit-changes +extract-changes-shown+)
                                                             (cons :total-changes t))))))
                            )))))
       )
                          :record-options
                          options
                          :record-positionals
                          (constantly (list path))))
))))
