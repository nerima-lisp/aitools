;;;; packages/feature/edit/src/application/archive-flows.lisp
;;;;
;;;; `archive extract` and `archive create`.
(in-package #:aitools.edit.application)

(defparameter +extract-changes-shown+ 200
  "`archive extract`'s `changes` lists at most this many entries; `total` counts all.")

(defparameter +extract-default-max-bytes+ (* 256 1024 1024)
  "The default output cap for `archive extract` when --max-bytes is not
given. The former 1GiB default let one gzip/tar bomb allocate most of the
heap; the text-domain inflater enforces this bound (ARCHIVE-LIMIT-EXCEEDED)
per member and in total.")

(defun %archive-octets/k (context env path reject on-octets)
  "The archive PATH's bytes: through the write's view when it is inside the
workspace (so --tx sees staged archives), else from the host, since reads
are not bounded by the workspace boundary."
  (declare (type function reject on-octets))
  (let ((view (write-context-view context)))
    (flet ((from-host ()
             (let* ((host (command-env-host env))
                    (absolute (aitools.workspace.application:user-path-absolute host path))
                    (octets (aitools.workspace.application:host-read-octets host absolute)))
               (if octets
                   (funcall on-octets (coerce octets 'octets))
                   (funcall reject "input.not-found" (format nil "archive ~A does not exist" path))))))
      (resolve-extra-path/k context path
                            :base :cwd
                            :on-inside (lambda (relative)
                                         (if (eq (aitools.store.domain:entry-state-kind
                                                  (aitools.store.application:view-path-state view relative))
                                                 :file)
                                             (funcall on-octets (coerce (aitools.store.application:view-read-file view relative)
                                                                        'octets))
                                             (from-host)))
                            :on-outside (lambda (message) (declare (ignore message)) (from-host))))))

(defun %check-extract-paths/k (context steps reject on-ok)
  "Route every extracted path through the workspace boundary so an
entry named `.git/...` (or one a symlinked parent would take outside the
root) is refused rather than written, the same rule every other write obeys.
Calls ON-OK once every step is inside the workspace."
  (declare (type function reject on-ok))
  (labels ((walk (remaining)
             (if (null remaining)
                 (funcall on-ok)
                 (resolve-extra-path/k
                  context (extract-step-path (first remaining)) :base :root
                  :on-inside (lambda (relative) (declare (ignore relative)) (walk (rest remaining)))
                  :on-outside (lambda (message) (funcall reject "refusal.outside-workspace" message))))))
    (walk steps)))

(define-write-command "archive.extract" (ports env positionals options on-plan fail)
  (let* ((path (first positionals))
         (destination (getf options :to))
         (max-bytes (if (getf options :max-bytes)
                        (handler-case (aitools.kernel.domain:size-bytes
                                       (aitools.kernel.domain:parse-size (getf options :max-bytes)))
                          (error () nil))
                        +extract-default-max-bytes+))
         (max-entries (parse-count (or (getf options :max-entries) "100000"))))
    (cond
      ((or (null path) (rest positionals)) (funcall fail "argument.invalid" "archive extract takes exactly one archive PATH"))
      ((null destination) (funcall fail "argument.invalid" "archive extract needs --to <dir>"))
      ((null max-bytes) (funcall fail "argument.invalid" (format nil "--max-bytes ~S is not a size" (getf options :max-bytes))))
      ((null max-entries) (funcall fail "argument.invalid" "--max-entries must be a non-negative integer"))
      (t
       (funcall on-plan
                (make-write-plan
                 :command "archive.extract"
                 :targets (list (make-write-target destination))
                 :plan (lambda (context commit reject)
                         (%archive-octets/k
                          context env path reject
                          (lambda (octets)
                            (let ((format (aitools.text.domain:detect-archive-format octets path)))
                              (if (null format)
                                  (funcall reject "input.unsupported-format"
                                           (format nil "~A is not a zip, tar, tar.gz or gz archive" path))
                                  (let* ((root (context-path context))
                                         (steps (plan-archive-extraction
                                                 octets format root
                                                 :archive-path path
                                                 :selected (getf options :entry)
                                                 :max-bytes max-bytes :max-entries max-entries
                                                 :lookup-kind (lambda (relative)
                                                                (aitools.store.domain:entry-state-kind
                                                                 (aitools.store.application:view-path-state
                                                                  (write-context-view context) relative))))))
                                    (%check-extract-paths/k
                                     context steps reject
                                     (lambda ()
                                       (let ((requests
                                               (append
                                                (and (plusp (length root)) (list (aitools.store.domain:mkdir-request root)))
                                                (mapcar (lambda (step)
                                                          (ecase (extract-step-kind step)
                                                            (:directory (aitools.store.domain:mkdir-request (extract-step-path step)))
                                                            (:file (aitools.store.domain:write-file-request
                                                                    (extract-step-path step) (extract-step-data step)
                                                                    ;; CWE-732: mask by #o755, never #o777, so a 0777
                                                                    ;; or 0666 entry never extracts a group/other-writable
                                                                    ;; (or setuid/setgid/sticky) file. Deterministic, no
                                                                    ;; process-umask dependency; matches tar -x under umask
                                                                    ;; 022 for the common modes.
                                                                    :mode (logand (extract-step-mode step) #o755)))
                                                            (:symlink (aitools.store.domain:symlink-request
                                                                       (extract-step-path step) (extract-step-target step)))))
                                                        steps))))
                                         (funcall commit requests
                                                  (list (cons :limit-changes +extract-changes-shown+)
                                                        (cons :total-changes t))))))))))))
                 :record-options options
                 :record-positionals (constantly (list path))))))))

