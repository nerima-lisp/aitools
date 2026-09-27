;;;; packages/feature/process/src/domain/wait-condition.lisp
;;;;
;;;; `wait` takes exactly one condition -- `--file P --pattern RE`,
;;;; `--port N`, `--bg ID --pattern RE`, `--bg ID --exit`, or `--duration D`.
(in-package #:aitools.process.domain)

(defstruct (wait-condition (:constructor %make-wait-condition) (:copier nil))
  (kind nil :type (member :file-pattern :port :bg-pattern :bg-exit :duration) :read-only t)
  (path nil :type (or null string) :read-only t)
  (pattern nil :type (or null string) :read-only t)
  (port nil :type (or null (integer 1 65535)) :read-only t)
  (bg-id nil :type (or null string) :read-only t)
  ;; The `--duration` text as given, echoed back in repair commands.
  (duration-text nil :type (or null string) :read-only t)
  (duration-ms nil :type (or null (integer 0)) :read-only t))

(defun make-wait-condition (&key file pattern port bg exit duration-text duration-ms)
  "(VALUES CONDITION NIL) for a valid option combination, else (VALUES NIL
MESSAGE). DURATION-MS is DURATION-TEXT already parsed by the caller."
  (let ((kinds (remove nil (list (and file :file) (and port :port) (and bg :bg)
                                 (and duration-text :duration)))))
    (flet ((reject (message) (return-from make-wait-condition (values nil message))))
      (cond
        ((null kinds)
         (reject "wait needs one condition: --file with --pattern, --port, --bg with --pattern or --exit, or --duration"))
        ((rest kinds)
         (reject "wait accepts exactly one of --file, --port, --bg, and --duration"))
        ((and exit (not bg)) (reject "--exit only applies to --bg"))
        ((and pattern (not (or file bg))) (reject "--pattern only applies to --file or --bg"))
        ((and file (not pattern)) (reject "--file needs --pattern"))
        ((and bg (not (or pattern exit))) (reject "--bg needs --pattern or --exit"))
        ((and bg pattern exit) (reject "--bg takes either --pattern or --exit, not both"))
        (file (values (%make-wait-condition :kind :file-pattern :path file :pattern pattern) nil))
        (port (values (%make-wait-condition :kind :port :port port) nil))
        ((and bg pattern) (values (%make-wait-condition :kind :bg-pattern :bg-id bg :pattern pattern) nil))
        (bg (values (%make-wait-condition :kind :bg-exit :bg-id bg) nil))
        (t (values (%make-wait-condition :kind :duration :duration-text duration-text
                                         :duration-ms duration-ms)
                   nil))))))

(defun wait-condition-arguments (condition)
  "CONDITION as the `wait` flags that express it, for repair commands."
  (ecase (wait-condition-kind condition)
    (:file-pattern (list "--file" (wait-condition-path condition) "--pattern" (wait-condition-pattern condition)))
    (:port (list "--port" (princ-to-string (wait-condition-port condition))))
    (:bg-pattern (list "--bg" (wait-condition-bg-id condition) "--pattern" (wait-condition-pattern condition)))
    (:bg-exit (list "--bg" (wait-condition-bg-id condition) "--exit"))
    (:duration (list "--duration" (wait-condition-duration-text condition)))))

(defun first-matching-line (text pattern strip-p)
  "(VALUES LINE REDACTIONS): the first line of TEXT that PATTERN matches,
after normalization and redaction, or NIL."
  (dolist (raw (split-output-lines text) (values nil 0))
    (multiple-value-bind (line count)
        (aitools.protocol.domain:redact-secrets (normalize-terminal-line raw strip-p))
      (when (line-pattern-matches-p pattern line)
        (return (values line count))))))
