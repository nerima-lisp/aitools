;;;;
;;;; What the command flows share: reading content inputs (--content,
;;;; --content-file, --stdin, never stdin implicitly), reading a text target
;;;; for editing, naming similar paths for input.not-found, and
;;;; resolving selectors through the inspect context.
(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow
  (%utf8-text/k octets &key on-text on-invalid)
  (multiple-value-bind (status value)
      (aitools.text.domain:decode-utf8-strict/k
       (coerce octets (quote octets))
       :on-decoded
       (lambda (text) (values :decoded text))
       :on-invalid
       (lambda (offset) (values :invalid offset)))
    (if (eq status :decoded)
        (aitools.kernel.domain:flow-continue :on-text value)
        (aitools.kernel.domain:flow-continue :on-invalid value))))

(aitools.kernel.domain:define-flow
  (read-stdin/k ports options &key on-octets on-error)
  "The --stdin input: --stdin-data when present (a recorded or inline
input), else standard input read through the port. Calls ON-OCTETS
(octets) or ON-ERROR (code message)."
  (let ((inline (getf options :stdin-data)))
    (cond
      (inline
       (aitools.kernel.domain:flow-continue
         :on-octets
         (aitools.text.domain:encode-utf8 inline)))
      ((null (and ports (edit-ports-read-stdin-octets ports)))
       (aitools.kernel.domain:flow-continue
         :on-error
         "environment.unavailable"
         "standard input cannot be read here"))
      (t
       (multiple-value-bind (status payload)
           (funcall (edit-ports-read-stdin-octets ports) +max-input-bytes+)
         (case status
           (:octets
            (aitools.kernel.domain:flow-continue :on-octets payload))
           (:too-large
            (aitools.kernel.domain:flow-continue
              :on-error
              "refusal.too-large"
              (format nil "standard input exceeds ~D bytes" +max-input-bytes+)))
           (:failure
            (aitools.kernel.domain:flow-continue :on-error "environment.io" payload))
           (otherwise
            (error "Unknown standard-input result ~S." status))))))))

(aitools.kernel.domain:define-flow
  (read-stdin-text/k ports options &key on-text on-error)
  "The --stdin input as UTF-8 text; ON-TEXT (text)."
  (aitools.kernel.domain:flow-call
    (read-stdin/k ports options)
    (:on-octets (octets)
      (aitools.kernel.domain:flow-call
        (%utf8-text/k octets)
        (:on-text (text)
          (aitools.kernel.domain:flow-continue
            :on-text
            text))
        (:on-invalid (offset)
          (aitools.kernel.domain:flow-continue
            :on-error
            "input.not-utf8"
            (format
              nil
              "--stdin is not UTF-8 at byte ~D"
              offset)))))
    (:on-error (code message)
      (aitools.kernel.domain:flow-continue
        :on-error
        code
        message))))

(aitools.kernel.domain:define-flow
  (read-stdin-json/k ports options &key on-json on-error)
  "The --stdin input parsed as JSON (the edit value model); ON-JSON (value
text) with TEXT the raw input, recorded as --stdin-data."
  (aitools.kernel.domain:flow-call
    (read-stdin-text/k ports options)
    (:on-text (text)
      (aitools.kernel.domain:flow-call
        (parse-json-text/k text)
        (:on-value (value)
          (aitools.kernel.domain:flow-continue
            :on-json
            value
            text))
        (:on-invalid (message)
          (aitools.kernel.domain:flow-continue
            :on-error
            "input.syntax-error"
            (format
              nil
              "--stdin is not JSON: ~A"
              message)))))
    (:on-error (code message)
      (aitools.kernel.domain:flow-continue
        :on-error
        code
        message))))

(defun %absolute-input-path (ports path)
  (aitools.workspace.application:user-path-absolute (edit-ports-workspace-host ports) path))

(defun inline-stdin-options (options text)
  (let ((copy (copy-list options)))
    (remf copy :stdin)
    (setf (getf copy :stdin-data) text)
    copy))

(defstruct (command-env (:constructor make-command-env (host root ports)) (:copier nil))
  "The live invocation's workspace host and resolved WORKSPACE-ROOT, for the
few preparations that look at the workspace before the write (scans). A
`tx rebase` replay has no env: its recorded paths are already resolved."
  (host nil :read-only t)
  (root nil :read-only t)
  (ports nil :read-only t))

(defvar *preparers*
  (make-hash-table :test 'equal)
  "Dispatch name -> registered define-write-command flow.")

(defmacro define-write-command (name lambda-list &body body)
  "Declare a write command as a CPS flow and register its dispatch name."
  (destructuring-bind (ports env positionals options marker on-plan on-error) lambda-list
    (unless
        (and (stringp name)
             (plusp (length name))
             (symbolp marker)
             (string= (symbol-name marker) "&KEY")
             (symbolp on-plan)
             (string= (symbol-name on-plan) "ON-PLAN")
             (symbolp on-error)
             (string= (symbol-name on-error) "ON-ERROR"))
      (error "Invalid write command declaration: ~S." name))
    (let ((flow-name
           (intern
            (format
              nil
              "PREPARE-~A/K"
              (substitute #\- #\. (string-upcase name)))
            *package*)))
      `(progn
         (aitools.kernel.domain:define-flow
           (,flow-name ,ports ,env ,positionals ,options ,marker ,on-plan ,on-error)
           ,@body)
         (setf (gethash ,name *preparers*) (symbol-function (quote ,flow-name)))))))

(aitools.kernel.domain:define-flow
  (commit-document context path document &key on-committed extra)
  "COMMIT the write of DOCUMENT to PATH, or nothing when its bytes equal
the current file's."
  (let ((octets (render-document document))
        (current (aitools.store.application:view-read-file (write-context-view context) path)))
    (aitools.kernel.domain:flow-continue
      :on-committed
      (if (and current (equalp octets current))
          '()
          (list (aitools.store.domain:write-file-request path octets)))
      extra)))
