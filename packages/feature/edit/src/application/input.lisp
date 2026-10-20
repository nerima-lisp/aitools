;;;; packages/feature/edit/src/application/input.lisp
;;;;
;;;; What the command flows share: reading content inputs (--content,
;;;; --content-file, --stdin, never stdin implicitly), reading a text target
;;;; for editing, naming similar paths for input.not-found, and
;;;; resolving selectors through the inspect context.
(in-package #:aitools.edit.application)

(defun %utf8-text/k (octets &key on-text on-invalid)
  (aitools.text.domain:decode-utf8-strict/k (coerce octets 'octets) :on-decoded on-text :on-invalid on-invalid))

(defun read-stdin/k (ports options &key on-octets on-error)
  "The --stdin input: --stdin-data when present (a recorded or inline
input), else standard input read through the port. Calls ON-OCTETS
(octets) or ON-ERROR (code message)."
  (declare (type function on-octets on-error))
  (let ((inline (getf options :stdin-data)))
    (cond
      (inline (funcall on-octets (aitools.text.domain:encode-utf8 inline)))
      ((null (and ports (edit-ports-read-stdin-octets ports)))
       (funcall on-error "environment.unavailable" "standard input cannot be read here"))
      (t (funcall (edit-ports-read-stdin-octets ports) +max-input-bytes+
                  :on-octets on-octets
                  :on-too-large (lambda ()
                                  (funcall on-error "refusal.too-large"
                                           (format nil "standard input exceeds ~D bytes" +max-input-bytes+)))
                  :on-failure (lambda (message) (funcall on-error "environment.io" message)))))))

(defun read-stdin-text/k (ports options &key on-text on-error)
  "The --stdin input as UTF-8 text; ON-TEXT (text)."
  (declare (type function on-text on-error))
  (read-stdin/k ports options
                :on-octets (lambda (octets)
                             (%utf8-text/k octets :on-text on-text
                                                  :on-invalid (lambda (offset)
                                                                (funcall on-error "input.not-utf8"
                                                                         (format nil "--stdin is not UTF-8 at byte ~D" offset)))))
                :on-error on-error))

(defun read-stdin-json/k (ports options &key on-json on-error)
  "The --stdin input parsed as JSON (the edit value model); ON-JSON (value
text) with TEXT the raw input, recorded as --stdin-data."
  (declare (type function on-json on-error))
  (read-stdin-text/k ports options
                     :on-text (lambda (text)
                                (parse-json-text/k text
                                                   :on-value (lambda (value) (funcall on-json value text))
                                                   :on-invalid (lambda (message)
                                                                 (funcall on-error "input.syntax-error"
                                                                          (format nil "--stdin is not JSON: ~A" message)))))
                     :on-error on-error))

(defun inline-content-options (options text)
  "OPTIONS with the content input recorded as --content TEXT, so a replayed
op does not depend on a file or stdin that may be gone."
  (let ((copy (copy-list options)))
    (dolist (key '(:content-file :stdin :stdin-data))
      (remf copy key))
    (setf (getf copy :content) text)
    copy))

(defun inline-stdin-options (options text)
  (let ((copy (copy-list options)))
    (remf copy :stdin)
    (setf (getf copy :stdin-data) text)
    copy))


;;; ---------------------------------------------------------------- commands

(defstruct (command-env (:constructor make-command-env (host root ports)) (:copier nil))
  "The live invocation's workspace host and resolved WORKSPACE-ROOT, for the
few preparations that look at the workspace before the write (scans). A
`tx rebase` replay has no env: its recorded paths are already resolved."
  (host nil :read-only t)
  (root nil :read-only t)
  (ports nil :read-only t))

(defvar *preparers* (make-hash-table :test 'equal)
  "Dispatch name -> (lambda (ports env positionals options on-plan fail)).
ON-PLAN receives a WRITE-PLAN; FAIL takes (code message &key candidates
diagnostics conflicts repairs path).")

(defmacro define-write-command (name (ports env positionals options on-plan fail) &body body)
  "Define how command NAME turns its positionals and options into a
WRITE-PLAN (see *PREPARERS*). BODY may leave early with (RETURN-FROM PREPARE ...)."
  `(setf (gethash ,name *preparers*)
         (lambda (,ports ,env ,positionals ,options ,on-plan ,fail)
           (declare (ignorable ,ports ,env ,positionals ,options)
                    (type function ,on-plan ,fail))
           (block prepare ,@body))))

(defun commit-document (context path document commit &optional extra)
  "COMMIT the write of DOCUMENT to PATH, or nothing when its bytes equal
the current file's."
  (let ((octets (render-document document))
        (current (aitools.store.application:view-read-file (write-context-view context) path)))
    (funcall commit (if (and current (equalp octets current))
                        '()
                        (list (aitools.store.domain:write-file-request path octets)))
             extra)))
