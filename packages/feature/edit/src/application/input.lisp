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

(defun %absolute-input-path (ports path)
  (aitools.workspace.application:user-path-absolute (edit-ports-workspace-host ports) path))

(defun read-input-file/k (ports path &key on-octets on-error)
  "--content-file PATH (relative to the working directory, outside the
workspace allowed: reads are not bounded by the workspace boundary) as bytes."
  (declare (type function on-octets on-error))
  (let ((source (edit-ports-text-source ports))
        (absolute (%absolute-input-path ports path)))
    (multiple-value-bind (size problem) (aitools.text.application:source-file-size source absolute)
      (cond
        ((eq problem :unreadable) (funcall on-error "environment.io" (format nil "cannot read ~A" path)))
        ((null size) (funcall on-error "input.not-found" (format nil "--content-file ~A does not exist" path)))
        ((> size +max-input-bytes+)
         (funcall on-error "refusal.too-large" (format nil "--content-file ~A exceeds ~D bytes" path +max-input-bytes+)))
        (t (let ((octets (aitools.text.application:source-read-octets source absolute)))
             (if octets
                 (funcall on-octets (coerce octets 'octets))
                 (funcall on-error "environment.io" (format nil "cannot read ~A" path)))))))))

(defun read-content/k (ports options &key on-content on-error)
  "The single content input: exactly one of --content, --content-file,
--stdin (or --stdin-data). ON-CONTENT (octets source) where SOURCE is
:CONTENT, :CONTENT-FILE or :STDIN."
  (declare (type function on-content on-error))
  (let ((given (remove nil (list (and (getf options :content) :content)
                                 (and (getf options :content-file) :content-file)
                                 (and (or (getf options :stdin) (getf options :stdin-data)) :stdin)))))
    (cond
      ((null given)
       (funcall on-error "argument.invalid" "no content: pass --content, --content-file, or --stdin (stdin is never read implicitly)"))
      ((rest given)
       (funcall on-error "argument.invalid" "pass only one of --content, --content-file, --stdin"))
      (t (ecase (first given)
           (:content (funcall on-content (aitools.text.domain:encode-utf8 (getf options :content)) :content))
           (:content-file (read-input-file/k ports (getf options :content-file)
                                             :on-octets (lambda (octets) (funcall on-content octets :content-file))
                                             :on-error on-error))
           (:stdin (read-stdin/k ports options
                                 :on-octets (lambda (octets) (funcall on-content octets :stdin))
                                 :on-error on-error)))))))

(defun read-content-text/k (ports options &key on-text on-error)
  "READ-CONTENT/K decoded as UTF-8 text (text commands); ON-TEXT (text)."
  (declare (type function on-text on-error))
  (read-content/k ports options
                  :on-content (lambda (octets source)
                                (%utf8-text/k octets :on-text on-text
                                                     :on-invalid (lambda (offset)
                                                                   (funcall on-error "input.not-utf8"
                                                                            (format nil "the ~(~A~) input is not UTF-8 at byte ~D"
                                                                                    source offset)))))
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

;;; --------------------------------------------------------- reading targets

(defun path-candidates (view path)
  "Up to 3 entries of PATH's directory with names closest to PATH's, as
{path} JSON objects, for input.not-found."
  (let* ((slash (position #\/ path :from-end t))
         (directory (if slash (subseq path 0 slash) ""))
         (name (subseq path (if slash (1+ slash) 0)))
         (entries (handler-case (aitools.store.application:view-directory-entries view directory)
                    (error () '()))))
    (loop for (nil . candidate)
            in (sort (mapcar (lambda (entry)
                               (cons (levenshtein-distance name (car entry))
                                     (if (zerop (length directory)) (car entry)
                                         (concatenate 'string directory "/" (car entry)))))
                             entries)
                     (lambda (a b) (or (< (car a) (car b))
                                       (and (= (car a) (car b)) (string< (cdr a) (cdr b))))))
          repeat 3
          collect (json-object "path" candidate))))

(defun read-file-octets/k (context path reject on-octets)
  "PATH's bytes in the write's view: ON-OCTETS (octets mode), or REJECT with
input.not-found (with candidates) or refusal.not-a-file."
  (declare (type function reject on-octets))
  (let* ((view (write-context-view context))
         ;; Only the kind and mode decide the branch here; VIEW-READ-FILE
         ;; reads the bytes below, so hashing the file through VIEW-PATH-STATE
         ;; would read it a second time for a value never used.
         (state (aitools.store.application:view-path-kind view path)))
    (case (aitools.store.domain:entry-state-kind state)
      (:file (funcall on-octets (coerce (aitools.store.application:view-read-file view path) 'octets)
                      (aitools.store.domain:entry-state-mode state)))
      (:absent (funcall reject "input.not-found" (format nil "~A does not exist" path)
                        :candidates (path-candidates view path)))
      (t (funcall reject "refusal.not-a-file" (format nil "~A is a ~(~A~), not a file" path
                                                      (aitools.store.domain:entry-state-kind state)))))))

(defun read-document/k (context path reject on-document &key prefilter on-filtered)
  "PATH decoded for a text edit: ON-DOCUMENT (document), or
REJECT refusal.not-a-file (binary), input.not-utf8, input.not-found.

When PREFILTER (a predicate on the file's raw octets) and ON-FILTERED are
both supplied and PREFILTER returns NIL, PATH cannot match, so ON-FILTERED
is called without decoding the bytes. The prefilter must be sound: a NIL
result must mean no possible match."
  (declare (type function reject on-document))
  (read-file-octets/k context path reject
                      (lambda (octets mode)
                        (declare (ignore mode))
                        (if (and prefilter on-filtered (not (funcall prefilter octets)))
                            (funcall on-filtered)
                          (decode-text-document/k
                           octets
                           :on-decoded on-document
                           :on-binary (lambda ()
                                        (funcall reject "refusal.not-a-file"
                                                 (format nil "~A is binary; text edits refuse it" path)))
                           :on-invalid (lambda (offset)
                                         (funcall reject "input.not-utf8"
                                                  (format nil "~A is not valid UTF-8 at byte ~D" path offset)
                                                  :diagnostics (list (json-object "path" path "offset" offset)))))))))

(defun %child (directory name)
  "NAME below the workspace-relative DIRECTORY (\"\" for the root)."
  (if (zerop (length directory)) name (concatenate 'string directory "/" name)))

(defun write-document-request (path document)
  (aitools.store.domain:write-file-request path (render-document document)))

;;; --------------------------------------------------------------- selectors

(defun selector-options (options)
  "The selector keywords of OPTIONS for PARSE-SELECTOR-OPTIONS/K."
  (list :range (getf options :range) :symbol (getf options :symbol) :kind (getf options :kind)
        :between (getf options :between) :exclusive (getf options :exclusive)
        :match (getf options :match) :invert (getf options :invert)))

(defun parse-selector/k (command options &key extra-exclusive on-selector on-none on-error)
  "The one selector of OPTIONS for COMMAND (a kernel command keyword).
ON-ERROR (code message &key repairs)."
  (declare (type function on-selector on-none on-error))
  (apply #'aitools.inspect.application:parse-selector-options/k
         command
         :extra-exclusive extra-exclusive
         :on-selector on-selector
         :on-none on-none
         :on-invalid (lambda (message repairs) (funcall on-error "argument.invalid" message :repairs repairs))
         (selector-options options)))

(defun selector-guards (selector path)
  "The guard requirements a SELECTOR brings for a write to PATH."
  (append (and (aitools.kernel.domain:guard-required-p :expect-hash selector) (list (list :expect-hash path)))
          (and (aitools.kernel.domain:guard-required-p :expect-count selector) (list (list :expect-count)))))

(defun content-selector-p (selector)
  (or (null selector) (eq (aitools.kernel.domain:selector-basis selector) :content)))

(defun resolve-lines/k (document selector path reject on-ranges)
  "SELECTOR resolved against DOCUMENT: ON-RANGES (ranges), 1-based
inclusive (start . end) pairs; the no-match, ambiguity and invalid cases go
to REJECT with their candidates."
  (declare (type function reject on-ranges))
  (aitools.inspect.application:resolve-selector/k
   (text-document-lines document) selector
   :path path
   :on-selected on-ranges
   :on-no-match (lambda (candidates)
                  (funcall reject "selection.no-match" (format nil "the selector matches nothing in ~A" path)
                           :candidates candidates))
   :on-ambiguous (lambda (candidates)
                   (funcall reject "selection.ambiguous"
                            (format nil "the selector matches ~D places in ~A; it must match one" (length candidates) path)
                            :candidates candidates))
   :on-invalid (lambda (code message) (funcall reject code message))))

(defun lines-candidates (pairs)
  "(line . text) pairs as {line,text} JSON objects."
  (mapcar (lambda (pair) (json-object "line" (car pair) "text" (cdr pair))) pairs))


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
