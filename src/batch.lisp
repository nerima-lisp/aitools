;;;; src/batch.lisp
;;;;
;;;; `batch --stdin [--continue-on-error | --atomic]`. Composition
;;;; root, like `schema`, because each element is a whole
;;;; aitools invocation: it goes through DISPATCH on the app this process
;;;; already built, so it gets exactly the parse, validation, recovery and
;;;; envelope a standalone call gets. DISPATCH writes each element's envelope
;;;; to a string stream; batch reads it back (member order kept) into
;;;; `results[]`.
;;;;
;;;; `--atomic` is transaction sugar (docs/src/reference/transactions.md): `tx begin`, every element with
;;;; `--tx <tx>`, then `tx commit`, each dispatched the same way; a failed
;;;; element or a conflicting commit is followed by `tx abort`, so nothing
;;;; reaches the working tree.
;;;;
;;;; A batch whose elements all succeed answers `status:"ok"` with
;;;; `results[]`. Otherwise it answers with the first failing element's
;;;; error.code (hence its exit code), its message and repairs, and
;;;; `results[]` in `error.diagnostics`, the error envelope's slot for
;;;; command-specific detail.
(in-package #:aitools/cli)

(defvar %batch-registry% nil
  "Bound by REGISTER-BATCH-COMMAND to the COMMAND-REGISTRY the running
process built; %BATCH-HANDLER dispatches every element against it.")

(defconstant +batch-max-input-chars+ (* 16 1024 1024)
  "Largest --stdin document batch reads.")

(defun %read-batch-input (stream)
  "STREAM's text, or NIL when it holds more than +BATCH-MAX-INPUT-CHARS+."
  (let ((buffer (make-string 65536)))
    (with-output-to-string (out)
      (loop with total = 0
            for count = (read-sequence buffer stream)
            while (plusp count)
            do (when (> (incf total count) +batch-max-input-chars+)
                 (return-from %read-batch-input nil))
               (write-string buffer out :end count)))))

(defun %parse-batch-argvs (text)
  "(values argvs nil) for a JSON array of non-empty string arrays, or
(values nil code message)."
  (let ((value (handler-case (json-kit:parse text)
                 (json-kit:json-kit-error (condition)
                   (return-from %parse-batch-argvs
                     (values nil "input.syntax-error" (format nil "--stdin is not JSON: ~A" condition)))))))
    (if (and (vectorp value) (not (stringp value))
             (every (lambda (element)
                      (and (vectorp element) (not (stringp element)) (plusp (length element))
                           (every #'stringp element)))
                    value))
        (values (map 'list (lambda (element) (coerce element 'list)) value) nil)
        (values nil "argument.invalid"
                "--stdin must be a JSON array of argv arrays of strings, such as [[\"read\",\"a.lisp\"]]"))))

(defun %options-part (argv)
  "The tokens of ARGV before a `--`, the only ones that can be options."
  (subseq argv 0 (position "--" argv :test #'string=)))

(defun %has-option-p (argv name)
  (let ((flag (concatenate 'string "--" name)))
    (some (lambda (token)
            (or (string= token flag)
                (and (> (length token) (length flag))
                     (string= flag token :end2 (length flag))
                     (char= (char token (length flag)) #\=))))
          (%options-part argv))))

(defun %element-command-name (app argv)
  "ARGV's dispatch name when it parses, else NIL (DISPATCH reports the
parse error when the element runs)."
  (handler-case (%full-command-name (parse-argv app (cons "aitools" argv)))
    (error () nil)))

(defun %element-problem (app argv index atomic)
  "The reason element INDEX (0-based) cannot run inside a batch, or NIL."
  (cond
    ((equal (%element-command-name app argv) "batch")
     (format nil "element ~D is a batch; batches do not nest" index))
    ((and atomic (%has-option-p argv "tx"))
     (format nil "element ~D has --tx; batch --atomic runs every element in its own tx" index))
    ((%has-option-p argv "stdin")
     (format nil "element ~D reads --stdin, which batch has consumed; pass the input inline (--content, --stdin-data)"
             index))))

(defun %with-tx (argv tx)
  "ARGV with `--tx TX` added among its options (before a `--`)."
  (let ((split (position "--" argv :test #'string=)))
    (append (subseq argv 0 (or split (length argv))) (list "--tx" tx)
            (and split (subseq argv split)))))

(defun %parse-envelope (text)
  "TEXT (one envelope line) as json-kit values with member order kept."
  (json-kit:parse text :object-type :alist :object-hook #'json-kit:make-json-object))

(defun %dispatch-captured (app registry argv)
  "(values exit-code envelope) of dispatching ARGV (no argv0) on APP."
  (let* ((out (make-string-output-stream))
         (err (make-string-output-stream))
         (code (dispatch app registry (cons "aitools" argv) :stdout out :stderr err))
         (stdout (get-output-stream-string out)))
    (values code (%parse-envelope (if (plusp (length stdout)) stdout (get-output-stream-string err))))))

(defun %member (object &rest keys)
  (let ((value object))
    (dolist (key keys value)
      (setf value (and (json-kit:json-object-p value)
                       (cdr (assoc key (json-kit:json-object-members value) :test #'string=)))))))

(defun %skipped (argv)
  (aitools.protocol.domain:json-object-from-alist (list (cons "status" "skipped") (cons "argv" argv))))

(defun %repair-plists (envelope)
  (map 'list (lambda (repair)
               (list :action (%member repair "action") :detail (%member repair "detail")
                     :command (%member repair "command")))
       (or (%member envelope "error" "repairs") #())))

(defun %fail-with-envelope (on-error envelope message results &key repairs)
  "ON-ERROR with ENVELOPE's error.code, MESSAGE, its conflicts, and RESULTS
as diagnostics; REPAIRS replaces the envelope's own."
  (funcall on-error (%member envelope "error" "code") message
           :conflicts (let ((conflicts (%member envelope "error" "conflicts")))
                        (and conflicts (coerce conflicts 'list)))
           :diagnostics results
           :repairs (or repairs (%repair-plists envelope))))

(defun %batch-command-line (globals &rest words)
  (format nil "aitools~{ ~A~}" (append globals (list "batch") words)))

(defun %run-elements (app registry argvs globals tx continue-on-error)
  "Dispatch every element (with `--tx TX` when TX). Returns (values results
failure), FAILURE being (index argv envelope) of the first failing element
or NIL; after it every element is skipped unless CONTINUE-ON-ERROR."
  (let ((failure nil) (results '()))
    (loop for argv in argvs
          for index from 0
          do (if (and failure (not continue-on-error))
                 (push (%skipped argv) results)
                 (multiple-value-bind (code envelope)
                     (%dispatch-captured app registry (append globals (if tx (%with-tx argv tx) argv)))
                   (push envelope results)
                   (unless (or failure (member code '(0 3)))
                     (setf failure (list index argv envelope))))))
    (values (nreverse results) failure)))

(defun %element-failure-message (failure count)
  (destructuring-bind (index argv envelope) failure
    (format nil "element ~D of ~D (~{~A~^ ~}) failed: ~A" index count argv (%member envelope "error" "message"))))

(defun %batch-plain/k (app registry argvs globals continue-on-error on-ok on-error)
  (multiple-value-bind (results failure) (%run-elements app registry argvs globals nil continue-on-error)
    (if failure
        (%fail-with-envelope on-error (third failure) (%element-failure-message failure (length argvs)) results)
        (funcall on-ok (list (cons "results" results))))))

(defun %atomic-commit-failed/k (abort-tx tx committed results globals on-error)
  "The commit of an --atomic batch failed. A commit can fail before its commit
point (a conflict: the tx aborts cleanly and nothing reaches the workspace) or
after it (an I/O failure once some files are already written: the intent is
committed and recovery finishes it). `tx abort` succeeds only in the first
case, so its exit code, not the commit's, decides whether we may claim nothing
was written. ABORT-TX runs `tx abort` and returns its (values exit-code
envelope)."
  (declare (type function abort-tx on-error))
  (multiple-value-bind (abort-code aborted) (funcall abort-tx)
    (if (zerop abort-code)
        (%fail-with-envelope
         on-error committed
         (format nil "tx ~A could not commit, so it was aborted and nothing was written: ~A"
                 tx (%member committed "error" "message"))
         results
         :repairs (list (%repair "retry"
                                 "Re-read the conflicting paths, then run the batch again."
                                 (%batch-command-line globals "--stdin" "--atomic"))))
        (%fail-with-envelope
         on-error committed
         (format nil "tx ~A could not commit and could not be aborted (~A); some files may already be written and recovery will complete the committed op on the next command: ~A"
                 tx (%member aborted "error" "message") (%member committed "error" "message"))
         results
         :repairs (list (%repair "inspect-state"
                                 "Inspect the transaction and the journal before retrying; the next command also runs recovery."
                                 (format nil "aitools tx status ~A" tx)))))))

(defun %batch-atomic/k (app registry argvs globals on-ok on-error)
  (multiple-value-bind (code begun) (%dispatch-captured app registry (append globals (list "tx" "begin" "--name" "batch")))
    (if (/= code 0)
        (%fail-with-envelope on-error begun (format nil "batch --atomic could not begin its tx: ~A"
                                                    (%member begun "error" "message"))
                             '())
        (let ((tx (%member begun "tx")))
          (flet ((abort-tx () (%dispatch-captured app registry (append globals (list "tx" "abort" tx)))))
            (multiple-value-bind (results failure) (%run-elements app registry argvs globals tx nil)
              (if failure
                  (progn
                    (abort-tx)
                    (%fail-with-envelope on-error (third failure)
                                         (format nil "~A; tx ~A was aborted and nothing was written"
                                                 (%element-failure-message failure (length argvs)) tx)
                                         results))
                  (multiple-value-bind (code committed) (%dispatch-captured app registry (append globals (list "tx" "commit" tx)))
                    (if (/= code 0)
                        (%atomic-commit-failed/k #'abort-tx tx committed results globals on-error)
                        (funcall on-ok
                                 (append (list (cons "results" results) (cons "tx" tx))
                                         (loop for key in '("changes" "op_id" "next_commands")
                                               for value = (%member committed key)
                                               when value collect (cons key value)))))))))))))

(defun %batch-flow/k (app registry invocation &key on-ok on-error)
  (declare (type function on-ok on-error))
  (let* ((atomic (option-value invocation :atomic))
         (continue-on-error (option-value invocation :continue-on-error))
         (globals (append (let ((root (option-value invocation :root))) (and root (list "--root" root)))
                          (let ((timeout (option-value invocation :lock-timeout)))
                            (and timeout (list "--lock-timeout" timeout)))))
         (schema-repair (list (%repair "inspect-schema" "Show batch's input format and rules."
                                       "aitools schema batch"))))
    (flet ((invalid (code message)
             (funcall on-error code message :repairs schema-repair)))
      (cond
        ((not (option-value invocation :stdin))
         (invalid "argument.invalid" "batch reads its argv arrays from standard input: pass --stdin"))
        ((and atomic continue-on-error)
         (invalid "argument.invalid" "--atomic and --continue-on-error cannot be combined"))
        (t
         (let ((text (%read-batch-input *standard-input*)))
           (if (null text)
               (invalid "argument.invalid" (format nil "--stdin is larger than ~D characters" +batch-max-input-chars+))
               (multiple-value-bind (argvs code message) (%parse-batch-argvs text)
                 (if code
                     (invalid code message)
                     (let ((problem (loop for argv in argvs
                                          for index from 0
                                          thereis (%element-problem app argv index atomic))))
                       (cond
                         (problem (invalid "argument.invalid" problem))
                         (atomic (%batch-atomic/k app registry argvs globals on-ok on-error))
                         (t (%batch-plain/k app registry argvs globals continue-on-error on-ok on-error)))))))))))))

(defun %batch-handler (invocation)
  (aitools.protocol.application:call-with-command-result/k
   (lambda (&key on-ok on-partial on-error)
     (declare (ignore on-partial))
     (%batch-flow/k (invocation-app invocation) %batch-registry% invocation :on-ok on-ok :on-error on-error))))

(defun register-batch-command (registry)
  (setf %batch-registry% registry)
  (register-command
   registry
   :name "batch"
   :cli-command (make-command
                 :name "batch"
                 :description "Run several aitools invocations from one JSON input, optionally as one tx."
                 :options (list (make-option :name "stdin" :kind :flag
                                             :description "Read the JSON array of argv arrays from standard input (required).")
                                (make-option :name "continue-on-error" :kind :flag
                                             :description "Run every element even after one fails.")
                                (make-option :name "atomic" :kind :flag
                                             :description "Run every element in one tx and commit it at the end, or write nothing."))
                 :handler #'%batch-handler)
   :schema (aitools.protocol.domain:make-command-schema
            "batch"
            "Run several aitools invocations from one JSON input, optionally as one tx."
            :description "--stdin is a JSON array of argv arrays without the leading aitools, e.g. [[\"read\",\"a.lisp\"],[\"edit\",\"a.lisp\",\"--old\",\"x\",\"--new\",\"y\"]]. Each element is parsed and validated exactly as a standalone call, with batch's --root and --lock-timeout placed before it. Elements run in order; after the first failure the rest are skipped unless --continue-on-error. Without --atomic each write commits on its own: undo the successful elements' op_id values to reverse a partial batch. --atomic runs tx begin, every element with --tx <tx>, then tx commit, as one op; a failing element or a conflicting commit aborts the tx and writes nothing. An element may not be a batch, may not read --stdin (batch has consumed it; pass --content or --stdin-data), and under --atomic may not carry --tx. On failure the answer is an error with the first failing element's error.code and exit code, and results[] in error.diagnostics."
            :args '((:name "--stdin" :type "flag" :required t :description "Read the argv arrays from standard input.")
                    (:name "--continue-on-error" :type "flag" :description "Keep running after a failed element; not with --atomic.")
                    (:name "--atomic" :type "flag" :description "All elements in one tx, committed at the end, or nothing; not with --continue-on-error."))
            :output-fields '((:name "results" :description "One envelope per element, in order; {status:\"skipped\",argv} for an element not run after a failure.")
                             (:name "tx" :description "--atomic: the tx the elements were staged in.")
                             (:name "changes" :description "--atomic: the committed tx's changes, in the standard write output shape.")
                             (:name "op_id" :description "--atomic: the single journal op of the whole batch (undo reverses it)."))
            :error-codes '("argument.invalid" "input.syntax-error" "refusal.target-changed" "environment.busy")))
  registry)
