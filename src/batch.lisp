;;;; `batch --stdin [--continue-on-error | --atomic]` is a top-level command
;;;; beside `schema`. Each element contains only a child command's arguments,
;;;; without the `aitools` executable name.
;;;; `%batch-handler` uses the invocation's app and `%batch-registry%`, bound
;;;; by `register-batch-command`, so child commands use the same app and
;;;; registry. Dispatch gives each child the same parsing, validation,
;;;; recovery, and envelope behavior as a standalone invocation. It writes
;;;; each envelope to a string stream. Batch parses them without changing
;;;; JSON member order and returns them in element order in `results[]`.
;;;;
;;;; `--atomic` is transaction sugar (docs/src/reference/transactions.md).
;;;; Batch begins a tx, dispatches each element with `--tx <tx>`, and commits
;;;; at the end. A failed element triggers `tx abort`. If abort fails, staged
;;;; changes may remain and batch adds a `tx status` repair while keeping the
;;;; element's error code.
;;;;
;;;; A batch returns status:"ok" when every element exits with code 0 or 3.
;;;; Code 3 means the child returned a partial result, kept as status:"partial"
;;;; in `results[]`.
;;;; An element failure returns its error code, message, and repairs, with
;;;; `results[]` in `error.diagnostics`. Invalid input and tx-begin failures
;;;; report their own errors. A commit failure reports the commit error. If
;;;; abort fails, the original element or commit error is kept with recovery
;;;; details.
(in-package #:aitools/cli)

(defvar %batch-registry% nil)

(defconstant +batch-max-input-chars+ (* 16 1024 1024))

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

(defun %element-invocation (app argv)
  "ARGV's invocation when it parses, else NIL (DISPATCH reports the
parse error when the element runs)."
  (handler-case (parse-argv app (cons "aitools" argv))
    (error () nil)))

(defun %element-problem (app argv index atomic)
  "The reason element INDEX (0-based) cannot run inside a batch, or NIL."
  (let ((invocation (%element-invocation app argv)))
    (cond
    ((and invocation (equal (%full-command-name invocation) "batch"))
     (format nil "element ~D is a batch; batches do not nest" index))
    ((and atomic invocation (option-value invocation :tx))
     (format nil "element ~D has --tx; batch --atomic runs every element in its own tx" index))
    ((and invocation (option-value invocation :stdin))
     (format nil "element ~D reads --stdin, which batch has consumed; pass the input inline (--content, --stdin-data)"
             index)))))

(defun %with-tx (app argv tx)
  "Insert `--tx TX` after the final command component and before its option values or a delimiter."
  (let* ((invocation (%element-invocation app argv))
         (commands (and invocation (invocation-command-path invocation))))
    (if (null commands)
        argv
        (loop with remaining = commands
              for tail on argv
              for position from 0
              for token = (car tail)
              do (cond
                   ((and (plusp (length token)) (char= (char token 0) #\-))
                    ;; Before the leaf, only global value options consume a token.
                    (when (and (not (find #\= token))
                               (find (string-left-trim "-" token) (cl-cli:app-global-options app)
                                     :test (lambda (name option)
                                             (member name (cl-cli:option-names option) :test #'string=))))
                      (setf tail (cdr tail))
                      (incf position)))
                   ((member token (cons (command-name (first remaining))
                                        (cl-cli:command-aliases (first remaining))) :test #'string=)
                    (pop remaining)
                    (when (null remaining)
                      (return (append (subseq argv 0 (1+ position))
                                      (list "--tx" tx) (cdr tail))))))))))

(defun %parse-envelope (text)
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

(aitools.kernel.domain:define-flow
    (%fail-with-envelope envelope message results &key repairs on-error)
  "Continue with the error code and metadata from ENVELOPE, using RESULTS as diagnostics."
  (declare (type function on-error))
  (aitools.kernel.domain:flow-continue
   :on-error (%member envelope "error" "code")
   message
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
                     (%dispatch-captured app registry (append globals (if tx (%with-tx app argv tx) argv)))
                   (push envelope results)
                   (unless (or failure (member code '(0 3)))
                     (setf failure (list index argv envelope))))))
    (values (nreverse results) failure)))

(defun %element-failure-message (failure count)
  (destructuring-bind (index argv envelope) failure
    (format nil "element ~D of ~D (~{~A~^ ~}) failed: ~A" index count argv (%member envelope "error" "message"))))
