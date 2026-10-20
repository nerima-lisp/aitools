(in-package #:aitools/cli)

(aitools.kernel.domain:define-flow
    (%batch-plain app registry argvs globals continue-on-error &key on-ok on-error)
  (declare (type function on-ok on-error))
  (multiple-value-bind (results failure)
      (%run-elements app registry argvs globals nil continue-on-error)
    (if failure
        (aitools.kernel.domain:flow-call
            (%fail-with-envelope
             (third failure)
             (%element-failure-message failure (length argvs))
             results)
          (:on-error (code message &key candidates diagnostics conflicts repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message
             :candidates candidates
             :diagnostics diagnostics
             :conflicts conflicts
             :repairs repairs)))
        (aitools.kernel.domain:flow-continue
         :on-ok
         (list (cons "results" results))))))

(aitools.kernel.domain:define-flow
    (%batch-flow app registry invocation &key on-ok on-error)
  (declare (type function on-ok on-error))
  (let* ((atomic (option-value invocation :atomic))
         (continue-on-error (option-value invocation :continue-on-error))
         (globals
           (append
            (let ((root (option-value invocation :root)))
              (and root (list "--root" root)))
            (let ((timeout (option-value invocation :lock-timeout)))
              (and timeout (list "--lock-timeout" timeout)))))
         (schema-repair
           (list (repair "inspect-schema"
                         "Show batch's input format and rules."
                         "aitools schema batch"))))
    (flet ((invalid (code message)
             (aitools.kernel.domain:flow-continue
              :on-error code message :repairs schema-repair)))
      (cond
        ((not (option-value invocation :stdin))
         (invalid
          "argument.invalid"
          "batch reads its argv arrays from standard input: pass --stdin"))
        ((and atomic continue-on-error)
         (invalid
          "argument.invalid"
          "--atomic and --continue-on-error cannot be combined"))
        (t
         (let ((text (%read-batch-input *standard-input*)))
           (if (null text)
               (invalid
                "argument.invalid"
                (format nil
                        "--stdin is larger than ~D characters"
                        +batch-max-input-chars+))
               (multiple-value-bind (argvs code message)
                   (%parse-batch-argvs text)
                 (if code
                     (invalid code message)
                     (let ((problem
                             (loop for argv in argvs
                                   for index from 0
                                   thereis (%element-problem app argv index atomic))))
                       (cond
                         (problem
                          (invalid "argument.invalid" problem))
                         (atomic
                          (aitools.kernel.domain:flow-call
                              (%batch-atomic app registry argvs globals)
                            (:on-ok (fields)
                              (aitools.kernel.domain:flow-continue
                               :on-ok fields))
                            (:on-error (code message &key candidates diagnostics conflicts repairs)
                              (aitools.kernel.domain:flow-continue
                               :on-error code message
                               :candidates candidates
                               :diagnostics diagnostics
                               :conflicts conflicts
                               :repairs repairs))))
                         (t
                          (aitools.kernel.domain:flow-call
                              (%batch-plain
                               app registry argvs globals continue-on-error)
                            (:on-ok (fields)
                              (aitools.kernel.domain:flow-continue
                               :on-ok fields))
                            (:on-error (code message &key candidates diagnostics conflicts repairs)
                              (aitools.kernel.domain:flow-continue
                               :on-error code message
                               :candidates candidates
                               :diagnostics diagnostics
                               :conflicts conflicts
                               :repairs repairs)))))))))))))))

(defun %batch-handler (invocation)
  (aitools.protocol.application:call-with-command-result/k
   (lambda (&key on-ok on-partial on-error)
     (declare (ignore on-partial))
     (aitools.kernel.domain:flow-call
         (%batch-flow
          (invocation-app invocation) %batch-registry% invocation)
       (:on-ok (fields)
         (funcall on-ok fields))
       (:on-error (code message &key candidates diagnostics conflicts repairs)
         (funcall on-error code message
                  :candidates candidates
                  :diagnostics diagnostics
                  :conflicts conflicts
                  :repairs repairs))))))

(defun register-batch-command (registry)
  (setf %batch-registry% registry)
  (aitools.protocol.application:register-command-declarations
   registry
   (list
    (aitools.protocol.application:define-command
        "batch"
      (:command-builder
       (lambda (ports)
         (declare (ignore ports))
         (make-command
          :name "batch"
          :description
          "Run several aitools invocations from one JSON input, optionally as one tx."
          :options
          (list
           (make-option
            :name "stdin" :kind :flag
            :description
            "Read the JSON array of argv arrays from standard input (required).")
           (make-option
            :name "continue-on-error" :kind :flag
            :description "Run every element even after one fails.")
           (make-option
            :name "atomic" :kind :flag
            :description
            "Run every element in one tx and commit it at the end, or write nothing."))
          :handler #'%batch-handler))
       :schema
       (aitools.protocol.domain:make-command-schema
        "batch"
        "Run several aitools invocations from one JSON input, optionally as one tx."
        :description
        (format nil "~{~A~}"
                '("--stdin is a JSON array of argv arrays without the leading aitools, e.g. "
                  "[[\"read\",\"a.lisp\"],[\"edit\",\"a.lisp\",\"--old\",\"x\",\"--new\",\"y\"]]]. "
                  "Each element is parsed and validated exactly as a standalone call, with "
                  "batch's --root and --lock-timeout placed before it. "
                  "Elements run in order; after the first failure the rest are skipped unless "
                  "--continue-on-error. "
                  "Without --atomic each write commits on its own: undo the successful elements' "
                  "op_id values to reverse a partial batch. "
                  "--atomic runs tx begin, every element with --tx <tx>, then tx commit, as one op; "
                  "a failing element or a conflicting commit aborts the tx and writes nothing. "
                  "An element may not be a batch, may not read --stdin "
                  "(batch has consumed it; pass --content or --stdin-data), and under --atomic "
                  "may not carry --tx. "
                  "On failure the answer is an error with the first failing element's error.code "
                  "and exit code, and results[] in error.diagnostics."))
        :args
        '((:name "--stdin" :type "flag" :required t
           :description "Read the argv arrays from standard input.")
          (:name "--continue-on-error" :type "flag" :description
           "Keep running after a failed element; not with --atomic.")
          (:name "--atomic" :type "flag" :description
           "All elements in one tx, committed at the end, or nothing; not with --continue-on-error."))
        :output-fields
        '((:name "results" :description
           "One envelope per element, in order; {status:\"skipped\",argv} for an element not run after a failure.")
          (:name "tx" :description
           "--atomic: the tx the elements were staged in.")
          (:name "changes" :description
           "--atomic: the committed tx's changes, in the standard write output shape.")
          (:name "op_id" :description
           "--atomic: the single journal op of the whole batch (undo reverses it).")
          (:name "next_commands" :description
           "Commands to show full diffs when a committed change diff is truncated."))
        :error-codes
        (remove-duplicates
         (append
          '("argument.invalid" "input.syntax-error"
            "refusal.target-changed" "environment.busy")
          (loop for schema in (all-command-schemas registry)
                append (aitools.protocol.domain:command-schema-error-codes schema)))
         :test #'string= :from-end t))))) nil) registry)
