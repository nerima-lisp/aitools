;;;; Shared YAML/TOML document preflight and output fields.
(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow
  (%read-format/k format command context path &key on-error on-json)
  "Read and preflight one bounded YAML or TOML document before applying an edit."
  (aitools.kernel.domain:flow-call
    (read-document/k context path :max-bytes +max-format-document-bytes+)
    (:on-error (code message &key candidates diagnostics conflicts repairs
                     ((:path ignored-path)))
      (declare (ignore ignored-path))
      (aitools.kernel.domain:flow-continue
        :on-error code message
        :candidates candidates
        :diagnostics diagnostics
        :conflicts conflicts
        :repairs repairs))
    (:on-document (document)
      (if (string= format "yaml")
          (aitools.kernel.domain:flow-call
            (aitools.inspect.application:parse-yaml-write-document/k
             (document-logical-text document)
             :max-input-length +max-format-document-bytes+
             :max-depth +max-format-document-depth+)
            (:on-value (json-text comment-count)
              (aitools.kernel.domain:flow-continue
                :on-json (parse-json-text json-text) document comment-count))
            (:on-error (code message)
              (aitools.kernel.domain:flow-continue
                :on-error code message
                :repairs (%format-repairs format command path code nil))))
          (aitools.kernel.domain:flow-call
            (aitools.inspect.application:parse-toml-write-document/k
             (document-logical-text document)
             :max-depth +max-format-document-depth+)
            (:on-value (json-text comment-count)
              (aitools.kernel.domain:flow-continue
                :on-json (parse-json-text json-text) document comment-count))
            (:on-error (code message)
              (aitools.kernel.domain:flow-continue
                :on-error code message
                :repairs (%format-repairs format command path code nil))))))
    (:on-filtered ())))

(defun %format-repairs (format command path code pointer)
  (let* ((file (aitools.protocol.domain:shell-quote path))
         (schema-command
          (format nil "aitools schema ~A" (command-display-name command))))
    (cond
      ((string= code "input.unsupported-value")
       (list (repair "convert-to-supported-value"
                     "Convert the value to the supported subset, then retry."
                     schema-command)))
      ((string= code "input.syntax-error")
       (list (repair "fix-format-syntax"
                     "Fix the YAML/TOML syntax or TOML integer range, then retry."
                     schema-command)))
      ((and pointer (string= code "input.not-found"))
       (list (repair "list-keys"
                     "List the keys or elements at the pointer's parent."
                     (format nil "aitools doc get ~A ~A --format ~A"
                             file
                             (aitools.protocol.domain:shell-quote
                              (format-json-pointer (butlast pointer)))
                             format))))
      ((member code '("input.not-found" "selection.no-match") :test #'string=)
       (list (repair (format nil "read-~A" format)
                     (format nil "Read the ~A value to choose a current pointer."
                             (string-upcase format))
                     (format nil "aitools doc get ~A ~A --format ~A"
                             file
                             (aitools.protocol.domain:shell-quote "")
                             format))))
      (t
       (default-repairs code command
                        (append (command-words command) (list path))
                        path)))))

(defun %format-document (format document value comment-count serializer)
  (declare (ignore document comment-count))
  (funcall (if (string= format "yaml")
               (function validate-yaml-write-value)
               (function validate-toml-write-value))
           value)
  (let* ((serialized (funcall serializer format value))
         (text (if (and (plusp (length serialized))
                        (char= (char serialized (1- (length serialized))) #\Newline))
                    serialized
                    (concatenate (quote string) serialized (string #\Newline))))
         (result (make-text-document text :bom-p nil :eol +lf+)))
    (document-with-final-newline (document-with-eol result +lf+) t)))

(defun %format-warning-fields (comment-count)
  (list
   (cons "warnings"
         (if (plusp comment-count)
             (vector
              (aitools.protocol.domain:json-object-from-alist
               (list (cons "code" "format.comments-dropped")
                     (cons "message"
                           "Comments were dropped when the document was rewritten.")
                     (cons "count" comment-count))))
             #()))))

(defun %document-format-repair (verb positionals options)
  (let* ((name (format nil "doc.~A" verb))
         (repair-options (copy-list options)))
    (setf (getf repair-options :format) "<json|yaml|toml>")
    (let* ((argv (options-argv name positionals repair-options))
           (retry-options
            (append
             (when (getf options :tx)
               (list "--tx" (getf options :tx)))
             (when (getf options :dry-run)
               (list "--dry-run"))
             (when (or (getf options :stdin)
                       (getf options :stdin-data))
               (list "--stdin"))))
           (separator (position "--" argv :test (function string=)))
           (command
            (apply (function aitools.protocol.domain:command-line)
                   (append
                    (list "aitools")
                    (if separator
                        (append (subseq argv 0 separator)
                                retry-options
                                (subseq argv separator))
                      (append argv retry-options))))))
      (repair "specify-format"
              "Pass --format with the document format, then retry."
              command))))

(defun %format-plan (format command path transform options
                            &key serializer repair-pointer record-options
                            record-positionals inputs indent-function sort-keys)
  (make-write-plan
   :command command
   :targets (list (make-write-target path))
   :inputs inputs
   :expect-hashes (getf options :expect-hash)
   :replayable (null (getf options :expect-hash))
   :plan (%json-plan transform
                     :repair-pointer repair-pointer
                     :indent-function indent-function
                     :sort-keys sort-keys
                     :reader (unless (string= format "json")
                               (lambda (context target &key on-error on-json)
                                 (%read-format/k format command context target
                                                 :on-error on-error
                                                 :on-json on-json)))
                     :document-function
                     (unless (string= format "json")
                       (lambda (document value comment-count)
                         (%format-document format document value comment-count serializer)))
                     :repairs-function
                     (unless (and (string= format "json")
                                  (not (getf options :format)))
                       (lambda (command path code pointer)
                         (if (string= format "json")
                             (%json-repairs path code pointer (getf options :format))
                             (%format-repairs format command path code pointer))))
                     :extra-function
                     (unless (string= format "json") (function %format-warning-fields)))
   :record-options (or record-options options)
   :record-positionals (or record-positionals
                           (lambda (paths) (list (first paths))))))


(aitools.kernel.domain:define-flow (%format-fmt/k format command ports positionals options &key on-plan on-error)
  (let* ((path (first positionals))
         (indent (getf options :indent))
         (indent-width (and indent (ignore-errors (parse-integer indent))))
         (json-only-options (or indent (getf options :minify)
                                (getf options :sort-keys))))
    (cond
      ((/= (length positionals) 1)
       (aitools.kernel.domain:flow-continue
         :on-error "argument.invalid"
         (format nil "~A takes exactly one PATH"
                 (command-display-name command))))
      ((and (not (string= format "json")) json-only-options)
       (aitools.kernel.domain:flow-continue
         :on-error "argument.invalid"
         "--indent, --minify, and --sort-keys are available only for JSON"))
      ((and indent
            (or (null indent-width) (< indent-width 0) (> indent-width 16)))
       (aitools.kernel.domain:flow-continue
         :on-error "argument.invalid"
         (format nil "--indent ~S must be 0 to 16" indent)))
      ((and indent (getf options :minify))
       (aitools.kernel.domain:flow-continue
         :on-error "argument.invalid"
         "--indent and --minify cannot be combined"))
      (t
       (aitools.kernel.domain:flow-continue
         :on-plan
         (%format-plan
          format command path (function identity) options
          :serializer (and ports (edit-ports-serialize-document ports))
          :indent-function
          (when (string= format "json")
            (cond
              ((getf options :minify)
               (lambda (metadata) (declare (ignore metadata)) nil))
              (indent
               (lambda (metadata)
                 (declare (ignore metadata))
                 (make-string indent-width :initial-element #\Space)))
              (t (lambda (metadata) (or metadata "  ")))))
          :sort-keys (and (string= format "json")
                          (getf options :sort-keys))))))))
