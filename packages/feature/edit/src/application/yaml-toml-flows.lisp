;;;; Shared YAML/TOML document preflight and output fields.
(in-package #:aitools.edit.application)

(aitools.kernel.domain:define-flow
  (%read-format/k format command context path &key on-error on-json)
  "Read and preflight one YAML or TOML document before applying an edit."
  (aitools.kernel.domain:flow-call
    (read-document/k context path)
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
      (handler-case
          (multiple-value-bind (value comment-count)
              (funcall (if (string= format "yaml")
                           #'parse-yaml-write-document
                           #'parse-toml-write-document)
                       (document-logical-text document))
            (aitools.kernel.domain:flow-continue
              :on-json value document comment-count))
        (edit-refusal (condition)
          (let ((code (edit-refusal-code condition)))
            (aitools.kernel.domain:flow-continue
              :on-error code
              (edit-refusal-detail condition)
              :repairs (%format-repairs command path code nil))))))
    (:on-filtered ())))

(defun %format-repairs (command path code pointer)
  (let* ((format (first (command-words command)))
         (file (aitools.protocol.domain:shell-quote path))
         (schema-command (format nil "aitools schema ~A"
                                 (command-display-name command))))
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
                     (format nil "aitools ~A get ~A ~A"
                             format file
                             (aitools.protocol.domain:shell-quote
                              (format-json-pointer (butlast pointer)))))))
      ((member code '("input.not-found" "selection.no-match") :test #'string=)
       (list (repair (format nil "read-~A" format)
                     (format nil "Read the ~A value to choose a current pointer."
                             (string-upcase format))
                     (format nil "aitools ~A get ~A" format file))))
      (t
       (default-repairs code command
                        (append (command-words command) (list path))
                        path)))))

(defun %format-document (format document value comment-count)
  (declare (ignore document comment-count))
  (let* ((serialized
           (funcall (if (string= format "yaml")
                        #'serialize-yaml-document
                        #'serialize-toml-document)
                    value))
         (text (if (and (plusp (length serialized))
                        (char= (char serialized (1- (length serialized))) #\Newline))
                    serialized
                    (concatenate 'string serialized (string #\Newline))))
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

(defun %format-plan (format command path transform options
                            &key repair-pointer record-options record-positionals inputs)
  (make-write-plan
   :command command
   :targets (list (make-write-target path))
   :inputs inputs
   :expect-hashes (getf options :expect-hash)
   :replayable (null (getf options :expect-hash))
   :plan (%json-plan transform
                     :repair-pointer repair-pointer
                     :reader (lambda (context target &key on-error on-json)
                               (%read-format/k format command context target
                                               :on-error on-error
                                               :on-json on-json))
                     :document-function (lambda (document value comment-count)
                                          (%format-document format document value
                                                            comment-count))
                     :repairs-function (function %format-repairs)
                     :extra-function (function %format-warning-fields))
   :record-options (or record-options options)
   :record-positionals (or record-positionals
                           (lambda (paths) (list (first paths))))))
