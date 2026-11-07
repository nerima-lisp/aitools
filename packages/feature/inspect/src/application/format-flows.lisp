(in-package #:aitools.inspect.application)

(defun %format-group (format)
  (ecase format
    (:json "json")
    (:yaml "yaml")
    (:toml "toml")))

(aitools.kernel.domain:define-flow
 (doc-inspect-flow ports action paths arguments format options
                   &key operation root tx lock-timeout on-ok on-partial on-error)
 (declare (type function operation on-ok on-partial on-error))
 (labels ((option-arguments (values)
            (loop for (name value) on values by #'cddr
                  for flag = (concatenate 'string
                                          "--"
                                          (string-downcase (symbol-name name)))
                  append (cond
                           ((null value) nil)
                           ((member name '(:keys :raw :desc))
                            (list flag))
                           ((member name '(:where :pick))
                            (loop for item in value
                                  append (list flag item)))
                           (t (list flag (princ-to-string value))))))
          (repair-command ()
            (apply (function aitools.protocol.domain:command-line)
                   (append (list "aitools")
                           (option-arguments
                            (list :root root :lock-timeout lock-timeout))
                           (list "doc" action)
                           arguments
                           (option-arguments (append (list :tx tx) options))
                           (list "--format" "<json|yaml|toml>")))))
   (let* ((formats (mapcar (lambda (path)
                             (%document-format path format))
                           paths))
          (unknown (position nil formats)))
     (if unknown
         (let ((path (nth unknown paths)))
           (aitools.kernel.domain:flow-continue
            :on-error
            "argument.invalid"
            (format nil
                    "Cannot determine the document format for ~A; pass --format json, yaml, or toml."
                    path)
            :repairs
            (list
             (repair "specify-format"
                     "Pass --format with the format that matches the document."
                     (repair-command)))))
         (cond
           ((string= action "get")
            (funcall operation ports (first arguments) (second arguments)
                     :root root :tx tx :lock-timeout lock-timeout
                     :format (first formats)
                     :max-bytes (getf options :max-bytes)
                     :keys (getf options :keys)
                     :raw (getf options :raw)
                     :on-ok on-ok :on-partial on-partial :on-error on-error))
           ((string= action "select")
            (funcall operation ports (first arguments) (second arguments)
                     :root root :tx tx :lock-timeout lock-timeout
                     :format (first formats)
                     :where (getf options :where)
                     :pick (getf options :pick)
                     :sort-by (getf options :sort-by)
                     :desc (getf options :desc)
                     :output (getf options :output)
                     :limit (getf options :limit)
                     :on-ok on-ok :on-partial on-partial :on-error on-error))
           (t
            (funcall operation ports (first arguments) (second arguments)
                     :root root :tx tx :lock-timeout lock-timeout
                     :format-a (first formats) :format-b (second formats)
                     :limit (getf options :limit)
                     :on-ok on-ok :on-partial on-partial :on-error on-error)))))))

(progn
  (defmacro %call-document-reader (call)
    `(aitools.kernel.domain:flow-call
      ,call
      (:on-document (document)
        (aitools.kernel.domain:flow-continue :on-document document))
      (:on-error (code message &key candidates diagnostics conflicts repairs)
        (aitools.kernel.domain:flow-continue
         :on-error code message
         :candidates candidates :diagnostics diagnostics
         :conflicts conflicts :repairs repairs))))

  (aitools.kernel.domain:define-flow
   (%inspect-document context target format action &key on-document on-error)
   (declare (type function on-document on-error))
   (case format
     (:json
      (%call-document-reader
       (%json-document context target)))
     (:yaml
      (%call-document-reader
       (%yaml-document context target action)))
     (:toml
      (%call-document-reader
       (%toml-document context target action))))))

(aitools.kernel.domain:define-flow
 (%call-with-format-file ports path format action
                         &key root tx lock-timeout record
                         on-document on-error)
 (declare (type function on-document on-error))
 (labels ((report-error (code message
                              &key candidates diagnostics conflicts repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message
             :candidates candidates :diagnostics diagnostics
             :conflicts conflicts :repairs repairs)))
   (aitools.kernel.domain:flow-call
    (call-with-inspect-file/k
     ports path :root root :tx tx :lock-timeout lock-timeout :record record)
    (:on-file (context target)
      (aitools.kernel.domain:flow-call
       (%inspect-document context target format action)
       (:on-document (document)
         (aitools.kernel.domain:flow-continue
          :on-document context target document))
       (:on-error (code message
                        &key candidates diagnostics conflicts repairs)
         (report-error code message
                       :candidates candidates :diagnostics diagnostics
                       :conflicts conflicts :repairs repairs))))
    (:on-error (code message
                     &key candidates diagnostics conflicts repairs)
      (report-error code message
                    :candidates candidates :diagnostics diagnostics
                    :conflicts conflicts :repairs repairs)))))
