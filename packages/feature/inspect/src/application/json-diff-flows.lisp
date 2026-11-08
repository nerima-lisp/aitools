(in-package #:aitools.inspect.application)

(defun %diff-op-json (op)
  (destructuring-bind (kind tokens old new) op
    (json-object-from-pairs
     (append
      (list (cons "op" (string-downcase (symbol-name kind)))
            (cons "pointer" (format-json-pointer tokens)))
      (unless (eq kind :add)
        (list (cons "old" old)))
      (unless (eq kind :remove)
        (list (cons "new" new)))))))

(aitools.kernel.domain:define-flow
 (json-diff-flow ports
                 a
                 b
                 &key
                 root
                 tx
                 lock-timeout
                 (format :json)
                 format-a
                 format-b
                 (limit 100)
                 format-override
                 on-ok
                 on-partial
                 on-error)
 "doc diff. Calls ON-OK or ON-PARTIAL (fields), or ON-ERROR."
 (declare (type function on-ok on-partial on-error))
 (labels ((report-error (code message
                              &key
                              candidates
                              diagnostics
                              conflicts
                              repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message
             :candidates candidates
             :diagnostics diagnostics
             :conflicts conflicts
             :repairs repairs)))
   (aitools.kernel.domain:flow-call
    (%call-with-format-file
     ports a (or format-a format) "diff"
     :root root
     :tx tx
     :lock-timeout lock-timeout)
    (:on-document (context target-a document-a)
      (aitools.kernel.domain:flow-call
       (%call-with-format-file
        ports b (or format-b format) "diff"
        :root root
        :tx tx
        :lock-timeout lock-timeout)
       (:on-document (context-b target-b document-b)
         (declare (ignore context-b))
         (let* ((ops (json-diff-ops document-a document-b))
                (shown (subseq ops 0 (min limit (length ops))))
                (truncated (< (length shown) (length ops)))
                (fields
                 (append
                  (list (cons "a" a)
                        (cons "b" b)
                        (cons "identical" (json-bool (null ops)))
                        (cons "ops" (mapcar (function %diff-op-json) shown))
                        (cons "total" (length ops))
                        (cons "truncated" (json-bool truncated)))
                  (when truncated
                    (list
                     (cons "next_commands"
                           (list
                            (command-line
                             context
                             (append
                              (list "doc" "diff"
                                    (file-target-argument target-a)
                                    (file-target-argument target-b)
                                    "--limit"
                                    (princ-to-string (length ops)))
                              (when format-override
                                (list "--format" format-override)))))))))))
           (if truncated
               (aitools.kernel.domain:flow-continue
                :on-partial fields)
               (aitools.kernel.domain:flow-continue
                :on-ok fields))))
       (:on-error (code message
                        &key candidates diagnostics conflicts repairs)
         (report-error code message
                       :candidates candidates
                       :diagnostics diagnostics
                       :conflicts conflicts
                       :repairs repairs))))
    (:on-error (code message
                     &key candidates diagnostics conflicts repairs)
      (report-error code message
                    :candidates candidates
                    :diagnostics diagnostics
                    :conflicts conflicts
                    :repairs repairs)))))

(defmacro %call-doc-inspect-flow (call)
  `(aitools.kernel.domain:flow-call
    ,call
    (:on-ok (fields)
            (aitools.kernel.domain:flow-continue :on-ok fields))
    (:on-partial (fields)
                 (aitools.kernel.domain:flow-continue :on-partial fields))
    (:on-error (code message &key candidates diagnostics conflicts repairs)
               (aitools.kernel.domain:flow-continue
                :on-error code message
                :candidates candidates :diagnostics diagnostics
                :conflicts conflicts :repairs repairs))))

(aitools.kernel.domain:define-flow
 (doc-inspect-flow ports action paths arguments format options
                   &key root tx lock-timeout on-ok on-partial on-error)
 (declare (type function on-ok on-partial on-error))
 (labels ((option-arguments (values)
                            (loop for (name value) on values by (function cddr)
                                  for flag = (concatenate (quote string) "--"
                                                          (string-downcase (symbol-name name)))
                                  append (cond
                                          ((null value) nil)
                                          ((or (eq name :keys) (eq name :raw) (eq name :desc))
                                           (list flag))
                                          ((or (eq name :where) (eq name :pick))
                                           (loop for item in value append (list flag item)))
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
            :on-error "argument.invalid"
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
         (%call-doc-inspect-flow
          (json-get-flow ports (first arguments) (second arguments)
                         :root root :tx tx :lock-timeout lock-timeout
                         :format (first formats) :format-override format
                         :max-bytes (getf options :max-bytes)
                         :keys (getf options :keys)
                         :raw (getf options :raw))))
        ((string= action "select")
         (%call-doc-inspect-flow
          (json-select-flow ports (first arguments) (second arguments)
                            :root root :tx tx :lock-timeout lock-timeout
                            :format (first formats) :format-override format
                            :where (getf options :where)
                            :pick (getf options :pick)
                            :sort-by (getf options :sort-by)
                            :desc (getf options :desc)
                            :output (getf options :output)
                            :limit (getf options :limit))))
        (t
         (%call-doc-inspect-flow
          (json-diff-flow ports (first arguments) (second arguments)
                          :root root :tx tx :lock-timeout lock-timeout
                          :format-a (first formats) :format-b (second formats)
                          :format-override format
                          :limit (getf options :limit)))))))))
