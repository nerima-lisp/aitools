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
 (json-diff-flow boundaries
                 a
                 b
                 &key
                 root
                 tx
                 lock-timeout
                 (format "json")
                 format-a
                 format-b
                 (limit 100)
                 format-override
                 on-ok
                 on-partial
                 on-error)
 "doc diff. Calls ON-OK or ON-PARTIAL (fields), or ON-ERROR."
 (declare (type function on-ok on-partial on-error))
 (%with-json-flow-error-forwarder
  (aitools.kernel.domain:flow-call
    (%call-with-format-file
     boundaries a (or format-a format) "diff"
     :root root
     :tx tx
     :lock-timeout lock-timeout)
    (:on-document (context target-a document-a)
      (aitools.kernel.domain:flow-call
       (%call-with-format-file
        boundaries b (or format-b format) "diff"
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

