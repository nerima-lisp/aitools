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
                 (limit 100)
                 on-ok
                 on-partial
                 on-error)
 "json diff. Calls ON-OK or ON-PARTIAL (fields), or ON-ERROR."
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
     ports a format "diff"
     :root root
     :tx tx
     :lock-timeout lock-timeout)
    (:on-document (context target-a document-a)
      (declare (ignore context target-a))
      (aitools.kernel.domain:flow-call
       (%call-with-format-file
        ports b format "diff"
        :root root
        :tx tx
        :lock-timeout lock-timeout)
       (:on-document (context target-b document-b)
         (declare (ignore context target-b))
         (let* ((ops (json-diff-ops document-a document-b))
                (shown (subseq ops 0 (min limit (length ops))))
                (truncated (< (length shown) (length ops)))
                (fields
                 (list (cons "a" a)
                       (cons "b" b)
                       (cons "identical" (json-bool (null ops)))
                       (cons "ops" (mapcar #'%diff-op-json shown))
                       (cons "total" (length ops))
                       (cons "truncated" (json-bool truncated)))))
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
