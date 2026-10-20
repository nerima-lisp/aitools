;;;; Shared normalization of the options accepted by workspace scans.
(in-package #:aitools.workspace.application)

(aitools.kernel.domain:define-flow
    (call-with-scan-options/k host
      &key path-absolute current-time language-predicate language-names
        glob lang no-ignore skip-larger-than newer overlay on-options on-error)
  "Parse common scan options and call ON-OPTIONS with workspace scan keywords.
ON-ERROR receives an error kind and offending value; unknown-language also
receives the known language names. PATH-ABSOLUTE and CURRENT-TIME are ports
so the same normalization works for each feature context."
  (let ((error-kind nil)
        (error-value nil)
        (error-names nil))
    (let* ((predicate (and lang (funcall language-predicate lang)))
           (limit +default-skip-larger-than+)
           (threshold nil))
      (flet ((invalid (kind value &optional names)
               (setf error-kind kind
                     error-value value
                     error-names names)))
        (when skip-larger-than
          (handler-case
              (setf limit
                    (aitools.kernel.domain:size-bytes
                     (aitools.kernel.domain:parse-size skip-larger-than)))
            (aitools.kernel.domain:invalid-size-error ()
              (invalid :invalid-size skip-larger-than))))
        (when (and (null error-kind) newer)
          (let* ((absolute (funcall path-absolute newer))
                 (entry (and absolute (host-stat host absolute))))
            (if entry
                (setf threshold (workspace-entry-mtime entry))
                (let ((milliseconds
                        (handler-case
                            (aitools.kernel.domain:duration-milliseconds
                             (aitools.kernel.domain:parse-duration newer))
                          (aitools.kernel.domain:invalid-duration-error ()
                            (invalid :invalid-newer newer)))))
                  (when (null error-kind)
                    (setf threshold
                          (- (funcall current-time)
                             (floor milliseconds 1000))))))))
        (when (and (null error-kind) lang (not predicate))
          (invalid :unknown-language lang language-names))
        (if error-kind
            (aitools.kernel.domain:flow-continue
             :on-error error-kind error-value error-names)
            (aitools.kernel.domain:flow-continue
             :on-options
             (list :glob glob :lang predicate :no-ignore no-ignore
                   :skip-larger-than limit :newer threshold :overlay overlay)))))))
