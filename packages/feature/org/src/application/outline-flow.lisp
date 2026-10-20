(in-package #:aitools.org.application)

(defun %outline-entry-json (entry)
  (json-object "kind" (string-downcase
                       (symbol-name (aitools.org.domain:org-outline-entry-kind entry)))
               "name" (aitools.org.domain:org-outline-entry-name entry)
               "level" (or (aitools.org.domain:org-outline-entry-level entry) (json-null))
               "line" (aitools.org.domain:org-outline-entry-line entry)))

(aitools.kernel.domain:define-flow
    (org-outline/k ports &key path limit root on-ok on-partial on-error)
  (declare (ignore on-partial) (type function on-ok on-error))
  (if (and limit (not (typep limit '(integer 0 *))))
      (aitools.kernel.domain:flow-continue
       :on-error "argument.invalid" "outline limit must be a non-negative integer"
       :repairs (%schema-repair "org outline" "Pass --limit with zero or a positive integer."))
      (aitools.kernel.domain:flow-call
          (org-read-document/k ports path root "org outline")
        (:on-document (document)
          (let ((outline (org-ports-outline-org ports)))
            (if (null outline)
                (aitools.kernel.domain:flow-continue
                 :on-error "internal.unexpected" "the org context has no outline port"
                 :repairs (%schema-repair "org outline" "Show this command's requirements."))
                (multiple-value-bind (result failure) (funcall outline document limit)
                  (if failure
                      (multiple-value-bind (code message diagnostics repairs)
                          (%failure-response failure "org outline")
                        (aitools.kernel.domain:flow-continue
                         :on-error code message :diagnostics diagnostics :repairs repairs))
                      (let ((entries (getf result :entries)))
                        (aitools.kernel.domain:flow-continue
                         :on-ok (list (cons "entries" (mapcar #'%outline-entry-json entries))
                                      (cons "total" (getf result :total))
                                      (cons "truncated"
                                            (aitools.protocol.domain:json-boolean
                                             (getf result :truncated)))))))))))
        (:on-error (code message &key repairs diagnostics)
          (aitools.kernel.domain:flow-continue :on-error code message
                                               :repairs repairs :diagnostics diagnostics))))
)
