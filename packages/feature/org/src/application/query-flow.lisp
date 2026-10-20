(in-package #:aitools.org.application)

(defun %heading-path-json (path)
  (mapcar (lambda (segment)
            (json-object "title" (aitools.org.domain:org-heading-path-entry-title segment)
                         "index" (aitools.org.domain:org-heading-path-entry-index segment)))
          path))

(defun %query-match-json (match)
  (json-object "line" (aitools.org.domain:org-query-match-line match)
               "level" (aitools.org.domain:org-query-match-level match)
               "title" (aitools.org.domain:org-query-match-title match)
               "todo" (or (aitools.org.domain:org-query-match-todo match) json-null)
               "tags" (aitools.org.domain:org-query-match-tags match)
               "heading_path"
               (%heading-path-json (aitools.org.domain:org-query-match-heading-path match))))

(aitools.kernel.domain:define-flow
    (org-query/k ports &key path match at tz dst-policy root on-ok on-partial on-error)
  (declare (ignore on-partial) (type function on-ok on-error))
  (if (not (and (stringp match) (plusp (length match))
                (stringp at) (plusp (length at))
                (stringp tz) (plusp (length tz))))
      (aitools.kernel.domain:flow-continue
       :on-error "argument.invalid" "--match, --at, and --tz are required"
       :repairs (%schema-repair "org query" "Pass all required query options."))
      (aitools.kernel.domain:flow-call
          (org-read-document/k ports path root "org query")
        (:on-document (document)
          (let ((query (org-ports-query-org ports)))
            (if (null query)
                (aitools.kernel.domain:flow-continue
                 :on-error "internal.unexpected" "the org context has no query port"
                 :repairs (%schema-repair "org query" "Show this command's requirements."))
                (multiple-value-bind (matches failure)
                    (funcall query document match at tz dst-policy)
                  (if failure
                      (multiple-value-bind (code message diagnostics repairs)
                          (%failure-response failure "org query")
                        (aitools.kernel.domain:flow-continue
                         :on-error code message :diagnostics diagnostics :repairs repairs))
                      (let ((items (mapcar #'%query-match-json matches)))
                        (aitools.kernel.domain:flow-continue
                         :on-ok (list (cons "matches" items)
                                      (cons "count" (length items))))))))))
        (:on-error (code message &key repairs diagnostics)
          (aitools.kernel.domain:flow-continue :on-error code message
                                               :repairs repairs :diagnostics diagnostics))))
)
