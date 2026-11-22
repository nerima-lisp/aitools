(in-package #:aitools.org.application)

(defun %heading-path-json (path)
  (mapcar (lambda (segment)
            (json-object "title" (aitools.org.domain:org-heading-path-entry-title segment)
                         "index" (aitools.org.domain:org-heading-path-entry-index segment)))
          path))

(defun %query-match-json (match path)
  (json-object "line" (aitools.org.domain:org-query-match-line match)
               "level" (aitools.org.domain:org-query-match-level match)
               "title" (aitools.org.domain:org-query-match-title match)
               "todo" (or (aitools.org.domain:org-query-match-todo match) (json-null))
               "tags" (aitools.org.domain:org-query-match-tags match)
               "heading_path" (%heading-path-json (aitools.org.domain:org-query-match-heading-path match))
               "path" path))

(aitools.kernel.domain:define-flow
    (org-query/k boundaries &key path paths match at tz dst-policy root on-ok on-partial on-error)
  (declare (ignore on-partial) (type function on-ok on-error))
  (if (and (stringp match) (plusp (length match))
           (stringp at) (plusp (length at))
           (stringp tz) (plusp (length tz)))
      (let ((query (cl-boundary-kit:boundary-context-require boundaries :org-query)))
        (if query
            (aitools.kernel.domain:flow-call
                (%org-map-documents/k
                 boundaries (or paths (and path (list path))) root "org query"
                 (lambda (document) (funcall query document match at tz dst-policy)))
              (:on-error (code message &key repairs diagnostics)
                (aitools.kernel.domain:flow-continue
                 :on-error code message :repairs repairs :diagnostics diagnostics))
              (:on-results (results)
                (let ((matches (loop for (source . result) in results
                                     append (mapcar (lambda (item) (%query-match-json item source))
                                                    result))))
                  (aitools.kernel.domain:flow-continue
                   :on-ok (list (cons "matches" matches)
                                (cons "count" (length matches)))))))
            (aitools.kernel.domain:flow-continue
             :on-error "internal.unexpected" "the org context has no query boundary capability"
             :repairs (%schema-repair "org query" "Show this command's requirements."))))
      (aitools.kernel.domain:flow-continue
       :on-error "argument.invalid" "--match, --at, and --tz are required"
       :repairs (%schema-repair "org query" "Pass all required query options."))))
