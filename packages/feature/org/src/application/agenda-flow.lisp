(in-package #:aitools.org.application)

(defun %org-agenda-entry-json (entry path)
  (json-object "date" (getf entry :date)
               "kind" (getf entry :kind)
               "line" (getf entry :line)
               "title" (getf entry :title)
               "todo" (or (getf entry :todo) (json-null))
               "tags" (getf entry :tags)
               "path" path))

(aitools.kernel.domain:define-flow
    (org-agenda/k boundaries &key path paths from through future-repeats at tz dst-policy root
                                on-ok on-partial on-error)
  (declare (ignore on-partial) (type function on-ok on-error))
  (let ((agenda (cl-boundary-kit:boundary-context-require boundaries :org-agenda)))
    (if agenda
        (aitools.kernel.domain:flow-call
            (%org-map-documents/k
             boundaries (or paths (and path (list path))) root "org agenda"
             (lambda (document) (funcall agenda document from through at tz dst-policy future-repeats)))
          (:on-error (code message &key repairs diagnostics)
            (aitools.kernel.domain:flow-continue
             :on-error code message :repairs repairs :diagnostics diagnostics))
          (:on-results (results)
            (let ((entries (loop for (source . result) in results
                                 append (mapcar (lambda (entry) (%org-agenda-entry-json entry source))
                                                (getf result :entries)))))
              (aitools.kernel.domain:flow-continue
               :on-ok (list (cons "from" from)
                            (cons "through" through)
                            (cons "entries" entries)
                            (cons "count" (length entries)))))))
        (aitools.kernel.domain:flow-continue
         :on-error "internal.unexpected" "the org context has no agenda boundary capability"
         :repairs (%schema-repair "org agenda" "Show this command's requirements.")))))
