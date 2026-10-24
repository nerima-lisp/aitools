(in-package #:aitools.org.application)

(defun %org-agenda-entry-json (entry)
  (json-object "date" (getf entry :date) "kind" (getf entry :kind) "line" (getf entry :line) "title" (getf entry :title) "todo" (or (getf entry :todo) (json-null)) "tags" (getf entry :tags)))

(aitools.kernel.domain:define-flow (org-agenda/k ports &key path from through future-repeats at tz dst-policy root on-ok on-partial on-error)
                                   (declare (ignore on-partial)
                                            (type function on-ok on-error))
                                   (aitools.kernel.domain:flow-call (org-read-document/k ports path root "org agenda")
                                                                    (:on-document (document)
                                                                                  (let ((agenda (org-ports-agenda-org ports)))
                                                                                    (if (null agenda)
                                                                                        (aitools.kernel.domain:flow-continue :on-error
                                                                                                                             "internal.unexpected"
                                                                                                                             "the org context has no agenda port"
                                                                                                                             :repairs
                                                                                                                             (%schema-repair "org agenda" "Show this command's requirements."))
                                                                                        (multiple-value-bind (result failure) (funcall agenda document from through at tz dst-policy future-repeats)
                                                                                          (if failure
                                                                                              (multiple-value-bind (code message diagnostics repairs) (%failure-response failure "org agenda")
                                                                                                (aitools.kernel.domain:flow-continue :on-error code message :diagnostics diagnostics :repairs repairs))
                                                                                              (aitools.kernel.domain:flow-continue :on-ok
                                                                                                                                   (list (cons "from" (getf result :from))
                                                                                                                                         (cons "through" (getf result :through))
                                                                                                                                         (cons "entries"
                                                                                                                                               (mapcar #'%org-agenda-entry-json (getf result :entries)))
                                                                                                                                         (cons "count" (getf result :count)))))))))
                                                                    (:on-error (code message &key repairs diagnostics)
                                                                               (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs :diagnostics diagnostics))))
