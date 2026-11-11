(in-package #:aitools.org.application)

(defun %org-clock-row-json (row)
  (json-object "level" (getf row :level) "title" (getf row :title) "tags" (getf row :tags) "minutes" (getf row :minutes)))

(aitools.kernel.domain:define-flow (org-clock-report/k boundaries &key path match from through at tz dst-policy root on-ok on-partial on-error)
                                   (declare (ignore on-partial)
                                            (type function on-ok on-error))
                                   (if (or (null (cl-boundary-kit:boundary-context-require boundaries :org-clock-report)) (not (and (stringp match) (plusp (length match)))))
                                       (aitools.kernel.domain:flow-continue :on-error
                                                                            (if (null (cl-boundary-kit:boundary-context-require boundaries :org-clock-report))
                                                                                "internal.unexpected"
                                                                                "argument.invalid")
                                                                            (if (null (cl-boundary-kit:boundary-context-require boundaries :org-clock-report))
                                                                                "the org context has no clock report port"
                                                                                "--match is required")
                                                                            :repairs
                                                                            (%schema-repair "org clock report" "Show this command's requirements."))
                                       (aitools.kernel.domain:flow-call (org-read-document/k boundaries path root "org clock report")
                                                                        (:on-document (document)
                                                                                      (multiple-value-bind (result failure)
                                                                                          (funcall (cl-boundary-kit:boundary-context-require boundaries :org-clock-report) document match from through at tz dst-policy)
                                                                                        (if failure
                                                                                            (multiple-value-bind (code message diagnostics repairs) (%failure-response failure "org clock report")
                                                                                              (aitools.kernel.domain:flow-continue :on-error code message :diagnostics diagnostics :repairs repairs))
                                                                                            (aitools.kernel.domain:flow-continue :on-ok
                                                                                                                                 (list (cons "from" (getf result :from))
                                                                                                                                       (cons "through" (getf result :through))
                                                                                                                                       (cons "total_minutes" (getf result :total-minutes))
                                                                                                                                       (cons "rows" (mapcar #'%org-clock-row-json (getf result :rows))))))))
                                                                        (:on-error (code message &key repairs diagnostics)
                                                                                   (aitools.kernel.domain:flow-continue :on-error code message :repairs repairs :diagnostics diagnostics)))))
