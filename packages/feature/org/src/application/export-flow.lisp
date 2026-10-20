(in-package #:aitools.org.application)

(aitools.kernel.domain:define-flow
    (org-export/k ports &key path backend root on-ok on-partial on-error)
  (declare (ignore on-partial) (type function on-ok on-error))
  (if (not (member backend '(:html :gfm :ascii)))
      (aitools.kernel.domain:flow-continue
       :on-error "argument.invalid" "--backend must be html, gfm, or ascii"
       :repairs (%schema-repair "org export" "Pass a supported --backend value."))
      (aitools.kernel.domain:flow-call
          (org-read-document/k ports path root "org export")
        (:on-document (document)
          (let ((exporter (org-ports-export-org ports)))
            (if (null exporter)
                (aitools.kernel.domain:flow-continue
                 :on-error "internal.unexpected" "the org context has no export port"
                 :repairs (%schema-repair "org export" "Show this command's requirements."))
                (multiple-value-bind (content failure) (funcall exporter document backend)
                  (if failure
                      (multiple-value-bind (code message diagnostics repairs)
                          (%failure-response failure "org export")
                        (aitools.kernel.domain:flow-continue
                         :on-error code message :diagnostics diagnostics :repairs repairs))
                      (let ((name (cdr (assoc backend '((:html . "html")
                                                       (:gfm . "gfm")
                                                       (:ascii . "ascii"))))))
                        (aitools.kernel.domain:flow-continue
                         :on-ok (list (cons "backend" name)
                                      (cons "content" content)))))))))
        (:on-error (code message &key repairs diagnostics)
          (aitools.kernel.domain:flow-continue :on-error code message
                                               :repairs repairs :diagnostics diagnostics)))
))
