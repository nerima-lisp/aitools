(in-package #:aitools.inspect.application)

(defun %format-group (format)
  (ecase format
    (:json "json")
    (:yaml "yaml")
    (:toml "toml")))

(aitools.kernel.domain:define-flow
 (%inspect-document context target format action &key on-document on-error)
 (declare (type function on-document on-error))
 (case format
   (:json
    (aitools.kernel.domain:flow-call
     (%json-document context target)
     (:on-document (document)
       (aitools.kernel.domain:flow-continue :on-document document))
     (:on-error (code message
                      &key candidates diagnostics conflicts repairs)
       (aitools.kernel.domain:flow-continue
        :on-error code message
        :candidates candidates :diagnostics diagnostics
        :conflicts conflicts :repairs repairs))))
   (:yaml
    (aitools.kernel.domain:flow-call
     (%yaml-document context target action)
     (:on-document (document)
       (aitools.kernel.domain:flow-continue :on-document document))
     (:on-error (code message
                      &key candidates diagnostics conflicts repairs)
       (aitools.kernel.domain:flow-continue
        :on-error code message
        :candidates candidates :diagnostics diagnostics
        :conflicts conflicts :repairs repairs))))
   (:toml
    (aitools.kernel.domain:flow-call
     (%toml-document context target action)
     (:on-document (document)
       (aitools.kernel.domain:flow-continue :on-document document))
     (:on-error (code message
                      &key candidates diagnostics conflicts repairs)
       (aitools.kernel.domain:flow-continue
        :on-error code message
        :candidates candidates :diagnostics diagnostics
        :conflicts conflicts :repairs repairs))))))

(aitools.kernel.domain:define-flow
 (%call-with-format-file ports path format action
                         &key root tx lock-timeout record
                         on-document on-error)
 (declare (type function on-document on-error))
 (labels ((report-error (code message
                              &key candidates diagnostics conflicts repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message
             :candidates candidates :diagnostics diagnostics
             :conflicts conflicts :repairs repairs)))
   (aitools.kernel.domain:flow-call
    (call-with-inspect-file/k
     ports path :root root :tx tx :lock-timeout lock-timeout :record record)
    (:on-file (context target)
      (aitools.kernel.domain:flow-call
       (%inspect-document context target format action)
       (:on-document (document)
         (aitools.kernel.domain:flow-continue
          :on-document context target document))
       (:on-error (code message
                        &key candidates diagnostics conflicts repairs)
         (report-error code message
                       :candidates candidates :diagnostics diagnostics
                       :conflicts conflicts :repairs repairs))))
    (:on-error (code message
                     &key candidates diagnostics conflicts repairs)
      (report-error code message
                    :candidates candidates :diagnostics diagnostics
                    :conflicts conflicts :repairs repairs)))))
