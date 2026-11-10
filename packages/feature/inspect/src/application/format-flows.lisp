(in-package #:aitools.inspect.application)

(defmacro %call-document-reader (call)
  `(aitools.kernel.domain:flow-call
    ,call
    (:on-document (document)
                  (aitools.kernel.domain:flow-continue :on-document document))
    (:on-error (code message &key candidates diagnostics conflicts repairs)
               (aitools.kernel.domain:flow-continue
                :on-error code message
                :candidates candidates :diagnostics diagnostics
                :conflicts conflicts :repairs repairs))))

(aitools.kernel.domain:define-flow
 (%inspect-document context target format action &key on-document on-error)
 (declare (type function on-document on-error))
 (cond
   ((string= format "json")
    (%call-document-reader
     (%json-document context target)))
   ((string= format "yaml")
    (%call-document-reader
     (%yaml-document context target action)))
   ((string= format "toml")
    (%call-document-reader
     (%toml-document context target action)))))

(aitools.kernel.domain:define-flow
 (%call-with-format-file boundaries path format action
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
     boundaries path :root root :tx tx :lock-timeout lock-timeout :record record)
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
