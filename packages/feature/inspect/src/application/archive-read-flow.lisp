(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (%archive-read-target context
                       target
                       path
                       entry
                       selector
                       mode
                       max-lines
                       &key on-ok on-partial on-error)
 (declare (type function on-ok on-partial on-error))
 (aitools.kernel.domain:flow-call
  (call-with-archive/k context target :max-gzip-output +archive-max-entry+)
  (:on-error (code message &key candidates diagnostics conflicts repairs)
    (aitools.kernel.domain:flow-continue
     :on-error code message
     :candidates candidates
     :diagnostics diagnostics
     :conflicts conflicts
     :repairs repairs))
  (:on-archive (archive)
    (aitools.kernel.domain:flow-call
     (%choose-item/k context path entry archive)
     (:on-error (code message &key candidates diagnostics conflicts repairs)
       (aitools.kernel.domain:flow-continue
        :on-error code message
        :candidates candidates
        :diagnostics diagnostics
        :conflicts conflicts
        :repairs repairs))
     (:on-item (item)
       (aitools.kernel.domain:flow-call
        (%archive-read-item context
                            path
                            entry
                            archive
                            item
                            selector
                            mode
                            max-lines)
        (:on-ok (fields)
          (aitools.kernel.domain:flow-continue :on-ok fields))
        (:on-partial (fields)
          (aitools.kernel.domain:flow-continue :on-partial fields))
        (:on-error (code message &key candidates diagnostics conflicts repairs)
          (aitools.kernel.domain:flow-continue
           :on-error code message
           :candidates candidates
           :diagnostics diagnostics
           :conflicts conflicts
           :repairs repairs))))))))

(aitools.kernel.domain:define-flow
 (archive-read-flow ports
                    path
                    entry
                    &key
                    root
                    tx
                    lock-timeout
                    range
                    symbol
                    kind
                    between
                    exclusive
                    match
                    invert
                    (as "text")
                    (max-lines 80)
                    on-ok
                    on-partial
                    on-error)
 "Archive read command. Calls ON-OK, ON-PARTIAL (fields), or ON-ERROR."
 (declare (type function on-ok on-partial on-error))
 (labels ((continue-with (selector)
            (aitools.kernel.domain:flow-call
             (call-with-inspect-file/k ports path
                                       :root root
                                       :tx tx
                                       :lock-timeout lock-timeout
                                       :record t)
             (:on-file (context target)
               (aitools.kernel.domain:flow-call
                (%archive-read-target context
                                      target
                                      path
                                      entry
                                      selector
                                      (if (string= as "hex") :hex :text)
                                      max-lines)
                (:on-ok (fields)
                  (aitools.kernel.domain:flow-continue :on-ok fields))
                (:on-partial (fields)
                  (aitools.kernel.domain:flow-continue :on-partial fields))
                (:on-error
                 (code message &key candidates diagnostics conflicts repairs)
                  (aitools.kernel.domain:flow-continue
                   :on-error code message
                   :candidates candidates
                   :diagnostics diagnostics
                   :conflicts conflicts
                   :repairs repairs))))
             (:on-error
              (code message &key candidates diagnostics conflicts repairs)
               (aitools.kernel.domain:flow-continue
                :on-error code message
                :candidates candidates
                :diagnostics diagnostics
                :conflicts conflicts
                :repairs repairs)))))
   (aitools.kernel.domain:flow-call
    (parse-selector-options/k :archive-read
                              :range
                              range
                              :symbol
                              symbol
                              :kind
                              kind
                              :between
                              between
                              :exclusive
                              exclusive
                              :match
                              match
                              :invert
                              invert
                              :extra-exclusive
                              (list (cons "--as hex" (string= as "hex"))))
    (:on-invalid (message repairs)
      (aitools.kernel.domain:flow-continue
       :on-error "argument.invalid" message :repairs repairs))
    (:on-none ()
      (continue-with nil))
    (:on-selector (selector)
      (continue-with selector)))))
