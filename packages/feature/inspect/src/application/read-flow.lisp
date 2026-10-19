;;;; packages/feature/inspect/src/application/read-flow.lisp
;;;;
;;;; `read`. Three modes (`--as text|hex|strings`); text mode takes
;;;; one selector or `--tail`, always capped by `--max-lines`, and names
;;;; the next `--range` whenever it stopped before the end of the file. Usually
;;;; only shown lines are decoded; content selectors, explicit `--encoding`,
;;;; and `--escape-invisible` require decoding the whole file. The line-and-view
;;;; logic is shared with `archive read`, whose output has the same shape.
(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (%read-target context
               target
               selector
               mode
               &key
               tail
               bytes
               min-length
               escape-invisible
               charset
               max-lines
               on-ok
               on-partial
               on-error)
 (declare (type function on-ok on-partial on-error))
 (if (eq mode :text)
     (aitools.kernel.domain:flow-call
      (%read-text-mode context
                       target
                       (%text-options target
                                      selector
                                      tail
                                      max-lines
                                      escape-invisible
                                      charset)
                       charset)
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
         :repairs repairs)))
     (if (eq mode :hex)
         (aitools.kernel.domain:flow-call
          (%hex-view context target bytes max-lines)
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
             :repairs repairs)))
         (aitools.kernel.domain:flow-call
          (%strings-view context target min-length max-lines)
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
             :repairs repairs))))))

(aitools.kernel.domain:define-flow
 (%read-with-selector ports
                      path
                      selector
                      mode
                      &key
                      root
                      tx
                      lock-timeout
                      tail
                      bytes
                      min-length
                      escape-invisible
                      charset
                      max-lines
                      on-ok
                      on-partial
                      on-error)
 (declare (type function on-ok on-partial on-error))
 (aitools.kernel.domain:flow-call
  (call-with-inspect-file/k ports path
                            :root root
                            :tx tx
                            :lock-timeout lock-timeout
                            :record t)
  (:on-file (context target)
    (aitools.kernel.domain:flow-call
     (%read-target context
                   target
                   selector
                   mode
                   :tail tail
                   :bytes bytes
                   :min-length min-length
                   :escape-invisible escape-invisible
                   :charset charset
                   :max-lines max-lines)
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
        :repairs repairs))))
  (:on-error (code message &key candidates diagnostics conflicts repairs)
    (aitools.kernel.domain:flow-continue
     :on-error code message
     :candidates candidates
     :diagnostics diagnostics
     :conflicts conflicts
     :repairs repairs))))
