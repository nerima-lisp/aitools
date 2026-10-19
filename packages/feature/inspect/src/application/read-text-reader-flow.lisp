(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (%read-text-mode context target options charset &key on-ok on-partial on-error)
 (declare (type function on-ok on-partial on-error))
 (labels ((render (octets)
            (aitools.kernel.domain:flow-call
             (text-view/k context octets options)
             (:on-ok (fields)
               (aitools.kernel.domain:flow-continue
                :on-ok
                (list* (cons "mode" "text")
                       (cons "path" (target-display-path target))
                       fields)))
             (:on-partial (fields)
               (aitools.kernel.domain:flow-continue
                :on-partial
                (list* (cons "mode" "text")
                       (cons "path" (target-display-path target))
                       fields)))
             (:on-error (code message &key candidates diagnostics conflicts repairs)
               (aitools.kernel.domain:flow-continue
                :on-error code message
                :candidates candidates
                :diagnostics diagnostics
                :conflicts conflicts
                :repairs repairs))))
          (missing ()
            (aitools.kernel.domain:flow-call
             (%read-target-error/k context target nil)
             (:on-error (code message &key candidates diagnostics conflicts repairs)
               (aitools.kernel.domain:flow-continue
                :on-error code message
                :candidates candidates
                :diagnostics diagnostics
                :conflicts conflicts
                :repairs repairs))))
          (unreadable ()
            (aitools.kernel.domain:flow-call
             (%read-target-error/k context target :unreadable)
             (:on-error (code message &key candidates diagnostics conflicts repairs)
               (aitools.kernel.domain:flow-continue
                :on-error code message
                :candidates candidates
                :diagnostics diagnostics
                :conflicts conflicts
                :repairs repairs)))))
   (if charset
       (multiple-value-bind (octets problem)
           (read-target-octets context target)
         (if octets
             (render octets)
             (aitools.kernel.domain:flow-call
              (%read-target-error/k context target problem)
              (:on-error (code message &key candidates diagnostics conflicts repairs)
                (aitools.kernel.domain:flow-continue
                 :on-error code message
                 :candidates candidates
                 :diagnostics diagnostics
                 :conflicts conflicts
                 :repairs repairs))))
       (aitools.kernel.domain:flow-call
        (call-with-target-text/k context target)
        (:on-text (octets)
          (render octets))
        (:on-binary (prefix size)
          (aitools.kernel.domain:flow-continue
           :on-ok
           (%binary-fields target prefix size)))
        (:on-missing ()
          (missing))
        (:on-unreadable ()
          (unreadable)))))))
