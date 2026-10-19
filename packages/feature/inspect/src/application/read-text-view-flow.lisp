(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (text-view/k context octets options &key on-ok on-partial on-error)
 "Render OCTETS as read text under OPTIONS (see TEXT-VIEW-OPTIONS). Call
ON-OK or ON-PARTIAL with the fields after mode/path, or ON-ERROR when
selection fails."
 (declare (type function on-ok on-partial on-error))
 (let ((selector (text-view-options-selector options))
       (max-lines (text-view-options-max-lines options))
       (escape (text-view-options-escape-invisible options))
       (path (text-view-options-path options))
       (hash (content-hash octets)))
   (labels ((emit (first shown truncated total errors next &key line-numbers cuts)
              (aitools.kernel.domain:flow-call
               (%text-view-emit options
                                context
                                hash
                                escape
                                first
                                shown
                                truncated
                                total
                                errors
                                next
                                :line-numbers line-numbers
                                :cuts cuts)
               (:on-ok (fields)
                 (aitools.kernel.domain:flow-continue :on-ok fields))
               (:on-partial (fields)
                 (aitools.kernel.domain:flow-continue :on-partial fields))))
            (contiguous (lines total start end errors tail)
              (aitools.kernel.domain:flow-call
               (%text-view-contiguous octets
                                      options
                                      max-lines
                                      context
                                      hash
                                      escape
                                      lines
                                      total
                                      start
                                      end
                                      errors
                                      tail)
               (:on-ok (fields)
                 (aitools.kernel.domain:flow-continue :on-ok fields))
               (:on-partial (fields)
                 (aitools.kernel.domain:flow-continue :on-partial fields))))
            (selected (lines total ranges errors)
              (aitools.kernel.domain:flow-call
               (%text-view-selected octets
                                    options
                                    max-lines
                                    context
                                    hash
                                    escape
                                    selector
                                    lines
                                    total
                                    ranges
                                    errors)
               (:on-ok (fields)
                 (aitools.kernel.domain:flow-continue :on-ok fields))
               (:on-partial (fields)
                 (aitools.kernel.domain:flow-continue :on-partial fields))))
            (window (index total start end tail)
              (aitools.kernel.domain:flow-call
               (%text-view-indexed-window octets
                                           index
                                           options
                                           max-lines
                                           context
                                           hash
                                           escape
                                           total
                                           start
                                           end
                                           tail)
               (:on-ok (fields)
                 (aitools.kernel.domain:flow-continue :on-ok fields))
               (:on-partial (fields)
                 (aitools.kernel.domain:flow-continue :on-partial fields)))))
     (if (or (and selector (not (eq (selector-kind selector) :range)))
             (text-view-options-encoding options)
             escape)
         (aitools.kernel.domain:flow-call
          (%decode-all/k octets options)
          (:on-decoded (lines errors)
            (let ((total (length lines)))
              (cond
                ((null selector)
                 (let ((tail (text-view-options-tail options)))
                   (if (zerop total)
                       (emit 1 (vector) nil 0 errors nil)
                     (contiguous lines
                                 total
                                 (if tail (max 1 (- total tail -1)) 1)
                                 total
                                 errors
                                 tail))))
                (t
                 (aitools.kernel.domain:flow-call
                  (resolve-selector/k lines selector :path path)
                  (:on-selected (ranges)
                    (selected lines total ranges errors))
                  (:on-no-match (candidates)
                    (aitools.kernel.domain:flow-call
                     (selection-error context :no-match candidates :path path)
                     (:on-error (code message &key candidates diagnostics conflicts repairs)
                       (aitools.kernel.domain:flow-continue
                        :on-error code message
                        :candidates candidates
                        :diagnostics diagnostics
                        :conflicts conflicts
                        :repairs repairs))))
                  (:on-ambiguous (candidates)
                    (aitools.kernel.domain:flow-call
                     (selection-error context :ambiguous candidates :path path)
                     (:on-error (code message &key candidates diagnostics conflicts repairs)
                       (aitools.kernel.domain:flow-continue
                        :on-error code message
                        :candidates candidates
                        :diagnostics diagnostics
                        :conflicts conflicts
                        :repairs repairs))))
                  (:on-invalid (code message)
                    (aitools.kernel.domain:flow-call
                     (selection-error context
                                      :invalid
                                      nil
                                      :path path
                                      :code code
                                      :message message)
                     (:on-error
                      (error-code error-message &key candidates diagnostics conflicts repairs)
                       (aitools.kernel.domain:flow-continue
                        :on-error error-code error-message
                        :candidates candidates
                        :diagnostics diagnostics
                        :conflicts conflicts
                        :repairs repairs))))))))
       (let* ((bom (utf8-bom-length octets))
              (index (build-line-index octets :start bom))
              (total (line-index-count index))
              (tail (text-view-options-tail options)))
         (cond
           ((zerop total)
            (if selector
                (aitools.kernel.domain:flow-call
                 (selection-error context :no-match '() :path path)
                 (:on-error (code message &key candidates diagnostics conflicts repairs)
                   (aitools.kernel.domain:flow-continue
                    :on-error code message
                    :candidates candidates
                    :diagnostics diagnostics
                    :conflicts conflicts
                    :repairs repairs)))
              (emit 1 (vector) nil 0 0 nil)))
           ((null selector)
            (window index
                    total
                    (if tail (max 1 (- total tail -1)) 1)
                    total
                    tail))
           (t
            (let ((start (selector-range-start selector))
                  (end (min total (or (selector-range-end selector) total))))
              (if (> start total)
                  (aitools.kernel.domain:flow-call
                   (selection-error
                    context
                    :no-match
                    (multiple-value-bind (lines)
                        (decode-line-range octets
                                           index
                                           total
                                           total
                                           :max-line-bytes
                                           +max-line-bytes+)
                      (list
                       (json-object "line" total "text" (svref lines 0))))
                    :path path)
                   (:on-error (code message &key candidates diagnostics conflicts repairs)
                     (aitools.kernel.domain:flow-continue
                      :on-error code message
                      :candidates candidates
                      :diagnostics diagnostics
                      :conflicts conflicts
                      :repairs repairs)))
                (window index total start end nil)))))))))
)))
