(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (%text-view-selected octets options max-lines context hash escape selector lines total
                     ranges errors &key on-ok on-partial)
 (declare (type function on-ok on-partial))
 (if (eq (selector-kind selector) :match)
     (let* ((numbers (mapcar #'car ranges))
            (shown (subseq numbers 0 (min max-lines (length numbers))))
            (truncated (< (length shown) (length numbers))))
       (multiple-value-bind (texts cuts)
           (%cut-decoded-lines octets
                               options
                               shown
                               (map 'vector
                                    (lambda (number)
                                      (svref lines (1- number)))
                                    shown))
         (aitools.kernel.domain:flow-call
          (%text-view-emit options
                           context
                           hash
                           escape
                           (if shown (first shown) 1)
                           texts
                           truncated
                           total
                           errors
                           (and truncated
                                (list
                                 (command-line context
                                               (append
                                                (text-view-options-command-words
                                                 options)
                                                (list "--match"
                                                      (selector-match-pattern
                                                       selector))
                                                (when (selector-invert selector)
                                                  (list "--invert"))
                                                (list "--max-lines"
                                                      (princ-to-string
                                                       (length numbers)))
                                                (text-view-options-extra-words
                                                 options)))))
                           :line-numbers shown
                           :cuts cuts)
          (:on-ok (fields)
            (aitools.kernel.domain:flow-continue :on-ok fields))
          (:on-partial (fields)
            (aitools.kernel.domain:flow-continue :on-partial fields)))))
   (destructuring-bind ((start . end)) ranges
     (if (> start end)
         (aitools.kernel.domain:flow-call
          (%text-view-emit options
                           context
                           hash
                           escape
                           start
                           (vector)
                           nil
                           total
                           errors
                           nil)
          (:on-ok (fields)
            (aitools.kernel.domain:flow-continue :on-ok fields))
          (:on-partial (fields)
            (aitools.kernel.domain:flow-continue :on-partial fields)))
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
                               nil)
        (:on-ok (fields)
          (aitools.kernel.domain:flow-continue :on-ok fields))
        (:on-partial (fields)
          (aitools.kernel.domain:flow-continue :on-partial fields)))))))