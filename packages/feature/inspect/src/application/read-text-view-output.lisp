(in-package #:aitools.inspect.application)

(defun %text-view-present (lines escape)
  (if escape
      (map 'list #'escape-invisible lines)
      (coerce lines 'list)))

(aitools.kernel.domain:define-flow
 (%text-view-emit options context
                  hash
                  escape
                  first
                  shown
                  truncated
                  total
                  errors
                  next
                  &key
                  line-numbers
                  cuts
                  on-ok
                  on-partial)
 (declare (type function on-ok on-partial))
 (let* ((texts (%text-view-present shown escape))
        (hex-words (text-view-options-hex-words options))
        (offset (cdr (first cuts)))
        (truncated (or truncated (and cuts t)))
        (next
         (append next
                 (when (and hex-words offset)
                   (list
                    (command-line context
                                  (funcall hex-words offset (+ offset 256)))))))
        (fields
         (append (list (cons "start_line" first) (cons "lines" texts))
                 (when line-numbers
                   (list (cons "line_numbers" line-numbers)))
                 (when cuts
                   (list (cons "cut_lines" (mapcar #'car cuts))))
                 (list (cons "total_lines" total)
                       (cons "hash" hash)
                       (cons "truncated" (json-bool truncated))
                       (cons "encoding_errors" errors))
                 (when (text-view-options-encoding options)
                   (list
                    (cons "encoding"
                          (encoding-name (text-view-options-encoding options)))))
                 (list (cons "approx_tokens" (%text-approx-tokens texts)))
                 (when next
                   (list (cons "next_commands" next))))))
   (if truncated
       (aitools.kernel.domain:flow-continue :on-partial fields)
     (aitools.kernel.domain:flow-continue :on-ok fields))))

(defun %text-view-next-range (context options
                                      max-lines
                                      first
                                      last
                                      total
                                      tail
                                      truncated)
  (cond
    ((and tail truncated)
     (list
      (%continue-command context
                         options
                         "--range"
                         (format nil
                                 "~D:~D"
                                 (max 1 (- first max-lines))
                                 (1- first)))))
    ((< last total)
     (list
      (%continue-command context
                         options
                         "--range"
                         (format nil
                                 "~D:~D"
                                 (1+ last)
                                 (min total (+ last max-lines))))))))

(aitools.kernel.domain:define-flow
 (%text-view-contiguous octets options
                        max-lines
                        context
                        hash
                        escape
                        lines
                        total
                        start
                        end
                        errors
                        tail
                        &key
                        on-ok
                        on-partial)
 (declare (type function on-ok on-partial))
 (multiple-value-bind (first last truncated)
     (%shown-window start end max-lines tail)
   (multiple-value-bind (shown cuts)
       (%cut-decoded-lines octets
                           options
                           (loop for number from first to last
                                 collect number)
                           (subseq lines (1- first) last))
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
                       (%text-view-next-range context
                                              options
                                              max-lines
                                              first
                                              last
                                              total
                                              tail
                                              truncated)
                       :cuts cuts)
      (:on-ok (fields)
        (aitools.kernel.domain:flow-continue :on-ok fields))
      (:on-partial (fields)
        (aitools.kernel.domain:flow-continue :on-partial fields))))))

(aitools.kernel.domain:define-flow
 (%text-view-indexed-window octets index
                            options
                            max-lines
                            context
                            hash
                            escape
                            total
                            start
                            end
                            tail
                            &key
                            on-ok
                            on-partial)
 (declare (type function on-ok on-partial))
 (multiple-value-bind (first last truncated)
     (%shown-window start end max-lines tail)
   (multiple-value-bind (shown errors cuts)
       (decode-line-range octets
                          index
                          first
                          last
                          :max-line-bytes
                          +max-line-bytes+)
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
                       (%text-view-next-range context
                                              options
                                              max-lines
                                              first
                                              last
                                              total
                                              tail
                                              truncated)
                       :cuts cuts)
      (:on-ok (fields)
        (aitools.kernel.domain:flow-continue :on-ok fields))
      (:on-partial (fields)
        (aitools.kernel.domain:flow-continue :on-partial fields))))))

