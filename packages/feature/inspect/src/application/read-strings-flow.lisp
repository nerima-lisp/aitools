(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (%strings-view context
                target
                min-length
                max-lines
                &key
                on-ok
                on-partial
                on-error)
 (declare (type function on-ok on-partial on-error))
 (let* ((items '())
        (total 0)
        (first-cut nil)
        (scan
         (make-strings-scan :min-length
                            min-length
                            :max-text-length
                            +max-line-bytes+
                            :emit
                            (lambda (offset text cut-offset)
                              (when (< total max-lines)
                                (push
                                 (if cut-offset
                                     (json-object "offset"
                                                  offset
                                                  "text"
                                                  text
                                                  "cut"
                                                  t)
                                     (json-object "offset" offset "text" text))
                                 items)
                                (when (and cut-offset (null first-cut))
                                  (setf first-cut cut-offset)))
                              (incf total)
                              nil)))
        (carry nil)
        (position 0))
   (multiple-value-bind (size problem)
       (map-target-chunks context
                          target
                          (lambda (chunk)
                            (let* ((octets
                                    (if carry
                                        (concatenate
                                         '(simple-array (unsigned-byte 8) (*))
                                         carry
                                         chunk)
                                        chunk))
                                   (base (- position (length carry)))
                                   (consumed
                                    (scan-strings-chunk scan octets base)))
                              (setf carry (and (< consumed (length octets))
                                               (subseq octets consumed))
                                    position (+ position (length chunk))))))
     (if (null size)
         (aitools.kernel.domain:flow-call
          (%read-target-error/k context target problem)
          (:on-error (code message &key candidates diagnostics conflicts repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message
             :candidates candidates
             :diagnostics diagnostics
             :conflicts conflicts
             :repairs repairs)))
         (progn
           (scan-strings-chunk scan
                               (or carry
                                   (make-array 0
                                               :element-type
                                               '(unsigned-byte 8)))
                               (- position (length carry))
                               :final
                               t)
           (let* ((truncated (or (> total max-lines) first-cut))
                  (items (nreverse items))
                  (texts
                   (mapcar
                    (lambda (item)
                      (json-object-get item "text"))
                    items))
                  (next
                   (append
                    (when (> total max-lines)
                      (list
                       (command-line context
                                     (list "read"
                                           (file-target-argument target)
                                           "--as"
                                           "strings"
                                           "--min-length"
                                           (princ-to-string min-length)
                                           "--max-lines"
                                           (princ-to-string total)))))
                    (when first-cut
                      (list
                       (command-line context
                                     (list "read"
                                           (file-target-argument target)
                                           "--as"
                                           "hex"
                                           "--bytes"
                                           (format nil
                                                   "~D:~D"
                                                   first-cut
                                                   (min size
                                                        (+ first-cut 256)))))))))
                  (fields
                   (append
                    (list (cons "mode" "strings")
                          (cons "path" (target-display-path target))
                          (cons "size" size)
                          (cons "strings" items)
                          (cons "total" total)
                          (cons "truncated" (json-bool truncated))
                          (cons "approx_tokens" (%text-approx-tokens texts)))
                    (when next
                      (list (cons "next_commands" next))))))
             (if truncated
                 (aitools.kernel.domain:flow-continue :on-partial fields)
                 (aitools.kernel.domain:flow-continue :on-ok fields))))))))
