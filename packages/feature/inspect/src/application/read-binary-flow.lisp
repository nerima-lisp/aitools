(in-package #:aitools.inspect.application)

(defun %text-options (target selector tail max-lines escape-invisible charset)
  (make-text-view-options :selector
                          selector
                          :tail
                          tail
                          :max-lines
                          max-lines
                          :escape-invisible
                          escape-invisible
                          :encoding
                          charset
                          :path
                          (target-display-path target)
                          :command-words
                          (list "read" (file-target-argument target))
                          :extra-words
                          (append
                           (when escape-invisible
                             (list "--escape-invisible"))
                           (when charset
                             (list "--encoding" (encoding-name charset))))
                          :hex-words
                          (lambda (start end)
                            (list "read"
                                  (file-target-argument target)
                                  "--as"
                                  "hex"
                                  "--bytes"
                                  (format nil "~D:~D" start end)))))

(defun %binary-fields (target prefix size)
  (list (cons "mode" "text")
        (cons "path" (target-display-path target))
        (cons "binary" t)
        (cons "size" size)
        (cons "mime" (guess-mime prefix :path (file-target-absolute target)))))

(defconstant +hex-redaction-margin+
  4096
  "Bytes read on each side of a `--as hex` window, so the secret mask sees
the whole lines around it without reading the file. A line running past
the margin is masked as if it began or ended there.")

(defun %hex-next-command (context target end size truncated)
  (when (< end size)
    (list
     (command-line context
                   (list "read"
                         (file-target-argument target)
                         "--as"
                         "hex"
                         "--bytes"
                         (format nil
                                 "~D:~D"
                                 end
                                 (if truncated
                                     size
                                     (min size (+ end 256)))))))))

(defun %parse-byte-span (text size)
  "(VALUES start end) for `--bytes S:E` or `S:` (0-based, END exclusive)
clamped to SIZE, or NIL when TEXT is malformed."
  (let ((colon (position #\: text)))
    (flet ((number (string)
             (and (plusp (length string))
                  (every
                   (lambda (char)
                     (char<= #\0 char #\9))
                   string)
                  (parse-integer string))))
      (when colon
        (let ((start (number (subseq text 0 colon)))
              (end
               (if (= colon (1- (length text)))
                   size
                   (number (subseq text (1+ colon))))))
          (when (and start end (<= start end))
            (values (min start size) (min end size))))))))

(aitools.kernel.domain:define-flow
 (%hex-view context target bytes max-lines &key on-ok on-partial on-error)
 (declare (type function on-ok on-partial on-error))
 (multiple-value-bind (start end)
     (if bytes
         (%parse-byte-span bytes most-positive-fixnum)
         (values 0 256))
   (if (not (integerp start))
       (aitools.kernel.domain:flow-continue
        :on-error
        "argument.invalid"
        (format nil
                "--bytes ~S is not S:E or S: (0-based byte offsets)"
                bytes)
        :repairs
        (list
         (repair "fix-bytes"
                 "Give a byte span such as 0:256."
                 (command-line context
                               (list "read"
                                     (file-target-argument target)
                                     "--as"
                                     "hex"
                                     "--bytes"
                                     "0:256"))))
       (let ((low (max 0 (- start +hex-redaction-margin+))))
         (multiple-value-bind (octets size-or-problem)
             (read-target-range context
                                target
                                low
                                (+ (min end (+ start (* 16 max-lines)))
                                   +hex-redaction-margin+))
           (if (null octets)
               (aitools.kernel.domain:flow-call
                (%read-target-error/k context target size-or-problem)
                (:on-error (code message &key candidates diagnostics conflicts repairs)
                  (aitools.kernel.domain:flow-continue
                   :on-error code message
                   :candidates candidates
                   :diagnostics diagnostics
                   :conflicts conflicts
                   :repairs repairs)))
               (let* ((size size-or-problem)
                      (low (min low size))
                      (start (min start size))
                      (end (min end size))
                      (shown-end (min end (+ start (* 16 max-lines))))
                      (truncated (< shown-end end))
                      (next
                       (%hex-next-command context
                                          target
                                          shown-end
                                          size
                                          truncated))
                      (masked
                       (redact-hex-octets octets
                                          (- start low)
                                          (- shown-end low)))
                      (fields
                       (append
                        (list (cons "mode" "hex")
                              (cons "path" (target-display-path target))
                              (cons "size" size)
                              (cons "start" start)
                              (cons "end" shown-end)
                              (cons "rows"
                                    (hex-rows masked
                                              (- start low)
                                              (- shown-end low)
                                              :base
                                              low))
                              (cons "truncated" (json-bool truncated))
                              (cons "approx_tokens"
                                    (approx-token-count
                                     (* 3 (- shown-end start)))))
                        (when next
                          (list (cons "next_commands" next))))))
                 (if truncated
                     (aitools.kernel.domain:flow-continue :on-partial fields)
                     (aitools.kernel.domain:flow-continue :on-ok fields))))))))))
