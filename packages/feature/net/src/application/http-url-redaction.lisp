(in-package #:aitools.net.application)
(defun %http-secret-query-key-p (key)
  (flet ((decode-once (value)
           (with-output-to-string (out)
             (loop with index = 0
                   while (< index (length value))
                   do (let* ((character (char value index))
                             (code (when (and (char= character #\%)
                                              (<= (+ index 2) (1- (length value))))
                                     (ignore-errors
                                       (parse-integer value :start (1+ index)
                                                      :end (+ index 3) :radix 16)))))
                        (if code
                            (write-char (code-char code) out)
                            (write-char (if (char= character #\+) #\Space character) out))
                        (incf index (if code 3 1))))))
         (contains-control-character-p (value)
           (find-if (lambda (character)
                      (let ((code (char-code character)))
                        (or (< code 32) (<= 127 code 159))))
                    value)))
    (let ((decoded key))
      (loop repeat 8
            for next = (decode-once decoded)
            while (not (string= next decoded))
            do (setf decoded next))
      (or (find #\% decoded)
          (contains-control-character-p decoded)
          (aitools.protocol.domain:secret-key-name-p
           (substitute #\_ #\- (string-downcase decoded)))))))
(defun %http-safe-continuation-url (value)
  (let* ((fragment (position #\# value))
         (query (position #\? value :end (or fragment (length value))))
         (end (or fragment (length value))))
    (if (null query)
        (aitools.protocol.domain:redact-secrets value)
        (aitools.protocol.domain:redact-secrets
         (concatenate
          'string (subseq value 0 (1+ query))
          (with-output-to-string (out)
            (loop with start = (1+ query)
                  for stop = (or (position #\& value :start start :end end) end)
                  for equal = (position #\= value :start start :end stop)
                  do (write-string (subseq value start (or equal stop)) out)
                     (when equal
                       (write-char #\= out)
                       (write-string
                        (if (%http-secret-query-key-p (subseq value start equal))
                            "[REDACTED_SECRET]"
                            (subseq value (1+ equal) stop))
                        out))
                     (when (= stop end) (return))
                     (write-char #\& out)
                     (setf start (1+ stop)))
          (subseq value end)))))))
(defun %http-continuation (url start end &optional line-offset)
  (apply #'aitools.protocol.domain:command-line
         (append (list "aitools" "net" "get"
                       (%http-safe-continuation-url url)
                       "--range" (format nil "~D:~D" start end))
                 (when line-offset
                   (list "--line-offset" (format nil "~D" line-offset))))))
