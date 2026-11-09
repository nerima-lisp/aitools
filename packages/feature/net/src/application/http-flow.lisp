(in-package #:aitools.net.application)

(defconstant +http-max-line-bytes+
  16384)

(defun %http-timeout-seconds (text)
  (when (stringp text)
    (handler-case (let* ((milliseconds
                          (aitools.kernel.domain:duration-milliseconds
                           (aitools.kernel.domain:parse-duration text)))
                         (seconds (/ milliseconds 1000)))
                    (when (and (plusp milliseconds) (< seconds most-positive-double-float))
                      seconds))
      (aitools.kernel.domain:invalid-duration-error ()
        nil))))

(defun %http-range-bounds (range)
  (if (null range)
      (values 1 nil t)
      (handler-case (multiple-value-bind (start end) (aitools.kernel.domain:parse-range-spec range)
                      (values start end t))
        (error ()
          (values nil nil nil)))))

(defun %http-url-input-p (url)
  (and (stringp url) (plusp (length url)) (not (find (code-char 0) url))))

(defun %http-secret-header-p (name)
  (or (aitools.protocol.domain:secret-key-name-p (substitute #\_ #\- name))
      (member name
              '("authorization" "proxy-authorization" "cookie" "set-cookie")
              :test
              #'string-equal)))

(defun %http-safe-headers (headers)
  (loop for (name . value) in headers
        unless (string-equal name "Location")
          collect (aitools.protocol.domain:json-object-from-alist
                   (list (cons "name" name)
                         (cons "value"
                               (if (%http-secret-header-p name)
                                   "[REDACTED_SECRET]"
                                   (aitools.protocol.domain:redact-secrets value)))))))

(defun %http-utf8-width (character)
  (let ((code (char-code character)))
    (cond
      ((<= code #x7f) 1)
      ((<= code #x7ff) 2)
      ((<= code #xffff) 3)
      (t 4))))

(defun %http-cap-line (line &optional (offset 0))
  (let ((bytes 0)
        (start (min offset (length line)))
        (end (min offset (length line))))
    (loop for index from start below (length line)
          for width = (%http-utf8-width (char line index))
          while (<= (+ bytes width) +http-max-line-bytes+)
          do (incf bytes width) (incf end))
    (values (subseq line start end) (< end (length line)))))

(defun %http-get-fields (response url range-start range-end max-lines line-offset)
  (let ((status (getf response :status))
        (headers (%http-safe-headers (getf response :headers)))
        (body (getf response :body))
        (final-url (or (getf response :url) url)))
    (unless (and (integerp status) (stringp final-url))
      (error "invalid HTTP response"))
    (multiple-value-bind (text errors)
        (aitools.text.domain:decode-utf8 body :start (aitools.text.domain:utf8-bom-length body))
      (let* ((lines (aitools.text.domain:split-text-lines (aitools.protocol.domain:redact-secrets text)))
             (total (length lines))
             (first (max 1 range-start))
             (last (min total (or range-end total)))
             (available (if (<= first last)
                            (1+ (- last first))
                            0))
             (shown-count (min max-lines available))
             (row-truncated (< shown-count available))
             (shown '())
             (cut-lines '()))
        (loop for number from first below (+ first shown-count)
              for offset = (if (= number first) (or line-offset 0) 0)
              do (multiple-value-bind (line cut)
                     (%http-cap-line (svref lines (1- number)) offset)
                   (push line shown)
                   (when cut
                     (push (cons number (+ offset (length line))) cut-lines))))
        (setf shown (nreverse shown)
              cut-lines (nreverse cut-lines))
        (let* ((truncated (or row-truncated cut-lines))
               (page-end (1- (+ first shown-count)))
               (commands
                (append
                 (when cut-lines
                   (loop with next-line = first
                         for (number . offset) in cut-lines
                         append (append
                                 (when (< next-line number)
                                   (list (%http-continuation final-url next-line (1- number))))
                                 (list (%http-continuation final-url number number offset)))
                           into cut-commands
                         do (setf next-line (1+ number))
                         finally
                           (return
                             (append cut-commands
                                     (when (<= next-line page-end)
                                       (list (%http-continuation final-url next-line page-end)))))))
                 (when row-truncated
                   (let ((next-start (+ first shown-count)))
                     (list (%http-continuation
                            final-url next-start
                            (min last (+ next-start max-lines -1)))))))
               )
               (fields
                (append
                 (list (cons "status" status)
                       (cons "headers" headers)
                       (cons "start_line" first))
                 (when line-offset (list (cons "line_offset" line-offset)))
                 (list (cons "lines" shown)
                       (cons "total_lines" total)
                       (cons "truncated" (aitools.protocol.domain:json-boolean truncated))
                       (cons "encoding_errors" errors))
                 (when commands
                   (list (cons "next_commands" commands)))))
               )
          (values fields truncated))))))

(defun %http-head-fields (response)
  (let ((status (getf response :status)))
    (unless (integerp status)
      (error "invalid HTTP response"))
    (list (cons "status" status) (cons "headers" (%http-safe-headers (getf response :headers))))))

(aitools.kernel.domain:define-flow
 (get/k ports url &key (timeout "30s") (max-lines 80) range line-offset on-ok on-partial on-error)
 (declare (type function on-ok on-partial on-error))
 (multiple-value-bind (range-start range-end range-valid-p) (%http-range-bounds range)
   (let ((timeout-seconds (%http-timeout-seconds timeout)))
     (cond
       ((not (%http-url-input-p url))
        (aitools.kernel.domain:flow-continue :on-error
                                             "argument.invalid"
                                             "URL must be a non-empty string."))
       ((null timeout-seconds)
        (aitools.kernel.domain:flow-continue :on-error
                                             "argument.invalid"
                                             "Timeout must be a positive finite duration."))
       ((not (and (integerp max-lines) (plusp max-lines)))
        (aitools.kernel.domain:flow-continue :on-error
                                             "argument.invalid"
                                             "Maximum lines must be a positive integer."))
       ((not range-valid-p)
        (aitools.kernel.domain:flow-continue :on-error
                                             "argument.invalid"
                                             "Range must be a valid 1-based line range."))
       ((not (or (null line-offset)
                 (and (integerp line-offset) (not (minusp line-offset)))))
        (aitools.kernel.domain:flow-continue :on-error
                                             "argument.invalid"
                                             "Line offset must be a non-negative integer."))
       (t
        (multiple-value-bind (fields truncated code detail)
            (handler-case (let ((response
                                  (funcall (net-ports-http-request ports) "GET" url timeout-seconds)))
                            (multiple-value-bind (result partial-p)
                                (%http-get-fields response url range-start range-end max-lines line-offset)
                              (values result partial-p nil nil)))
              (aitools.net.domain:net-error (condition)
                (values nil
                        nil
                        (aitools.net.domain:net-error-code condition)
                        (aitools.net.domain:net-error-detail condition)))
              (error ()
                (values nil nil "environment.io" "The HTTP request failed.")))
          (cond
            (code (aitools.kernel.domain:flow-continue :on-error code detail))
            (truncated (aitools.kernel.domain:flow-continue :on-partial fields))
            (t (aitools.kernel.domain:flow-continue :on-ok fields)))))))))

(aitools.kernel.domain:define-flow (head/k ports url &key (timeout "30s") on-ok on-error)
                                   (declare (type function on-ok on-error))
                                   (let ((timeout-seconds (%http-timeout-seconds timeout)))
                                     (cond
                                       ((not (%http-url-input-p url))
                                        (aitools.kernel.domain:flow-continue :on-error
                                                                             "argument.invalid"
                                                                             "URL must be a non-empty string."))
                                       ((null timeout-seconds)
                                        (aitools.kernel.domain:flow-continue :on-error
                                                                             "argument.invalid"
                                                                             "Timeout must be a positive finite duration."))
                                       (t
                                        (multiple-value-bind (fields code detail)
                                            (handler-case (let ((response
                                                                 (funcall
                                                                  (net-ports-http-request ports)
                                                                  "HEAD"
                                                                  url
                                                                  timeout-seconds)))
                                                            (values (%http-head-fields response)
                                                                    nil
                                                                    nil))
                                              (aitools.net.domain:net-error (condition)
                                                (values nil
                                                        (aitools.net.domain:net-error-code
                                                         condition)
                                                        (aitools.net.domain:net-error-detail
                                                         condition)))
                                              (error ()
                                                (values nil
                                                        "environment.io"
                                                        "The HTTP request failed.")))
                                          (if code
                                              (aitools.kernel.domain:flow-continue :on-error
                                                                                   code
                                                                                   detail)
                                              (aitools.kernel.domain:flow-continue :on-ok fields)))))))
