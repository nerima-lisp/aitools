(in-package #:aitools.net.test)

(defun run-http-get (sender url &rest options)
  (let ((result nil))
    (apply (function aitools.net.application:get/k)
           (aitools.net.application:make-net-ports
            :http-request sender
            :resolve-host (lambda (&rest ignored)
                            (declare (ignore ignored))
                            nil))
           url
           (append options
                   (list :on-ok
                         (lambda (fields)
                           (setf result (list :ok fields)))
                         :on-partial
                         (lambda (fields)
                           (setf result (list :partial fields)))
                         :on-error
                         (lambda (code detail)
                           (setf result (list :error (list code detail)))))))
    (values-list result)))

(defun run-http-head (sender url &rest options)
  (let ((result nil))
    (apply (function aitools.net.application:head/k)
           (aitools.net.application:make-net-ports
            :http-request sender
            :resolve-host (lambda (&rest ignored)
                            (declare (ignore ignored))
                            nil))
           url
           (append options
                   (list :on-ok
                         (lambda (fields)
                           (setf result (list :ok fields)))
                         :on-error
                         (lambda (code detail)
                           (setf result (list :error (list code detail)))))))
    (values-list result)))

(defun http-fields-value (fields name)
  (cdr (assoc name fields :test #'string=)))

(defun http-response (status body &key (headers '()) (url "https://example.test/final"))
  (list :status status :headers headers :body body :url url))

(describe "aitools.net.application HTTP flows"
          (it "returns a 5xx status and decodes UTF-8 lines after a GET"
              (let ((call nil)
                    (response
                     (http-response 503
                                    (aitools.text.domain:encode-utf8 (format nil "alpha~%beta~%"))
                                    :headers
                                    '(("Content-Type" . "text/plain")))))
                (multiple-value-bind (kind fields)
                    (run-http-get
                     (lambda (method url timeout)
                       (setf call (list method url timeout))
                       response)
                     "https://example.test/start")
                  (expect kind :to-be :ok)
                  (expect call :to-equal '("GET" "https://example.test/start" 30))
                  (expect (http-fields-value fields "status") :to-be 503)
                  (expect (http-fields-value fields "start_line") :to-be 1)
                  (expect (http-fields-value fields "total_lines") :to-be 2)
                  (expect (http-fields-value fields "lines") :to-equal '("alpha" "beta"))
                  (expect (http-fields-value fields "truncated")
                          :to-be
                          (aitools.protocol.domain:json-boolean nil)))))
          (it "uses range and max-lines and masks the continuation URL"
              (let ((response
                     (http-response 200
                                    (aitools.text.domain:encode-utf8 (format nil "one~%two~%three~%four"))
                                    :url
                                    "https://example.test/final?api_key=topsecret")))
                (multiple-value-bind (kind fields)
                    (run-http-get
                     (lambda (&rest ignored)
                       (declare (ignore ignored))
                       response)
                     "https://example.test/start"
                     :max-lines
                     2)
                  (expect kind :to-be :partial)
                  (expect (http-fields-value fields "lines") :to-equal '("one" "two"))
                  (expect (http-fields-value fields "truncated")
                          :to-be
                          (aitools.protocol.domain:json-boolean t))
                  (let ((command (first (http-fields-value fields "next_commands"))))
                    (expect (not (null (search "--range 3:4" command))) :to-be t)
                    (expect (not (null (search "[REDACTED_SECRET]" command))) :to-be t)
                    (expect (search "topsecret" command) :to-be nil)))))
          (it "uses the default 80-line window and parses open-ended ranges"
              (let ((response
                     (http-response 200
                                    (aitools.text.domain:encode-utf8
                                     (format nil
                                             "~{~D~%~}"
                                             (loop for n from 1 to 100
                                                   collect n))))))
                (multiple-value-bind (kind fields)
                    (run-http-get
                     (lambda (&rest ignored)
                       (declare (ignore ignored))
                       response)
                     "https://example.test/"
                     :range
                     "1:")
                  (expect kind :to-be :partial)
                  (expect (http-fields-value fields "start_line") :to-be 1)
                  (expect (http-fields-value fields "total_lines") :to-be 100)
                  (expect (length (http-fields-value fields "lines")) :to-be 80)
                  (expect
                   (not (null (search "--range 81:100" (first (http-fields-value fields "next_commands")))))
                   :to-be
                   t))))
          (it "caps each displayed line at 16384 UTF-8 bytes"
              (let* ((line (concatenate 'string (make-string 16383 :initial-element #\a) "é"))
                     (response (http-response 200 (aitools.text.domain:encode-utf8 line))))
                (multiple-value-bind (kind fields)
                    (run-http-get
                     (lambda (&rest ignored)
                       (declare (ignore ignored))
                       response)
                     "https://example.test/"
                     :max-lines
                     1)
                  (expect kind :to-be :partial)
                  (expect (length (first (http-fields-value fields "lines"))) :to-be 16383)
                  (expect (not (null (http-fields-value fields "next_commands"))) :to-be t))))
          (it "redacts a whole PEM body and masks secret headers while omitting Location"
              (let* ((body
                      (aitools.text.domain:encode-utf8
                       (format nil
                               "-----BEGIN PRIVATE KEY-----~%MIIEowIBAAKCAQEAdummy~%-----END PRIVATE KEY-----~%safe")))
                     (response
                      (http-response 200
                                     body
                                     :headers
                                     '(("Authorization" . "Bearer authvalue")
                                       ("X-Api-Key" . "headerkey")
                                       ("X-Note" . "api_key=inlinekey")
                                       ("Location" . "https://other.test/?token=hidden")))))
                (multiple-value-bind (kind fields)
                    (run-http-get
                     (lambda (&rest ignored)
                       (declare (ignore ignored))
                       response)
                     "https://example.test/")
                  (expect kind :to-be :ok)
                  (let* ((lines (http-fields-value fields "lines"))
                         (body-text (format nil "~{~A~%~}" lines))
                         (headers (http-fields-value fields "headers"))
                         (members (mapcar #'aitools.protocol.domain:json-object-members headers)))
                    (expect (not (null (search "[REDACTED_SECRET]" body-text))) :to-be t)
                    (expect (not (null (search "MIIEowIBAAKCAQEAdummy" body-text))) :to-be nil)
                    (expect members
                            :to-equal
                            '((("name" . "Authorization") ("value" . "[REDACTED_SECRET]"))
                              (("name" . "X-Api-Key") ("value" . "[REDACTED_SECRET]"))
                              (("name" . "X-Note") ("value" . "api_key=[REDACTED_SECRET]"))))))))
          (it "rejects invalid timeout and range without calling the sender"
              (let* ((calls 0)
                    (sender
                     (lambda (&rest ignored)
                       (declare (ignore ignored))
                       (incf calls)
                       (http-response 200 (aitools.text.domain:encode-utf8 "ok")))))
                (dolist (options '((:timeout "0s") (:timeout "bad") (:range "3:2")))
                  (multiple-value-bind (kind fields)
                      (apply #'run-http-get sender "https://example.test/" options)
                    (expect kind :to-be :error)
                    (expect (first fields) :to-equal "argument.invalid")))
                (progn
                  (expect calls :to-be 0)
                  (multiple-value-bind (fields truncated)
                      (aitools.net.application::%http-get-fields
                       (http-response 200
                                      (aitools.text.domain:encode-utf8 "only"))
                       "https://example.test/final" 99 101 80 nil)
                    (expect truncated :to-be nil)
                    (expect (http-fields-value fields "lines") :to-equal nil)))
                (multiple-value-bind (kind fields)
                    (run-http-get
                     (lambda (&rest ignored)
                       (declare (ignore ignored))
                       (error "private failure detail"))
                     "https://example.test/")
                  (expect kind :to-be :error)
                  (expect (first fields) :to-equal "environment.io")
                  (expect (search "private failure detail" (second fields)) :to-be nil))))
          (it "reports invalid UTF-8 replacements"
              (let ((response
                     (http-response 200
                                    (make-array 1
                                                :element-type
                                                '(unsigned-byte 8)
                                                :initial-element
                                                #xff))))
                (multiple-value-bind (kind fields)
                    (run-http-get
                     (lambda (&rest ignored)
                       (declare (ignore ignored))
                       response)
                     "https://example.test/")
                  (expect kind :to-be :ok)
                  (expect (http-fields-value fields "encoding_errors") :to-be 1)
                  (expect (first (http-fields-value fields "lines")) :to-equal "�"))))
          (it "sends HEAD and returns no body even for an HTTP error status"
              (let ((call nil)
                    (response
                     (http-response 404
                                    (aitools.text.domain:encode-utf8 "must not appear")
                                    :headers
                                    '(("Content-Length" . "16")
                                      ("Location" . "https://other.test/")))))
                (multiple-value-bind (kind fields)
                    (run-http-head
                     (lambda (method url timeout)
                       (setf call (list method url timeout))
                       response)
                     "https://example.test/"
                     :timeout
                     "2s")
                  (expect kind :to-be :ok)
                  (expect call :to-equal '("HEAD" "https://example.test/" 2))
                  (expect (http-fields-value fields "status") :to-be 404)
                  (expect (assoc "body" fields :test #'string=) :to-be nil)
                  (expect (assoc "lines" fields :test #'string=) :to-be nil)
                  (expect (length (http-fields-value fields "headers")) :to-be 1)))))
