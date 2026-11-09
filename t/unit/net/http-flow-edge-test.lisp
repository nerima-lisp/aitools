(in-package #:aitools.net.test)

(describe "aitools.net.application HTTP flow edge cases"
          (it "caps a four-byte UTF-8 character without splitting it and resumes clipped middle rows"
              (let* ((line (concatenate 'string
                                        (make-string 16381 :initial-element #\a)
                                        "🙂"))
                     (response
                      (http-response 200 (aitools.text.domain:encode-utf8 line))))
                (multiple-value-bind (kind fields)
                    (run-http-get
                     (lambda (&rest ignored)
                       (declare (ignore ignored))
                       response)
                     "https://example.test/")
                  (expect kind :to-be :partial)
                  (expect (length (first (http-fields-value fields "lines")))
                          :to-be 16381)
                  (expect (search "🙂" (first (http-fields-value fields "lines")))
                          :to-be nil)))
              (let* ((long-line (make-string 16385 :initial-element #\a))
                     (body (format nil "short~%~A~%short" long-line))
                     (response
                      (http-response 200 (aitools.text.domain:encode-utf8 body))))
                (multiple-value-bind (kind fields)
                    (run-http-get
                     (lambda (&rest ignored)
                       (declare (ignore ignored))
                       response)
                     "https://example.test/" :range "1:3" :max-lines 3)
                  (let ((commands (http-fields-value fields "next_commands")))
                    (expect kind :to-be :partial)
                    (expect (length commands) :to-be 3)
                    (expect (search "--range 1:1" (first commands)) :to-be-truthy)
                    (expect (search "--line-offset 16384" (second commands)) :to-be-truthy)
                    (expect (search "--range 3:3" (third commands)) :to-be-truthy)))))
          (it
           "rejects invalid GET URLs, timeouts, and line limits before sending"
           (let* ((calls 0)
                 (sender
                  (lambda (&rest ignored)
                    (declare (ignore ignored))
                    (incf calls)
                    (http-response 200 (aitools.text.domain:encode-utf8 "ok")))))
             (dolist
                 (url
                  (list nil
                        ""
                        42
                        (format nil "https://bad~C.example" (code-char 0))))
               (multiple-value-bind (kind fields) (run-http-get sender url)
                 (expect kind :to-be :error)
                 (expect (first fields) :to-equal "argument.invalid")))
             (dolist
                 (options
                  '((:timeout "0s") (:timeout "bad") (:timeout 30)
                    (:max-lines 0) (:max-lines nil)
                    (:line-offset -1) (:line-offset "bad")))
               (multiple-value-bind (kind fields)
                   (apply #'run-http-get sender "https://example.test/" options)
                 (expect kind :to-be :error)
                 (expect (first fields) :to-equal "argument.invalid")))
             (expect calls :to-be 0)))
          (it "rejects invalid HEAD URLs and timeouts before sending"
              (let* ((calls 0)
                    (sender
                     (lambda (&rest ignored)
                       (declare (ignore ignored))
                       (incf calls)
                       (http-response 200 #()))))
                (dolist (url (list nil "" 42))
                  (multiple-value-bind (kind fields) (run-http-head sender url)
                    (expect kind :to-be :error)
                    (expect (first fields) :to-equal "argument.invalid")))
                (multiple-value-bind (kind fields)
                    (run-http-head sender "https://example.test/" :timeout "0s")
                  (expect kind :to-be :error)
                  (expect (first fields) :to-equal "argument.invalid"))
                (expect calls :to-be 0)))
          (it "preserves net-error code and detail for GET and HEAD"
              (dolist (flow (list #'run-http-get #'run-http-head))
                (multiple-value-bind (kind fields)
                    (funcall flow
                             (lambda (&rest ignored)
                               (declare (ignore ignored))
                               (error 'aitools.net.domain:net-error
                                      :code
                                      "environment.timeout"
                                      :detail
                                      "safe transport detail"))
                             "https://example.test/")
                  (expect kind :to-be :error)
                  (expect fields
                          :to-equal
                          '("environment.timeout" "safe transport detail")))))
          (it "hides generic sender errors for GET and HEAD"
              (dolist (flow (list #'run-http-get #'run-http-head))
                (multiple-value-bind (kind fields)
                    (funcall flow
                             (lambda (&rest ignored)
                               (declare (ignore ignored))
                               (error "private exception detail"))
                             "https://example.test/")
                  (expect kind :to-be :error)
                  (expect (first fields) :to-equal "environment.io")
                  (expect (search "private exception detail" (second fields))
                          :to-be
                          nil))))
          (it "maps invalid response status values to a generic I/O error"
              (dolist (flow (list #'run-http-get #'run-http-head))
                (multiple-value-bind (kind fields)
                    (funcall flow
                             (lambda (&rest ignored)
                               (declare (ignore ignored))
                               (http-response "200"
                                              (aitools.text.domain:encode-utf8
                                               "body")))
                             "https://example.test/")
                  (expect kind :to-be :error)
                  (expect (first fields) :to-equal "environment.io")
                  (expect (search "invalid HTTP response" (second fields))
                          :to-be
                          nil))))
          (it "continues a clipped UTF-8 line by Unicode character offset"
              (let* ((line (concatenate 'string
                                         (make-string 16381 :initial-element #\a)
                                         "🙂"
                                         (make-string 20000 :initial-element #\z)))
                     (response (http-response 200 (aitools.text.domain:encode-utf8 line))))
                (multiple-value-bind (kind fields)
                    (run-http-get (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    response)
                                  "https://example.test/" :range "1:1" :max-lines 1)
                  (expect kind :to-be :partial)
                  (expect (length (aitools.text.domain:encode-utf8
                                   (first (http-fields-value fields "lines"))))
                          :to-be 16381)
                  (expect (search "--range 1:1 --line-offset 16381"
                                  (first (http-fields-value fields "next_commands")))
                          :to-be-truthy))
                (multiple-value-bind (kind fields)
                    (run-http-get (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    response)
                                  "https://example.test/" :range "1:1" :max-lines 1
                                  :line-offset 16381)
                  (let ((line-part (first (http-fields-value fields "lines"))))
                    (expect kind :to-be :partial)
                    (expect (http-fields-value fields "line_offset") :to-be 16381)
                    (expect (search "🙂" line-part) :to-be 0)
                    (expect (length (aitools.text.domain:encode-utf8 line-part))
                            :to-be 16384)
                    (expect (search "--range 1:1 --line-offset 32762"
                                    (first (http-fields-value fields "next_commands")))
                            :to-be-truthy)))
                (multiple-value-bind (kind fields)
                    (run-http-get (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    response)
                                  "https://example.test/" :range "1:1" :max-lines 1
                                  :line-offset 32762)
                  (expect kind :to-be :ok)
                  (expect (http-fields-value fields "line_offset") :to-be 32762)
                  (expect (first (http-fields-value fields "lines"))
                          :to-equal (subseq line 32762))
                  (expect (length (aitools.text.domain:encode-utf8
                                   (first (http-fields-value fields "lines"))))
                          :to-be 3620))))
          (it "keeps every clipped row and the explicit range end in continuations"
    (let* ((body (format nil "before~%~A~%middle~%~A~%tail~%outside"
                         (make-string 16385 :initial-element #\a)
                         (make-string 16385 :initial-element #\b)))
           (response (http-response 200 (aitools.text.domain:encode-utf8 body))))
      (multiple-value-bind (kind fields)
          (run-http-get (lambda (&rest ignored)
                          (declare (ignore ignored))
                          response)
                        "https://example.test/" :range "1:5" :max-lines 4)
        (let ((commands (http-fields-value fields "next_commands"))
              (lines (http-fields-value fields "lines")))
          (expect kind :to-be :partial)
          (expect (first lines) :to-equal "before")
          (expect (length (aitools.text.domain:encode-utf8 (second lines))) :to-be 16384)
          (expect (third lines) :to-equal "middle")
          (expect (length (aitools.text.domain:encode-utf8 (fourth lines))) :to-be 16384)
          (expect (length commands) :to-be 5)
          (expect (search "--range 1:1" (first commands)) :to-be-truthy)
          (expect (search "--range 2:2 --line-offset 16384" (second commands)) :to-be-truthy)
          (expect (search "--range 3:3" (third commands)) :to-be-truthy)
          (expect (search "--line-offset" (third commands)) :to-be nil)
          (expect (search "--range 4:4 --line-offset 16384" (fourth commands)) :to-be-truthy)
          (expect (search "--range 5:5" (fifth commands)) :to-be-truthy)
          (expect (search "--range 5:6" (fifth commands)) :to-be nil)
          (expect (search "outside" (format nil "~{~A~%~}" commands)) :to-be nil)))))
          (it "redacts encoded secret query keys in continuation commands"
  (dolist (url (list "https://fixture.invalid/?%61pi%5Fkey=topsecret&ok=yes#fragment" "https://fixture.invalid/?%2561pi%255Fkey=topsecret&ok=yes#fragment" "https://fixture.invalid/?api%00_key=topsecret&ok=yes#fragment" "https://fixture.invalid/?github+token=topsecret&ok=yes#fragment"))
    (let ((response
            (http-response 200
                           (aitools.text.domain:encode-utf8
                            (make-string 16385 :initial-element #\x))
                           :url url)))
      (multiple-value-bind (kind fields)
          (run-http-get (lambda (&rest ignored)
                          (declare (ignore ignored))
                          response)
                        "https://fixture.invalid/start" :range "1:1")
        (let ((command (first (http-fields-value fields "next_commands"))))
          (expect kind :to-be :partial)
          (expect (search "[REDACTED_SECRET]" command) :to-be-truthy)
          (expect (search "topsecret" command) :to-be nil))))))
)
