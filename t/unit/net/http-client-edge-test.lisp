(in-package #:aitools.net.test)

(cl-weave:describe "aitools.net.infrastructure HTTP client edge behavior"
  (cl-weave:it "rejects invalid URLs and accepts parsed URIs with empty bodies"
  (let* ((lookup-count 0)
         (send-count 0)
         (requester
           (make-http-requester
            (http-test-resolver
             (http-test-loopback-address)
             (lambda (host timeout deadline)
               (declare (ignore host timeout deadline))
               (incf lookup-count)))))
         (factory-error
           (http-test-error (lambda () (make-http-requester 42))))
         (method-error
           (http-test-error
            (lambda ()
              (funcall requester 17 "http://origin.test/" 2d0))))
         (uri-error
           (http-test-error
            (lambda ()
              (funcall requester "GET" 17 2d0))))
         (userinfo-error
           (http-test-error
            (lambda ()
              (funcall requester "GET" "http://user:secret@origin.test/" 2d0)))))
    (expect (first factory-error) :to-equal "argument.invalid")
    (expect (first method-error) :to-equal "argument.invalid")
    (expect (first uri-error) :to-equal "argument.invalid")
    (expect (first userinfo-error) :to-equal "argument.invalid")
    (progn
      (expect lookup-count :to-be 0)
      (expect (aitools.net.infrastructure::%normalized-method "GET")
              :to-equal "GET")
      (expect (aitools.net.infrastructure::%normalized-method :head)
              :to-equal "HEAD")
      (expect (aitools.net.infrastructure::%normalized-method "POST")
              :to-be nil)
      (expect (aitools.net.infrastructure::%normalized-method nil)
              :to-be nil)
      (expect (aitools.net.infrastructure::%valid-http-uri-p 42)
              :to-be nil)
      (let ((uri (http-kit:parse-http-uri "http://origin.test/")))
        (cl-weave:with-mocked-functions
            (((symbol-function (quote http-kit:http-uri-scheme))
              (lambda (ignored) (declare (ignore ignored)) "ftp")))
          (expect
           (first
            (http-test-error
             (lambda ()
               (aitools.net.infrastructure::%parse-request-uri uri))))
           :to-equal "argument.invalid")))
      (let ((uri (http-kit:parse-http-uri "http://origin.test/")))
        (cl-weave:with-mocked-functions
            (((symbol-function (quote http-kit:http-uri-authority))
              (lambda (ignored) (declare (ignore ignored)) nil)))
          (expect (aitools.net.infrastructure::%valid-http-uri-p uri)
                  :to-be nil)))
      (let ((uri (http-kit:parse-http-uri "http://origin.test/")))
        (cl-weave:with-mocked-functions
            (((symbol-function (quote http-kit:http-uri-authority))
              (lambda (ignored)
                (declare (ignore ignored))
                "user@origin.test")))
          (expect (aitools.net.infrastructure::%valid-http-uri-p uri)
                  :to-be nil)
          (expect
           (first
            (http-test-error
             (lambda ()
               (aitools.net.infrastructure::%parse-request-uri uri))))
           :to-equal "argument.invalid")))
      (let ((uri (http-kit:parse-http-uri "http://origin.test/")))
        (cl-weave:with-mocked-functions
            (((symbol-function (quote http-kit:http-uri-host))
              (lambda (ignored) (declare (ignore ignored)) nil)))
          (expect (aitools.net.infrastructure::%valid-http-uri-p uri)
                  :to-be nil)))
      (let ((uri (http-kit:parse-http-uri "http://origin.test/")))
        (cl-weave:with-mocked-functions
            (((symbol-function (quote http-kit:http-uri-host))
              (lambda (ignored) (declare (ignore ignored)) "")))
          (expect (aitools.net.infrastructure::%valid-http-uri-p uri)
                  :to-be nil))))
    (let ((aitools.net.infrastructure::*http-client-send-function*
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (incf send-count)
              (http-kit:make-http-response
               :status 204
               :headers
               (list (http-kit:make-http-header "Content-Type" "text/plain"))
               :body nil))))
      (let ((response
              (funcall requester :get
                       (http-kit:parse-http-uri "http://origin.test/")
                       2d0)))
        (expect send-count :to-be 1)
        (expect lookup-count :to-be 1)
        (expect (getf response :status) :to-be 204)
        (expect (equalp (getf response :body)
                        (make-array 0 :element-type (quote (unsigned-byte 8))))
                :to-be t)))))
  (cl-weave:it "keeps the prior response when a redirect target has no addresses"
    (let* ((resolved-hosts nil)
           (send-count 0)
           (requester
             (make-http-requester
              (lambda (host &key timeout deadline)
                (declare (ignore timeout deadline))
                (push host resolved-hosts)
                (unless (string= host "unresolved.test")
                  (list (http-test-loopback-address)))))))
      (let ((aitools.net.infrastructure::*http-client-send-function*
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf send-count)
                (http-test-response 302
                                    :location "http://unresolved.test/"))))
        (let ((response
                (funcall requester "GET" "http://origin.test/" 3d0)))
          (expect send-count :to-be 1)
          (expect (reverse resolved-hosts)
                  :to-equal (list "origin.test" "unresolved.test"))
          (expect (getf response :status) :to-be 302)
          (expect (getf response :url) :to-equal "http://origin.test/")))))
  (cl-weave:it "maps typed HTTP transport conditions to existing error codes"
    (dolist
        (case
         (list
          (list (make-condition (quote http-kit:http-timeout)
                                :message "fixture timeout")
                "environment.timeout")
          (list (make-condition (quote http-kit:http-size-limit-exceeded)
                                :message "fixture limit"
                                :limit 1 :observed 2 :kind :body)
                "refusal.too-large")
          (list (make-condition (quote http-kit:http-unsupported-feature)
                                :message "fixture feature"
                                :feature :fixture)
                "environment.unavailable")))
      (let ((requester
              (make-http-requester
               (http-test-resolver (http-test-loopback-address))))
            (failure nil))
        (let ((aitools.net.infrastructure::*http-client-send-function*
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (error (first case)))))
          (setf failure
                (http-test-error
                 (lambda ()
                   (funcall requester "GET" "http://origin.test/" 3d0)))))
        (expect (first failure) :to-equal (second case)))))
  (cl-weave:it "checks final response coding even when the sender skips its callback"
  (expect
   (aitools.net.infrastructure::%supported-content-encoding-p
    (list (http-kit:make-http-header "Content-Encoding" "gzip, deflate")))
   :to-be t)
  (let ((requester
          (make-http-requester
           (http-test-resolver (http-test-loopback-address))))
        (failure nil))
    (let ((aitools.net.infrastructure::*http-client-send-function*
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (http-kit:make-http-response
               :status 200
               :headers
               (list (http-kit:make-http-header "Content-Encoding" "br"))
               :body nil))))
      (setf failure
            (http-test-error
             (lambda ()
               (funcall requester "GET" "http://origin.test/" 3d0)))))
    (expect (first failure) :to-equal "environment.io")))
  (cl-weave:it "retries inspected addresses and stops immediately on timeout"
    (let* ((addresses
             (list (http-test-loopback-address)
                   (http-test-public-address)))
           (uri (http-kit:parse-http-uri "http://origin.test:8080/"))
           (attempts nil))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.net.infrastructure::%open-inspected-address))
            (lambda (address port deadline)
              (declare (ignore deadline))
              (push (list (net-address-value address) port) attempts)
              (if (string= (net-address-value address) "127.0.0.1")
                  (error "first address failed")
                  :connected))))
        (expect
         (aitools.net.infrastructure::%open-inspected-addresses
          addresses uri 5d0)
         :to-equal :connected)
        (expect (reverse attempts)
                :to-equal
                (list (list "127.0.0.1" 8080)
                      (list "93.184.216.34" 8080))))
      (let ((attempt-count 0)
            (timed-out nil))
        (cl-weave:with-mocked-functions
            (((symbol-function
               (quote aitools.net.infrastructure::%open-inspected-address))
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf attempt-count)
                (error
                 (make-condition (quote http-kit:http-timeout)
                                 :message "fixture timeout")))))
          (handler-case
              (aitools.net.infrastructure::%open-inspected-addresses
               addresses uri 5d0)
            (http-kit:http-timeout ()
              (setf timed-out t))))
        (expect attempt-count :to-be 1)
        (expect timed-out :to-be t))))
  (cl-weave:it "rejects a host mismatch before attempting an address"
    (let ((open-stream nil)
          (open-count 0)
          (rejected nil))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote http-kit/client:make-http-client))
            (lambda (&rest arguments)
              (setf open-stream (getf arguments :open-stream))
              :fake-client))
           ((symbol-function
             (quote aitools.net.infrastructure::%open-inspected-address))
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (incf open-count)
              :stream)))
        (aitools.net.infrastructure::%make-http-client
         "expected.test" (list (http-test-loopback-address))))
      (handler-case
          (funcall open-stream
                   (http-kit:make-http-request
                    :method "GET" :uri "http://other.test/")
                   :deadline 2d0)
        (error () (setf rejected t)))
      (expect rejected :to-be t)
      (expect open-count :to-be 0)))
  (cl-weave:it "ignores environment proxies for pinned requests"
    (let* ((environment-reads nil)
           (plan nil)
           (requester
             (make-http-requester
              (http-test-resolver (http-test-public-address)))))
      (let ((http-kit/client::*proxy-environment-function*
              (lambda (name)
                (push name environment-reads)
                (when (member name
                              (list "http_proxy" "HTTP_PROXY")
                              :test (function string=))
                  "http://proxy-user:proxy-pass@proxy.test:3128"))))
        (let ((aitools.net.infrastructure::*http-client-send-function*
                (lambda (client method uri
                         &key timeout deadline &allow-other-keys)
                  (declare (ignore method timeout deadline))
                  (setf plan
                        (http-kit/client:http-proxy-plan
                         (http-kit/client:http-client-proxy client)
                         uri))
                  (http-test-response 200))))
          (funcall requester "GET" "http://origin.test/" 3d0)))
      (expect environment-reads :to-equal nil)
      (expect (getf plan :mode) :to-equal :direct)
      (expect (getf plan :proxy-authorization) :to-equal nil)))
  (cl-weave:it "maps empty host resolution to environment.io without sending"
    (let* ((resolve-count 0)
           (send-count 0)
           (failure nil)
           (requester
             (make-http-requester
              (lambda (host &key timeout deadline)
                (declare (ignore host timeout deadline))
                (incf resolve-count)
                nil))))
      (let ((aitools.net.infrastructure::*http-client-send-function*
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf send-count)
                (http-test-response 200))))
        (setf failure
              (http-test-error
               (lambda ()
                 (funcall requester "GET" "http://unresolved.test/" 3d0)))))
      (expect (first failure) :to-equal "environment.io")
      (expect resolve-count :to-be 1)
      (expect send-count :to-be 0)))
  (cl-weave:it "normalizes an absent HTTP response body"
  (let* ((response (http-kit:make-http-response :status 204 :body nil))
         (uri (http-kit:parse-http-uri "http://origin.test/"))
         (body nil))
    (cl-weave:with-mocked-functions
        (((symbol-function (quote http-kit:http-response-body))
          (lambda (ignored) (declare (ignore ignored)) nil)))
      (setf body
            (getf
             (aitools.net.infrastructure::%response-plist response uri)
             :body)))
    (expect (equalp body
                    (make-array 0 :element-type (quote (unsigned-byte 8))))
            :to-be t)))
  (cl-weave:it "returns timeout when response processing crosses the request deadline"
  (let* ((clock-times (list 0d0 0d0 2d0))
         (requester
          (make-http-requester
           (http-test-resolver (http-test-public-address))))
         (failure nil))
    (let ((aitools.net.infrastructure::*http-client-send-function*
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (http-test-response 200))))
      (cl-weave:with-mocked-functions
          (((symbol-function (quote aitools.net.infrastructure::%monotonic-time))
            (lambda () (pop clock-times))))
        (setf failure
              (http-test-error
               (lambda ()
                 (funcall requester "GET" "http://origin.test/" 1d0)))))
    (expect (first failure) :to-equal "environment.timeout")
    (expect clock-times :to-equal nil)))))
