(in-package #:aitools.net.test)

(cl-weave:describe "aitools.net.infrastructure HTTP transport edge behavior"
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

(cl-weave:describe "aitools.net.infrastructure pinned transport contract"
  (cl-weave:it "requires the pinned numeric opener symbol with four required arguments"
    (let* ((network-package (find-package "HTTP-KIT/NETWORK"))
           (opener-symbol
             (and network-package
                  (find-symbol "%NETWORK-OPEN-ADDRESS" network-package))))
      (expect
       (and opener-symbol
            (fboundp opener-symbol)
            (let ((lambda-list
                    (sb-introspect:function-lambda-list
                     (symbol-function opener-symbol))))
              (and (listp lambda-list)
                   (= (length lambda-list) 4)
                   (every (lambda (parameter)
                            (and (symbolp parameter)
                                 (not (member parameter
                                              (quote (&optional &rest &key
                                                      &allow-other-keys &aux))
                                              :test (function eq)))))
                          lambda-list))))
       :to-be t)))
  (cl-weave:it "passes checked octets directly to the pinned opener"
    (let* ((address (http-test-loopback-address))
           (connection nil))
      (cl-weave:with-mocked-functions
          (((symbol-function (quote http-kit/network::%network-open-address))
            (lambda (address-entry port deadline clock-function)
              (declare (ignore deadline clock-function))
              (setf connection (list address-entry port))
              :stream)))
        (expect (aitools.net.infrastructure::%open-inspected-address
                 address 8080 3d0)
                :to-equal :stream))
      (expect (second (first connection))
              :to-equal (net-address-octets address))
      (expect (second connection) :to-be 8080)))
  (cl-weave:it "reports a changed opener signature as internal.unexpected"
    (let ((failure nil)
          (uri (http-kit:parse-http-uri "http://origin.test/")))
      (cl-weave:with-mocked-functions
          (((symbol-function (quote http-kit/network::%network-open-address))
            (lambda (address-entry port deadline)
              (declare (ignore address-entry port deadline))
              :stream)))
        (setf failure
              (http-test-error
               (lambda ()
                 (aitools.net.infrastructure::%open-inspected-addresses
                  (list (http-test-loopback-address)) uri 3d0)))))
      (expect (first failure) :to-equal "internal.unexpected")))
  (cl-weave:it "limits response content codings before body processing"
    (flet ((supported-p (value)
             (aitools.net.infrastructure::%supported-content-encoding-p
              (list (http-kit:make-http-header "Content-Encoding" value)))))
      (expect (aitools.net.infrastructure::%supported-content-encoding-p nil)
              :to-be t)
      (expect (supported-p "gzip") :to-be t)
      (expect (supported-p "gzip, deflate") :to-be t)
      (expect (supported-p "") :to-be nil)
      (expect (supported-p "gzip,,deflate") :to-be nil)
      (expect (supported-p "gzip, br") :to-be nil)
      (expect (supported-p "gzip, deflate, gzip") :to-be nil)
      (expect
       (aitools.net.infrastructure::%supported-content-encoding-p
        (list (http-kit:make-http-header "Content-Encoding" "gzip")
              (http-kit:make-http-header "Content-Encoding" "deflate")
              (http-kit:make-http-header "Content-Encoding" "gzip")))
       :to-be nil)
      (let* ((sender-completed-p nil)
             (error-code nil)
             (uri (http-kit:parse-http-uri "http://origin.test/"))
             (headers
               (list (http-kit:make-http-header
                      "Content-Encoding" "gzip, deflate, gzip"))))
        (let ((aitools.net.infrastructure::*http-client-send-function*
                (lambda (client method request-uri
                         &key on-response-headers &allow-other-keys)
                  (declare (ignore client method request-uri))
                  (funcall on-response-headers 200 headers)
                  (setf sender-completed-p t)
                  (http-test-response 200 :headers headers))))
          (setf error-code
                (first
                 (http-test-error
                  (lambda ()
                    (aitools.net.infrastructure::%send-http-request
                     "GET" uri (list (http-test-loopback-address))
                     (+ (aitools.net.infrastructure::%monotonic-time) 3d0))))))
        (expect error-code :to-equal "environment.io")
        (expect sender-completed-p :to-be nil))))))
