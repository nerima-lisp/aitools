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
  (cl-weave:it "preserves internal opener failures wrapped by cl-http-kit"
    (let ((requester
            (make-http-requester
             (http-test-resolver (http-test-loopback-address))))
          (failure nil))
      (cl-weave:with-mocked-functions
          (((symbol-function (quote http-kit/network::%network-open-address))
            (lambda (address-entry port deadline)
              (declare (ignore address-entry port deadline))
              :stream)))
        (setf failure
              (http-test-error
               (lambda ()
                 (funcall requester "GET" "http://origin.test/" 3d0)))))
      (expect (first failure) :to-equal "internal.unexpected"))))
