(in-package #:aitools.net.test)

(describe "aitools.net.infrastructure content coding"
          (it "advertises only gzip and deflate and rejects unsupported codings"
              (dolist (coding (list "br" "zstd" "identity"))
                (let ((accept-encoding nil)
                      (request-headers nil)
                      (failure nil)
                      (requester
                       (make-http-requester
                        (http-test-resolver (http-test-public-address)))))
                  (let ((aitools.net.infrastructure::*http-client-send-function*
                         (lambda (client method uri &key timeout deadline on-response-headers
                                         &allow-other-keys)
                           (setf (http-kit/client:http-client-transport-function client)
                                 (lambda (request &key on-response-headers &allow-other-keys)
                                   (setf request-headers (http-kit:http-request-headers request))
                                   (setf accept-encoding
                                         (http-kit:http-header-values request-headers "Accept-Encoding"))
                                   (let ((response
                                          (http-kit:make-http-response
                                           :status 200
                                           :headers
                                           (list (http-kit:make-http-header "Content-Encoding" coding))
                                           :body
                                           (make-array 0 :element-type (quote (unsigned-byte 8))))))
                                     (when on-response-headers
                                       (funcall on-response-headers
                                                (http-kit:http-response-status response)
                                                (http-kit:http-response-headers response)))
                                     response)))
                           (http-kit/client:http-client-send
                            client method uri :timeout timeout :deadline deadline
                            :on-response-headers on-response-headers))))
                    (setf failure
                          (http-test-error
                           (lambda ()
                             (funcall requester "GET" "https://origin.test/" 10d0)))))
                  (expect accept-encoding :to-equal (list "gzip, deflate"))
                  (expect (http-kit:http-header-values request-headers "Authorization")
                          :to-equal nil)
                  (expect (http-kit:http-header-values request-headers
                                                       "Proxy-Authorization")
                          :to-equal nil)
                  (expect (first failure) :to-equal "environment.io")))))

(describe "aitools.net.infrastructure redirect policy"
  (it "follows at most five redirects and returns the sixth response"
    (let* ((send-count 0)
          (resolved-hosts nil)
          (requester
            (make-http-requester
             (http-test-resolver
              (http-test-public-address)
              (lambda (host timeout deadline)
                (declare (ignore timeout deadline))
                (push host resolved-hosts))))))
      (let ((aitools.net.infrastructure::*http-client-send-function*
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf send-count)
                (http-test-response
                 302
                 :location (format nil "http://hop-~D.test/" send-count)))))
        (let ((response (funcall requester "GET"
                                 "http://origin.test/" 10d0)))
          (expect send-count :to-be 6)
          (expect (reverse resolved-hosts)
                  :to-equal
                  (list "origin.test" "hop-1.test" "hop-2.test"
                        "hop-3.test" "hop-4.test" "hop-5.test"))
          (expect (getf response :status) :to-be 302)
          (expect (getf response :url) :to-equal "http://hop-5.test/")
          (expect (assoc "Location" (getf response :headers)
                         :test (function string-equal))
                  :to-equal nil)))))

  (it "does not resolve or send an HTTPS downgrade"
    (let* ((send-count 0)
          (resolved-hosts nil)
          (requester
            (make-http-requester
             (http-test-resolver
              (http-test-public-address)
              (lambda (host timeout deadline)
                (declare (ignore timeout deadline))
                (push host resolved-hosts))))))
      (let ((aitools.net.infrastructure::*http-client-send-function*
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf send-count)
                (http-test-response 301 :location "http://clear.test/"))))
        (let ((response (funcall requester "GET"
                                 "https://secure.test/path" 10d0)))
          (expect send-count :to-be 1)
          (expect (reverse resolved-hosts) :to-equal (list "secure.test"))
          (expect (getf response :url)
                  :to-equal "https://secure.test/path")
          (expect (getf response :status) :to-be 301)
          (expect (assoc "Location" (getf response :headers)
                         :test (function string-equal))
                  :to-equal nil)))))

  (it "does not resolve or send a redirect containing userinfo"
    (let* ((send-count 0)
          (resolved-hosts nil)
          (requester
            (make-http-requester
             (http-test-resolver
              (http-test-public-address)
              (lambda (host timeout deadline)
                (declare (ignore timeout deadline))
                (push host resolved-hosts))))))
      (let ((aitools.net.infrastructure::*http-client-send-function*
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf send-count)
                (http-test-response 302
                                    :location "http://user@target.test/"))))
        (let ((response (funcall requester "GET"
                                 "http://origin.test/" 10d0)))
          (expect send-count :to-be 1)
          (expect (reverse resolved-hosts) :to-equal (list "origin.test"))
          (expect (getf response :url) :to-equal "http://origin.test/")
          (expect (assoc "Location" (getf response :headers)
                         :test (function string-equal))
                  :to-equal nil)))))

  (it "blocks mixed public/non-public redirect targets and follows public IPv6 redirects from public origins"
    (let* ((send-count 0)
           (resolved-hosts nil)
           (requester
            (make-http-requester
             (lambda (host &key timeout deadline)
               (declare (ignore timeout deadline))
               (push host resolved-hosts)
               (if (string= host "mixed.test")
                   (list (http-test-loopback-address)
                         (http-test-public-address))
                 (list (http-test-public-address)
                       (http-test-loopback-address)))))))
      (let ((aitools.net.infrastructure::*http-client-send-function*
             (lambda (&rest arguments)
               (declare (ignore arguments))
               (incf send-count)
               (http-test-response 302 :location "http://mixed-target.test/"))))
        (let ((response (funcall requester "GET"
                                 "http://mixed.test/" 10d0)))
          (expect send-count :to-be 1)
          (expect (reverse resolved-hosts)
                  :to-equal (list "mixed.test" "mixed-target.test"))
          (expect (getf response :url) :to-equal "http://mixed.test/")
          (expect (assoc "Location" (getf response :headers)
                         :test (function string-equal))
                  :to-equal nil)))
      (let* ((send-count 0)
             (resolved-hosts nil)
             (ipv6-address
              (make-net-address :type "AAAA"
                                :value "2001:4860:4860::8888"
                                :family :inet6
                                :octets #(32 1 72 96 72 96 0 0 0 0 0 0 0 0 136 136)))
             (requester
              (make-http-requester
               (lambda (host &key timeout deadline)
                 (declare (ignore timeout deadline))
                 (push host resolved-hosts)
                 (if (string= host "origin.test")
                     (list (http-test-public-address))
                     (list (http-test-public-address) ipv6-address)))))
             (aitools.net.infrastructure::*http-client-send-function*
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf send-count)
                (if (= send-count 1)
                    (http-test-response 302 :location "http://dualstack.test/")
                    (http-test-response 200)))))
        (let ((response (funcall requester "GET" "http://origin.test/" 10d0)))
          (expect send-count :to-be 2)
          (expect (reverse resolved-hosts)
                  :to-equal (list "origin.test" "dualstack.test"))
          (expect (getf response :status) :to-be 200)
          (expect (getf response :url) :to-equal "http://dualstack.test/")))))

  (it "allows a private origin to redirect to either private or public addresses"
  (dolist (target-address
           (list (http-test-loopback-address) (http-test-public-address)))
    (let* ((send-count 0)
           (resolved-hosts nil)
           (requester
            (make-http-requester
             (lambda (host &key timeout deadline)
               (declare (ignore timeout deadline))
               (push host resolved-hosts)
               (list (if (string= host "local.test")
                         (http-test-loopback-address)
                       target-address))))))
      (let ((aitools.net.infrastructure::*http-client-send-function*
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf send-count)
                (if (= send-count 1)
                    (http-test-response 302 :location "http://next.local/")
                  (http-test-response 200)))))
        (let ((response (funcall requester "GET" "http://local.test/" 10d0)))
          (expect send-count :to-be 2)
          (expect (reverse resolved-hosts)
                  :to-equal (list "local.test" "next.local"))
          (expect (getf response :status) :to-be 200)
          (expect (getf response :url) :to-equal "http://next.local/")))))))
