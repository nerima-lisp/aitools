(in-package #:aitools.net.test)

(defun http-ssrf-used-addresses (answers url)
  (let ((used-addresses nil)
        (requester
         (make-http-requester
          (lambda (host &key timeout deadline)
            (declare (ignore host timeout deadline))
            answers))))
    (cl-weave:with-mocked-functions
     (((symbol-function 'aitools.net.infrastructure::%send-http-request)
       (lambda (method uri addresses deadline)
         (declare (ignore method uri deadline))
         (setf used-addresses addresses)
         (http-test-response 200))))
     (funcall requester "GET" url 5d0))
    (mapcar #'net-address-value used-addresses)))

(describe "aitools.net.infrastructure initial address policy"
  (it "uses only public answers for a mixed public and private hostname"
    (let* ((loopback-v6
            (make-net-address :type "AAAA" :value "::1" :family :inet6
                              :octets #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
           (addresses
            (list (http-test-loopback-address)
                  loopback-v6
                  (http-test-public-address))))
      (expect (http-ssrf-used-addresses addresses "http://mixed.test/")
              :to-equal (list "93.184.216.34"))))
  (it "allows a private-only initial address when the caller selects it"
    (expect (http-ssrf-used-addresses (list (http-test-loopback-address))
                                     "http://127.0.0.1/")
            :to-equal (list "127.0.0.1")))
  (it "allows a caller-selected public IPv6 initial address"
    (let ((address
            (make-net-address :type "AAAA"
                              :value "2001:4860:4860::8888"
                              :family :inet6
                              :octets #(32 1 72 96 72 96 0 0 0 0 0 0 0 0 136 136))))
      (expect (http-ssrf-used-addresses (list address)
                                        "http://[2001:4860:4860::8888]/")
              :to-equal (list "2001:4860:4860::8888"))))
  (it "follows public-origin redirects to native IPv6 addresses"
    (let* ((send-count 0)
           (resolved-hosts nil)
           (ipv6-address
            (make-net-address :type "AAAA"
                              :value "2001:4860:4860::8888"
                              :family :inet6
                              :octets #(32 1 72 96 72 96 0 0 0 0 0 0 0 0 136 136)))
           (resolver
            (lambda (host &key timeout deadline)
              (declare (ignore timeout deadline))
              (push host resolved-hosts)
              (if (string= host "origin.test")
                  (list (http-test-public-address))
                  (list ipv6-address)))))
      (cl-weave:with-mocked-functions
          (((symbol-function 'aitools.net.infrastructure::%send-http-request)
            (lambda (method uri addresses deadline)
              (declare (ignore method addresses deadline))
              (incf send-count)
              (if (string= (http-kit:http-uri-host uri) "origin.test")
                  (http-test-response 302 :location "http://target.test/")
                  (http-test-response 200)))))
        (let ((response
                (aitools.net.infrastructure::%perform-http-request
                 resolver "GET" (http-kit:parse-http-uri "http://origin.test/")
                 (+ (aitools.net.infrastructure::%monotonic-time) 10d0))))
          (expect send-count :to-be 2)
          (expect (reverse resolved-hosts)
                  :to-equal (list "origin.test" "target.test"))
          (expect (getf response :status) :to-be 200)
          (expect (getf response :url) :to-equal "http://target.test/"))))))
