(in-package #:aitools.net.test)

(describe "aitools.net.infrastructure address policy edge cases"
          (it "rejects unrepresentable request timeouts and deadline overflow"
  (let* ((timeout (expt 10 1000))
         (failure
          (http-test-error
           (lambda ()
             (aitools.net.infrastructure::%request-deadline timeout)))))
    (expect (and (aitools.net.infrastructure::%finite-real-p timeout) (> timeout 0)) :to-be nil)
    (expect (first failure) :to-equal "argument.invalid"))
  #+sbcl
  (cl-weave:with-mocked-functions
      (((symbol-function 'aitools.net.infrastructure::%monotonic-time)
        (lambda () most-positive-double-float)))
    (expect
     (first
      (http-test-error
       (lambda () (aitools.net.infrastructure::%request-deadline 1d0))))
     :to-equal "argument.invalid")))
          (it "rejects malformed addresses and expired resolver calls"
              (let ((invalid-addresses
                     (list
                           nil
                           (make-net-address :type "AAAA" :value "192.0.2.1" :family :inet :octets #(192 0 2 1))
                           (make-net-address :type "A" :value "invalid" :family :inet6 :octets #(1 2 3 4))
                           (make-net-address :type "A" :value "invalid" :family :inet :octets #(-1 0 0 1))
                           (make-net-address :type "AAAA" :value "" :family :unknown :octets #())))
                    (uri (http-kit:parse-http-uri "https://origin.test/"))
                    (resolver-called nil))
                (dolist (address invalid-addresses)
                  (expect
                   (aitools.net.infrastructure::%valid-net-address-p address)
                   :to-be
                   nil))
                (let ((failure
                       (http-test-error
                        (lambda ()
                          (aitools.net.infrastructure::%resolve-addresses
                           (lambda (&rest arguments)
                             (declare (ignore arguments))
                             (setf resolver-called t)
                             nil)
                           uri
                           0d0)))))
                  (expect (first failure) :to-equal "environment.timeout")
                  (expect resolver-called :to-be nil))))
          (it "rejects malformed resolver values"
              (let* ((uri (http-kit:parse-http-uri "https://origin.test/"))
                     (failure
                      (http-test-error
                       (lambda ()
                         (aitools.net.infrastructure::%resolve-addresses
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            42)
                          uri
                          (+ (aitools.net.infrastructure::%monotonic-time) 10d0))))))
                (expect (first failure) :to-equal "environment.io")))
          (it "requires strictly positive finite request timeouts"
              (dolist (timeout (list 0d0 -1d0 (expt 10 1000) "1s"))
                (expect (and (aitools.net.infrastructure::%finite-real-p timeout) (> timeout 0))
                        :to-be
                        nil)
                (expect
                 (first
                  (http-test-error
                   (lambda ()
                     (aitools.net.infrastructure::%request-deadline timeout))))
                 :to-equal
                 "argument.invalid"))
              (expect
               (and (aitools.net.infrastructure::%finite-real-p 1d0) (> 1d0 0))
               :to-be
               t)
              (expect
               (realp (aitools.net.infrastructure::%request-deadline 1d0))
               :to-be
               t)
              #+sbcl
              (dolist (timeout
                       (list sb-ext:double-float-positive-infinity
                             sb-ext:double-float-negative-infinity))
                (expect
                 (aitools.net.infrastructure::%finite-real-p timeout)
                 :to-be
                 nil)
                (expect
                 (first
                  (http-test-error
                   (lambda ()
                     (aitools.net.infrastructure::%request-deadline timeout))))
                 :to-equal
                 "argument.invalid")))
          (it "opens IPv6 addresses, selects default ports, and reports exhausted connections"
  #+sbcl
  (let* ((captured nil)
         (address
          (make-net-address :type "AAAA"
                            :value "2001:4860:4860::8888"
                            :family :inet6
                            :octets #(32 1 72 96 72 96 0 0 0 0 0 0 0 0 136 136))))
    (cl-weave:with-mocked-functions
        (((symbol-function 'http-kit/network::%network-open-address)
          (lambda (address-spec port deadline clock-function)
            (setf captured (list address-spec port deadline clock-function))
            :stream)))
      (expect
       (aitools.net.infrastructure::%open-inspected-address address 443 5d0)
       :to-equal :stream))
    (expect (first (first captured))
            :to-equal http-kit/network::+network-ipv6-address-type+))
  (let ((ports nil))
    (cl-weave:with-mocked-functions
        (((symbol-function 'aitools.net.infrastructure::%open-inspected-address)
          (lambda (address port deadline)
            (declare (ignore address deadline))
            (push port ports)
            :connected)))
      (dolist (scheme (list "http" "https"))
        (expect
         (aitools.net.infrastructure::%open-inspected-addresses
          (list (http-test-loopback-address))
          (http-kit:parse-http-uri (format nil "~A://origin.test/" scheme))
          5d0)
         :to-equal :connected)))
    (expect (reverse ports) :to-equal (list 80 443)))
  (let ((failed nil))
    (cl-weave:with-mocked-functions
        (((symbol-function 'aitools.net.infrastructure::%open-inspected-address)
          (lambda (&rest arguments)
            (declare (ignore arguments))
            (error "fixture connection failure"))))
      (handler-case
          (aitools.net.infrastructure::%open-inspected-addresses
           (list (http-test-loopback-address) (http-test-public-address))
           (http-kit:parse-http-uri "http://origin.test/")
           5d0)
        (error () (setf failed t))))
    (expect failed :to-be t)))
          (it "returns timeout when the resolver deadline expires after worker start"
  (let* ((clock-values (list 0d0 0.5d0 1d0))
         (resolver
          (make-os-resolver
           :clock-function (lambda () (pop clock-values))
           :lookup-function (lambda (host) (declare (ignore host)) nil))))
    (expect
     (resolver-edge-captured-error
      (lambda () (funcall resolver "lookup.example" :deadline 1d0)))
     :to-equal
     (list "environment.timeout" "Name resolution exceeded its deadline."))))
          (it "accepts a trailing DNS dot and rejects malformed dotted IPv6 hosts"
  (let* ((hosts nil)
         (resolver
          (make-os-resolver
           :lookup-function
           (lambda (host)
             (push host hosts)
             nil))))
    (expect (funcall resolver "Example." :timeout 2d0) :to-be nil)
    (expect (reverse hosts) :to-equal (list "Example."))
    (expect
     (resolver-edge-captured-error
      (lambda () (funcall resolver "1.2.3.4::" :timeout 2d0)))
     :to-equal
     (list "argument.invalid"
           "The host must be a valid DNS name or numeric address."))))
          #+(and sbcl sb-thread)
(it "rejects NaN request deadlines"
  (let* ((nan (sb-kernel:make-double-float #x7ff80000 0))
         (failure nil))
    (cl-weave:with-mocked-functions
        (((symbol-function (quote aitools.net.infrastructure::%monotonic-time))
          (lambda () nan)))
      (sb-int:with-float-traps-masked (:invalid)
        (setf failure
              (http-test-error
               (lambda ()
                 (aitools.net.infrastructure::%request-deadline 1d0))))))
    (expect (first failure) :to-equal "argument.invalid"))))
