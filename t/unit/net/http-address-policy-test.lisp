(in-package #:aitools.net.test)

(describe "aitools.net.infrastructure address policy"
          (it "classifies special-use IPv4, IPv6, and embedded IPv4 addresses"
    (dolist
        (octets
         (list #(0 0 0 1)
               #(10 0 0 1)
               #(100 64 0 1)
               #(127 0 0 1)
               #(169 254 0 1)
               #(172 16 0 1)
               #(192 0 0 1)
               #(192 0 2 1)
               #(192 88 99 1)
               #(192 168 0 1)
               #(192 175 48 1)
               #(198 18 0 1)
               #(198 51 100 1)
               #(203 0 113 1)
               #(224 0 0 1)))
      (expect (aitools.net.infrastructure::%public-ipv4-p octets) :to-be nil))
    (expect (aitools.net.infrastructure::%public-ipv4-p #(93 184 216 34))
            :to-be t)
    (dolist
        (octets
         (list
               #(32 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
               #(32 1 0 2 0 0 0 0 0 0 0 0 0 0 0 1)
               #(32 1 0 16 0 0 0 0 0 0 0 0 0 0 0 1)
               #(32 1 0 32 0 0 0 0 0 0 0 0 0 0 0 1)
               #(32 1 13 184 0 0 0 0 0 0 0 0 0 0 0 1)
               #(32 2 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
               #(63 255 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
               #(45 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
               #(46 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
               #(48 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
               #(56 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
               #(60 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
               #(62 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
               #(63 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
               #(63 254 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
               #(95 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
               #(254 128 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
      (expect (aitools.net.infrastructure::%public-ipv6-p octets) :to-be nil))
    (expect
     (aitools.net.infrastructure::%public-ipv6-p
      #(32 1 72 96 0 0 0 0 0 0 0 0 0 0 0 1))
     :to-be t)
    (dolist
        (addresses
         (list
          (cons #(0 100 255 155 0 0 0 0 0 0 0 0 93 184 216 34)
                #(0 100 255 155 0 0 0 0 0 0 0 0 10 0 0 1))
          (cons #(0 0 0 0 0 0 0 0 0 0 255 255 93 184 216 34)
                #(0 0 0 0 0 0 0 0 0 0 255 255 10 0 0 1))
          (cons #(0 0 0 0 0 0 0 0 0 0 0 0 93 184 216 34)
                #(0 0 0 0 0 0 0 0 0 0 0 0 10 0 0 1))
          (cons #(32 2 93 184 216 34 0 0 0 0 0 0 0 0 0 0)
                #(32 2 10 0 0 1 0 0 0 0 0 0 0 0 0 0))
          (cons #(32 1 0 0 192 0 2 1 0 0 0 0 162 71 39 221)
                #(32 1 0 0 192 0 2 1 0 0 0 0 245 255 255 254))))
      (expect (aitools.net.infrastructure::%public-ipv6-p (car addresses))
              :to-be t)
      (expect (aitools.net.infrastructure::%public-ipv6-p (cdr addresses))
              :to-be nil))
    (dolist
        (octets
         (list #(0 100 255 155 0 1 0 0 0 0 0 0 93 184 216 34)
               #(0 100 255 155 0 1 0 0 0 0 0 0 10 0 0 1)))
      (expect (aitools.net.infrastructure::%public-ipv6-p octets) :to-be nil)))
          (it "validates IPv4 and IPv6 resolver addresses by family and type"
              (let ((ipv4 (http-test-public-address))
                    (ipv6
                     (make-net-address :type
                                       "AAAA"
                                       :value
                                       "2001:4860:0:0:0:0:0:1"
                                       :family
                                       :inet6
                                       :octets
                                       #(32 1 72 96 0 0 0 0 0 0 0 0 0 0 0 1)))
                    (mismatched
                     (make-net-address :type
                                       "AAAA"
                                       :value
                                       "192.0.2.1"
                                       :family
                                       :inet
                                       :octets
                                       #(192 0 2 1))))
                (expect (aitools.net.infrastructure::%valid-net-address-p ipv4)
                        :to-be
                        t)
                (expect (aitools.net.infrastructure::%valid-net-address-p ipv6)
                        :to-be
                        t)
                (expect
                 (aitools.net.infrastructure::%valid-net-address-p mismatched)
                 :to-be
                 nil)
                (expect (aitools.net.infrastructure::%public-address-p ipv4)
                        :to-be
                        t)
                (expect (aitools.net.infrastructure::%public-address-p ipv6)
                        :to-be
                        t)))
          (it "distinguishes empty and malformed host answers without sending"
              (let ((send-count 0))
                (let ((aitools.net.infrastructure::*http-client-send-function*
                       (lambda (&rest arguments)
                         (declare (ignore arguments))
                         (incf send-count)
                         (http-test-response 200))))
                  (dolist (answer (list nil 42))
                    (let ((requester
                           (make-http-requester
                            (lambda (host &key timeout deadline)
                              (declare (ignore host timeout deadline))
                              answer))))
                      (let ((failure
                             (http-test-error
                              (lambda ()
                                (funcall requester
                                         "GET"
                                         "http://origin.test/"
                                         10d0)))))
                        (expect (first failure) :to-equal "environment.io"))))
                  (expect send-count :to-be 0)))))
