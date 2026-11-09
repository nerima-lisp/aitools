(in-package #:aitools.net.test)

(defun http-test-public-address ()
  (make-net-address :type "A"
                    :value "93.184.216.34"
                    :family :inet
                    :octets #(93 184 216 34)))
(defun http-test-loopback-address ()
  (make-net-address :type "A"
                    :value "127.0.0.1"
                    :family :inet
                    :octets #(127 0 0 1)))
(defun http-test-resolver (address &optional observer)
  (lambda (host &key timeout deadline)
    (when observer
      (funcall observer host timeout deadline))
    (list address)))
(defun http-test-response (status &key location headers body)
  (http-kit:make-http-response
   :status status
   :headers (append
             (when location
               (list (http-kit:make-http-header "Location" location)))
             headers)
   :body (or body (make-array 0 :element-type (quote (unsigned-byte 8))))))
(defun http-test-error (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (net-error (condition)
      (list (net-error-code condition) (net-error-detail condition)))))

(describe "aitools.net.infrastructure HTTP request contract"
  (it "uses one finite deadline and bounded, unauthenticated decoding"
    (let* ((resolver-observation nil)
           (sender-observation nil)
           (send-count 0)
           (resolver
             (http-test-resolver
              (http-test-public-address)
              (lambda (host timeout deadline)
                (setf resolver-observation (list host timeout deadline)))))
           (requester (make-http-requester resolver)))
      (let ((aitools.net.infrastructure::*http-client-send-function*
              (lambda (client method uri &key deadline &allow-other-keys)
  (declare (ignore uri))
  (incf send-count)
  (setf sender-observation
        (list method
              deadline
              (http-kit/client:http-client-max-body-bytes client)
              (http-kit/client:http-client-default-headers client)
              (http-kit/client:http-client-auth-provider client)
              (http-kit/client:http-client-challenge-auth-provider client)
              (http-kit/client:http-client-proxy-challenge-auth-provider client)
              (mapcar (function car)
                      (http-kit/client:http-client-content-decoders client))))
  (http-test-response
   200
   :location "https://hidden.invalid/"
   :headers (list (http-kit:make-http-header "Content-Type" "text/plain"))
   :body (make-array 2
                     :element-type (quote (unsigned-byte 8))
                     :initial-contents (list 7 9))))))
        (let ((response (funcall requester "GET" "http://origin.test/" 10d0)))
          (expect send-count :to-be 1)
          (expect (first sender-observation) :to-equal "GET")
          (expect (first resolver-observation) :to-equal "origin.test")
          (expect (and (realp (second resolver-observation))
                       (> (second resolver-observation) 0)
                       (realp (third resolver-observation)))
                  :to-be t)
          (expect (= (third resolver-observation) (second sender-observation))
                  :to-be t)
          (expect (third sender-observation) :to-be (* 16 1024 1024))
          (expect (http-kit:http-header-values (fourth sender-observation) "Accept-Encoding") :to-equal (list "gzip, deflate"))
          (expect (fifth sender-observation) :to-equal nil)
          (expect (sixth sender-observation) :to-equal nil)
          (expect (seventh sender-observation) :to-equal nil)
          (expect (eighth sender-observation)
                  :to-equal (list "gzip" "deflate"))
          (expect (getf response :status) :to-be 200)
          (expect (getf response :url) :to-equal "http://origin.test/")
          (expect (equalp (getf response :body) #(7 9)) :to-be t)
          (expect (assoc "Location" (getf response :headers)
                         :test (function string-equal))
                  :to-equal nil)))))
  (it "passes HEAD as the only other supported method"
    (let ((methods nil)
          (requester (make-http-requester
                      (http-test-resolver (http-test-public-address)))))
      (let ((aitools.net.infrastructure::*http-client-send-function*
              (lambda (client method uri &rest arguments)
                (declare (ignore client uri arguments))
                (push method methods)
                (http-test-response 200))))
        (funcall requester :head "https://origin.test/" 10d0)
        (expect (reverse methods) :to-equal (list "HEAD")))))
  (it "rejects userinfo and unsupported methods before resolving or sending"
    (let ((lookup-count 0)
          (send-count 0))
      (let ((requester
              (make-http-requester
               (http-test-resolver
                (http-test-public-address)
                (lambda (host timeout deadline)
                  (declare (ignore host timeout deadline))
                  (incf lookup-count))))))
        (let ((aitools.net.infrastructure::*http-client-send-function*
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (incf send-count)
                  (http-test-response 200))))
          (let ((userinfo-error
                  (http-test-error
                   (lambda ()
                     (funcall requester "GET"
                              "https://user@origin.test/" 10d0))))
                (method-error
                  (http-test-error
                   (lambda ()
                     (funcall requester "POST"
                              "https://origin.test/" 10d0)))))
            (expect (first userinfo-error) :to-equal "argument.invalid")
            (expect (first method-error) :to-equal "argument.invalid")
            (expect lookup-count :to-be 0)
            (expect send-count :to-be 0))))))
  (it "rejects a zero timeout before resolving"
    (let ((lookup-count 0)
          (send-count 0))
      (let ((requester
              (make-http-requester
               (http-test-resolver
                (http-test-public-address)
                (lambda (host timeout deadline)
                  (declare (ignore host timeout deadline))
                  (incf lookup-count))))))
        (let ((aitools.net.infrastructure::*http-client-send-function*
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (incf send-count)
                  (http-test-response 200))))
          (let ((failure
                  (http-test-error
                   (lambda ()
                     (funcall requester "GET" "https://origin.test/" 0d0)))))
            (expect (first failure) :to-equal "argument.invalid")
            (expect lookup-count :to-be 0)
            (expect send-count :to-be 0))))))
  (it "does not expose raw transport exceptions"
    (let ((requester
            (make-http-requester
             (http-test-resolver (http-test-public-address))))
          (failure nil))
      (let ((aitools.net.infrastructure::*http-client-send-function*
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (error "secret transport exception"))))
        (setf failure
              (http-test-error
               (lambda ()
                 (funcall requester "GET" "https://origin.test/" 10d0)))))
      (expect (first failure) :to-equal "environment.io")
      (expect (search "secret transport exception" (second failure))
              :to-equal nil)))
  (it "maps proxy errors unless their cause is internal.unexpected"
    (let ((requester
            (make-http-requester
             (http-test-resolver (http-test-public-address)))))
      (dolist (case
                (list
                 (cons :proxy-failure
                       '("environment.io" "The HTTP request failed."))
                 (cons
                  (make-condition 'aitools.net.domain:net-error
                                  :code "environment.io"
                                  :detail "resolver detail")
                  '("environment.io" "The HTTP request failed."))
                 (cons
                  (make-condition 'aitools.net.domain:net-error
                                  :code "internal.unexpected"
                                  :detail "internal detail")
                  '("internal.unexpected" "internal detail"))))
        (let* ((cause (car case))
               (expected (cdr case))
               (aitools.net.infrastructure::*http-client-send-function*
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (error
                    (make-condition
                     'http-kit/client:http-proxy-error
                     :message "fixture proxy failure"
                     :detail (list :cause cause)))))
               (failure
                 (http-test-error
                  (lambda ()
                    (funcall requester "GET" "https://origin.test/" 10d0)))))
          (expect failure :to-equal expected))))))

