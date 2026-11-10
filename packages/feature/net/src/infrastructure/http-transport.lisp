(in-package #:aitools.net.infrastructure)

(defconstant +max-http-body-bytes+ (* 16 1024 1024))

;; The sender seam keeps redirect policy tests entirely off the network.
(defvar *http-client-send-function* #'http-kit/client:http-client-send)

(defun %http-kit-address-opener ()
  (let* ((symbol (quote http-kit/network::%network-open-address))
         (function (and (fboundp symbol)
                        (symbol-function symbol)))
         (lambda-list
           (and function
                (handler-case
                    (sb-introspect:function-lambda-list function)
                  (error () nil)))))
    (unless
        (handler-case
            (and (listp lambda-list)
                 (= (length lambda-list) 4)
                 (every (lambda (parameter)
                          (and (symbolp parameter)
                               (not (member parameter
                                            (quote (&optional &rest &key
                                                    &allow-other-keys &aux))
                                            :test (function eq)))))
                        lambda-list))
          (error () nil))
      (%raise-net-error "internal.unexpected"
                        "The pinned HTTP address opener is missing or changed."))
    function))

(defun %open-inspected-address (address port deadline)
  ;; cl-http-kit 0.5.0 has no public opener for an inspected numeric address.
  (let* ((family (aitools.net.domain:net-address-family address))
         (address-type
           (ecase family
             (:inet http-kit/network::+network-ipv4-address-type+)
             (:inet6 http-kit/network::+network-ipv6-address-type+))))
    (funcall (%http-kit-address-opener)
             (list address-type (aitools.net.domain:net-address-octets address))
             port deadline (function %monotonic-time))))

(defun %open-inspected-addresses (addresses uri deadline)
  (let ((port (or (http-kit:http-uri-port uri)
                  (if (string-equal (http-kit:http-uri-scheme uri)
                                    "https")
                      443
                    80))))
    (loop for address in addresses
          do (handler-case
                 (return-from %open-inspected-addresses
                              (%open-inspected-address address port deadline))
               (http-kit:http-timeout (condition)
                                      (error condition))
               (net-error (condition) (error condition))
               (error () nil)))
    (error "No resolved address accepted the connection.")))

(defun %make-http-client (host addresses)
  (let ((tls-upgrade
         (http-kit/tls:make-http-tls-upgrader
          :verify :required
          :alpn-protocols (list "http/1.1")
          :clock-function (function %monotonic-time))))
    (http-kit/client:make-http-client
     :open-stream
     (lambda (request &key deadline &allow-other-keys)
       (let ((uri (http-kit:http-request-uri request)))
         (unless (string-equal host (http-kit:http-uri-host uri))
           (error "The transport host differs from its resolved host."))
         (%open-inspected-addresses addresses uri deadline)))
     :close-stream (function close)
     :tls-upgrade tls-upgrade
     :redirect-policy
     (http-kit/client:make-http-redirect-policy
      :max-redirects 0 :statuses nil :allow-downgrade-p nil)
     :retry-policy (http-kit/client:make-http-retry-policy :max-attempts 1)
     :proxy (constantly nil)
     :auth-provider nil
     :challenge-auth-provider nil
     :proxy-challenge-auth-provider nil
     :strict-transport-store nil
     :alternative-service-store nil
     :cache nil
     :default-headers (list (http-kit:make-http-header "Accept-Encoding" "gzip, deflate"))
     :default-timeout nil
     :wall-clock-function (function %monotonic-time)
     :max-body-bytes +max-http-body-bytes+
     :automatic-decompression-p t
     :content-decoders
     (remove-if-not
      (lambda (decoder)
        (member (car decoder) (list "gzip" "deflate") :test (function string=)))
      (http-kit/client:make-http-content-decoders)))))

(defun %supported-content-encoding-p (headers)
  (let ((coding-count 0))
    (loop for field in (http-kit:http-header-values headers "Content-Encoding")
          always
          (loop with start = 0
                for comma = (position #\, field :start start)
                for coding = (string-downcase
                              (string-trim (list #\Space #\Tab)
                                           (subseq field start comma)))
                do (incf coding-count)
                always (and (<= coding-count 2)
                            (member coding (list "gzip" "deflate")
                                    :test (function string=)))
                while comma
                do (setf start (1+ comma))))))

(defun %send-http-request (method uri addresses deadline)
  (let* ((client (%make-http-client (http-kit:http-uri-host uri) addresses))
         (response
          (funcall *http-client-send-function*
                   client method uri
                   :timeout nil
                   :deadline deadline
                   :on-response-headers
                   (lambda (status headers)
                     (declare (ignore status))
                     (unless (%supported-content-encoding-p headers)
                       (%raise-net-error
                        "environment.io"
                        "The HTTP response uses an unsupported content coding."))))))
    (when (<= deadline (%monotonic-time))
      (%raise-net-error "environment.timeout" "The request deadline expired."))
    (unless (%supported-content-encoding-p
             (http-kit:http-response-headers response))
      (%raise-net-error "environment.io"
                        "The HTTP response uses an unsupported content coding."))
    response))
