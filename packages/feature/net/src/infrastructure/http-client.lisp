(in-package #:aitools.net.infrastructure)
  (defconstant +max-redirects+ 5)
  (defparameter +redirect-statuses+ (list 301 302 303 307 308))

(defun %normalized-method (method)
  (let ((name (typecase method
                (string method)
                (symbol (symbol-name method)))))
    (when name
      (let ((normalized (string-upcase name)))
        (when (member normalized '("GET" "HEAD") :test #'string=)
          normalized)))))

(defun %valid-http-uri-p (uri)
  (and (http-kit:http-uri-p uri)
       (member (http-kit:http-uri-scheme uri) '("http" "https")
               :test #'string-equal)
       (let ((authority (http-kit:http-uri-authority uri))
             (host (http-kit:http-uri-host uri)))
         (and (stringp authority)
              (not (find #\@ authority))
              (stringp host)
              (plusp (length host))))))

(defun %parse-request-uri (uri)
  (handler-case
      (let ((parsed (if (http-kit:http-uri-p uri)
                        uri
                      (http-kit:parse-http-uri uri))))
        (unless (%valid-http-uri-p parsed)
          (%raise-net-error "argument.invalid" "The request URL is invalid."))
        parsed)
    (net-error (condition)
               (error condition))
    (error ()
           (%raise-net-error "argument.invalid" "The request URL is invalid."))))

(defun %redirect-target (uri response)
  (let ((status (http-kit:http-response-status response))
        (locations
         (http-kit:http-header-values
          (http-kit:http-response-headers response) "Location")))
    (when (and (member status +redirect-statuses+ :test #'eql)
               (= (length locations) 1))
      (handler-case
          (let ((target
                 (http-kit/client:resolve-http-uri uri (first locations))))
            (and (%valid-http-uri-p target)
                 (not (and (string-equal
                            (http-kit:http-uri-scheme uri) "https")
                           (string-equal
                            (http-kit:http-uri-scheme target) "http")))
                 target))
        (error () nil)))))

(defun %response-plist (response uri)
  (list :status (http-kit:http-response-status response)
        :headers
        (loop for header in (http-kit:http-response-headers response)
              for name = (http-kit:http-header-name header)
              unless (string-equal name "Location")
              collect (cons name (http-kit:http-header-content header)))
        :body (or (http-kit:http-response-body response)
                  (make-array 0 :element-type '(unsigned-byte 8)))
        :url (http-kit:http-uri-string uri)))

(defun %perform-http-request (resolver method uri deadline)
  (let* ((current-uri uri)
         (addresses (%resolve-addresses resolver current-uri deadline)))
    (unless addresses
      (%raise-net-error "environment.io" "The request host did not resolve."))
    (let ((public-origin-p (some (function %public-address-p) addresses))
          (redirect-count 0))
      ;; Do not fall back to a private answer when the initial hostname is public.
      (when public-origin-p
        (setf addresses
              (remove-if-not (function %public-address-p) addresses)))
      (loop
       for response = (%send-http-request
                       method current-uri addresses deadline)
       for target = (%redirect-target current-uri response)
       do (when (or (null target)
                    (>= redirect-count +max-redirects+))
            (return (%response-plist response current-uri)))
       (let ((next-addresses
              (%resolve-addresses resolver target deadline)))
         (unless next-addresses
           (return (%response-plist response current-uri)))
         (when (and public-origin-p
                    (not (every (function %public-address-p) next-addresses)))
           (return (%response-plist response current-uri)))
         (setf current-uri target
               addresses next-addresses)
         (incf redirect-count))))))

(defun make-http-requester (resolve-host)
  (unless (functionp resolve-host)
    (%raise-net-error "argument.invalid" "A host resolver function is required."))
  (lambda (method uri timeout-seconds)
    (handler-case
        (let ((normalized-method (%normalized-method method))
              (parsed-uri (%parse-request-uri uri))
              (deadline (%request-deadline timeout-seconds)))
          (unless normalized-method
            (%raise-net-error "argument.invalid"
                              "Only GET and HEAD requests are supported."))
          (%perform-http-request resolve-host normalized-method
                                 parsed-uri deadline))
      (net-error (condition)
                 (error condition))
      (http-kit:http-timeout (condition)
                             (declare (ignore condition))
                             (%raise-net-error "environment.timeout" "The HTTP request timed out."))
      (http-kit:http-size-limit-exceeded (condition)
                                         (declare (ignore condition))
                                         (%raise-net-error "refusal.too-large"
                                                           "The HTTP response exceeded its size limit."))
      (http-kit:http-unsupported-feature (condition)
                                         (declare (ignore condition))
                                         (%raise-net-error "environment.unavailable"
                                                           "The HTTP transport feature is unavailable."))
      (error (condition)
             (declare (ignore condition))
             (%raise-net-error "environment.io" "The HTTP request failed.")))))
