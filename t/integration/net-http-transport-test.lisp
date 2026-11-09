(in-package #:aitools.net.test)

(defun %https-test-stop-server (process)
  (when process
    (when (sb-ext:process-alive-p process)
      (sb-ext:process-kill process 15)
      (sb-ext:process-wait process))
    (sb-ext:process-close process)))

(defun %https-test-certificate (directory name)
  (let ((certificate (merge-pathnames (format nil "~A.crt" name) directory))
        (private-key (merge-pathnames (format nil "~A.key" name) directory)))
    (let ((process
            (sb-ext:run-program
             "openssl"
             (list "req" "-x509" "-newkey" "rsa:2048" "-nodes"
                   "-keyout" (namestring private-key)
                   "-out" (namestring certificate)
                   "-days" "1"
                   "-subj" "/CN=localhost"
                   "-addext" "subjectAltName=DNS:localhost,IP:127.0.0.1"
                   "-addext" "basicConstraints=critical,CA:TRUE"
                   "-addext" "keyUsage=critical,digitalSignature,keyEncipherment,keyCertSign"
                   "-addext" "extendedKeyUsage=serverAuth")
             :search t :wait t :output nil :error nil)))
      (unwind-protect
          (unless (eql (sb-ext:process-exit-code process) 0)
            (error "OpenSSL could not create the HTTPS fixture certificate."))
        (sb-ext:process-close process)))
    (values certificate private-key)))
(defun %https-test-signed-certificate (directory name ca-certificate ca-key)
  (let ((certificate (merge-pathnames (format nil "~A.crt" name) directory))
        (csr (merge-pathnames (format nil "~A.csr" name) directory))
        (private-key (merge-pathnames (format nil "~A.key" name) directory))
        (extension-file
         (merge-pathnames (format nil "~A-extensions.cnf" name) directory)))
    (with-open-file (stream extension-file :direction :output :if-exists :supersede)
      (write-line "[server_cert]" stream)
      (write-line "subjectAltName=DNS:other.test" stream)
      (write-line "basicConstraints=critical,CA:FALSE" stream)
      (write-line "keyUsage=critical,digitalSignature,keyEncipherment" stream)
      (write-line "extendedKeyUsage=serverAuth" stream))
    (labels ((run-openssl (arguments)
               (let ((process
                       (sb-ext:run-program "openssl" arguments
                                           :search t :wait t :output nil :error nil)))
                 (unwind-protect
                     (unless (eql (sb-ext:process-exit-code process) 0)
                       (error "OpenSSL could not create the signed HTTPS fixture certificate."))
                   (sb-ext:process-close process)))))
      (run-openssl
       (list "req" "-new" "-newkey" "rsa:2048" "-nodes"
             "-keyout" (namestring private-key)
             "-out" (namestring csr)
             "-subj" "/CN=other.test"))
      (run-openssl
       (list "x509" "-req" "-in" (namestring csr)
             "-CA" (namestring ca-certificate)
             "-CAkey" (namestring ca-key)
             "-CAcreateserial"
             "-out" (namestring certificate)
             "-days" "1"
             "-extfile" (namestring extension-file)
             "-extensions" "server_cert")))
    (values certificate private-key)))

(defun %https-test-start-server (certificate private-key)
  (let* ((script
           (asdf:system-relative-pathname
            "aitools" "t/integration/fixtures/net-https-server.py"))
         (process
           (sb-ext:run-program
            "python3"
            (list (namestring script)
                  (namestring certificate)
                  (namestring private-key))
            :search t :wait nil :output :stream :error nil))
         (keep-process nil))
    (unwind-protect
        (let* ((port-line (read-line (sb-ext:process-output process) nil nil))
               (port (parse-integer port-line)))
          (setf keep-process t)
          (values process port))
      (unless keep-process
        (%https-test-stop-server process)))))

(defun %https-test-requester ()
  (let ((address
          (make-net-address :type "A" :value "127.0.0.1"
                            :family :inet :octets #(127 0 0 1))))
    (make-http-requester
     (lambda (host &key timeout deadline)
       (declare (ignore timeout deadline))
       (unless (or (string= host "localhost")
                   (string= host "127.0.0.1"))
         (error "The HTTPS fixture received an unexpected host."))
       (list address)))))

(defun %https-test-outcome (requester method uri timeout)
  (handler-case
      (values :ok (funcall requester method uri timeout))
    (net-error (condition)
      (values :error (net-error-code condition)))))

(defun %https-test-call-with-trust-anchor (certificate function)
  (let ((previous (sb-posix:getenv "SSL_CERT_FILE")))
    (unwind-protect
        (progn
          (sb-posix:setenv "SSL_CERT_FILE" (namestring certificate) 1)
          (funcall function))
      (if previous
          (sb-posix:setenv "SSL_CERT_FILE" previous 1)
          (sb-posix:unsetenv "SSL_CERT_FILE")))))

(describe "aitools net HTTPS transport integration"
  (it "enforces the trusted CA, host identity, timeout, and body limit"
    (aitools.test.support:call-with-temporary-directory
     (lambda (directory)
       (multiple-value-bind (trusted-certificate trusted-key)
           (%https-test-certificate directory "trusted")
         (multiple-value-bind (mismatch-certificate mismatch-key)
             (%https-test-signed-certificate
              directory "mismatch" trusted-certificate trusted-key)
           (multiple-value-bind (untrusted-certificate untrusted-key)
               (%https-test-certificate directory "untrusted")
             (let ((trusted-server nil)
                   (trusted-port nil)
                   (mismatch-server nil)
                   (mismatch-port nil)
                   (untrusted-server nil)
                   (untrusted-port nil))
               (unwind-protect
                    (progn
                      (multiple-value-setq (trusted-server trusted-port)
                        (%https-test-start-server trusted-certificate trusted-key))
                      (multiple-value-setq (mismatch-server mismatch-port)
                        (%https-test-start-server mismatch-certificate mismatch-key))
                      (multiple-value-setq (untrusted-server untrusted-port)
                        (%https-test-start-server untrusted-certificate untrusted-key))
                      (%https-test-call-with-trust-anchor
                       trusted-certificate
                       (lambda ()
                         (let ((requester (%https-test-requester)))
                           (multiple-value-bind (kind response)
                               (%https-test-outcome
                                requester "GET"
                                (format nil "https://localhost:~D/ok" trusted-port)
                                5d0)
                             (expect kind :to-be :ok)
                             (expect (getf response :status) :to-be 200)
                             (expect (sb-ext:octets-to-string
                                      (getf response :body) :external-format :utf-8)
                                     :to-equal (format nil "loopback response~%")))
                           (multiple-value-bind (kind response)
                               (%https-test-outcome
                                requester "HEAD"
                                (format nil "https://localhost:~D/ok" trusted-port)
                                5d0)
                             (expect kind :to-be :ok)
                             (expect (getf response :status) :to-be 200)
                             (expect (length (getf response :body)) :to-be 0))
                           (multiple-value-bind (kind response)
                               (%https-test-outcome
                                requester "GET"
                                (format nil "https://127.0.0.1:~D/ok" trusted-port)
                                5d0)
                             (expect kind :to-be :ok)
                             (expect (getf response :status) :to-be 200))
                           (multiple-value-bind (kind code)
                               (%https-test-outcome
                                requester "GET"
                                (format nil "https://localhost:~D/delay" trusted-port)
                                0.1d0)
                             (expect kind :to-be :error)
                             (expect code :to-equal "environment.timeout"))
                           (multiple-value-bind (kind code)
                               (%https-test-outcome
                                requester "GET"
                                (format nil "https://localhost:~D/large" trusted-port)
                                5d0)
                             (expect kind :to-be :error)
                             (expect code :to-equal "refusal.too-large"))
                           (multiple-value-bind (kind code)
                               (%https-test-outcome
                                requester "GET"
                                (format nil "https://localhost:~D/ok" mismatch-port)
                                5d0)
                             (expect kind :to-be :error)
                             (expect code :to-equal "environment.io"))
                           (multiple-value-bind (kind code)
                               (%https-test-outcome
                                requester "GET"
                                (format nil "https://localhost:~D/ok" untrusted-port)
                                5d0)
                             (expect kind :to-be :error)
                             (expect code :to-equal "environment.io"))))))
                 (%https-test-stop-server trusted-server)
                 (%https-test-stop-server mismatch-server)
                 (%https-test-stop-server untrusted-server))))))))
    :prefix "aitools-net-http"))

