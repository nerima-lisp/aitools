(in-package #:aitools.net.test)

(describe "aitools net decoded body limit integration"
  (it "rejects a compressed body that expands beyond 16 MiB"
    (aitools.test.support:call-with-temporary-directory
     (lambda (directory)
       (multiple-value-bind (ca-certificate ca-key)
           (%https-test-certificate directory "trusted")
         (multiple-value-bind (server-certificate server-key)
             (%https-test-signed-certificate
              directory "trusted-server" ca-certificate ca-key
              "localhost" "DNS:localhost,IP:127.0.0.1")
           (let ((server nil)
                 (port nil))
             (unwind-protect
                  (progn
                    (multiple-value-setq (server port)
                      (%https-test-start-server server-certificate server-key))
                    (%https-test-call-with-trust-anchor
                     ca-certificate
                     (lambda ()
                       (multiple-value-bind (kind code)
                           (%https-test-outcome
                            (%https-test-requester) "GET"
                            (format nil "https://localhost:~D/compressed-large" port)
                            5d0)
                         (expect kind :to-be :error)
                         (expect code :to-equal "refusal.too-large")))))
               (%https-test-stop-server server)))))))))
