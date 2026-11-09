(in-package #:aitools.net.test)
(progn
  (defun resolver-edge-captured-error (thunk)
    (handler-case
        (progn (funcall thunk) :no-error)
      (net-error (condition)
        (list (net-error-code condition)
              (net-error-detail condition)))))

  #+(and sbcl sb-thread)
  (describe "aitools.net.infrastructure resolver edge cases"
    (it "uses the default OS resolver for numeric loopback"
      (let* ((resolver (make-os-resolver))
             (addresses (funcall resolver "127.0.0.1" :timeout 2d0))
             (ipv4 (find "127.0.0.1" addresses
                         :key #'net-address-value
                         :test #'equal)))
        (expect ipv4 :to-be-truthy)
        (expect (net-address-type ipv4) :to-equal "A")
        (expect (net-address-family ipv4) :to-be :inet)
        (expect (equalp (net-address-octets ipv4) #(127 0 0 1)) :to-be t)))

    (it "maps unexpected lookup conditions to environment.io"
      (let ((resolver
              (make-os-resolver
               :lookup-function
               (lambda (host)
                 (declare (ignore host))
                 (error "fake lookup failure")))))
        (expect (resolver-edge-captured-error
                 (lambda () (funcall resolver "lookup.example")))
                :to-equal
                (list "environment.io"
                      "The operating system resolver failed."))))

    (it "preserves net errors raised by the lookup worker"
      (let ((resolver
              (make-os-resolver
               :lookup-function
               (lambda (host)
                 (declare (ignore host))
                 (error 'net-error
                        :code "environment.timeout"
                        :detail "injected timeout")))))
        (expect (resolver-edge-captured-error
                 (lambda () (funcall resolver "lookup.example")))
                :to-equal
                (list "environment.timeout" "injected timeout"))))

    (it "applies the default deadline after a quick lookup completes"
      (let* ((clock-values (list 0d0 29d0 29d0 29d0 30d0))
             (lookups 0)
             (resolver
               (make-os-resolver
                :clock-function (lambda () (or (pop clock-values) 30d0))
                :lookup-function
                (lambda (host)
                  (declare (ignore host))
                  (incf lookups)
                  nil))))
        (expect (resolver-edge-captured-error
                 (lambda () (funcall resolver "lookup.example")))
                :to-equal
                (list "environment.timeout"
                      "Name resolution exceeded its deadline."))
        (expect lookups :to-be 1)))

    (it "rejects non-function resolver dependencies"
      (expect (resolver-edge-captured-error
               (lambda () (make-os-resolver :lookup-function 42)))
              :to-equal
              (list "argument.invalid"
                    "Resolver dependencies must be functions."))
      (expect (resolver-edge-captured-error
               (lambda () (make-os-resolver :clock-function 42)))
              :to-equal
              (list "argument.invalid"
                    "Resolver dependencies must be functions.")))

    (it "rejects malformed entries and unsupported address families"
      (dolist (answer (list (list :inet)
                            (list (cons :unsupported #(127 0 0 1)))))
        (let ((resolver
                (make-os-resolver
                 :lookup-function
                 (lambda (host)
                   (declare (ignore host))
                   answer))))
          (expect (resolver-edge-captured-error
                   (lambda () (funcall resolver "lookup.example")))
                  :to-equal
                  (list "environment.io"
                        (if (consp (first answer))
                            "The resolver returned an unsupported address family."
                            "The resolver returned an invalid address.")))))))
)
