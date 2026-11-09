(in-package #:aitools.net.test)
(defparameter +resolver-test-ipv4+ #(192 0 2 1))
(defparameter +resolver-test-ipv6+ #(32 1 13 184 0 0 0 0 0 0 0 0 0 0 0 1))

(defun resolver-error-code (thunk)
  (handler-case
      (progn (funcall thunk) :no-error)
    (net-error (condition) (net-error-code condition))))

#+sbcl
  (defun resolver-that-signals (condition-class)
    (make-os-resolver
     :lookup-function
     (lambda (host)
       (declare (ignore host))
       (error (make-condition condition-class)))))

  (describe "aitools.net.infrastructure OS resolver"
    (it "maps and deduplicates IPv4 and IPv6 answers"
      (let* ((observed-host nil)
            (resolver
              (make-os-resolver
               :lookup-function
               (lambda (host)
                 (setf observed-host host)
                 (list (cons :inet (copy-seq +resolver-test-ipv4+))
                       (cons :inet (copy-seq +resolver-test-ipv4+))
                       (cons :inet6 (copy-seq +resolver-test-ipv6+)))))))
        (let ((addresses (funcall resolver "lookup.example" :timeout 2d0)))
          (expect observed-host :to-equal "lookup.example")
          (expect (length addresses) :to-be 2)
          (expect (mapcar #'net-address-type addresses) :to-equal '("A" "AAAA"))
          (expect (net-address-value (first addresses)) :to-equal "192.0.2.1")
          (expect (net-address-family (first addresses)) :to-be :inet)
          (expect (equalp (net-address-octets (first addresses))
                          +resolver-test-ipv4+)
                  :to-be t)
          (expect (net-address-value (second addresses))
                  :to-equal "2001:db8:0:0:0:0:0:1")
          (expect (net-address-family (second addresses)) :to-be :inet6)
          (expect (equalp (net-address-octets (second addresses))
                          +resolver-test-ipv6+)
                  :to-be t))))

    (it "returns an empty list for an empty answer"
      (let ((resolver
              (make-os-resolver
               :lookup-function (lambda (host) (declare (ignore host)) nil))))
        (expect (funcall resolver "empty.example") :to-equal nil)))

    #+sbcl
    (it "maps EAI_NONAME to no addresses"
      (expect (funcall (resolver-that-signals 'sb-bsd-sockets::host-not-found-error)
                       "missing.example")
              :to-equal nil))

    #+sbcl
    (it "maps temporary and permanent resolver failures to environment.io"
      (dolist (condition-class '(sb-bsd-sockets::try-again-error
                                 sb-bsd-sockets::no-recovery-error))
        (expect (resolver-error-code
                 (lambda ()
                   (funcall (resolver-that-signals condition-class)
                            "failure.example")))
                :to-equal "environment.io")))

    (it "accepts DNS names and numeric addresses through the fake lookup"
      (let* ((observed-hosts nil)
            (resolver
              (make-os-resolver
               :lookup-function
               (lambda (host)
                 (push host observed-hosts)
                 nil))))
        (dolist (host '("lookup.example" "192.0.2.1" "2001:db8::1"))
          (expect (funcall resolver host) :to-equal nil))
        (expect (nreverse observed-hosts)
                :to-equal '("lookup.example" "192.0.2.1" "2001:db8::1"))))

    (it "rejects malformed hosts before invoking the lookup"
      (let* ((lookups 0)
            (resolver
              (make-os-resolver
               :lookup-function
               (lambda (host) (declare (ignore host)) (incf lookups)))))
        (dolist (host (list
                        "" "bad host" "bad..example" "-bad.example"
                        "bad-.example" "999.0.0.1" nil
                        "1..2.3" "01.2.3.4" "192.0.2"
                        (concatenate
                         (quote string)
                         (make-string 63 :initial-element #\a) "."
                         (make-string 63 :initial-element #\b) "."
                         (make-string 63 :initial-element #\c) "."
                         (make-string 62 :initial-element #\d))))
          (expect (resolver-error-code (lambda () (funcall resolver host)))
                  :to-equal "argument.invalid"))
        (progn (expect lookups :to-be 0) (expect (aitools.net.infrastructure::%valid-ipv6-host-p "lookup.example") :to-be nil) (expect (aitools.net.infrastructure::%valid-dns-host-p (make-string 254 :initial-element #\a)) :to-be nil))))

    (it "rejects nonpositive timeouts and invalid deadlines"
      (let ((resolver
              (make-os-resolver
               :lookup-function (lambda (host) (declare (ignore host)) nil))))
        (progn
                (expect (resolver-error-code
                         (lambda ()
                           (funcall resolver "lookup.example" :timeout 0d0)))
                        :to-equal "argument.invalid")
                (expect (resolver-error-code
                         (lambda ()
                           (funcall resolver "lookup.example" :timeout "later")))
                        :to-equal "argument.invalid"))
        (expect (resolver-error-code
                 (lambda () (funcall resolver "lookup.example" :deadline "later")))
                :to-equal "argument.invalid")))

    (it "reports an unavailable monotonic clock"
      (let ((resolver
              (make-os-resolver
               :lookup-function (lambda (host) (declare (ignore host)) nil)
               :clock-function (constantly nil))))
        (expect (resolver-error-code (lambda () (funcall resolver "lookup.example")))
                :to-equal "environment.io")))

    (it "rejects an expired deadline before invoking the lookup"
      (let* ((lookups 0)
            (resolver
              (make-os-resolver
               :lookup-function
               (lambda (host) (declare (ignore host)) (incf lookups))
               :clock-function (constantly 10d0))))
        (expect (resolver-error-code
                 (lambda () (funcall resolver "lookup.example" :deadline 9d0)))
                :to-equal "environment.timeout")
        (expect lookups :to-be 0)))

    (it "maps malformed resolver results to environment.io"
      (dolist (answer '(42
                        ((:other . #(1 2 3 4)))
                        ((:inet . #(192 0 2)))
                        ((:inet . #(256 0 2 1)))
                        ((:inet . #(192 "bad" 2 1)))
                        ((:inet . not-a-vector))))
        (let ((resolver
                (make-os-resolver
                 :lookup-function (lambda (host) (declare (ignore host)) answer))))
          (expect (resolver-error-code (lambda () (funcall resolver "lookup.example")))
                  :to-equal "environment.io"))))
    #+sbcl
(it "rejects NaN and minimum finite resolver values"
  (let ((nan (sb-kernel:make-double-float #x7ff80000 0)))
    (expect (aitools.net.infrastructure::%finite-real-p
             most-negative-double-float)
            :to-be nil)
    (expect (aitools.net.infrastructure::%finite-real-p nan)
            :to-be nil)
    (expect (aitools.net.infrastructure::%valid-ipv6-host-p "not-ipv6")
            :to-be nil)))

    )
