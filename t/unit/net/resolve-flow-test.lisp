(in-package #:aitools.net.test)
(defun run-resolve-flow (resolver host &rest options)
  (let ((result nil)
        (boundaries (cl-boundary-kit:make-boundary-context)))
    (setf boundaries
          (cl-boundary-kit:boundary-context-with
           boundaries :net-http-send
           (lambda (&rest ignored)
             (declare (ignore ignored))
             (error "HTTP request is not used by resolve tests."))))
    (setf boundaries
          (cl-boundary-kit:boundary-context-with
           boundaries :net-resolve resolver))
    (apply (function aitools.net.application:resolve/k)
           boundaries
           host
           :on-ok (lambda (fields)
                    (setf result (list :ok fields)))
           :on-error (lambda (code detail)
                       (setf result (list :error (list code detail))))
           options)
    (values-list result)))

  (defun make-resolve-test-address (type value family octets)
    (make-net-address :type type :value value :family family :octets octets))

  (defun resolve-test-record (name type value)
    (list (cons "name" name)
          (cons "type" type)
          (cons "value" value)))

  (describe "aitools.net.application resolve"
    (it "returns A and AAAA records with name, type, and numeric value"
      (let* ((host "lookup.example")
             (addresses
               (list (make-resolve-test-address "A" "192.0.2.1" :inet #(192 0 2 1))
                     (make-resolve-test-address "AAAA" "2001:db8::1" :inet6
                                                #(32 1 13 184 0 0 0 0 0 0 0 0 0 0 0 1))))
             (call nil)
             (resolver (lambda (seen-host &key timeout)
                         (setf call (list seen-host timeout))
                         addresses)))
        (multiple-value-bind (kind fields)
            (run-resolve-flow resolver host :timeout "1.5s")
          (expect kind :to-be :ok)
          (expect (mapcar #'car fields) :to-equal '("records"))
          (let ((records (cdr (assoc "records" fields :test #'string=))))
            (expect (every #'aitools.protocol.domain:json-object-p records)
                    :to-be t)
            (expect (mapcar #'aitools.protocol.domain:json-object-members records)
                    :to-equal
                    (list (resolve-test-record host "A" "192.0.2.1")
                          (resolve-test-record host "AAAA" "2001:db8::1"))))
          (expect call :to-equal (list host 3/2)))))

    (it "returns empty records for NODATA and uses the default timeout"
      (let* ((seen-timeout nil)
            (resolver (lambda (host &key timeout)
                        (declare (ignore host))
                        (setf seen-timeout timeout)
                        nil)))
        (multiple-value-bind (kind fields)
            (run-resolve-flow resolver "empty.example")
          (expect kind :to-be :ok)
          (expect (cdr (assoc "records" fields :test #'string=)) :to-equal nil)
          (expect seen-timeout :to-be 30))))

    (it "rejects invalid hosts and NUL before calling the resolver"
      (let* ((calls 0)
            (resolver (lambda (host &key timeout)
                        (declare (ignore host timeout))
                        (incf calls))))
        (dolist (host (list nil "" 42
                             (format nil "lookup~C.example" (code-char 0))))
          (multiple-value-bind (kind fields)
              (run-resolve-flow resolver host)
            (expect kind :to-be :error)
            (expect (first fields) :to-equal "argument.invalid")))
        (expect calls :to-be 0)))

    (it "rejects invalid, zero, and sub-millisecond timeouts"
      (let* ((calls 0)
            (resolver (lambda (host &key timeout)
                        (declare (ignore host timeout))
                        (incf calls))))
        (dolist (timeout '("invalid" "-1s" "0s" "0.1ms" nil 30))
          (multiple-value-bind (kind fields)
              (run-resolve-flow resolver "lookup.example" :timeout timeout)
            (expect kind :to-be :error)
            (expect (first fields) :to-equal "argument.invalid")))
        (expect calls :to-be 0)))

    (it "passes the smallest positive timeout in seconds"
  (let* ((seen-timeout nil)
        (resolver (lambda (host &key timeout)
                    (declare (ignore host))
                    (setf seen-timeout timeout)
                    nil)))
    (multiple-value-bind (kind fields)
        (run-resolve-flow resolver "lookup.example" :timeout "1ms")
      (declare (ignore fields))
      (expect kind :to-be :ok)
      (expect seen-timeout :to-equal 1/1000))))

    (it "preserves resolver error codes and details"
      (dolist (failure '(("environment.io" "resolver failed")
                         ("environment.timeout" "deadline")
                         ("argument.invalid" "invalid host")))
        (destructuring-bind (code detail) failure
          (let ((resolver
                  (lambda (host &key timeout)
                    (declare (ignore host timeout))
                    (error 'aitools.net.domain:net-error
                           :code code
                           :detail detail))))
            (multiple-value-bind (kind fields)
                (run-resolve-flow resolver "lookup.example")
              (expect kind :to-be :error)
              (expect (first fields) :to-equal code)
              (expect (second fields) :to-equal detail)))))))
