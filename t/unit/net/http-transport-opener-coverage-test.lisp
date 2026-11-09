(in-package #:aitools.net.test)
(cl-weave:describe "aitools.net.infrastructure numeric opener validation"
  (cl-weave:it "rejects malformed opener lambda lists and introspection errors"
    (let* ((uri (http-kit:parse-http-uri "http://origin.test/"))
           (address (http-test-loopback-address)))
      (flet ((failure-for (lambda-list)
               (cl-weave:with-mocked-functions
                   (((symbol-function
                      (quote sb-introspect:function-lambda-list))
                     (lambda (function)
                       (declare (ignore function))
                       lambda-list)))
                 (http-test-error
                  (lambda ()
                    (aitools.net.infrastructure::%open-inspected-addresses
                     (list address) uri 3d0))))))
        (dolist (lambda-list
                 (list (list (quote address-entry) (quote port)
                             (quote &optional) (quote deadline))
                       (list (quote address-entry) (quote port) 42
                             (quote clock-function))
                       :not-a-list
                       (cons (quote address-entry) (quote port))))
          (let ((failure (failure-for lambda-list)))
            (expect (first failure) :to-equal "internal.unexpected")
            (expect (second failure) :to-be-truthy)))
        (cl-weave:with-mocked-functions
            (((symbol-function
               (quote sb-introspect:function-lambda-list))
              (lambda (function)
                (declare (ignore function))
                (error "The opener lambda list is unavailable."))))
          (let ((failure
                  (http-test-error
                   (lambda ()
                     (aitools.net.infrastructure::%open-inspected-addresses
                      (list address) uri 3d0)))))
            (expect (first failure) :to-equal "internal.unexpected")
            (expect (second failure) :to-be-truthy))))))
  (cl-weave:it "fails closed when the pinned numeric opener is unavailable"
    (let* ((opener-symbol (quote http-kit/network::%network-open-address))
           (original-opener
             (and (fboundp opener-symbol)
                  (symbol-function opener-symbol)))
           (uri (http-kit:parse-http-uri "http://origin.test/"))
           (address (http-test-loopback-address))
           (failure nil))
      (expect (fboundp opener-symbol) :to-be-truthy)
      (unwind-protect
           (progn
             (fmakunbound opener-symbol)
             (setf failure
                   (http-test-error
                    (lambda ()
                      (aitools.net.infrastructure::%open-inspected-addresses
                       (list address) uri 3d0))))
             (expect (first failure) :to-equal "internal.unexpected"))
        (when original-opener
          (setf (symbol-function opener-symbol) original-opener)))
      (expect (fboundp opener-symbol) :to-be-truthy)
      (expect (symbol-function opener-symbol) :to-be original-opener))))
