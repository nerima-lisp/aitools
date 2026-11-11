(in-package #:aitools.net.test)
(cl-weave:describe "aitools.net.application HTTP flow macro expansions"
  (cl-weave:it "expands every validation into an error continuation before the body"
    (let ((expansion
            (macroexpand-1
             (quote
              (aitools.net.application::%with-net-validation
               ((first-valid "first failure")
                ((second-valid-p) "second failure"))
               (run-request url))))))
      (cl-weave:expect expansion
                        :to-equal
                        (quote
                         (cond
                           ((not first-valid)
                            (aitools.kernel.domain:flow-continue
                             :on-error "argument.invalid" "first failure"))
                           ((not (second-valid-p))
                            (aitools.kernel.domain:flow-continue
                             :on-error "argument.invalid" "second failure"))
                           (t (run-request url)))))))
  (cl-weave:it "adds the generic I/O continuation only when details are supplied"
    (let ((without-detail
            (macroexpand-1
             (quote
              (aitools.net.application::%with-net-error-continuation
               ()
               (send-request url)))))
          (with-detail
            (macroexpand-1
             (quote
              (aitools.net.application::%with-net-error-continuation
               ("safe transport detail")
               (send-request url))))))
      (cl-weave:expect without-detail
                       :to-equal
                       (quote
                        (handler-case
                            (progn (send-request url))
                          (aitools.net.domain:net-error
                              (aitools.net.application::condition)
                            (aitools.kernel.domain:flow-continue
                             :on-error
                             (aitools.net.domain:net-error-code
                              aitools.net.application::condition)
                             (aitools.net.domain:net-error-detail
                              aitools.net.application::condition))))))
      (cl-weave:expect with-detail
                       :to-equal
                       (quote
                        (handler-case
                            (progn (send-request url))
                          (aitools.net.domain:net-error
                              (aitools.net.application::condition)
                            (aitools.kernel.domain:flow-continue
                             :on-error
                             (aitools.net.domain:net-error-code
                              aitools.net.application::condition)
                             (aitools.net.domain:net-error-detail
                              aitools.net.application::condition)))
                          (error ()
                            (aitools.kernel.domain:flow-continue
                             :on-error "environment.io" "safe transport detail"))))))))
