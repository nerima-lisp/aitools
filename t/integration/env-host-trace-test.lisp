(in-package #:aitools.env.test)

(describe "aitools environment child-process trace span"
  (it "instruments env system-query process outcomes and preserves returned values"
    (let* ((host (host-ports))
           (sh (aitools.env.application:find-executable host "sh"))
           (sleep (aitools.env.application:find-executable host "sleep"))
           (observed nil)
           (ports
            (aitools.env.infrastructure:make-production-env-ports
             :with-span
             (lambda (kind name operation arguments &key outcome-function)
               (let* ((values (multiple-value-list (apply operation arguments)))
                      (outcome (funcall outcome-function values)))
                 (push (list kind name outcome) observed)
                 (values-list values)))))
           (run-program (aitools.env.application::env-ports-run-program ports)))
      (unless (and sh sleep) (cl-weave:skip "sh or sleep is not on PATH"))
      (expect (multiple-value-list
               (funcall run-program sh '("-c" "printf out; printf err >&2; exit 3") 10))
              :to-equal '(:exited 3 "out" "err"))
      (expect (nth-value 0 (funcall run-program sleep '("5") 1/5)) :to-be :timeout)
      (expect (multiple-value-list
               (funcall run-program "/nonexistent/aitools-no-such-program" '() 1))
              :to-equal '(:not-started nil nil nil))
      (expect (reverse observed)
              :to-equal '((:child_process "env.system-query" :ok)
                          (:child_process "env.system-query" :timeout)
                          (:child_process "env.system-query" :unavailable)))))
)