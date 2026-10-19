(in-package #:aitools.kernel.test)

(define-flow (flow-test-target value &key on-success on-error)
  (if value
      (flow-continue :on-success value :details :seen)
      (flow-continue :on-error "missing")))

(define-flow (flow-test-caller value &key on-success on-error)
  (flow-call (flow-test-target value)
    (:on-success (result &key details)
      (flow-continue :on-success result :details details))
    (:on-error (message)
      (flow-continue :on-error message))))

(describe "aitools.kernel.domain flow DSL"
  (it "passes named continuations and their positional and keyword values"
    (expect (flow-test-caller t
                              :on-success (lambda (value &key details)
                                            (list :success value details))
                              :on-error #'identity)
            :to-equal '(:success t :seen)))

  (it "routes the error clause through its named continuation"
    (expect (flow-test-caller nil :on-success #'identity :on-error #'list)
            :to-equal '("missing")))

  (it "checks every continuation at function entry"
    (signals type-error
      (flow-test-target t :on-success nil :on-error #'identity)))

  (it "rejects positional continuation declarations during macro expansion"
    (signals error
      (macroexpand-1 '(define-flow (flow-test-invalid on-success &key) nil))))

  (it "rejects undeclared continuation clauses during macro expansion"
    (signals error
      (macroexpand-1
       '(flow-call (flow-test-target t)
         (:on-missing (value) value)))))

  (it "rejects duplicate continuation declarations during macro expansion"
    (signals error
      (macroexpand-1
       '(define-flow (flow-test-invalid &key on-success (:on-success callback)) nil))))

  (it "rejects an undeclared flow-continue key during compilation"
    (multiple-value-bind (function warnings-p failure-p)
        (compile nil (quote (lambda ()
                              (define-flow (flow-test-invalid &key on-success)
                                (flow-continue :on-missing :value)))))
      (declare (ignore function warnings-p))
      (expect failure-p :to-be-truthy))))

(define-flow (flow-test-redefined value &key on-old)
  value)

(describe "aitools.kernel.domain flow macro boundaries"
  (it "rejects flow-continue outside a flow during macro expansion"
    (signals error
      (macroexpand-1 '(flow-continue :on-success :value))))

  (it "replaces a registered continuation signature when a flow is redefined"
    (macroexpand-1
     '(define-flow (flow-test-redefined value &key on-new)
       value))
    (expect (macroexpand-1
             '(flow-call (flow-test-redefined nil)
                (:on-new () :new)))
            :to-equal
            '(flow-test-redefined nil :on-new (lambda () :new)))
    (signals error
      (macroexpand-1
       '(flow-call (flow-test-redefined nil)
          (:on-old () :old))))))
