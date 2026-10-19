;;;; t/structure/cps-contract-test.lisp
(in-package #:aitools.integration.cps-contract-test)

(defparameter *legacy-cps-allowlist* (list *legacy-cps-slash-k-allowlist* *legacy-cps-on-key-allowlist*))

(describe "aitools CPS structure contract"
  (it "matches every legacy continuation definition to its exact allowlist key"
    (let* ((actual (%cps-contract-violations (repository-root)))
           (allowed (%allowlist-keys))
           (unexpected (set-difference actual allowed :test #'equal))
           (stale (set-difference allowed actual :test #'equal)))
      (progn
        (expect (not (null actual)) :to-equal t)
        (expect (count :slash-k-defun actual :key #'third) :to-equal 167)
        (expect (count :on-key-defun actual :key #'third) :to-equal 156)
        (expect
         (member '("packages/feature/process/src/application/run-flow.lisp"
                   "RUN-COMMAND/K" :slash-k-defun)
                 actual :test #'equal)
         :to-equal nil)
        (expect
         (member '("packages/feature/process/src/application/run-flow.lisp"
                   "RUN-COMMAND/K" :slash-k-defun)
                 allowed :test #'equal)
         :to-equal nil))
      (expect unexpected :to-equal nil)
      (expect stale :to-equal nil)))

  (it "passes named continuation flows and quoted data"
    (expect (%fixture-violations
             "(define-flow (callee value &key on-ok on-error) (if value (flow-continue :on-ok value) (flow-continue :on-error :missing))) (define-flow (caller value &key on-ok on-error) (flow-call (callee value) (:on-ok (result) (flow-continue :on-ok result)) (:on-error (reason) (flow-continue :on-error reason)))) '(defun quoted/k (&key on-later) (return-from quoted/k nil)) (define-flow (quoted-flow &key on-ok) '(return-from quoted-flow nil) '(apply on-ok nil) (flow-continue :on-ok))")
            :to-equal nil))

  (it "rejects new legacy continuation definitions"
    (expect (%fixture-violations
             "(defun new/k (value) value) (defun new-helper (&key on-ok) (funcall on-ok))")
            :to-equal '(( "fixture.lisp" "NEW/K" :slash-k-defun)
                        ("fixture.lisp" "NEW-HELPER" :on-key-defun))))

  (it "rejects return-from in a flow, including a nested lambda"
    (expect (%fixture-violations
             "(define-flow (bad-flow &key on-ok) (lambda () (return-from bad-flow nil)))")
            :to-equal '(("fixture.lisp" "BAD-FLOW" :return-from))))

  (it "rejects positional forwarding and apply of a flow continuation"
    (expect (%fixture-violations
             "(define-flow (bad-positional &key on-ok) (helper on-ok)) (define-flow (bad-apply &key on-error) (apply on-error nil))")
            :to-equal '(("fixture.lisp" "BAD-POSITIONAL" :positional-continuation)
                        ("fixture.lisp" "BAD-APPLY" :apply-continuation)))))
