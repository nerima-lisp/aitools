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
        (expect (count :slash-k-defun actual :key #'third) :to-equal 89)
        (expect (count :on-key-defun actual :key #'third) :to-equal 78)
        ;; RUN-COMMAND/K is a migrated DEFINE-FLOW, not a legacy /k DEFUN.
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
    (expect
     (%fixture-violations
      "(define-flow (callee value &key on-ok on-error)
  (if value
      (flow-continue :on-ok value)
      (flow-continue :on-error :missing)))
(define-flow (caller value &key on-ok on-error)
  (flow-call
   (callee value)
   (:on-ok (result)
     (flow-continue :on-ok result))
   (:on-error (reason)
     (flow-continue :on-error reason))))
'(defun quoted/k (&key on-later)
  (return-from quoted/k nil))
(define-flow (quoted-flow &key on-ok)
  '(return-from quoted-flow nil)
  '(apply on-ok nil)
  (flow-continue :on-ok))")
     :to-equal nil)
    (expect
     (%fixture-violations
      (concatenate 'string
                   "(define-flow (dotted-flow &key on-ok) "
                   "(destructuring-bind (key . value) (cons :key :value) "
                   "(flow-continue :on-ok value)))"))
     :to-equal nil))

  (it "rejects new legacy continuation definitions"
    (expect (%fixture-violations
             "(defun new/k (value) value) (defun new-helper (&key on-ok) (funcall on-ok)) (defun (setf new-setter/k) (value object) (setf object value))")
            :to-equal (list (list "fixture.lisp" "NEW/K" :slash-k-defun)
                            (list "fixture.lisp" "NEW-HELPER" :on-key-defun)
                            (list "fixture.lisp" "NEW-SETTER/K" :slash-k-defun))))

  (it "accepts the command-result boundary adapter in its declared package"
    (expect (%fixture-violations
             "(in-package #:aitools.protocol.application) (defun call-with-command-result/k (flow-function) flow-function)"
             "packages/core/protocol/src/application/command-result.lisp")
            :to-equal nil))

  (it "rejects another slash-k definition in the adapter package"
    (expect (%fixture-violations
             "(in-package #:aitools.protocol.application) (defun alias/k (flow-function) flow-function)"
             "packages/core/protocol/src/application/command-result.lisp")
            :to-equal (list (list "packages/core/protocol/src/application/command-result.lisp"
                                  "ALIAS/K" :slash-k-defun))))

  (it "rejects the adapter symbol in a different package"
    (expect (%fixture-violations
             "(in-package #:aitools.protocol.application.other) (defun call-with-command-result/k (flow-function) flow-function)"
             "packages/core/protocol/src/application/command-result.lisp")
            :to-equal (list (list "packages/core/protocol/src/application/command-result.lisp"
                                  "CALL-WITH-COMMAND-RESULT/K" :slash-k-defun)))
    (expect (%fixture-violations
             "(in-package #:aitools.protocol.application) (defun cl-user::call-with-command-result/k (flow-function) flow-function)"
             "packages/core/protocol/src/application/command-result.lisp")
            :to-equal (list (list "packages/core/protocol/src/application/command-result.lisp"
                                  "CALL-WITH-COMMAND-RESULT/K" :slash-k-defun)))
    (expect (%fixture-violations
             "(in-package #:aitools.protocol.application) (defun aitools.cps.contract.reader::call-with-command-result/k (flow-function) flow-function)"
             "packages/core/protocol/src/application/command-result.lisp")
            :to-equal (list (list "packages/core/protocol/src/application/command-result.lisp"
                                  "CALL-WITH-COMMAND-RESULT/K" :slash-k-defun))))

  (it "rejects spoofed declarations and ignores a non-defun form"
    (expect (%fixture-violations
             "(in-package #:aitools.protocol.application) (defun call-with-command-result/k (flow-function) flow-function)")
            :to-equal (list (list "fixture.lisp" "CALL-WITH-COMMAND-RESULT/K" :slash-k-defun)))
    (expect (%fixture-violations
             "(in-package #:aitools.protocol.application) (defun |call-with-command-result/k| (flow-function) flow-function)"
             "packages/core/protocol/src/application/command-result.lisp")
            :to-equal (list (list "packages/core/protocol/src/application/command-result.lisp"
                                  "CALL-WITH-COMMAND-RESULT/K" :slash-k-defun)))
    (expect (%fixture-violations
             "(:in-package #:aitools.protocol.application) (defun call-with-command-result/k (flow-function) flow-function)"
             "packages/core/protocol/src/application/command-result.lisp")
            :to-equal (list (list "packages/core/protocol/src/application/command-result.lisp"
                                  "CALL-WITH-COMMAND-RESULT/K" :slash-k-defun)))
    (expect (%fixture-violations
             "(in-package #:aitools.protocol.application) (:defun call-with-command-result/k (flow-function) flow-function)"
             "packages/core/protocol/src/application/command-result.lisp")
            :to-equal nil))

  (it "rejects return-from in a flow, including a nested lambda"
    (expect (%fixture-violations
             "(define-flow (bad-flow &key on-ok) (lambda () (return-from bad-flow nil)))")
            :to-equal '(("fixture.lisp" "BAD-FLOW" :return-from))))

  (it "rejects positional forwarding and apply, including quote-like tails"
    (expect (%fixture-violations
             "(define-flow (bad-positional &key on-ok) (helper on-ok)) (define-flow (bad-apply &key on-error) (apply on-error nil))")
            :to-equal '(("fixture.lisp" "BAD-POSITIONAL" :positional-continuation)
                        ("fixture.lisp" "BAD-APPLY" :apply-continuation)))
    (expect (%fixture-violations
             "(define-flow (bad-quoted-tail &key on-ok) (helper :quote on-ok)) (define-flow (bad-apply-quoted-tail &key on-error) (apply (helper :quote on-error) nil))")
            :to-equal '(("fixture.lisp" "BAD-QUOTED-TAIL" :positional-continuation)
                        ("fixture.lisp" "BAD-APPLY-QUOTED-TAIL" :positional-continuation))))

  (it "rejects continuation references in proper and dotted list tails"
    (expect (%fixture-violations
             "(define-flow (bad-quote-tail &key on-ok)
  (apply (foo quote on-ok) nil))
(define-flow (bad-function-tail &key on-error)
  (apply (foo function on-error) nil))
(define-flow (bad-dotted-tail &key on-later)
  (apply (foo quote . on-later) nil))")
            :to-equal '(("fixture.lisp" "BAD-QUOTE-TAIL" :apply-continuation)
                        ("fixture.lisp" "BAD-FUNCTION-TAIL" :apply-continuation)
                        ("fixture.lisp" "BAD-DOTTED-TAIL" :apply-continuation)))))
