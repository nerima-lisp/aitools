(in-package #:aitools.integration.cps-contract-test)

(defparameter *legacy-cps-allowlist* (list *legacy-cps-slash-k-allowlist* *legacy-cps-on-key-allowlist*))

(describe "aitools CPS structure contract"
  (it "matches every legacy continuation definition to its exact allowlist key"
    (let* ((actual (%cps-contract-violations (repository-root)))
           (allowed (%allowlist-keys))
           (unexpected (set-difference actual allowed :test #'equal))
           (stale (set-difference allowed actual :test #'equal)))
      (progn
        (expect (count :slash-k-defun actual :key #'third) :to-equal 0)
        (expect (count :on-key-defun actual :key #'third) :to-equal 0)
        (expect (count :positional-continuation-defun actual :key #'third) :to-equal 0)
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
    (let ((source
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
(define-flow (mapped-function items &key on-ok)
  (flow-continue :on-ok (mapcar #'identity items)))
(define-flow (mapped-lambda items &key on-ok)
  (flow-continue :on-ok (mapcar (lambda (item) item) items)))
(defun map-with-function (function items)
  (mapcar function items))
(defun map-with-on-map (on-map items)
  (mapcar on-map items))
(defun boolean-on-error (on-error)
  (if on-error :enabled :disabled))
'(defun quoted/k (&key on-later)
  (return-from quoted/k nil))
(define-flow (quoted-flow &key on-ok)
  '(return-from quoted-flow nil)
  '(apply on-ok nil)
  (flow-continue :on-ok))"))
      (expect (%fixture-violations source) :to-equal nil)
      (expect (%fixture-positional-continuation-inventory source) :to-equal nil)
      (expect
       (%fixture-violations
        (concatenate 'string
                     "(define-flow (dotted-flow &key on-ok) "
                     "(destructuring-bind (key . value) (cons :key :value) "
                     "(flow-continue :on-ok value)))"))
       :to-equal nil)))

  (it "handles dotted destructuring in flow bodies"
    (expect (%fixture-violations
             "(define-flow (dotted-flow blocks &key on-ok) (loop for (start . end) in blocks collect (cons start end)))")
            :to-equal nil))

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

  (it "rejects continuation references in proper and dotted tails and uninterned operators"
    (expect (%fixture-violations
             "(define-flow (bad-quote-tail &key on-ok)
  (apply (foo quote on-ok) nil))
(define-flow (bad-function-tail &key on-error)
  (apply (foo function on-error) nil))
(define-flow (bad-dotted-tail &key on-later)
  (apply (foo quote . on-later) nil))")
            :to-equal (list (list "fixture.lisp" "BAD-QUOTE-TAIL" :positional-continuation)
                            (list "fixture.lisp" "BAD-FUNCTION-TAIL" :positional-continuation)
                            (list "fixture.lisp" "BAD-DOTTED-TAIL" :positional-continuation)))
    (expect (%fixture-violations
             "(define-flow (bad-uninterned-quote &key on-ok)
  (#:quote on-ok))
(define-flow (bad-uninterned-function &key on-error)
  (#:function on-error))
(define-flow (bad-uninterned-apply &key on-later)
  (#:apply on-later nil))
(define-flow (bad-uninterned-let &key on-value)
  (#:let ((on-value nil)) on-value))")
            :to-equal (list (list "fixture.lisp" "BAD-UNINTERNED-QUOTE" :positional-continuation)
                            (list "fixture.lisp" "BAD-UNINTERNED-FUNCTION" :positional-continuation)
                            (list "fixture.lisp" "BAD-UNINTERNED-APPLY" :positional-continuation)
                            (list "fixture.lisp" "BAD-UNINTERNED-LET" :positional-continuation))))
  (it "checks write-command CPS bodies"
    (expect
     (%fixture-violations
      "(define-write-command \"clean.command\"
  (ports env positionals options &key on-plan on-error)
  nil)")
     :to-equal nil)
    (expect
     (%fixture-violations
      "(define-write-command \"bad.command\"
  (ports env positionals options &key on-plan on-error)
  (helper on-plan))")
     :to-equal
     (list (list "fixture.lisp" "PREPARE-BAD-COMMAND/K" :positional-continuation)))
    (expect
     (%fixture-violations
      "(define-write-command \"bad.return\"
  (ports env positionals options &key on-plan on-error)
  (return-from prepare-bad-return/k nil))")
     :to-equal
     (list (list "fixture.lisp" "PREPARE-BAD-RETURN/K" :return-from))))
  (it "detects forwarding through a FLOW-CALL target"
    (let* ((inventory
             (%fixture-positional-continuation-inventory
              "(define-flow (terminal-target callback value &key on-result)
  (funcall callback value))
(define-flow (forward-through-flow-target callback value &key on-result)
  (flow-call (terminal-target callback value)
    (:on-result (result) (flow-continue :on-result result))))
(define-flow (caller value &key on-ok)
  (flow-call
   (forward-through-flow-target
    (lambda (item) (flow-continue :on-ok item)) value)
   (:on-result (result) (flow-continue :on-ok result))))"))
           (forwarder
             (find "FORWARD-THROUGH-FLOW-TARGET" inventory
                   :key (function (lambda (record) (getf record :symbol)))
                   :test (function string=))))
      (expect
       (equal (mapcar (function (lambda (record) (getf record :symbol))) inventory)
              (list "FORWARD-THROUGH-FLOW-TARGET" "TERMINAL-TARGET"))
       :to-equal t)
      (expect
       (equal (getf (first (getf forwarder :invocations)) :via)
              (list "TERMINAL-TARGET"))
       :to-equal t)
      (expect
       (equal
        (mapcar (function (lambda (site)
                            (list (getf site :call-form)
                                  (getf site :enclosing-symbol)
                                  (getf site :argument-shape))))
                (getf forwarder :callsites))
        (list (list :direct "CALLER" (list :lambda))))
       :to-equal t))))
