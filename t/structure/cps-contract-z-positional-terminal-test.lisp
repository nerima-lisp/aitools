(in-package #:aitools.integration.cps-contract-test)

(describe "terminal positional callbacks"
          (it "detects positional continuations across terminal control forms"
              (let ((fixtures
                     '(("if" "(if value (funcall callback value) (funcall callback nil))")
                       ("return-from" "(return-from terminal-return-from (funcall callback value))")
                       ("cond" "(cond (value (funcall callback value)) (t (funcall callback nil)))")
                       ("case" "(case value (:one (funcall callback value)) (otherwise (funcall callback nil)))")
                       ("ecase" "(ecase value (:one (funcall callback value)) (:two (funcall callback nil)))")
                       ("typecase" "(typecase value (integer (funcall callback value)) (t (funcall callback nil)))")
                       ("etypecase"
                        "(etypecase value (integer (funcall callback value)) (string (funcall callback nil)))")
                       ("ccase" "(ccase value ((:one) (funcall callback value)) ((:two) (funcall callback nil)))")
                       ("ctypecase"
                        "(ctypecase value (integer (funcall callback value)) (string (funcall callback nil)))")
                       ("progn" "(progn :ready (funcall callback value))")
                       ("locally" "(locally (declare (optimize speed)) (funcall callback value))")
                       ("when" "(when value (funcall callback value))")
                       ("unless" "(unless value (funcall callback value))")
                       ("let" "(let ((copy value)) (funcall callback copy))")
                       ("let*" "(let* ((copy value)) (funcall callback copy))")
                       ("let-alias" "(let ((saved callback)) (funcall saved value))")
                       ("let*-alias" "(let* ((saved callback) (next saved)) (funcall next value))")
                       ("let-self-alias" "(let ((callback callback)) (funcall callback value))")
                       ("symbol-macrolet-alias" "(symbol-macrolet ((saved callback)) (funcall saved value))")
                       ("symbol-macrolet-shadow"
                        "(symbol-macrolet ((callback (lambda (item) (1+ item))))
                (funcall callback value))"
                        nil)
                       ("symbol-macrolet-alias-chain"
                        "(symbol-macrolet ((saved alias) (alias callback))
                (funcall saved value))")
                       ("symbol-macrolet-nested-shadow"
                        "(symbol-macrolet ((saved callback))
                (symbol-macrolet ((callback (lambda (item) (1+ item))))
                  (funcall saved value)))"
                        nil)
                       ("let-shadow" "(let ((callback (lambda (item) (1+ item)))) (funcall callback value))" nil)
                       ("let-alias-shadow"
                        "(let ((saved callback)) (let ((saved (lambda (item) (1+ item)))) (funcall saved value)))"
                        nil)
                       ("let*-alias-shadow"
                        "(let* ((saved callback) (saved (lambda (item) (1+ item)))) (funcall saved value))"
                        nil)
                       ("multiple-value-bind"
                        "(multiple-value-bind (copy present) (values value t) (funcall callback copy))")
                       ("multiple-value-bind-unrelated"
                        "(multiple-value-bind (saved ignored) (values value callback) (funcall saved value))"
                        nil)
                       ("multiple-value-bind-alias-second"
                        "(multiple-value-bind (ignored saved) (values :ignored callback) (funcall saved value))")
                       ("multiple-value-bind-alias"
                        "(multiple-value-bind (saved) (values callback) (funcall saved value))")
                       ("handler-case" "(handler-case value (error (condition) (funcall callback condition)))")
                       ("flow-call" "(flow-call value (:on-ok (result) (funcall callback result)))")
                       ("block" "(block terminal-block (funcall callback value))")
                       ("block-local-return" "(block b (return-from b (funcall callback value)))")
                       ("flet-deferred-return"
                        "(flet ((unused () (return-from
                    terminal-flet-deferred-return
                    (funcall callback value))))
               :done)"
                        nil)
                       ("flet-tail-callback" "(flet ((unused () :done)) (funcall callback value))")
                       ("labels-deferred-return"
                        "(labels ((unused () (return-from
                      terminal-labels-deferred-return
                      (funcall callback value))))
                :done)"
                        nil)
                       ("labels-tail-callback" "(labels ((unused () :done)) (funcall callback value))")
                       ("macrolet-discarded-return"
                        "(macrolet ((discard (&rest forms) nil))
                (discard
                 (return-from terminal-macrolet-discarded-return
                   (funcall callback value)))
                :done)"
                        nil)
                       ("macrolet-tail-callback"
                        "(macrolet ((discard (&rest forms) nil))
                (discard :ignored)
                (funcall callback value))")
                       ("macrolet-shadowed-when"
                        "(macrolet ((when (&rest forms) :done))
                (when value (funcall callback value)))"
                        nil)
                       ("block-shadow" "(progn (block b (return-from b (funcall callback value))) :done)" nil)
                       ("loop-return-from"
                        "(loop repeat 1 do (return-from terminal-loop-return-from (funcall callback value)))")
                       ("loop-return" "(loop repeat 1 do (return (funcall callback value)))")
                       ("loop-nonterminal" "(progn (loop repeat 1 do (return (funcall callback value))) :done)" nil)
                       ("loop-nested-return" "(loop repeat 1 do (loop repeat 1 do (return (funcall callback value))))"
                                             nil)
                       ("loop-block-nil-shadow" "(loop repeat 1 do (block nil (return (funcall callback value))))" nil)
                       ("loop-return-clause" "(loop repeat 1 return (funcall callback value))")
                       ("loop-return-clause-nonterminal" "(progn (loop repeat 1 return (funcall callback value)) :done)"
                                                         nil)
                       ("immediate-lambda" "(funcall (lambda () (funcall callback value)))")
                       ("immediate-lambda-return-from" "(funcall (lambda () (return-from b (funcall callback value))))")
                       ("immediate-lambda-nonterminal-return-from"
                        "(progn (+ 1 (funcall (lambda () (return-from
                       terminal-immediate-lambda-nonterminal-return-from
                       (funcall callback value))))) :done)")
                       ("immediate-lambda-tail-unknown-return-from"
                        "(+ 1 (funcall (lambda () (return-from
                 terminal-immediate-lambda-tail-unknown-return-from
                 (funcall callback value)))))")
                       ("immediate-lambda-nonterminal"
                        "(progn (+ 1 (funcall (lambda () (funcall callback value)))) :done)"
                        nil)
                       ("immediate-lambda-loop-return"
                        "(loop repeat 1 do (funcall (lambda () (return-from nil (funcall callback value)))))")
                       ("immediate-lambda-loop-nonterminal"
                        "(progn (loop do (funcall (lambda () (return-from nil (funcall callback value))))) :done)"
                        nil)
                       ("immediate-lambda-block-shadow"
                        "(progn
               (block terminal-immediate-lambda-block-shadow
                 (block terminal-immediate-lambda-block-shadow
                   (funcall (lambda ()
                              (return-from
                               terminal-immediate-lambda-block-shadow
                               (funcall callback value))))
                   :after-inner)
                 :after-outer))"
                        nil)
                       ("case-designator" "(funcall (case value (:one callback) (otherwise callback)) value)")
                       ("loop-designator" "(funcall (loop repeat 1 return callback) value)")
                       ("symbol-macrolet-let-captured-callback"
                        "(let ((saved callback))
   (symbol-macrolet ((callback (lambda (item) (1+ item))))
     (funcall saved value)))")
                       ("symbol-macrolet-let-shadow"
                        "(symbol-macrolet ((saved callback))
   (let ((callback (lambda (item) (1+ item))))
     (funcall saved value)))"
                        nil)
                       ("flet-shadow-computed-designator"
                        "(let ((designator
         (flet ((funcall (ignored argument)
                  (declare (ignore ignored argument))
                  :done))
           (funcall callback value))))
   (funcall designator value))"
                        nil)
                       ("case-keyform-nonterminal" "(case (progn :ready (funcall callback value))
   (:done :result))"
                        nil)
                       ("case-designator-keyform-nonterminal"
                        "(funcall
    (case (progn :ready (funcall callback value))
      (:done (lambda (item) (1+ item))))
    value)"
                        nil))))
                (dolist (fixture fixtures)
                  (let* ((name (first fixture))
                         (body (second fixture))
                         (violation-p
                          (if (cddr fixture)
                              (third fixture)
                              t))
                         (helper (format nil "TERMINAL-~A" (string-upcase name)))
                         (bad-source
                          (format nil
                                  (concatenate 'string
                                               "(defun ~A (callback value)~%  ~A)"
                                               "~%(define-flow (probe-~A value &key on-ok)~%  (~A"
                                               "~%       (lambda (item) (flow-continue :on-ok item)) value))")
                                  helper
                                  body
                                  name
                                  helper))
                         (good-source
                          (format nil
                                  (concatenate 'string
                                               "(defun ~A (callback value)~%  ~A)"
                                               "~%(define-flow (probe-~A value &key on-ok)~%  (~A"
                                               "~%       (lambda (item) (1+ item)) value))")
                                  helper
                                  body
                                  name
                                  helper)))
                    (expect (%fixture-violations bad-source)
                            :to-equal
                            (if violation-p
                                (list (list "fixture.lisp" helper :positional-continuation-defun))
                                nil))
                    (expect (%fixture-violations good-source) :to-equal nil)))))
          (it "ignores local function names that shadow callback operators"
              (expect
               (%fixture-violations
                "(defun flet-shadow-funcall (callback value)
         (flet ((funcall (ignored argument)
                  (declare (ignore ignored argument))
                  :done))
           (funcall callback value)))")
               :to-equal
               nil)))
