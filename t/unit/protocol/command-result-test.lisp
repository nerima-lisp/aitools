;;;; t/unit/protocol/command-result-test.lisp
(in-package #:aitools.protocol.test)

(describe "aitools.protocol.application call-with-command-result/k"
  (it "captures on-ok's argument as :ok fields"
    (let ((result (call-with-command-result/k
                   (lambda (&key on-ok on-partial on-error)
                     (declare (ignore on-partial on-error))
                     (funcall on-ok (list (cons "path" "a.lisp")))))))
      (expect (command-result-kind result) :to-be :ok)
      (expect (command-result-fields result) :to-equal (list (cons "path" "a.lisp")))))

  (it "captures on-partial's argument as :partial fields"
    (let ((result (call-with-command-result/k
                   (lambda (&key on-ok on-partial on-error)
                     (declare (ignore on-ok on-error))
                     (funcall on-partial (list (cons "truncated" t)))))))
      (expect (command-result-kind result) :to-be :partial)))

  (it "captures on-error's arguments as :error fields"
    (let ((result (call-with-command-result/k
                   (lambda (&key on-ok on-partial on-error)
                     (declare (ignore on-ok on-partial))
                     (funcall on-error "selection.no-match" "no match"
                              :candidates (list "a.lisp"))))))
      (expect (command-result-kind result) :to-be :error)
      (expect (getf (command-result-fields result) :code) :to-equal "selection.no-match")
      (expect (getf (command-result-fields result) :message) :to-equal "no match")
      (expect (getf (command-result-fields result) :candidates) :to-equal (list "a.lisp")))))

(describe "aitools.protocol.application unknown-command-error"
  (it "always returns argument.invalid with at least one repair"
    (multiple-value-bind (code message repairs) (unknown-command-error "cat")
      (expect code :to-equal "argument.invalid")
      (expect message :to-contain "cat")
      (expect repairs :not :to-be-falsy))))
