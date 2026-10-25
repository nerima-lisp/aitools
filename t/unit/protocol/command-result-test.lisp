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

  (it "rejects positional continuations at the public CPS boundary"
    (signals error
      (normalize-command-continuations
       (lambda (fields) fields)
       (lambda (fields) fields)
       (lambda (code message &rest options)
         (declare (ignore code message options))))))
  (it "accepts a continuation plist as one list argument"
    (let* ((on-ok (lambda (fields) fields))
           (on-partial (lambda (fields) fields))
           (on-error (lambda (code message &rest options)
                       (declare (ignore code message options))))
           (actual (normalize-command-continuations
                    (list :on-ok on-ok :on-partial on-partial :on-error on-error))))
      (expect (eq (getf actual :on-ok) on-ok) :to-be t)
      (expect (eq (getf actual :on-partial) on-partial) :to-be t)
      (expect (eq (getf actual :on-error) on-error) :to-be t)))

