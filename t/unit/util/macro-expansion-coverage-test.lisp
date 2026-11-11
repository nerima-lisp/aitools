(in-package #:aitools.util.test)

(describe "aitools.util.domain %with-depth macro expansion"
  (it "preserves the generated guard and cleanup around the body"
    (let ((expansion
            (macroexpand-1
             '(aitools.util.domain::%with-depth (parser offset)
                (list :body)))))
      (expect expansion
              :to-equal
              '(progn
                 (when (> (incf (aitools.util.domain::%parser-depth parser))
                          aitools.util.domain::+calc-max-depth+)
                   (aitools.util.domain::%calc-fail
                    offset
                    (format nil "nesting exceeds ~D levels"
                            aitools.util.domain::+calc-max-depth+)))
                 (multiple-value-prog1
                     (progn (list :body))
                   (decf (aitools.util.domain::%parser-depth parser))))))))

(describe "aitools.util.presentation %with-command-result macro expansion"
  (it "binds command continuations and preserves the command body"
    (let* ((body '(list :body on-ok on-error))
           (expansion
             (macroexpand-1
              `(aitools.util.presentation::%with-command-result
                   (on-ok on-error)
                 ,body)))
           (callback (second expansion))
           (lambda-list (second callback))
           (on-partial (second (first (third lambda-list)))))
      (expect (first expansion) :to-be
              'aitools.protocol.application:call-with-command-result/k)
      (expect (first callback) :to-be 'lambda)
      (expect (first lambda-list) :to-be '&key)
      (expect (second lambda-list) :to-equal '((:on-ok on-ok)))
      (expect (fourth lambda-list) :to-equal '((:on-error on-error)))
      (expect (symbolp on-partial) :to-be t)
      (expect (symbol-package on-partial) :to-be nil)
      (expect (second (third callback)) :to-equal (list 'ignore on-partial))
      (expect (cdddr callback) :to-equal (list body)))))
