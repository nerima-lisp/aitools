(in-package #:aitools.inspect.test)

(describe "selector option parsing"
          (cl-weave:it-property
           "rejects generated combinations of extra exclusive flags"
           ((count (cl-weave:gen-integer :min 0 :max 4)))
           (let* ((extras
                   (loop for index below
                         count
                         collect (cons (format nil "--extra-~D" index) t)))
                  (outcome
                   (nth-value 0 (parse-options :read :extra-exclusive extras))))
             (expect outcome
                     :to-be
                     (if (> count 1)
                         :invalid
                       :none))))
          (it
           "kills a comparison mutation in the production conflict predicate"
           (let* ((form
                   aitools.inspect.application::*selector-option-conflict-form*)
                  (baseline
                   aitools.inspect.application::*selector-option-conflict-p*)
                  (results
                   (cl-weave:run-mutations form
                                           (lambda (mutated-form mutation)
                                             (declare (ignore mutation))
                                             (let ((mutant
                                                    (compile nil mutated-form)))
                                               (equal (mapcar mutant '(0 1 2))
                                                      (mapcar baseline
                                                              '(0 1 2)))))
                                           :operators
                                           '(:comparison-operator))))
             (expect (length results) :to-be 1)
             (cl-weave:assert-mutation-score results 1.0))))
