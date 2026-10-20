(in-package #:aitools.integration.structure-test)

(defun %complex-budget-fixture-violations (depth ratchet
                                           &optional
                                             (definition-template
                                               "(defun fixture () ~A)~%"))
  (aitools.test.support:call-with-workspace
   (lambda (root state)
     (declare (ignore state))
     (let* ((root-pathname (uiop:ensure-directory-pathname root))
            (source-path
              (merge-pathnames "src/composition.lisp" root-pathname))
            (form "nil"))
       (loop repeat depth
             do (setf form (format nil "(if t ~A nil)" form)))
       (ensure-directories-exist source-path)
       (with-open-file (output source-path
                               :direction :output
                               :if-exists :supersede)
         (write-string
          (if (eq definition-template :define-flow)
              (format nil "(define-flow (fixture) ~S ~A)~%" "fixture flow" form)
              (format nil definition-template form))
          output))
       (let ((*complex-function-ratchet* ratchet))
         (complex-function-violations root-pathname))))
   :prefix "aitools-complex-budget"))

(describe "aitools structure budgets (fixture control nesting)"
  (it "passes a clean top-level src fixture with a matching depth ratchet"
    (expect
     (%complex-budget-fixture-violations
      5
      '(("src/composition.lisp" "FIXTURE" 5)))
     :to-equal nil))
  (it "rejects new deeply nested unratcheted definitions in top-level src"
    (expect
     (some (lambda (violation)
             (search "new control nesting depth" violation))
           (%complex-budget-fixture-violations 5 nil))
     :to-be-truthy)
    (dolist (definition-template
              '("(define-flow (fixture &key on-ok) \"fixture doc\" ~A)~%"
                "(define-flow (fixture &key on-ok) ~A)~%"))
      (expect
       (some (lambda (violation)
               (and (search "new control nesting depth" violation)
                    (search "depth 5 (limit 4)" violation)
                    (search "FIXTURE" violation)))
             (%complex-budget-fixture-violations
              5 nil definition-template))
       :to-be-truthy)))
  (it "rejects nesting depths above their ratchets"
    (expect
     (some (lambda (violation)
             (search "exceeds its ratchet" violation))
           (%complex-budget-fixture-violations
            6
            '(("src/composition.lisp" "FIXTURE" 5))))
     :to-be-truthy))
  (it "rejects nesting ratchets above the measured depths"
    (expect
     (some (lambda (violation)
             (search "below its ratchet" violation))
           (%complex-budget-fixture-violations
            5
            '(("src/composition.lisp" "FIXTURE" 6))))
     :to-be-truthy))
  (it "rejects stale complex-function ratchets"
    (expect
     (some (lambda (violation)
             (search "stale complex-function ratchet entry" violation))
           (%complex-budget-fixture-violations
            4
            '(("src/composition.lisp" "FIXTURE" 5))))
     :to-be-truthy))
  (it "inspects define-flow bodies and their named depth ratchets"
    (let ((violations
            (%complex-budget-fixture-violations 5 nil :define-flow)))
      (expect (length violations) :to-equal 1)
      (expect
       (search "src/composition.lisp:FIXTURE has new control nesting depth 5"
               (first violations))
       :to-be-truthy))
    (expect
     (%complex-budget-fixture-violations
      5
      '(("src/composition.lisp" "FIXTURE" 5))
      :define-flow)
     :to-equal
     nil))
)
