(in-package #:aitools.search.test)

(describe "aitools.search.domain parser and entry-depth contracts"
  (it-property "parses generated symbolic and detached HEAD values"
      ((id (gen-integer :min 0 :max #xffffffff)))
    (let* ((branch (format nil "branch-~D" id))
           (ref (format nil "refs/heads/~A" branch))
           (sha (format nil "~(~40,'0X~)" id)))
      (expect (multiple-value-list
               (parse-head-file (format nil "ref: ~A~%" ref)))
              :to-equal (list branch nil ref))
      (expect (multiple-value-list (parse-head-file sha))
              :to-equal (list nil sha nil))))
  (it "checks entry-depth and gates a nonempty mutation set"
    (let* ((source-form
             (or (function-lambda-expression #'entry-depth)
               (error "ENTRY-DEPTH source is unavailable for mutation testing.")))
           (cases '(("" "a" 1)
                    ("src" "src/a" 1)
                    ("src" "src/deep/a" 2)
                    ("src/deep" "src/deep/a" 1)
                    ("src" "src/deep/nested/a" 3)))
           (mutants (cl-weave:collect-mutations source-form)))
      (expect (plusp (length mutants)) :to-be t)
      (dolist (case cases)
        (destructuring-bind (start path expected) case
          (expect (entry-depth start path) :to-be expected)))
      (let* ((results
               (cl-weave:run-mutations
                source-form
                (lambda (form mutation)
                  (declare (ignore mutation))
                  (let ((mutant-entry-depth (eval form)))
                    (every (lambda (case)
                             (destructuring-bind (start path expected) case
                               (= (funcall mutant-entry-depth start path)
                                  expected)))
                           cases)))))
             (summary (cl-weave:assert-mutation-score results 0.9)))
        (expect (getf summary :total) :to-be (length mutants))))))

