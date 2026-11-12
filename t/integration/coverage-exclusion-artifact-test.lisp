(in-package #:aitools.integration.structure-test)

(describe "Coverage exclusion enforcement"
          (it "applies protocol exclusions and resolves kernel definitions"
              (let* ((root (asdf:system-source-directory "aitools/test/shared"))
                     (baseline (cl-user::read-coverage-baseline root :protocol))
                     (rules (getf baseline :coverage-exclusions))
                     (definitions
                      (loop for pathname in (uiop:symbol-call "CL-WEAVE"
                                                              "ASDF-SYSTEM-FILES"
                                                              "aitools/core/protocol"
                                                              :include-dependencies
                                                              nil)
                            append (cl-user::coverage-source-definitions-for
                                    pathname)))
                     (locations
                      (loop for rule in rules
                            for owner = (find-if
                                         (lambda (definition)
                                           (and
                                            (string-equal (getf rule :package)
                                                          (getf definition
                                                                :package))
                                            (string-equal (getf rule :symbol)
                                                          (getf definition
                                                                :symbol))))
                                         definitions)
                            append (progn
                                     (unless owner
                                       (error
                                        "Protocol exclusion has no source definition: ~A::~A"
                                        (getf rule :package)
                                        (getf rule :symbol)))
                                     (loop repeat (if (eq (getf rule :kind)
                                                          :expression)
                                                      2
                                                      1)
                                           collect (list :kind
                                                         (getf rule :kind)
                                                         :pathname
                                                         (getf owner :pathname)
                                                         :start
                                                         (1+
                                                          (getf owner :start))
                                                         :end
                                                         (+ 2
                                                            (getf owner :start))
                                                         :uncovered-count
                                                         (if (eq
                                                              (getf rule :kind)
                                                              :expression)
                                                             1
                                                             2))))))
                     (statistics
                      (list :expression-covered
                            2
                            :expression-total
                            10
                            :branch-covered
                            12
                            :branch-total
                            20))
                     (kernel-rules
                      (getf (cl-user::read-coverage-baseline root :kernel)
                            :coverage-exclusions))
                     (kernel-definitions
                      (loop for pathname in (uiop:symbol-call "CL-WEAVE"
                                                              "ASDF-SYSTEM-FILES"
                                                              "aitools/core/kernel"
                                                              :include-dependencies
                                                              nil)
                            append (cl-user::coverage-source-definitions-for
                                    pathname))))
                (multiple-value-bind (adjusted applied)
                    (cl-user::coverage-statistics-with-exclusions statistics
                                                                  rules
                                                                  locations)
                  (expect (length rules) :to-equal 5)
                  (expect (length applied) :to-equal 5)
                  (expect (getf adjusted :expression-total) :to-equal 6)
                  (expect (getf adjusted :branch-total) :to-equal 14)
                  (expect
                   (and
                    (=
                     (loop for entry in applied
                           when (eq (getf entry :kind) :expression)
                             sum (getf entry :excluded-count))
                     4)
                    (=
                     (loop for entry in applied
                           when (eq (getf entry :kind) :branch)
                             sum (getf entry :excluded-count))
                     6)
                    (equal
                     (mapcar
                      (lambda (state)
                        (cl-user::coverage-uncovered-unit-count-for-state state))
                      '(2 6 9 10 5))
                     '(1 1 1 2 0))
                    (not
                     (or (getf baseline :coverage-exclusion-counts)
                         (getf baseline :coverage-uncovered-counts)))
                    (consp kernel-rules)
                    (cl-user::coverage-exclusion-records-valid-p kernel-rules)
                    (every
                     (lambda (rule)
                       (find-if
                        (lambda (definition)
                          (and
                           (string-equal (getf rule :package)
                                         (getf definition :package))
                           (string-equal (getf rule :symbol)
                                         (getf definition :symbol))))
                        kernel-definitions))
                     kernel-rules))
                   :to-be-truthy)))))
