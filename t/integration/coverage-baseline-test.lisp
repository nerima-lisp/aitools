(in-package #:aitools.integration.structure-test)
(progn
  (defun %call-with-coverage-baseline-fixture
    (state function &key (context :aggregate) coverage-exclusions)
  (aitools.test.support:call-with-workspace
   (lambda (root state-directory)
     (declare (ignore state-directory))
     (let* ((relative-path (if (eq context :aggregate)
                               "coverage-baseline.lisp"
                             (format nil "coverage-baselines/~(~A~).lisp" context)))
            (path (merge-pathnames relative-path
                                   (uiop:ensure-directory-pathname root)))
            (total (if (eq state :bootstrap) 1000000000 3))
            (baseline (list :context context :state state :source :linux-ci
                            :expression-covered 1 :expression-total total
                            :branch-covered 1 :branch-total total)))
       (when coverage-exclusions
         (setf baseline (append baseline (list :coverage-exclusions coverage-exclusions))))
       (ensure-directories-exist path)
       (with-open-file (stream path :direction :output :if-exists :supersede)
         (write baseline :stream stream)
         (terpri stream))
       (funcall function (uiop:ensure-directory-pathname root))))))

  (describe "coverage baseline loading"
    (it "accepts a measured Linux baseline"
      (let ((baseline
              (%call-with-coverage-baseline-fixture
               :measured
               (lambda (root)
                 (cl-user::read-coverage-baseline root :aggregate)))))
        (expect (getf baseline :state) :to-equal :measured)
        (expect (getf baseline :source) :to-equal :linux-ci)))

    (it "rejects a bootstrap baseline when it is read"
      (let ((message
              (%call-with-coverage-baseline-fixture
               :bootstrap
               (lambda (root)
                 (handler-case
                     (progn (cl-user::read-coverage-baseline-if-present root :aggregate) nil)
                   (error (condition) (princ-to-string condition)))))))
        (expect (search "Invalid coverage baseline" message) :to-be-truthy))))
)

(describe "Context coverage measurements"
  (it "fails closed when a measured context has no baseline"
    (let ((result
            (aitools.test.support:call-with-workspace
             (lambda (root state-directory)
               (declare (ignore state-directory))
               (let* ((statistics (list :expression-covered 2 :expression-total 3
                                        :branch-covered 1 :branch-total 2))
                      (missing-baseline
                        (cl-user::read-coverage-baseline-if-present root :org))
                      (measurement (list :org 1 statistics missing-baseline))
                      (path
                        (cl-user::write-context-measurements
                         root nil
                         (list (cl-user::coverage-measurement-record
                                :org 1 statistics missing-baseline))
                         nil))
                      (failure
                        (handler-case
                            (progn
                              (cl-user::require-context-coverage-baselines
                               (list measurement))
                              nil)
                          (error (condition) (princ-to-string condition))))
                      (artifact
                        (with-open-file (stream path) (read stream nil :eof))))
                 (list :failure failure :baseline missing-baseline
                       :artifact-exists (probe-file path) :artifact artifact))))))
      (expect (search "Missing coverage baselines" (getf result :failure))
              :to-be-truthy)
      (expect (getf result :baseline) :to-be nil)
      (expect (getf result :artifact-exists) :to-be-truthy)
      (expect (getf (getf result :artifact) :schema-version) :to-equal 1))))
(describe "Coverage exclusion enforcement"
  (it "subtracts matching uncovered expressions and branch units, and rejects changed exclusion counts"
    (let* ((rules
           (list (list :kind :expression :package "TEST.PACKAGE" :symbol "TARGET"
                       :reason "Synthetic expression reason."
                       :expected-uncovered-count 1)
                 (list :kind :branch :package "TEST.PACKAGE" :symbol "TARGET"
                       :reason "Synthetic branch reason."
                       :expected-uncovered-count 3)))
         (owned-locations
           (list (list (list :kind :expression)
                       (list :package "TEST.PACKAGE" :symbol "TARGET"))
                 (list (list :kind :branch :uncovered-count 1)
                       (list :package "TEST.PACKAGE" :symbol "TARGET"))
                 (list (list :kind :branch :uncovered-count 2)
                       (list :package "TEST.PACKAGE" :symbol "TARGET"))))
         (statistics
           (list :expression-covered 2 :expression-total 5
                 :branch-covered 4 :branch-total 8)))
    (multiple-value-bind (adjusted applied)
        (cl-user::coverage-adjust-statistics-for-exclusions
         statistics rules owned-locations)
      (expect (getf adjusted :expression-covered) :to-equal 2)
      (expect (getf adjusted :expression-total) :to-equal 4)
      (expect (getf adjusted :branch-covered) :to-equal 4)
      (expect (getf adjusted :branch-total) :to-equal 5)
      (expect (getf (second applied) :excluded-count) :to-equal 3))
    (let ((failure
            (handler-case
                (progn
                  (cl-user::coverage-adjust-statistics-for-exclusions
                   statistics
                   (list (list :kind :branch :package "TEST.PACKAGE"
                               :symbol "TARGET" :reason "Count drift."
                               :expected-uncovered-count 2))
                   owned-locations)
                  nil)
              (error (condition) (princ-to-string condition)))))
      (expect (search "expected 2, measured 3" failure) :to-be-truthy))))

  (it "fails when an exclusion no longer matches an uncovered location"
    (let* ((rule
             (list :kind :expression :package "TEST.PACKAGE" :symbol "MISSING"
                   :reason "A stale rule must fail."))
           (failure
             (handler-case
                 (progn
                   (cl-user::coverage-adjust-statistics-for-exclusions
                    (list :expression-covered 1 :expression-total 2
                          :branch-covered 1 :branch-total 2)
                    (list rule)
                    nil)
                   nil)
               (error (condition) (princ-to-string condition)))))
      (expect (search "Stale coverage exclusion" failure) :to-be-truthy)))

  (it "rejects malformed, duplicate, aggregate, and unpinned Store exclusions"
    (flet ((invalid-baseline-p (context exclusions)
           (let ((failure
                   (%call-with-coverage-baseline-fixture
                    :measured
                    (lambda (root)
                      (handler-case
                          (progn (cl-user::read-coverage-baseline root context) nil)
                        (error (condition) (princ-to-string condition))))
                    :context context :coverage-exclusions exclusions)))
             (or (search "Invalid coverage baseline" failure)
                 (search "Store coverage exclusions require expected counts" failure)))))
    (expect
     (and (invalid-baseline-p
           :protocol
           (list (list :kind :expression :package "TEST.PACKAGE" :symbol "TARGET")))
          (invalid-baseline-p
           :protocol
           (list (list :kind :expression :package "TEST.PACKAGE" :symbol "TARGET"
                       :reason "   ")))
          (invalid-baseline-p
           :protocol
           (list (list :kind :expression :package "TEST.PACKAGE" :symbol "TARGET"
                       :reason "Zero expected count."
                       :expected-uncovered-count 0)))
          (invalid-baseline-p
           :store
           (list (list :kind :expression :package "TEST.PACKAGE" :symbol "TARGET"
                       :reason "Store exclusions require counts."))))
     :to-be-truthy)
    (expect
     (and (invalid-baseline-p
           :protocol
           (list (list :kind :expression :package "TEST.PACKAGE" :symbol "TARGET"
                       :reason "First record.")
                 (list :kind :expression :package "test.package" :symbol "target"
                       :reason "Duplicate after case folding.")))
          (invalid-baseline-p
           :aggregate
           (list (list :kind :expression :package "TEST.PACKAGE" :symbol "TARGET"
                       :reason "Aggregate exclusions are not context-scoped."))))
     :to-be-truthy)))

  (it "keeps unknown uncovered locations in the measured total"
    (let* ((rule
             (list :kind :expression :package "TEST.PACKAGE" :symbol "KNOWN"
                   :reason "Only the known owner is excluded."))
           (owned-locations
             (list (list (list :kind :expression)
                         (list :package "TEST.PACKAGE" :symbol "KNOWN"))
                   (list (list :kind :expression) nil)))
           (statistics
             (list :expression-covered 0 :expression-total 2
                   :branch-covered 1 :branch-total 2)))
      (multiple-value-bind (adjusted applied)
          (cl-user::coverage-adjust-statistics-for-exclusions
           statistics (list rule) owned-locations)
        (expect (getf adjusted :expression-total) :to-equal 1)
        (expect (getf (first applied) :excluded-count) :to-equal 1)))))
