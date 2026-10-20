(in-package #:aitools.integration.structure-test)
(progn
  (defun %call-with-coverage-baseline-fixture
      (state function &key (context :aggregate)
             (uncovered-in-package-expressions :missing))
    (aitools.test.support:call-with-workspace
     (lambda (root state-directory)
       (declare (ignore state-directory))
       (let* ((relative-path
               (if (eq context :aggregate)
                   "coverage-baseline.lisp"
                 (format nil "coverage-baselines/~(~A~).lisp" context)))
              (path (merge-pathnames relative-path
                                     (uiop:ensure-directory-pathname root)))
              (total (if (eq state :bootstrap) 1000000000 3))
              (metadata
               (unless (eq uncovered-in-package-expressions :missing)
                 (list :uncovered-in-package-expressions
                       uncovered-in-package-expressions))))
         (ensure-directories-exist path)
         (with-open-file (stream path :direction :output :if-exists :supersede)
           (write (append (list :context context :state state :source :linux-ci
                                :expression-covered 1 :expression-total total
                                :branch-covered 1 :branch-total total)
                          metadata)
                  :stream stream)
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

(describe "Context coverage normalization"
  (it "accepts a measured Journal baseline with a zero uncovered IN-PACKAGE count"
    (let ((baseline
            (%call-with-coverage-baseline-fixture
             :measured
             (lambda (root)
               (cl-user::read-coverage-baseline root :journal))
             :context :journal
             :uncovered-in-package-expressions 0)))
      (expect (getf baseline :uncovered-in-package-expressions) :to-equal 0)))

  (it "accepts a measured context baseline without optional IN-PACKAGE metadata"
    (let ((baseline
            (%call-with-coverage-baseline-fixture
             :measured
             (lambda (root)
               (cl-user::read-coverage-baseline root :process))
             :context :process)))
      (expect (getf baseline :context) :to-equal :process)
      (expect (nth-value 0 (get-properties baseline '(:uncovered-in-package-expressions)))
              :to-be nil)))

  (it "rejects invalid IN-PACKAGE counts on any context baseline"
    (dolist (count (list nil -1 14.0 "14"))
      (let ((message
              (%call-with-coverage-baseline-fixture
               :measured
               (lambda (root)
                 (handler-case
                     (progn (cl-user::read-coverage-baseline root :process) nil)
                   (error (condition) (princ-to-string condition))))
               :context :process
               :uncovered-in-package-expressions count)))
        (expect (search "Invalid uncovered IN-PACKAGE expression count"
                        message)
                :to-be-truthy))))

  (it "counts only uncovered IN-PACKAGE expression locations"
    (let ((count
            (aitools.test.support:call-with-workspace
             (lambda (root state-directory)
               (declare (ignore state-directory))
               (let* ((path (merge-pathnames "sample.lisp"
                                             (uiop:ensure-directory-pathname root)))
                      (source (format nil
                                      "(in-package #:cl-user)~%(#:in-package #:cl-user)~%(defun sample () t)~%"))
                      (first-package (search "(in-package" source))
                      (uninterned-package (search "(#:in-package" source))
                      (function-form (search "(defun" source))
                      (locations
                        (list (list :kind :expression :start first-package)
                              (list :kind :expression :start uninterned-package)
                              (list :kind :branch :start first-package)
                              (list :kind :expression :start function-form))))
                 (with-open-file (stream path :direction :output :if-exists :supersede
                                                :if-does-not-exist :create)
                   (write-string source stream))
                 (cl-user::uncovered-in-package-expression-count
                  (list (list :pathname path :uncovered-forms locations))))))))
      (expect count :to-equal 1)))

  (it "leaves statistics unchanged without IN-PACKAGE baseline metadata"
    (let* ((statistics (list :expression-covered 1455 :expression-total 1486
                             :branch-covered 60 :branch-total 62))
           (baseline
             (%call-with-coverage-baseline-fixture
              :measured
              (lambda (root)
                (cl-user::read-coverage-baseline root :process))
              :context :process))
           (adjusted
             (cl-user::adjust-context-expression-total
              statistics :process 16 baseline)))
      (expect adjusted :to-equal statistics)))

  (it "adjusts another context's expression denominator and fails closed for a missing baseline"
    (let* ((baseline
             (%call-with-coverage-baseline-fixture
              :measured
              (lambda (root)
                (cl-user::read-coverage-baseline root :process))
              :context :process
              :uncovered-in-package-expressions 14))
           (statistics (list :expression-covered 1455 :expression-total 1486
                             :branch-covered 60 :branch-total 62))
           (adjusted
             (cl-user::adjust-context-expression-total
              statistics :process 16 baseline))
           (missing-result
             (aitools.test.support:call-with-workspace
              (lambda (root state-directory)
                (declare (ignore state-directory))
                (let* ((measured-statistics
                         (list :expression-covered 2 :expression-total 3
                               :branch-covered 1 :branch-total 2
                               :uncovered-in-package-expressions 0))
                       (missing-baseline
                         (cl-user::read-coverage-baseline-if-present root :org))
                       (measurement (list :org 1 measured-statistics missing-baseline))
                       (record
                         (cl-user::coverage-measurement-record
                          :org 1 measured-statistics missing-baseline))
                       (path (cl-user::write-context-measurements
                              root nil (list record) nil))
                       (failure
                         (handler-case
                             (progn
                               (cl-user::require-context-coverage-baselines
                                (list measurement))
                               nil)
                           (error (condition) (princ-to-string condition))))
                       (artifact
                         (with-open-file (stream path)
                           (read stream nil :eof))))
                  (list :failure failure :baseline missing-baseline
                        :artifact-exists (probe-file path)
                        :record (first (getf artifact :contexts))))))))
      (expect (getf adjusted :expression-covered) :to-equal 1455)
      (expect (getf adjusted :expression-total) :to-equal 1484)
      (expect (getf adjusted :branch-covered) :to-equal 60)
      (expect (getf adjusted :branch-total) :to-equal 62)
      (expect (getf adjusted :uncovered-in-package-expressions) :to-equal 16)
      (expect (getf adjusted :baseline-uncovered-in-package-expressions) :to-equal 14)
      (expect (getf statistics :expression-total) :to-equal 1486)
      (expect (search "Missing coverage baselines"
                      (getf missing-result :failure))
              :to-be-truthy)
      (expect (getf missing-result :baseline) :to-be nil)
      (expect (getf missing-result :artifact-exists) :to-be-truthy)
      (expect (getf (getf missing-result :record) :baseline-state) :to-be nil)
      (expect (getf (getf missing-result :record)
                    :uncovered-in-package-expressions)
              :to-equal 0)
      (expect (getf (getf missing-result :record)
                    :baseline-uncovered-in-package-expressions)
              :to-be nil))))
