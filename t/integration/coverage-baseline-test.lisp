(in-package #:aitools.integration.structure-test)
(progn
  (defun %call-with-coverage-baseline-fixture (state function &key (context :aggregate))
    (aitools.test.support:call-with-workspace
     (lambda (root state-directory)
       (declare (ignore state-directory))
       (let* ((relative-path
               (if (eq context :aggregate)
                   "coverage-baseline.lisp"
                 (format nil "coverage-baselines/~(~A~).lisp" context)))
              (path (merge-pathnames relative-path
                                     (uiop:ensure-directory-pathname root)))
              (total (if (eq state :bootstrap) 1000000000 3)))
         (ensure-directories-exist path)
         (with-open-file (stream path :direction :output :if-exists :supersede)
           (write (list :context context :state state :source :linux-ci
                        :expression-covered 1 :expression-total total
                        :branch-covered 1 :branch-total total)
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

(describe "Context coverage measurements"
  (it "fails closed when a measured context has no baseline"
    (let ((result
            (aitools.test.support:call-with-workspace
             (lambda (root state-directory)
               (declare (ignore state-directory))
               (let* ((statistics
                       (list :expression-covered 2 :expression-total 3
                             :branch-covered 1 :branch-total 2))
                      (missing-baseline
                       (cl-user::read-coverage-baseline-if-present root :org))
                      (measurement (list :org 1 statistics missing-baseline))
                      (record
                       (cl-user::coverage-measurement-record
                        :org 1 statistics missing-baseline))
                      (path
                       (cl-user::write-context-measurements
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
                 (list :failure failure
                       :baseline missing-baseline
                       :artifact-exists (probe-file path)
                       :artifact artifact))))))
      (expect (search "Missing coverage baselines" (getf result :failure))
              :to-be-truthy)
      (expect (getf result :baseline) :to-be nil)
      (expect (getf result :artifact-exists) :to-be-truthy)
      (expect (getf (getf result :artifact) :schema-version) :to-equal 1))))
