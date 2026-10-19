(in-package #:aitools.integration.structure-test)
(progn
  (defun %call-with-coverage-baseline-fixture (state function)
    (aitools.test.support:call-with-workspace
     (lambda (root state-directory)
       (declare (ignore state-directory))
       (let* ((path (merge-pathnames "coverage-baseline.lisp"
                                     (uiop:ensure-directory-pathname root)))
              (total (if (eq state :bootstrap) 1000000000 3)))
         (ensure-directories-exist path)
         (with-open-file (stream path :direction :output :if-exists :supersede)
           (write (list :context :aggregate :state state :source :linux-ci
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
                     (progn (cl-user::read-coverage-baseline root :aggregate) nil)
                   (error (condition) (princ-to-string condition)))))))
        (expect (search "Invalid coverage baseline" message) :to-be-truthy))))
)
