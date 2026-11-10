(in-package #:aitools.integration.structure-test)

(defun %coverage-gap-report-fixture-location ()
  (let* ((root (asdf:system-source-directory "aitools/test/shared"))
         (pathname (merge-pathnames
                    "t/integration/fixtures/coverage-gap-report-source.lisp" root))
         (source (uiop:read-file-string pathname))
         (form "(+ 40 2)")
         (start (search form source)))
    (unless start
      (error "Coverage gap report fixture form is missing"))
    (values root
            (list :pathname pathname :kind :expression
                  :start start :end (+ start (length form))))))

(describe "Coverage gap reporting"
  (it "prints kind, owner, and a root-relative source location without source text"
    (multiple-value-bind (root location)
        (%coverage-gap-report-fixture-location)
      (let* ((records (cl-user::coverage-gap-records root :env (list location) nil))
             (output
               (with-output-to-string (stream)
                 (cl-user::report-coverage-gap-records :env records stream))))
        (expect output
                :to-equal
                (format nil "coverage gap ENV EXPRESSION CL-USER::COVERAGE-REPORT-FIXTURE t/integration/fixtures/coverage-gap-report-source.lisp:5~%"))
        (expect (search "SENSITIVE-COVERAGE-SOURCE" output) :to-be nil))))

  (it "reports unknown owners and omits matching exclusions"
    (multiple-value-bind (root location)
        (%coverage-gap-report-fixture-location)
      (let* ((pathname (getf location :pathname))
             (unknown (list :pathname pathname :kind :branch :start 0 :end 10))
             (records (cl-user::coverage-gap-records root :env (list unknown) nil))
             (exclusion (list :kind :expression :package "CL-USER"
                              :symbol "COVERAGE-REPORT-FIXTURE")))
        (expect (getf (first records) :owner) :to-equal "unknown-owner")
        (expect
         (with-output-to-string (stream)
           (cl-user::report-coverage-gap-records :env records stream))
         :to-equal
         (format nil
                 "coverage gap ENV BRANCH unknown-owner t/integration/fixtures/coverage-gap-report-source.lisp:1~%"))
        (expect (cl-user::coverage-gap-records root :env (list location)
                                               (list exclusion))
                :to-be nil))))

  (it "prints at most 200 sites per context and summarizes the remainder"
    (multiple-value-bind (root location)
        (%coverage-gap-report-fixture-location)
      (let* ((record (first (cl-user::coverage-gap-records root :env (list location) nil)))
             (records (make-list 201 :initial-element record))
             (output
               (with-output-to-string (stream)
                 (cl-user::report-coverage-gap-records :env records stream))))
        (expect (count #\Newline output) :to-equal 201)
        (expect (search "coverage gap ENV: 1 additional uncovered site omitted" output)
                :to-be-truthy))))

  (it "names a source file when its owner parse reaches end of file"
    (let ((failure
            (aitools.test.support:call-with-workspace
             (lambda (root state-directory)
               (declare (ignore state-directory))
               (let ((pathname (merge-pathnames "coverage-gap-report-invalid.lisp" root)))
                 (with-open-file (stream pathname :direction :output :if-exists :supersede)
                   (write-string "(defun malformed ()" stream))
                 (handler-case
                     (progn (cl-user::coverage-source-definitions-for pathname) nil)
                   (error (condition) (princ-to-string condition))))))))
      (expect (search "coverage-gap-report-invalid.lisp" failure) :to-be-truthy))))
