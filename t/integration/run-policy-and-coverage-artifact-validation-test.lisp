(in-package #:aitools/test)

(defun %passed-test-events (count)
  (loop repeat count
        collect (let ((event (make-hash-table :test 'equal)))
                  (setf (gethash "status" event) "pass")
                  event)))

(defun %with-coverage-artifacts (coverage-text html-text function)
  (aitools.test.support:call-with-temporary-directory
   (lambda (directory)
     (let ((coverage-path (merge-pathnames "coverage" directory))
           (html-path (merge-pathnames "index.html" directory)))
       (aitools.test.support:write-text (namestring coverage-path) coverage-text)
       (aitools.test.support:write-text (namestring html-path) html-text)
       (funcall function coverage-path html-path)))
   :prefix "aitools-coverage-artifacts"))

(defun %valid-coverage-sidecar-text ()
  (with-output-to-string (stream)
    (write '(("source.lisp" #((0) (1 0)) . #*01))
           :stream stream :readably t)))

(defun %valid-coverage-report-html ()
  "<table class='summary'><th>Source file</th><th>Expression</th><th>Branch</th></table>")

(defun %coverage-validation-error (coverage-text html-text)
  (%with-coverage-artifacts
   coverage-text html-text
   (lambda (coverage-path html-path)
     (handler-case
         (progn (%validate-coverage-artifacts coverage-path html-path) nil)
       (error (condition) (princ-to-string condition))))))

(describe "Run policy and coverage artifact validation"
  (it "rejects 3,590 executed specs and accepts the 3,591 minimum"
    (multiple-value-bind (problems executed)
        (%policy-problems (%passed-test-events 3590))
      (expect *minimum-executed-specs* :to-equal 3591)
      (expect executed :to-equal 3590)
      (expect (length problems) :to-equal 1))
    (multiple-value-bind (problems executed)
        (%policy-problems (%passed-test-events 3591))
      (expect *minimum-executed-specs* :to-equal 3591)
      (expect executed :to-equal 3591)
      (expect (null problems) :to-be-truthy)))
  (it "does not count skipped or todo specs toward the executed-spec minimum"
    (dolist (status (list "skip" "todo"))
      (let ((event (make-hash-table :test (function equal))))
        (setf (gethash "status" event) status)
        (multiple-value-bind (problems executed)
            (%policy-problems
             (append (%passed-test-events 3590)
                     (list event)))
          (expect executed :to-equal 3590)
          (expect
           (find-if (lambda (problem)
                      (search "only 3590 specs ran" problem))
                    problems)
           :to-be-truthy)))))

  (it "accepts nonempty coverage artifacts with positive counts"
    (%with-coverage-artifacts
     (%valid-coverage-sidecar-text) (%valid-coverage-report-html)
     (lambda (coverage-path html-path)
       (multiple-value-bind (files expressions)
           (%validate-coverage-artifacts coverage-path html-path)
         (expect files :to-equal 1)
         (expect expressions :to-equal 2)))))

  (it "rejects an empty coverage sidecar"
    (expect (search "Coverage sidecar is empty"
                    (%coverage-validation-error "" (%valid-coverage-report-html)))
            :to-be-truthy))

  (it "rejects an empty coverage HTML report"
  (expect (search "Coverage HTML report is empty"
                  (%coverage-validation-error (%valid-coverage-sidecar-text) ""))
          :to-be-truthy))
  (it "rejects nonempty coverage HTML without expected markup"
  (expect (search "missing expected markup"
                  (%coverage-validation-error (%valid-coverage-sidecar-text)
                                         "not a coverage report"))
          :to-be-truthy))
  (it "rejects a nonempty coverage sidecar with no expressions"
  (let ((coverage-text
          (with-output-to-string (stream)
            (write (list (cons "source.lisp" (cons #() #*)))
                   :stream stream :readably t))))
    (expect (search "no expression counts"
                    (%coverage-validation-error coverage-text (%valid-coverage-report-html)))
            :to-be-truthy))))
