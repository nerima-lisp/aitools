(in-package #:aitools/test)

(defun %u11-pass-events (count)
  (loop repeat count
        collect (let ((event (make-hash-table :test 'equal)))
                  (setf (gethash "status" event) "pass")
                  event)))

(defun %u11-write-text (pathname text)
  (with-open-file (stream pathname :direction :output :if-exists :supersede
                                  :if-does-not-exist :create)
    (write-string text stream)))

(defun %u11-with-artifacts (coverage-text html-text function)
  (let* ((prefix (format nil "aitools-u11-~36R-~36R-"
                         (get-universal-time) (random (expt 36 10))))
         (directory (uiop:ensure-directory-pathname (uiop:temporary-directory)))
         (coverage-path (merge-pathnames (concatenate 'string prefix "coverage") directory))
         (html-path (merge-pathnames (concatenate 'string prefix "index.html") directory)))
    (unwind-protect
         (progn
           (%u11-write-text coverage-path coverage-text)
           (%u11-write-text html-path html-text)
           (funcall function coverage-path html-path))
      (when (probe-file coverage-path) (delete-file coverage-path))
      (when (probe-file html-path) (delete-file html-path)))))

(defun %u11-valid-coverage-text ()
  (with-output-to-string (stream)
    (write '(("source.lisp" #((0) (1 0)) . #*01))
           :stream stream :readably t)))

(defun %u11-valid-cover-html ()
  "<table class='summary'><th>Source file</th><th>Expression</th><th>Branch</th></table>")

(defun %u11-validation-error (coverage-text html-text)
  (%u11-with-artifacts
   coverage-text html-text
   (lambda (coverage-path html-path)
     (handler-case
         (progn (%validate-coverage-artifacts coverage-path html-path) nil)
       (error (condition) (princ-to-string condition))))))

(describe "U-11 section 7 gates"
  (it "rejects an executed-spec count below the sourced F0 minimum"
    (multiple-value-bind (problems executed)
        (%policy-problems (%u11-pass-events 3148))
      (expect *minimum-executed-specs* :to-equal 3149)
      (expect executed :to-equal 3148)
      (expect (length problems) :to-equal 1)))
  (it "does not count skipped or todo specs toward the executed-spec minimum"
    (dolist (status (list "skip" "todo"))
      (let ((event (make-hash-table :test (function equal))))
        (setf (gethash "status" event) status)
        (multiple-value-bind (problems executed)
            (%policy-problems
             (append (%u11-pass-events 3148)
                     (list event)))
          (expect executed :to-equal 3148)
          (expect
           (find-if (lambda (problem)
                      (search "only 3148 specs ran" problem))
                    problems)
           :to-be-truthy)))))

  (it "accepts nonempty coverage artifacts with positive counts"
    (%u11-with-artifacts
     (%u11-valid-coverage-text) (%u11-valid-cover-html)
     (lambda (coverage-path html-path)
       (multiple-value-bind (files expressions)
           (%validate-coverage-artifacts coverage-path html-path)
         (expect files :to-equal 1)
         (expect expressions :to-equal 2)))))

  (it "rejects an empty coverage sidecar"
    (expect (search "Coverage sidecar is empty"
                    (%u11-validation-error "" (%u11-valid-cover-html)))
            :to-be-truthy))

  (it "rejects an empty coverage HTML report"
  (expect (search "Coverage HTML report is empty"
                  (%u11-validation-error (%u11-valid-coverage-text) ""))
          :to-be-truthy))
  (it "rejects nonempty coverage HTML without expected markup"
  (expect (search "missing expected markup"
                  (%u11-validation-error (%u11-valid-coverage-text)
                                         "not a coverage report"))
          :to-be-truthy))
  (it "rejects a nonempty coverage sidecar with no expressions"
  (let ((coverage-text
          (with-output-to-string (stream)
            (write (list (cons "source.lisp" (cons #() #*)))
                   :stream stream :readably t))))
    (expect (search "no expression counts"
                    (%u11-validation-error coverage-text (%u11-valid-cover-html)))
            :to-be-truthy))))
