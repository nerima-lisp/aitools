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

(defun %coverage-source-path (&optional name)
  (namestring
   (make-pathname :directory (quote (:relative "src"))
                  :name name
                  :type (when name "lisp"))))

(defun %valid-coverage-sidecar-text (&optional (directory (%coverage-source-path)))
  (with-output-to-string (stream)
    (write `((,(concatenate 'string directory "source.lisp")
               #((0) (1 0) (:then 1) (:else 1)) . #*0101)
             (,(concatenate 'string directory "test.lisp") #((0)) . #*1))
           :stream stream :readably t)))

(defun %valid-coverage-report-html
    (&optional (expression-total 2) (directory (%coverage-source-path))
               (source-name "source.lisp") extra-source-name)
  (let ((html
          (format nil
                  "<table class='summary'><tr class='head-row'><td></td><td class='main-head' colspan='3'>Expression</td><td class='main-head' colspan='3'>Branch</td></tr><tr class='head-row'><td width='80px'>Source file</td><td width='80px'>Covered</td><td width='80px'>Total</td><td width='80px'>%</td><td width='80px'>Covered</td><td width='80px'>Total</td><td width='80px'>%</td></tr><tr class='subheading'><td colspan='7'>~A</td></tr><tr class='odd'><td class='text-cell'><a href='source.lisp.html'>~A</a></td><td>1</td><td>~D</td><td>50.0</td><td>1</td><td>2</td><td>50.0</td></tr></table>"
                  directory
                  source-name
                  expression-total)))
    (if extra-source-name
        (let ((table-end (search "</table>" html)))
          (concatenate
           'string
           (subseq html 0 table-end)
           (format nil
                   "<tr class='even'><td class='text-cell'><a href='~A.html'>~A</a></td><td>1</td><td>1</td><td>100</td><td>1</td><td>2</td><td>50.0</td></tr>"
                   extra-source-name
                   extra-source-name)
           (subseq html table-end)))
        html)))

(defun %coverage-validation-error
    (coverage-text html-text &optional (expected-paths nil expected-paths-p))
  (%with-coverage-artifacts
   coverage-text html-text
   (lambda (coverage-path html-path)
     (handler-case
         (progn
           (if expected-paths-p
               (%validate-coverage-artifacts coverage-path html-path expected-paths)
               (%validate-coverage-artifacts coverage-path html-path))
           nil)
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

  (it "validates coverage HTML summary counts against the sidecar"
    (%with-coverage-artifacts
     (%valid-coverage-sidecar-text) (%valid-coverage-report-html)
     (lambda (coverage-path html-path)
       (multiple-value-bind (files expressions)
           (%validate-coverage-artifacts coverage-path html-path)
         (expect files :to-equal 1)
         (expect expressions :to-equal 2))))
    (%with-coverage-artifacts
     (%valid-coverage-sidecar-text (%coverage-source-path))
     (%valid-coverage-report-html 2 (%coverage-source-path) (%coverage-source-path "source"))
     (lambda (coverage-path html-path)
       (multiple-value-bind (files expressions)
           (%validate-coverage-artifacts coverage-path html-path)
         (expect files :to-equal 1)
         (expect expressions :to-equal 2))))
    (expect
     (search "file count mismatch"
             (%coverage-validation-error
              (%valid-coverage-sidecar-text)
              (%valid-coverage-report-html)
              (list (%coverage-source-path "source") (%coverage-source-path "test"))))
     :to-be-truthy)
    (expect
     (search "expression count mismatch"
             (%coverage-validation-error
              (%valid-coverage-sidecar-text)
              (%valid-coverage-report-html 1)
              (list (%coverage-source-path "source"))))
     :to-be-truthy)
    (expect
     (search "missing included file"
             (%coverage-validation-error
              (%valid-coverage-sidecar-text)
              (%valid-coverage-report-html)
              (list (%coverage-source-path "source") (%coverage-source-path "missing"))))
     :to-be-truthy))
  (it "rejects a selected coverage file omitted from HTML"
    (expect
     (search "file paths mismatch"
             (%coverage-validation-error
              (%valid-coverage-sidecar-text)
              (%valid-coverage-report-html
               2 (%coverage-source-path) "source.lisp" "other.lisp")
              (list (%coverage-source-path "source")
                    (%coverage-source-path "test"))))
     :to-be-truthy))

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
            (write (list (cons (%coverage-source-path "source") (cons #() #*)))
                   :stream stream :readably t))))
    (expect (search "no expression counts"
                    (%coverage-validation-error coverage-text (%valid-coverage-report-html)))
            :to-be-truthy))))
