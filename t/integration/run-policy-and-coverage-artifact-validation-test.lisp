(in-package #:aitools/test)

(progn
  (defun %unique-context-test-source-file (context source-files)
    (loop for entry in source-files
          when (and (eq context (car entry))
                    (= 1 (count (cdr entry) source-files
                                :key (function cdr)
                                :test (function string=))))
            return (cdr entry)))
  (defun %passed-test-events (count context &optional (source-files (%context-test-source-files)))
    (let* ((source-file (%unique-context-test-source-file context source-files))
           (location (make-hash-table :test (function equal))))
      (unless source-file
        (error "No unique test source file for context ~A" context))
      (setf (gethash "file" location) source-file)
      (loop repeat count
            collect (let ((event (make-hash-table :test (function equal))))
                      (setf (gethash "status" event) "pass"
                            (gethash "location" event) location)
                      event)))))

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
  (it "derives aggregate and context floors from source-attributed baselines"
      (let* ((source-files (%context-test-source-files)) (source-index (%make-test-context-source-index source-files))
             (context-entries *context-spec-count-baselines*)
             (aggregate
              (loop for entry in context-entries
                    sum (getf (cdr entry) :minimum-executed-specs)))
             (full-events
              (loop for entry in context-entries
                    append (%passed-test-events
                            (getf (cdr entry) :minimum-executed-specs)
                            (car entry)
                            source-files))))
        (expect *minimum-executed-specs*
                :to-equal (%minimum-executed-specs-for-context))
        (expect (%minimum-executed-specs-for-context nil) :to-equal aggregate)
        (expect aggregate :to-equal *all-minimum-executed-specs*)
        (multiple-value-bind (problems executed context-counts)
            (%policy-problems full-events nil source-index)
          (expect (list executed (null problems) (mapcar (function car) context-counts))
                  :to-equal (list aggregate t (mapcar (function car) context-entries))))
        (dolist (underfilled-context context-entries)
          (let* ((context (car underfilled-context))
                 (events
                  (loop for entry in context-entries
                        for minimum = (getf (cdr entry) :minimum-executed-specs)
                        append (%passed-test-events
                                (if (eq (car entry) context)
                                    (1- minimum)
                                  minimum)
                                (car entry)
                                source-files))))
            (multiple-value-bind (problems executed context-counts)
                (%policy-problems events nil source-index)
              (expect (list executed (cdr (assoc context context-counts)))
                      :to-equal (list (1- aggregate) (1- (getf (cdr underfilled-context) :minimum-executed-specs))))
              (expect
               (find-if (lambda (problem)
                          (search (format nil "context ~A has only" context) problem))
                        problems)
               :to-be-truthy))))
        (signals error (%minimum-executed-specs-for-context :shared))
        (signals error (%minimum-executed-specs-for-context "unknown"))
        (let* ((entry (find-if (lambda (item) (not (eq (car item) :shared)))
                               context-entries))
               (context (car entry))
               (minimum (getf (cdr entry) :minimum-executed-specs)))
          (multiple-value-bind (problems executed)
              (%policy-problems
               (%passed-test-events minimum context source-files)
               context)
            (expect executed :to-equal minimum)
            (expect (null problems) :to-be-truthy))
          (dolist (status (list "skip" "todo"))
            (let ((event (make-hash-table :test #'equal)))
              (setf (gethash "status" event) status)
              (multiple-value-bind (problems executed)
                  (%policy-problems
                   (append
                    (%passed-test-events (1- minimum) context source-files)
                    (list event))
                   context)
                (expect executed :to-equal (1- minimum))
                (expect
                 (find-if (lambda (problem)
                            (search
                             (format nil "only ~D specs ran" (1- minimum))
                             problem))
                          problems)
                 :to-be-truthy)))))))
  (it "detects missing and unowned context test systems"
      (let* ((shared-entry (assoc :shared *context-spec-count-baselines*))
             (context-entries
              (remove (car shared-entry) *context-spec-count-baselines*
                      :key #'car :test #'eq))
             (shared-system
              (format nil "aitools/test/~A"
                      (string-downcase (symbol-name (car shared-entry)))))
             (systems
              (append (list "aitools/test/base" shared-system)
                      (mapcar (lambda (entry)
                                (format nil "aitools/test/~A"
                                        (string-downcase (symbol-name (car entry)))))
                              context-entries)))
             (missing-entry (first context-entries))
             (missing-context (string-downcase (symbol-name (car missing-entry))))
             (missing-system (format nil "aitools/test/~A" missing-context))
             (missing (remove missing-system systems :test #'string=))
             (unowned (append systems (list "aitools/test/unowned"))))
        (expect (%context-test-system-problems-for systems) :to-equal nil)
        (expect
         (find-if (lambda (problem)
                    (search (format nil "context ~A has a floor" missing-context)
                            problem))
                  (%context-test-system-problems-for missing))
         :to-be-truthy)
        (expect
         (find-if (lambda (problem)
                    (search "has a registered test system but no floor" problem))
                  (%context-test-system-problems-for unowned))
         :to-be-truthy)
        (expect (%context-test-system-problems-for nil) :to-equal nil)))

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
