;;;; Set AITOOLS_TEST_CONTEXT=<context> to run one context; see docs/src/project/development.md.
(require :asdf)
(defun script-directory ()
  (make-pathname :name nil
                 :type nil
                 :defaults (or *load-truename*
                               *compile-file-truename*
                               (error "Unable to determine the script location"))))
;; Context systems live in packages/*/*.asd, so the checkout tree must be
;; registered. The source tree contains no generated or scratch ASDF systems.
(defun configure-local-source-registry (root)
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,root)
     :inherit-configuration)))
(defun assert-coverage-baseline (statistics baseline context)
  (dolist (metric '(:expression :branch))
    (multiple-value-bind (covered total)
        (require-positive-coverage-counts statistics context metric)
      (multiple-value-bind (base-covered base-total)
          (require-positive-coverage-counts baseline context metric)
        (unless (>= (* covered base-total) (* base-covered total))
          (error "~A ~A coverage ~D/~D fell below baseline ~D/~D"
                 context metric covered total base-covered base-total))
        (format t
                "~&coverage comparison ~A ~A: measured ~D/~D, baseline ~D/~D (pass)~%"
                context metric covered total base-covered base-total))))
  t)
(defvar *harvest-sb-cover-marks-after-source-load* nil)
(defmethod asdf:perform :around ((operation asdf:load-op)
                                (component asdf:cl-source-file))
  (declare (ignore operation component))
  (if *harvest-sb-cover-marks-after-source-load*
      (sb-sys:without-gcing
        (uiop:symbol-call "SB-COVER" "REFRESH-COVERAGE-BITS")
        (prog1 (call-next-method)
          (uiop:symbol-call "SB-COVER" "REFRESH-COVERAGE-BITS")))
      (call-next-method)))
(defvar *coverage-package-form-files-asserted* (make-hash-table :test (function equal)))
(defun assert-coverage-package-forms-covered (pathnames)
  (let ((pending (remove-if (lambda (pathname)
                              (gethash (namestring pathname)
                                       *coverage-package-form-files-asserted*))
                            pathnames)))
    (when pending
      (let ((coverage (uiop:symbol-call "SB-COVER" "SAVE-COVERAGE")))
        (dolist (pathname pending)
          (let* ((filename (namestring pathname))
                 (entry (assoc filename coverage :test (function string=))))
            (unless entry
              (error "Coverage has no source entry for ~A" filename))
            (let* ((form-indices
                     (with-input-from-string
                         (stream (uiop:read-file-string pathname))
                       (let ((*read-eval* t))
                         (loop for index from 0
                               for form = (read stream nil :eof)
                               until (eq form :eof)
                               for operator = (and (consp form) (car form))
                               when (and (symbolp operator)
                                         (member (symbol-name operator)
                                                 (list "IN-PACKAGE" "DEFPACKAGE")
                                                 :test (function string=)))
                                 collect index))))
                   (paths (second entry))
                   (bits (cddr entry)))
              (dolist (form-index form-indices)
                (unless (loop for path across paths
                              for bit across bits
                              for source-path = (if (integerp (car path)) path (cdr path))
                              thereis (and (= (car (last source-path)) form-index)
                                           (= bit 1)))
                  (error "Coverage did not harvest top-level package form ~D in ~A"
                         form-index filename)))
              (setf (gethash filename *coverage-package-form-files-asserted*) t)))))))
  t)
(defun coverage-statistics-for (pathnames &optional exclusions)
  (assert-coverage-package-forms-covered pathnames)
  (let ((statistics
         (uiop:symbol-call "CL-WEAVE" "COVERAGE-STATISTICS"
                           :include-pathnames pathnames)))
    (if exclusions
        (coverage-statistics-with-exclusions statistics exclusions (coverage-uncovered-form-locations-for pathnames))
      (values statistics nil))))
(defun coverage-uncovered-form-locations-for (pathnames)
  (loop for file in (uiop:symbol-call "CL-WEAVE" "COVERAGE-FILE-STATISTICS"
                                      :include-pathnames pathnames)
        for source = (namestring (getf file :pathname))
        append (coverage-uncovered-form-locations-for-file source)))
(defun measure-context-coverage (root groups)
  (mapcar (lambda (group)
            (let* ((context (car group))
                   (pathnames (cdr group))
                   (baseline (read-coverage-baseline-if-present root context)))
              (multiple-value-bind (statistics applied)
                  (coverage-statistics-for
                   pathnames (getf baseline :coverage-exclusions))
                (list context (length pathnames) statistics baseline applied))))
          groups))
(defun run-coverage (root)
  (asdf:load-system "cl-weave")
  (uiop:symbol-call "CL-WEAVE" "REQUIRE-COVERAGE-SUPPORT")
  (proclaim (list 'optimize
                  (list (find-symbol "STORE-COVERAGE-DATA" "SB-COVER") 3)))
  (when (find-package "AITOOLS.DATA")
    (delete-package "AITOOLS.DATA"))
  (uiop:symbol-call "CL-WEAVE" "RESET-COVERAGE")
  (let ((*harvest-sb-cover-marks-after-source-load* t))
    (asdf:load-system "aitools" :force t)
    (asdf:load-system "aitools/cli")
    (asdf:load-system "aitools/test")
    (uiop:symbol-call :asdf-user :%load-aitools-test-systems))
  (let* ((include (coverage-candidate-files))
         (aggregate-baseline (read-coverage-baseline root :aggregate))
         (json (make-string-output-stream)))
    (setf (symbol-value (find-symbol "*LAST-TEST-OUTCOMES*" "AITOOLS/TEST")) nil)
    (multiple-value-bind (groups exclusions)
        (classify-coverage-inputs include root)
      (let* ((selected (loop for group in groups append (copy-list (cdr group))))
             (passed (uiop:symbol-call
                      "CL-WEAVE" "RUN-ALL"
                      :timeout-ms 60000 :reporter :json :stream json :coverage t
                      :coverage-output (merge-pathnames "coverage.json" root)
                      :coverage-report-directory (merge-pathnames "coverage/" root)
                      :coverage-include-pathnames selected
                      :coverage-minimum-expression 0 :coverage-minimum-branch 0
                      :coverage-reset nil
                      :pass-with-no-tests nil :max-workers 1))
             (report (get-output-stream-string json))
             (parsed (uiop:symbol-call :json-kit :parse report))
             (events (coerce (gethash "events" parsed) 'list)))
        (setf (symbol-value (find-symbol "*LAST-TEST-OUTCOMES*" "AITOOLS/TEST"))
              (cons :json events))
        (multiple-value-bind (problems executed context-counts)
            (uiop:symbol-call "AITOOLS/TEST" "%POLICY-PROBLEMS" events nil)
          (let* ((measured (measure-context-coverage root groups))
                 (statistics (aggregate-context-coverage-statistics measured))
                 (aggregate
                  (append
                   (coverage-measurement-record
                    :aggregate (length selected) statistics aggregate-baseline)
                   (list :executed-spec-count executed :coverage-exclusions (context-coverage-exclusions measured))))
                 (contexts
                  (mapcar
                   (lambda (entry)
                     (append
                      (coverage-measurement-record
                       (first entry) (second entry) (third entry) (fourth entry))
                      (list :executed-spec-count (cdr (assoc (first entry) context-counts)) :coverage-exclusions (fifth entry))))
                   measured)))
            (write-context-measurements root aggregate contexts (append exclusions (context-coverage-exclusions measured)))
            (format t "~&aitools coverage suite: ~D specs run~%" executed)
            (format t "~&aitools coverage: expression ~D/~D, branch ~D/~D~%"
                    (getf statistics :expression-covered)
                    (getf statistics :expression-total)
                    (getf statistics :branch-covered)
                    (getf statistics :branch-total))
            (format t "~&coverage context measurements: ~A~%"
                    (namestring
                     (merge-pathnames "coverage/context-measurements.sexp" root)))
            (format t "~&coverage exclusions:~%")
            (let ((all-exclusions
                   (append exclusions (context-coverage-exclusions measured))))
              (if all-exclusions
                  (dolist (entry all-exclusions)
                    (report-coverage-exclusion entry))
                (format t "  none~%")))
            (dolist (entry measured)
              (let ((stats (third entry))
                    (spec-count (cdr (assoc (first entry) context-counts))))
                (format t "~&coverage context ~A specs run: ~D~%"
                        (first entry) spec-count)
                (format t "~&~A coverage: expression ~D/~D, branch ~D/~D~%"
                        (first entry) (getf stats :expression-covered)
                        (getf stats :expression-total)
                        (getf stats :branch-covered) (getf stats :branch-total))))
            (uiop:symbol-call :aitools/test :%validate-coverage-artifacts
                              (merge-pathnames "coverage.json" root)
                              (merge-pathnames "coverage/cover-index.html" root)
                              selected)
            (assert-coverage-baseline statistics aggregate-baseline :aggregate)
            (dolist (entry measured)
              (when (fourth entry)
                (assert-coverage-baseline
                 (third entry) (fourth entry) (first entry))))
            (require-context-coverage-baselines measured)
            (dolist (problem problems)
              (format t "~&aitools coverage: ~A~%" problem))
            (unless (null problems)
              (error "aitools coverage test policy failed"))
            (unless passed
              (error "aitools coverage suite failed"))))))))

(handler-case
    (let ((root (script-directory)))
      (configure-local-source-registry root)
      (let ((coverage (uiop:getenv "AITOOLS_COVERAGE"))
            (context (uiop:getenv "AITOOLS_TEST_CONTEXT")))
        (when (and coverage context)
          (error "AITOOLS_TEST_CONTEXT cannot be combined with AITOOLS_COVERAGE."))
        (if coverage
            (run-coverage root)
          (progn
            (asdf:load-system "aitools")
            (asdf:test-system "aitools/test"))))
      (uiop:quit 0))
  (error (condition)
         (let* ((test-package (find-package "AITOOLS/TEST"))
                (reporter (and test-package
                               (find-symbol "%REPORT-TEST-RUNNER-OUTCOMES" test-package)))
                (outcomes-symbol (and test-package
                                      (find-symbol "*LAST-TEST-OUTCOMES*" test-package))))
           (format t "~&aitools tests failed: ~A: ~A~%"
                   (type-of condition) condition)
           (when (and reporter (fboundp reporter)
                      outcomes-symbol (boundp outcomes-symbol))
             (funcall (symbol-function reporter)
                      (symbol-value outcomes-symbol) *standard-output*)))
         (finish-output *standard-output*)
         (uiop:quit 1)))
