;;;;
;;;; Lisp-level test entry point:
;;;;
;;;;     sbcl --script run-tests.lisp
;;;;
;;;; Registers this checkout on ASDF's source registry, inherits the caller's
;;;; configuration for sibling dependencies (cl-cli, cl-weave, and every kit
;;;; -- set CL_SOURCE_REGISTRY to the nerima-lisp checkout root so ASDF can
;;;; find them), and runs the test system. The exit goes through a
;;;; HANDLER-CASE around ASDF:TEST-SYSTEM so a
;;;; failing suite is a named one-line diagnostic and a deliberate exit
;;;; rather than an escaping backtrace, and the zero exit means only "TEST-OP
;;;; signalled nothing" -- read the "successful completion" line for whether
;;;; any spec actually ran. See docs/src/project/development.md,
;;;; "Running the tests", for the environment it expects.

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

(defun coverage-context-directory-prefixes (root)
  (let ((groups (list (cons :composition (list "src/")))))
    (dolist (system
             (sort
              (remove-if-not
               (lambda (name)
                 (and (stringp name)
                      (or (coverage-prefix-p "aitools/core/" name)
                          (coverage-prefix-p "aitools/feature/" name))))
               (asdf:registered-systems))
              #'string<))
      (let* ((prefix (if (coverage-prefix-p "aitools/core/" system)
                         "aitools/core/"
                       "aitools/feature/"))
             (suffix (subseq system (length prefix)))
             (slash (position #\/ suffix))
             (context-name (if slash (subseq suffix 0 slash) suffix))
             (context (intern (string-upcase context-name) :keyword))
             (directory (coverage-relative-path
                         (asdf:system-source-directory system)
                         root))
             (directory-prefix
              (if (and (plusp (length directory))
                       (char/= (char directory (1- (length directory))) #\/))
                  (concatenate 'string directory "/")
                directory))
             (entry (assoc context groups)))
        (unless entry
          (push (cons context nil) groups)
          (setf entry (assoc context groups)))
        (push directory-prefix (cdr entry))))
    (mapcar (lambda (entry)
              (cons (car entry)
                    (remove-duplicates (nreverse (cdr entry)) :test #'string=)))
            groups)))
  (defparameter *coverage-exclusion-rules* '(("data/" . "Shared data tables have no single executable-context owner.")))
  (defun coverage-prefix-p (prefix string)
    (and (<= (length prefix) (length string))
         (string= prefix string :end2 (length prefix))))
  (defun coverage-relative-path (pathname root)
    (let ((relative (namestring (enough-namestring pathname root))))
      (when (or (coverage-prefix-p "/" relative)
                (coverage-prefix-p "../" relative)
                (string= relative ".."))
        (error "Coverage input is outside the project root: ~A" relative))
      relative))
  (defun coverage-system-files (system)
    (uiop:symbol-call "CL-WEAVE" "ASDF-SYSTEM-FILES" system :include-dependencies nil))
  (defun coverage-candidate-files ()
    (loop for system in (cons "aitools/cli" (asdf:system-depends-on (asdf:find-system "aitools")))
          append (coverage-system-files system)))
(defun classify-coverage-inputs (include root)
  (unless include
    (error "No ASDF source files were included for coverage"))
  (let* ((context-prefixes (coverage-context-directory-prefixes root))
         (groups (mapcar (lambda (entry) (cons (car entry) nil))
                         context-prefixes))
         (excluded nil))
    (dolist (pathname include)
      (let* ((relative (coverage-relative-path pathname root))
             (context (find-if (lambda (entry)
                                 (find-if (lambda (prefix)
                                            (coverage-prefix-p prefix relative))
                                          (cdr entry)))
                               context-prefixes))
             (rule (find-if (lambda (entry)
                              (coverage-prefix-p (car entry) relative))
                            *coverage-exclusion-rules*)))
        (cond
         (context (push pathname (cdr (assoc (car context) groups))))
         (rule (push (list :rule (car rule) :path relative :reason (cdr rule))
                     excluded))
         (t (error "Unclassified coverage input: ~A" relative)))))
    (dolist (group groups)
      (unless (cdr group)
        (error "Coverage context has no included ASDF source files: ~S" (car group)))
      (setf (cdr group) (nreverse (cdr group))))
    (values groups (nreverse excluded))))
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
  (defun coverage-statistics-for (pathnames &optional context baseline &key new-baseline-p)
    (let* ((statistics (uiop:symbol-call "CL-WEAVE" "COVERAGE-STATISTICS"
                                         :include-pathnames pathnames))
           (current-count
             (when (or new-baseline-p
                       (getf baseline :uncovered-in-package-expressions))
               (uncovered-in-package-expression-count
                (uiop:symbol-call "CL-WEAVE" "COVERAGE-FILE-STATISTICS"
                                  :include-pathnames pathnames)))))
      (cond
        (new-baseline-p
         (let ((measured (copy-list statistics)))
           (setf (getf measured :uncovered-in-package-expressions) current-count)
           measured))
        ((getf baseline :uncovered-in-package-expressions)
         (adjust-context-expression-total statistics context current-count baseline))
        (t statistics))))
  (defun measure-context-coverage (root groups)
    (mapcar (lambda (group)
              (let* ((context (car group)) (pathnames (cdr group))
                     (baseline (read-coverage-baseline-if-present root context)))
                (list context (length pathnames)
                      (coverage-statistics-for pathnames context baseline
                                               :new-baseline-p (null baseline))
                      baseline)))
            groups))
(defun run-coverage (root)
  (asdf:load-system "cl-weave")
  (uiop:symbol-call "CL-WEAVE" "REQUIRE-COVERAGE-SUPPORT")
  (proclaim (list 'optimize
                  (list (find-symbol "STORE-COVERAGE-DATA" "SB-COVER") 3)))
  (when (find-package "AITOOLS.DATA")
    (delete-package "AITOOLS.DATA"))
  (asdf:load-system "aitools" :force t)
  (asdf:load-system "aitools/cli" :force t)
  (asdf:load-system "aitools/test")
  (uiop:symbol-call :asdf-user :%load-aitools-test-systems)
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
                      :pass-with-no-tests nil))
             (report (get-output-stream-string json))
             (parsed (uiop:symbol-call :json-kit :parse report))
             (events (coerce (gethash "events" parsed) 'list)))
        (progn
          (setf (symbol-value (find-symbol "*LAST-TEST-OUTCOMES*" "AITOOLS/TEST"))
                (cons :json events))
          (multiple-value-bind (problems executed)
              (uiop:symbol-call "AITOOLS/TEST" "%POLICY-PROBLEMS" events)
            (dolist (problem problems)
              (format t "~&aitools coverage: ~A~%" problem))
            (unless (null problems)
              (error "aitools coverage test policy failed"))
            (format t "~&aitools coverage suite: ~D specs run~%" executed)))
        (let* ((statistics (coverage-statistics-for selected :aggregate aggregate-baseline))
               (measured (measure-context-coverage root groups))
               (aggregate (coverage-measurement-record
                           :aggregate (length selected) statistics aggregate-baseline))
               (contexts (mapcar (lambda (entry)
                                   (coverage-measurement-record
                                    (first entry) (second entry) (third entry)
                                    (fourth entry)))
                                 measured)))
          (uiop:symbol-call :aitools/test :%validate-coverage-artifacts
                            (merge-pathnames "coverage.json" root)
                            (merge-pathnames "coverage/cover-index.html" root)
                            selected)
          (write-context-measurements root aggregate contexts exclusions)
          (assert-coverage-baseline statistics aggregate-baseline :aggregate)
          (dolist (entry measured)
            (when (fourth entry)
              (assert-coverage-baseline
               (third entry) (fourth entry) (first entry))))
          (unless passed (error "aitools coverage suite failed"))
          (format t "~&aitools coverage: expression ~D/~D, branch ~D/~D~%"
                  (getf statistics :expression-covered)
                  (getf statistics :expression-total)
                  (getf statistics :branch-covered)
                  (getf statistics :branch-total))
          (format t "~&coverage context measurements: ~A~%"
                  (namestring
                   (merge-pathnames "coverage/context-measurements.sexp" root)))
          (format t "~&coverage exclusions:~%")
          (if exclusions
              (dolist (entry exclusions)
                (format t "  ~A via ~A: ~A~%"
                        (getf entry :path) (getf entry :rule) (getf entry :reason)))
            (format t "  none~%"))
          (progn
            (dolist (entry measured)
              (let ((stats (third entry)))
                (format t "~&~A coverage: expression ~D/~D, branch ~D/~D~%"
                        (first entry) (getf stats :expression-covered)
                        (getf stats :expression-total) (getf stats :branch-covered)
                        (getf stats :branch-total))))
            (require-context-coverage-baselines measured)))))))


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
