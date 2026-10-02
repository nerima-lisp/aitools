;;;; run-tests.lisp
;;;;
;;;; Lisp-level test entry point:
;;;;
;;;;     sbcl --script run-tests.lisp
;;;;
;;;; Registers this checkout on ASDF's source registry, inherits the caller's
;;;; configuration for sibling dependencies (cl-cli, cl-weave, and every kit
;;;; -- set CL_SOURCE_REGISTRY to the nerima-lisp checkout root so ASDF can
;;;; find them), and runs the test system. Mirrors cl-cowsay's run-tests.lisp:
;;;; the exit goes through a HANDLER-CASE around ASDF:TEST-SYSTEM so a
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

(defun run-coverage (root)
  (let* ((include (uiop:symbol-call "CL-WEAVE" "ASDF-SYSTEM-FILES" "aitools"))
         (passed (uiop:symbol-call "CL-WEAVE" "RUN-ALL"
                  :reporter :json
                  :coverage t
                  :coverage-output "coverage.json"
                  :coverage-report-directory "coverage"
                  :coverage-include-pathnames include
                  :coverage-minimum-expression 0
                  :coverage-minimum-branch 0
                  :pass-with-no-tests nil)))
    (unless passed
      (error "aitools coverage suite failed"))
    (let* ((statistics (uiop:symbol-call "CL-WEAVE" "COVERAGE-STATISTICS"
                        :include-pathnames include))
           (baseline (with-open-file (stream (merge-pathnames "coverage-baseline.lisp" root))
                       (read stream)))
           (expression-percentage (* 100.0
                                    (/ (getf statistics :expression-covered)
                                       (max 1 (getf statistics :expression-total)))))
           (branch-percentage (* 100.0
                                  (/ (getf statistics :branch-covered)
                                     (max 1 (getf statistics :branch-total))))))
      (unless (and (>= expression-percentage (getf baseline :expression))
                   (>= branch-percentage (getf baseline :branch))
                   (probe-file "coverage.json")
                   (probe-file "coverage/cover-index.html"))
        (error "aitools coverage is below baseline or has no report artifact"))
      (format t "~&aitools coverage: expression ~D/~D, branch ~D/~D~%"
              (getf statistics :expression-covered)
              (getf statistics :expression-total)
              (getf statistics :branch-covered)
              (getf statistics :branch-total)))))

(let ((root (script-directory)))
  (configure-local-source-registry root)
  (handler-case
      (progn
        (asdf:load-system "aitools")
        (asdf:load-system "aitools/test")
        ;; Keep the suite's explicit policy checks in AITOOLS/TEST:RUN-TESTS,
        ;; while preventing the framework's per-spec default from aborting a
        ;; deliberately long integration or e2e case.
        (setf (symbol-value (find-symbol "*DEFAULT-TIMEOUT-MS*" "CL-WEAVE"))
              60000)
        (if (uiop:getenv "AITOOLS_COVERAGE")
            (run-coverage root)
            (unless (uiop:symbol-call :aitools/test :run-tests)
              (error "aitools self test suite failed."))))
    (error (condition)
      (format *error-output* "~&aitools tests failed: ~A~%" condition)
      (uiop:quit 1)))
  (uiop:quit 0))
