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
  (asdf:load-system "cl-weave")
  (uiop:symbol-call "CL-WEAVE" "REQUIRE-COVERAGE-SUPPORT")
  (proclaim
   (list (quote optimize)
         (list (find-symbol "STORE-COVERAGE-DATA" "SB-COVER") 3)))
  (when (find-package "AITOOLS.DATA")
    (delete-package "AITOOLS.DATA"))
  (asdf:load-system "aitools" :force t)
  (asdf:load-system "aitools/cli" :force t)
  (asdf:load-system "aitools/test")
  (setf (symbol-value (find-symbol "*DEFAULT-TIMEOUT-MS*" "CL-WEAVE"))
        60000)
  (let* ((include
           (uiop:symbol-call "CL-WEAVE" "ASDF-SYSTEM-FILES" "aitools"))
         (json (make-string-output-stream)))
    (setf (symbol-value (find-symbol "*LAST-TEST-OUTCOMES*" "AITOOLS/TEST"))
          nil)
    (let* ((passed
             (uiop:symbol-call "CL-WEAVE" "RUN-ALL"
                               :reporter :json
                               :stream json
                               :coverage t
                               :coverage-output (merge-pathnames "coverage.json" root)
                               :coverage-report-directory (merge-pathnames "coverage/" root)
                               :coverage-include-pathnames include
                               :coverage-minimum-expression 0
                               :coverage-minimum-branch 0
                               :pass-with-no-tests nil))
           (report (get-output-stream-string json))
           (parsed (uiop:symbol-call :json-kit :parse report))
           (events (coerce (gethash "events" parsed) (quote list))))
      (setf (symbol-value (find-symbol "*LAST-TEST-OUTCOMES*" "AITOOLS/TEST"))
            (cons :json events))
      (unless passed
        (error "aitools coverage suite failed"))
      (let* ((statistics
               (uiop:symbol-call "CL-WEAVE" "COVERAGE-STATISTICS"
                                 :include-pathnames include))
             (baseline
               (with-open-file (stream (merge-pathnames "coverage-baseline.lisp" root))
                 (read stream)))
             (expression-percentage
               (* 100.0
                  (/ (getf statistics :expression-covered)
                     (max 1 (getf statistics :expression-total)))))
             (branch-percentage
               (* 100.0
                  (/ (getf statistics :branch-covered)
                     (max 1 (getf statistics :branch-total))))))
        (unless (and (>= expression-percentage (getf baseline :expression))
                     (>= branch-percentage (getf baseline :branch))
                     (probe-file (merge-pathnames "coverage.json" root))
                     (probe-file (merge-pathnames "coverage/cover-index.html" root)))
          (error "aitools coverage is below baseline or has no report artifact"))
        (format t "~&aitools coverage: expression ~D/~D, branch ~D/~D~%"
                (getf statistics :expression-covered)
                (getf statistics :expression-total)
                (getf statistics :branch-covered)
                (getf statistics :branch-total))))))

(handler-case
    (let ((root (script-directory)))
      (configure-local-source-registry root)
      (if (uiop:getenv "AITOOLS_COVERAGE")
          (run-coverage root)
          (progn
            (asdf:load-system "aitools")
            (asdf:load-system "aitools/test")
            (setf (symbol-value (find-symbol "*DEFAULT-TIMEOUT-MS*" "CL-WEAVE"))
                  60000)
            (unless (uiop:symbol-call :aitools/test :run-tests)
              (error "aitools self test suite failed."))))
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
