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

(progn
  (defparameter *coverage-contexts*
    '((:kernel . "packages/core/kernel/src/") (:protocol . "packages/core/protocol/src/")
      (:workspace . "packages/core/workspace/src/") (:text . "packages/core/text/src/")
      (:store . "packages/core/store/src/") (:composition . "src/") (:search . "packages/feature/search/src/")
      (:inspect . "packages/feature/inspect/src/") (:edit . "packages/feature/edit/src/")
      (:journal . "packages/feature/journal/src/") (:process . "packages/feature/process/src/")
      (:vcs . "packages/feature/vcs/src/") (:env . "packages/feature/env/src/") (:util . "packages/feature/util/src/")))
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
    (unless include (error "No ASDF source files were included for coverage"))
    (let ((groups (mapcar (lambda (entry) (cons (car entry) nil))
                          *coverage-contexts*))
          (excluded nil))
      (dolist (pathname include)
        (let* ((relative (coverage-relative-path pathname root))
               (context (find-if (lambda (entry)
                                   (coverage-prefix-p (cdr entry) relative))
                                 *coverage-contexts*))
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
    (dolist (metric (quote (:expression :branch)))
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
  (defun coverage-statistics-for (pathnames)
    (uiop:symbol-call "CL-WEAVE" "COVERAGE-STATISTICS" :include-pathnames pathnames))
  (defun measure-context-coverage (root groups)
    (mapcar (lambda (group)
              (list (car group) (length (cdr group))
                    (coverage-statistics-for (cdr group))
                    (read-coverage-baseline root (car group))))
            groups))
  (defun coverage-measurement-record (context source-count statistics baseline)
    (list :context context :source-count source-count
          :baseline-state (getf baseline :state) :baseline-source (getf baseline :source)
          :expression-covered (getf statistics :expression-covered)
          :expression-total (getf statistics :expression-total)
          :branch-covered (getf statistics :branch-covered)
          :branch-total (getf statistics :branch-total)))
  (defun write-context-measurements (root aggregate contexts exclusions)
    (let* ((path (merge-pathnames "coverage/context-measurements.sexp" root))
           (record (list :schema-version 1 :platform (software-type) :aggregate aggregate
                         :contexts contexts :exclusions exclusions)))
      (ensure-directories-exist path)
      (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write record :stream stream :pretty t :readably t) (terpri stream))
      (format t "~&coverage-context-measurements-sexp ") (write record :stream *standard-output* :readably t) (terpri)
      path))
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
    (setf (symbol-value (find-symbol "*DEFAULT-TIMEOUT-MS*" "CL-WEAVE")) 60000)
    (let* ((include (coverage-candidate-files))
           (aggregate-baseline (read-coverage-baseline root :aggregate))
           (json (make-string-output-stream)))
      (setf (symbol-value (find-symbol "*LAST-TEST-OUTCOMES*" "AITOOLS/TEST")) nil)
      (multiple-value-bind (groups exclusions)
          (classify-coverage-inputs include root)
        (let* ((selected (loop for group in groups append (copy-list (cdr group))))
               (passed (uiop:symbol-call
                        "CL-WEAVE" "RUN-ALL" :reporter :json :stream json :coverage t
                        :coverage-output (merge-pathnames "coverage.json" root)
                        :coverage-report-directory (merge-pathnames "coverage/" root)
                        :coverage-include-pathnames selected
                        :coverage-minimum-expression 0 :coverage-minimum-branch 0
                        :pass-with-no-tests nil))
               (report (get-output-stream-string json))
               (parsed (uiop:symbol-call :json-kit :parse report))
               (events (coerce (gethash "events" parsed) 'list)))
          (setf (symbol-value (find-symbol "*LAST-TEST-OUTCOMES*" "AITOOLS/TEST"))
                (cons :json events))
          (let* ((statistics (coverage-statistics-for selected))
                 (measured (measure-context-coverage root groups))
                 (aggregate (coverage-measurement-record
                             :aggregate (length selected) statistics aggregate-baseline))
                 (contexts (mapcar (lambda (entry)
                                     (coverage-measurement-record
                                      (first entry) (second entry) (third entry)
                                      (fourth entry)))
                                   measured)))
            (unless (and (probe-file (merge-pathnames "coverage.json" root))
                         (probe-file (merge-pathnames "coverage/cover-index.html" root)))
              (error "aitools coverage report artifact is missing"))
            (write-context-measurements root aggregate contexts exclusions)
            (assert-coverage-baseline statistics aggregate-baseline :aggregate)
            (dolist (entry measured)
              (assert-coverage-baseline (third entry) (fourth entry) (first entry)))
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
            (dolist (entry measured)
              (let ((stats (third entry)))
                (format t "~&~A coverage: expression ~D/~D, branch ~D/~D~%"
                        (first entry) (getf stats :expression-covered)
                        (getf stats :expression-total) (getf stats :branch-covered)
                        (getf stats :branch-total)))))))))
 )

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
