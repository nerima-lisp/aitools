;;;; t/package.lisp
;;;;
;;;; The root test package. Every unit-test package (AITOOLS.<CONTEXT>.TEST)
;;;; re-exports these same cl-weave imports rather than importing cl-weave
;;;; directly a second time, so `describe`/`it`/`expect` mean one thing
;;;; everywhere in the suite. See docs/src/project/development.md ("Test
;;;; layout", "The component lists") for where each context's tests live
;;;; and how they load.
(in-package #:cl-user)

(defpackage #:aitools/test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave
                #:it #:it-each #:describe-each #:it-property #:gen-string
                #:expect #:expect-not #:expect-poll #:signals #:run-all
                #:with-soft-assertions #:defmatcher #:before-each
                #:gen-boolean #:gen-character #:gen-integer #:gen-keyword
                #:gen-list #:gen-map #:gen-member #:gen-one-of #:gen-tuple
                #:gen-vector #:gen-such-that)
  (:export #:run-tests
           #:describe #:it #:it-each #:describe-each #:it-property
           #:expect #:expect-not #:expect-poll #:signals #:run-all
           #:with-soft-assertions #:defmatcher #:before-each
           #:gen-string #:gen-boolean #:gen-character #:gen-integer
           #:gen-keyword #:gen-list #:gen-map #:gen-member #:gen-one-of
           #:gen-tuple #:gen-vector #:gen-such-that))

(in-package #:aitools/test)

(progn
  (defparameter cl-weave:*default-timeout-ms* 30000
    "Bound every cl-weave spec to 30 seconds unless it declares a tighter limit.
The limit covers a single spec, while external-process helpers retain their
own operation-specific budgets.")
  (defvar *last-test-outcomes* nil))

(defparameter *spec-count-baseline*
  (let ((pathname (merge-pathnames "t/spec-count-baseline.lisp"
                                   (asdf:system-source-directory "aitools/test/base"))))
    (with-open-file (stream pathname)
      (let* ((*read-eval* nil)
             (baseline (read stream nil :eof))
             (minimum (getf baseline :minimum-executed-specs))
             (starts (getf baseline :outer-start-events))
             (ends (getf baseline :outer-end-events))
             (nested (getf baseline :nested-e2e-diagnostic-events)))
        (unless (and (listp baseline) (evenp (length baseline))
                     (= (getf baseline :schema-version) 1)
                     (eq (getf baseline :source) :successful-darwin-ci-run)
                     (stringp (getf baseline :run-id))
                     (stringp (getf baseline :revision))
                     (eq (getf baseline :platform) :darwin)
                     (integerp minimum) (plusp minimum)
                     (integerp starts) (= starts minimum)
                     (integerp ends) (= ends minimum)
                     (integerp nested) (not (minusp nested))
                     (stringp (getf baseline :reason))
                     (plusp (length (getf baseline :reason))))
          (error "Invalid executed-spec baseline: ~S" baseline))
        (unless (eq (read stream nil :eof) :eof)
          (error "Executed-spec baseline contains trailing data: ~A" pathname))
        baseline))))

(defparameter *minimum-executed-specs*
  (getf *spec-count-baseline* :minimum-executed-specs)
  "Fewer executed specs (passed, failed or errored; not skipped or todo) than the data-backed baseline fails the run.")
(defparameter *allowed-unrun-specs*
  ;; Optional fields are an environment variable, its required state, and exact-reason matching.
  '((:skip "zip/unzip are not on PATH" "archive interop oracle needs the host tools")
    (:skip "tar is not on PATH" "archive interop oracle needs the host tools")
    (:skip "tar/gzip are not on PATH" "archive interop oracle needs the host tools")
    (:skip "git is not on PATH" "gitignore parity oracle needs git")
    (:skip "no zoneinfo database on this host" "host zoneinfo check; the Nix sandbox has none")
    (:skip "skipped without the cl-process-kit-spawn trampoline"
     "bg needs the native spawn helper" "CL_PROCESS_KIT_SPAWN")
    (:skip "oracle unavailable"
     "an e2e row's reference tool is not on PATH; checks.default adds the common ones and niche tools (tree, dos2unix, nkf) skip. A wholesale e2e skip still trips *minimum-executed-specs*.")
    (:skip "trace specs require a binary with --trace"
     "trace-positive e2e cases are skipped during no-trace byte captures"
     "AITOOLS_E2E_BYTE_CAPTURE_DIR" :set :exact-reason)))

(defvar *progress-lock* (sb-thread:make-mutex :name "aitools test progress"))
(defvar *current-spec-id* nil)
(defvar *current-spec-no-trace-invocation* 0)

(defun %report-progress (control &rest arguments)
  (sb-thread:with-mutex (*progress-lock*)
    (format *error-output* "~&~?~%" control arguments)
    (finish-output *error-output*)))

(defun call-with-spec-progress (function)
  "Call FUNCTION with a start and an end line (with elapsed milliseconds)
written to *ERROR-OUTPUT* around every spec attempt, flushed at once, so a
run killed by an outer timeout shows the spec it stopped in. cl-weave has no
public per-spec hook (AROUND-EACH does not see the spec's name, and the
reporters only run after the whole suite), so this wraps its internal
CALL-TEST-CASE/K, whose (SUITE TEST CONTINUE) arguments TEST-PATH turns into
the reported path."
  (sb-int:encapsulate
   'cl-weave::call-test-case/k 'aitools-progress
   (lambda (next suite test continue)
     (let* ((name (cl-weave::path-string (cl-weave::test-path suite test)))
            (*current-spec-id* name)
            (*current-spec-no-trace-invocation* 0)
            (start (get-internal-real-time)))
       (%report-progress "[start] ~A" name)
       (unwind-protect (funcall next suite test continue)
         (%report-progress "[end] ~A (~D ms)" name
                           (round (* 1000 (- (get-internal-real-time) start))
                                  internal-time-units-per-second))))))
  (unwind-protect (funcall function)
    (sb-int:unencapsulate 'cl-weave::call-test-case/k 'aitools-progress)))

(defun %event-field (event key)
  (gethash key event))

(defun %allowed-unrun-p (event)
  (let ((status (if (string= (%event-field event "status") "skip") :skip :todo))
        (reason (or (%event-field event "reason") ""))
        (path (%event-field event "pathString")))
    (find-if (lambda (entry)
               (destructuring-bind (entry-status text why &optional env-var env-mode match-mode) entry
                 (declare (ignore why))
                 (and (eq status entry-status)
                      (if (eq match-mode :exact-reason)
                          (string= text reason)
                        (or (search text reason) (search text path)))
                      (if (eq env-mode :set)
                          (let ((value (and env-var (uiop:getenv env-var))))
                            (and value (plusp (length value))))
                        (not (and env-var (uiop:getenv env-var)))))))
             *allowed-unrun-specs*)))

(defun %policy-problems (events)
  "Why the run fails even though no spec failed, as a list of strings."
  (let ((executed (count-if (lambda (event)
                              (member (%event-field event "status") '("pass" "fail" "error") :test #'string=))
                            events))
        (problems '()))
    (when (< executed *minimum-executed-specs*)
      (push (format nil "only ~D specs ran; at least ~D are expected (*MINIMUM-EXECUTED-SPECS*)"
                    executed *minimum-executed-specs*)
            problems))
    (dolist (event events)
      (when (and (member (%event-field event "status") '("skip" "todo") :test #'string=)
                 (not (%allowed-unrun-p event)))
        (push (format nil "~A was not run (~A: ~A) and is not in *ALLOWED-UNRUN-SPECS*"
                      (%event-field event "pathString") (%event-field event "status")
                      (%event-field event "reason"))
              problems)))
    (values (nreverse problems) executed)))
(progn
  (defun %report-test-outcomes (events &optional (stream *standard-output*))
    (let ((outcomes
            (remove-if-not
             (lambda (event)
               (member (cl-weave::test-event-status event) (list :fail :error :skip)))
             (cl-weave::normalize-run-results events))))
      (when outcomes
        (format stream "~&aitools/test: failed, errored, or skipped specs (~D):~%"
                (length outcomes))
        (dolist (event outcomes)
          (let* ((status (cl-weave::test-event-status event))
                 (condition (cl-weave::test-event-condition event))
                 (reason (cl-weave::test-event-reason event))
                 (assertion (cl-weave::test-event-assertion event)))
            (format stream "  [~(~A~)] ~A"
                    status (cl-weave::path-string (cl-weave::test-event-path event)))
            (when condition
              (format stream " condition ~A: ~A" (type-of condition) condition))
            (when assertion
              (format stream " assertion ~A expected ~S, got ~S"
                      (cl-weave::assertion-detail-matcher assertion)
                      (cl-weave::assertion-detail-expected assertion)
                      (cl-weave::assertion-detail-actual assertion)))
            (when (or reason (eq status :skip))
              (format stream " reason: ~A" (or reason "unspecified")))
            (terpri stream)))
        (finish-output stream))
      (length outcomes)))
  (defun %report-json-test-outcomes (events &optional (stream *standard-output*))
    (let ((outcomes
           (remove-if-not
            (lambda (event)
              (member (gethash "status" event) (list "fail" "error" "skip")
                      :test (function string=)))
            events)))
      (when outcomes
        (format stream "~&aitools/test: failed, errored, or skipped specs (~D):~%"
                (length outcomes))
        (dolist (event outcomes)
          (let* ((status (gethash "status" event))
                 (condition (gethash "condition" event))
                 (raw-reason (gethash "reason" event))
                 (reason (unless (or (null raw-reason)
                                     (json-kit:json-null-p raw-reason))
                           raw-reason))
                 (assertion (gethash "assertion" event)))
            (format stream "  [~A] ~A" status (gethash "pathString" event))
            (when (and condition (not (json-kit:json-null-p condition)))
              (format stream " condition ~A" condition))
            (when (and assertion (hash-table-p assertion))
              (format stream " assertion ~A expected ~S, got ~S"
                      (gethash "matcher" assertion)
                      (gethash "expected" assertion)
                      (gethash "actual" assertion)))
            (when (or reason (string= status "skip"))
              (format stream " reason: ~A" (or reason "unspecified")))
            (terpri stream)))
        (finish-output stream))
      (length outcomes)))
  (defun %report-test-runner-outcomes (events &optional (stream *standard-output*))
    (if (and (consp events) (eq (car events) :json))
        (%report-json-test-outcomes (cdr events) stream)
        (%report-test-outcomes events stream))))

(defun run-tests ()
  "Run every registered cl-weave spec with the spec report on
*STANDARD-OUTPUT*, and signal, so ASDF's TEST-OP fails, when a spec failed,
when too few ran, or when a spec was skipped for a reason
*ALLOWED-UNRUN-SPECS* does not list."
  (setf *last-test-outcomes* nil)
  (let* ((json (make-string-output-stream))
         (events (call-with-spec-progress
                  (lambda () (cl-weave:run nil :reporter :json :stream json))))
         (report (json-kit:parse (get-output-stream-string json))))
    (setf *last-test-outcomes* events)
    (cl-weave:explain! events)
    (multiple-value-bind (problems executed)
        (%policy-problems (coerce (gethash "events" report) (quote list)))
      (dolist (problem problems)
        (format t "~&aitools/test: ~A~%" problem))
      (unless (and (cl-weave:results-status events) (null problems))
        (error "aitools test suite failed"))
      (format t "~&aitools/test: successful completion with 0 failures, ~D specs run~%"
              executed))
    (%report-test-outcomes events))
  t)
(defun %call-with-test-runner-reporting (thunk)
  (handler-case
      (funcall thunk)
    (error (condition)
      (format t "~&aitools tests failed: ~A: ~A~%"
              (type-of condition) condition)
      (%report-test-runner-outcomes *last-test-outcomes*)
      (finish-output *standard-output*)
      (error condition))))
