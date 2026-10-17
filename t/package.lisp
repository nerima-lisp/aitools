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

(defparameter cl-weave:*default-timeout-ms* 30000
  "Bound every cl-weave spec to 30 seconds unless it declares a tighter limit.
The limit covers a single spec, while external-process helpers retain their
own operation-specific budgets.")

(defparameter *minimum-executed-specs* 2853
  "Fewer specs than this actually running (passed, failed or errored; not
skipped or todo) fails the run: a load order mistake or a suite-wide skip
must not read as a green run. Raise it when specs are added.")

(defparameter *allowed-unrun-specs*
  ;; (status text reason &optional env-var): a skipped or todo spec is
  ;; allowed only when its reason or path contains TEXT, and, with ENV-VAR,
  ;; only while ENV-VAR is unset (setting it asserts the dependency exists).
  ;; Anything else not run fails the suite, including an e2e case skipped
  ;; because the binary under test could not be obtained.
  '((:skip "zip/unzip are not on PATH" "archive interop oracle needs the host tools")
    (:skip "tar is not on PATH" "archive interop oracle needs the host tools")
    (:skip "tar/gzip are not on PATH" "archive interop oracle needs the host tools")
    (:skip "git is not on PATH" "gitignore parity oracle needs git")
    (:skip "no zoneinfo database on this host" "host zoneinfo check; the Nix sandbox has none")
    (:skip "skipped without the cl-process-kit-spawn trampoline"
     "bg needs the native spawn helper" "CL_PROCESS_KIT_SPAWN")
    (:skip "oracle unavailable"
     "an e2e row's reference tool is not on PATH; checks.default adds the common ones and niche tools (tree, dos2unix, nkf) skip. A wholesale e2e skip still trips *minimum-executed-specs*.")))

(defvar *progress-lock* (sb-thread:make-mutex :name "aitools test progress"))

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
     (let ((name (cl-weave::path-string (cl-weave::test-path suite test)))
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
               (destructuring-bind (entry-status text why &optional env-var) entry
                 (declare (ignore why))
                 (and (eq status entry-status)
                      (or (search text reason) (search text path))
                      (not (and env-var (uiop:getenv env-var))))))
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

(defun run-tests ()
  "Run every registered cl-weave spec with the spec report on
*STANDARD-OUTPUT*, and signal, so ASDF's TEST-OP fails, when a spec failed,
when too few ran, or when a spec was skipped for a reason
*ALLOWED-UNRUN-SPECS* does not list."
  (let* ((json (make-string-output-stream))
         (events (call-with-spec-progress
                  (lambda () (cl-weave:run nil :reporter :json :stream json))))
         (report (json-kit:parse (get-output-stream-string json))))
    (cl-weave:explain! events)
    (multiple-value-bind (problems executed) (%policy-problems (coerce (gethash "events" report) 'list))
      (dolist (problem problems)
        (format t "~&aitools/test: ~A~%" problem))
      (unless (and (cl-weave:results-status events) (null problems))
        (error "aitools test suite failed"))
      (format t "~&aitools/test: successful completion with 0 failures, ~D specs run~%" executed)))
  t)
