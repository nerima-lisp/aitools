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

(defconstant +default-test-timeout-ms+ 60000)
(defvar *last-test-outcomes* nil)

(defvar *minimum-executed-specs* 0
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
(defun %test-source-path-key (file)
  (let ((marker (search "/t/" file :from-end t)))
    (when marker (subseq file (+ marker 3)))))

;; Host events can name the Nix source copy while ASDF lists checkout paths.
(defun %test-context-from-source-file (file source-files)
  (let* ((source (when (and (stringp file) (probe-file file))
                   (namestring (truename file))))
         (source-key (when source (%test-source-path-key source)))
         (contexts
           (when source
             (remove-duplicates
              (loop for entry in source-files
                    for candidate = (cdr entry)
                    for candidate-key = (%test-source-path-key candidate)
                    when (or (string= source candidate)
                             (and source-key candidate-key
                                  (string= source-key candidate-key)))
                      collect (car entry))
              :test #'eq))))
    (when (cdr contexts)
      (error "Test source file belongs to multiple contexts: ~A" file))
    (car contexts)))
(defun %context-test-source-files ()
  (loop for context in (mapcar #'car *context-spec-count-baselines*)
        for system-name = (format nil "aitools/test/~A"
                                  (string-downcase (symbol-name context)))
        append
        (loop for pathname in
              (uiop:symbol-call "CL-WEAVE" "ASDF-SYSTEM-FILES"
                                system-name :include-dependencies nil)
              collect (cons context (namestring (truename pathname))))))

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

(defun %policy-problems (events &optional (context (uiop:getenv "AITOOLS_TEST_CONTEXT")))
  "Why the run fails even though no spec failed, as a list of strings."
  (let* ((minimum (%minimum-executed-specs-for-context context))
         (executed-events
           (remove-if-not
            (lambda (event)
              (member (%event-field event "status") '("pass" "fail" "error")
                      :test #'string=))
            events))
         (executed (length executed-events))
         (problems '())
         (context-counts nil))
    (when (< executed minimum)
      (push (format nil "only ~D specs ran; at least ~D are expected for ~A"
                    executed minimum (or context "the full suite"))
            problems))
    (when (null context)
      (let ((source-files (%context-test-source-files))
            (counts (make-hash-table :test #'eq)))
        (dolist (entry *context-spec-count-baselines*)
          (setf (gethash (car entry) counts) 0))
        (dolist (event executed-events)
          (let* ((location (%event-field event "location"))
                 (file (and (hash-table-p location)
                            (%event-field location "file")))
                 (owner (%test-context-from-source-file file source-files)))
            (if owner
                (incf (gethash owner counts))
              (push (format nil "executed spec ~A has no matching context source file ~S"
                            (or (%event-field event "pathString") "<unknown>")
                            file)
                    problems))))
        (dolist (entry *context-spec-count-baselines*)
          (let* ((context (car entry))
                 (minimum (getf (cdr entry) :minimum-executed-specs))
                 (context-executed (gethash context counts)))
            (push (cons context context-executed) context-counts)
            (when (< context-executed minimum)
              (push (format nil "context ~A has only ~D specs run; at least ~D are expected"
                            context context-executed minimum)
                    problems))))))
    (dolist (event events)
      (when (and (member (%event-field event "status") '("skip" "todo") :test #'string=)
                 (not (%allowed-unrun-p event)))
        (push (format nil "~A was not run (~A: ~A) and is not in *ALLOWED-UNRUN-SPECS*"
                      (%event-field event "pathString") (%event-field event "status")
                      (%event-field event "reason"))
              problems)))
    (values (nreverse problems) executed (nreverse context-counts))))
