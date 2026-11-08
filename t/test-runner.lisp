(in-package #:aitools/test)

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
    (%report-test-outcomes events stream)))

(defun run-tests (&key (timeout-ms +default-test-timeout-ms+))
  "Run registered cl-weave specs with a per-spec timeout and fail on test or run-policy errors."
  (setf *last-test-outcomes* nil)
  (let ((context (uiop:getenv "AITOOLS_TEST_CONTEXT")))
    (%validate-context-test-systems context)
    (let* ((json (make-string-output-stream))
           (events
            (call-with-spec-progress
             (lambda ()
               (cl-weave:run nil :timeout-ms timeout-ms
                             :reporter :json :stream json))))
           (report (json-kit:parse (get-output-stream-string json))))
      (setf *last-test-outcomes* events)
      (cl-weave:explain! events)
      (multiple-value-bind (problems executed)
          (%policy-problems (coerce (gethash "events" report) 'list) context)
        (dolist (problem problems)
          (format t "~&aitools/test: ~A~%" problem))
        (unless (and (cl-weave:results-status events) (null problems))
          (error "aitools test suite failed"))
        (format t "~&aitools/test: successful completion with 0 failures, ~D specs run~%"
                executed))
      (%report-test-outcomes events)))
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

(in-package #:cl-user)

(defparameter *coverage-exclusion-rules* '(("data/" . "Shared data tables have no single executable-context owner.")))

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
  (loop for system in (asdf:registered-systems)
        when (and (stringp system)
                  (or (coverage-prefix-p "aitools/core/" system)
                      (coverage-prefix-p "aitools/feature/" system)
                      (member system
                              (list "aitools/data" "aitools/cli/foundation" "aitools/cli")
                              :test (function string=))))
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

(in-package #:aitools/test)
