;;;; t/perf/search-allocation-test.lisp
;;;;
;;;; Searching a file whose N lines do
;;;; not match allocates the same amount whatever N is. Lines are never
;;;; decoded, the literal prefilter keeps the regex engine off lines that
;;;; lack the pattern's required literal, and the per-line continuations are
;;;; DYNAMIC-EXTENT, so none of that cost scales with N. The measure is
;;;; bytes consed (deterministic), not time. One matching line is present so
;;;; the output path runs too.
(in-package #:aitools.search.test)

(defun %non-matching-file (line-count)
  (%bytes (with-output-to-string (out)
            (dotimes (i line-count) (write-line "the quick brown fox jumps over the lazy dog" out))
            (write-line "a needle 12345 here" out))))

(defun %bytes-consed (thunk &key (trials 5))
  "The least bytes THUNK conses across TRIALS measured runs, after two warmup
runs. sb-ext:get-bytes-consed is process-wide, so a worker or executor thread
allocating during a run lands in the counter too; under host load that noise
scales with the run's wall time and would inflate a single sample. The true
per-run cost is the floor across trials, so take the minimum rather than
widen the threshold."
  (funcall thunk)
  (funcall thunk)
  (loop repeat trials
        minimize (progn
                   (sb-ext:gc :full t)
                   (let ((before (sb-ext:get-bytes-consed)))
                     (funcall thunk)
                     (- (sb-ext:get-bytes-consed) before)))))
(defvar *search-allocation-measurement-cache* :unstarted)
(defun %allocation-system-directories ()
  (remove-duplicates
   (loop for name in (asdf:already-loaded-systems)
         for directory = (ignore-errors (asdf:system-source-directory name))
         when directory
         collect (namestring directory))
   :test
   #'string=))
(defun %allocation-child-environment ()
  (let ((prefix "AITOOLS_COVERAGE="))
    (remove-if
     (lambda (entry)
       (and (>= (length entry) (length prefix)) (string= prefix entry :end2 (length prefix))))
     (sb-ext:posix-environ))))
(defun %allocation-form-text (form)
  (let ((*package* (find-package :cl-user)))
    (prin1-to-string form)))
(defun %allocation-child-setup-form (source-directory cache-directory)
  (let* ((parent-translations asdf/output-translations:*output-translations-parameter*)
         (inherited-directives
           (if (and (consp parent-translations)
                    (eq (first parent-translations) :output-translations))
               (rest parent-translations) '(:inherit-configuration)))
         (project-translation
           (list (list (uiop:ensure-directory-pathname source-directory) :**/ :*.*.*)
                 (list (uiop:ensure-directory-pathname cache-directory) :**/ :*.*.*)))
         (translations (cons :output-translations
                             (cons project-translation inherited-directives))))
    `(handler-case
         (progn
           (let ((*standard-output* (make-broadcast-stream))
                 (*compile-verbose* nil) (*compile-print* nil))
             (setf asdf:*central-registry* ',(%allocation-system-directories))
             (asdf:initialize-output-translations ',translations)
             (mapc #'asdf:load-system
                   '("aitools/data" "aitools/core/workspace" "aitools/core/store"
                     "aitools/feature/search" "cl-weave" "cl-process-kit"))
             (mapc #'load
                   (list ,(namestring (merge-pathnames "t/support/package.lisp" source-directory))
                         ,(namestring (merge-pathnames "t/support/json-assertions.lisp" source-directory))
                         ,(namestring (merge-pathnames "t/unit/search/package.lisp" source-directory))
                         ,(namestring (merge-pathnames "t/perf/search-allocation-cache.lisp" source-directory))))
             (load (compile-file ,(namestring (merge-pathnames "t/unit/search/fakes.lisp" source-directory))
                                 :output-file (merge-pathnames "search-fakes.fasl"
                                                               (uiop:ensure-directory-pathname ,(namestring cache-directory)))))
             (load (compile-file ,(namestring (merge-pathnames "t/unit/search/allocation-child-protocol.lisp" source-directory))
                                 :output-file (merge-pathnames "allocation-child-protocol.fasl"
                                                               (uiop:ensure-directory-pathname ,(namestring cache-directory)))))
             (load (compile-file ,(namestring (merge-pathnames "t/perf/search-allocation-test.lisp" source-directory))
                                 :output-file (merge-pathnames "search-allocation-test.fasl"
                                                               (uiop:ensure-directory-pathname ,(namestring cache-directory))))))
           (write-line "@@aitools-search-allocation-ready@@")
           (finish-output)
           (unless (equal (read-line *standard-input* nil nil) "GO")
             (error "Expected GO line after READY"))
           (values))
       (error (condition)
         (format *standard-output* "@@aitools-search-allocation-error@@~%~A~%" condition)
         (finish-output)
         (uiop:quit 1)))))
(defun %allocation-child-run-form ()
  `(let ((*read-eval* nil))
     (write-line "@@aitools-search-allocation-results@@")
     (prin1 (aitools.search.test::%search-allocation-measurements))
     (terpri)
     (finish-output)
     (uiop:quit 0)))
(defun %allocation-results-valid-p (measurements)
  (and (listp measurements)
       (= (length measurements) 7)
       (every
        (lambda (row)
          (and (listp row)
               (= (length row) 3)
               (stringp (first row))
               (every
                (lambda (value)
                  (and (integerp value) (not (minusp value))))
                (rest row))))
        measurements)
       (equal (sort (mapcar #'first measurements) #'string<)
              '("flow-count" "flow-ignore-case" "flow-matches" "flow-needle" "flow-word" "matcher" "no-literal"))))
(defun %run-search-allocation-child ()
  (let* ((source-directory (asdf:system-source-directory "aitools"))
         (cache-directory
          (%allocation-cache-directory))
         (process
          (process-kit:spawn
           sb-ext:*runtime-pathname*
           (list "--core" (namestring sb-ext:*core-pathname*)
                 "--noinform" "--non-interactive" "--no-sysinit" "--no-userinit"
                 "--eval" "(require :asdf)"
                 "--eval" (%allocation-form-text
                           (%allocation-child-setup-form source-directory cache-directory))
                 "--eval" (%allocation-form-text (%allocation-child-run-form)))
           :search t :input :stream :output :stream :error :stream
           :directory (uiop:getcwd)
           :environment (%allocation-child-environment)
           :external-format :utf-8)))
    (multiple-value-bind (stdout stderr)
        (%run-allocation-child-protocol
         process
         (lambda (child timeout)
           (%wait-for-allocation-child-ready
            child (process-kit:process-output child) (process-kit:process-stderr child)
            :timeout timeout))
         (lambda (child input timeout)
           (process-kit:communicate child :input input :timeout timeout :on-timeout :return
                                    :max-output-characters 200000
                                    :external-format :utf-8
                                    :decoding-error-policy :replace)))
      (let* ((marker "@@aitools-search-allocation-results@@")
             (marker-position (search marker stdout)))
        (unless marker-position
          (error "Child search allocation SBCL did not return measurements.~%stdout:~%~A~%stderr:~%~A"
                 stdout stderr))
        (let ((*read-eval* nil))
          (with-input-from-string (input (subseq stdout (+ marker-position (length marker))))
            (let ((measurements (read input nil nil)))
              (unless (%allocation-results-valid-p measurements)
                (error "Child returned malformed search allocation measurements: ~S" measurements))
              measurements)))))))
(defun %allocation-measurements ()
  (when (eq *search-allocation-measurement-cache* :unstarted)
    (setf *search-allocation-measurement-cache* (handler-case (%run-search-allocation-child)
                                                  (error (condition)
                                                         condition))))
  (if (typep *search-allocation-measurement-cache* 'condition)
      (error *search-allocation-measurement-cache*)
    *search-allocation-measurement-cache*))


(defun %flow-allocation (line-count &rest arguments)
  (let ((ports (make-fake-ports :files (list (list "/w/a.txt" (%non-matching-file line-count))))))
    (%bytes-consed (lambda () (apply #'run-flow #'search/k ports arguments)))))
(defun %search-allocation-measurements ()
  (list
   (list "flow-needle"
         (%flow-allocation 1000 :patterns '("needle"))
         (%flow-allocation 100000 :patterns '("needle")))
   (list "flow-ignore-case"
         (%flow-allocation 1000 :patterns '("needle") :ignore-case t)
         (%flow-allocation 100000 :patterns '("needle") :ignore-case t))
   (list "flow-word"
         (%flow-allocation 1000 :patterns '("need[a-z]e") :word t)
         (%flow-allocation 100000 :patterns '("need[a-z]e") :word t))
   (list "flow-count"
         (%flow-allocation 1000 :patterns '("needle") :output :count)
         (%flow-allocation 100000 :patterns '("needle") :output :count))
   (list "flow-matches"
         (%flow-allocation 1000 :patterns '("needle") :output :matches)
         (%flow-allocation 100000 :patterns '("needle") :output :matches))
   (let* ((matcher (build-matcher/k '("needle") :on-built #'identity
                                    :on-syntax-error
                                    (lambda (&rest reasons)
                                      (fail (princ-to-string reasons)))))
          (measure (lambda (count)
                     (let ((octets (%non-matching-file count)))
                       (%bytes-consed
                        (lambda () (search-file matcher octets :blocks 15))))))
          (large (funcall measure 100000))
          (small (funcall measure 1000)))
     (list "matcher" small large))
   (list "no-literal"
         (%flow-allocation 1000 :patterns '("[0-9]{5}"))
         (%flow-allocation 100000 :patterns '("[0-9]{5}")))))

(defparameter *allocation-slack* 65536
  "Allowed difference between 1,000 and 100,000 non-matching lines. One
cons cell per line would already add 1.6 MB.")

(cl-weave:describe-sequential "aitools.search allocation for non-matching lines"
  (it-each (("needle" () "flow-needle")
            ("needle" (:ignore-case t) "flow-ignore-case")
            ("need[a-z]e" (:word t) "flow-word")
            ("needle" (:output :count) "flow-count")
            ("needle" (:output :matches) "flow-matches"))
      "stays flat in N for ~S ~S"
      (pattern options measurement-name)
    (declare (ignore pattern options))
    (let* ((measurement (assoc measurement-name (%allocation-measurements)
                               :test #'string=))
           (small (second measurement))
           (large (third measurement)))
      (expect (- large small) :to-be-less-than *allocation-slack*)))

  (it "stays flat in N inside the per-file matcher itself"
    (let ((measurement (assoc "matcher" (%allocation-measurements)
                              :test #'string=)))
      (expect (- (third measurement) (second measurement))
              :to-be-less-than 4096)))

  ;; A pattern with no required literal has nothing to prefilter on, so every
  ;; line reaches the engine; cl-regex-kit v2.1.1's Pike VM no longer allocates
  ;; per input byte, so the scan stays flat in N even though no line is decoded.
  (it "stays flat in N for a pattern without a required literal ([0-9]{5})"
    (let ((measurement (assoc "no-literal" (%allocation-measurements)
                              :test #'string=)))
      (expect (- (third measurement) (second measurement))
              :to-be-less-than *allocation-slack*))))
