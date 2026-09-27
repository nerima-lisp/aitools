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
            (write-line "a needle here" out))))

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

(defun %flow-allocation (line-count &rest arguments)
  (let ((ports (make-fake-ports :files (list (list "/w/a.txt" (%non-matching-file line-count))))))
    (%bytes-consed (lambda () (apply #'run-flow #'search/k ports arguments)))))

(defparameter *allocation-slack* 65536
  "Allowed difference between 1,000 and 100,000 non-matching lines. One
cons cell per line would already add 1.6 MB.")

(describe "aitools.search allocation for non-matching lines"
  (it-each (("needle" ()) ("needle" (:ignore-case t)) ("need[a-z]e" (:word t)) ("needle" (:output :count))
            ("needle" (:output :matches)))
      "stays flat in N for ~S ~S"
      (pattern options)
    (let ((small (apply #'%flow-allocation 1000 :patterns (list pattern) options))
          (large (apply #'%flow-allocation 100000 :patterns (list pattern) options)))
      (expect (- large small) :to-be-less-than *allocation-slack*)))

  (it "stays flat in N inside the per-file matcher itself"
    (let ((matcher (build-matcher/k '("needle") :on-built #'identity :on-syntax-error (lambda (&rest r) (fail (princ-to-string r))))))
      (flet ((measure (count)
               (let ((octets (%non-matching-file count)))
                 (%bytes-consed (lambda () (search-file matcher octets :blocks 15))))))
        (expect (- (measure 100000) (measure 1000)) :to-be-less-than 4096))))

  ;; A pattern with no required literal has nothing to prefilter on, so every
  ;; line reaches the engine; cl-regex-kit v2.1.1's Pike VM no longer allocates
  ;; per input byte, so the scan stays flat in N even though no line is decoded.
  (it "stays flat in N for a pattern without a required literal ([0-9]{5})"
    (let ((small (%flow-allocation 1000 :patterns '("[0-9]{5}")))
          (large (%flow-allocation 100000 :patterns '("[0-9]{5}"))))
      (expect (- large small) :to-be-less-than *allocation-slack*))))
