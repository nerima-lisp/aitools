;;;; packages/feature/process/src/domain/output-report.lisp
;;;;
;;;; `run`'s per-stream `{head, tail, total_lines, truncated, matches?}`.
;;;; `--grep` runs over every line before head/tail truncation (so an error
;;;; line in the middle of the output is never missed), and over the
;;;; redacted text, so a
;;;; pattern can never probe what a mask hides.
(in-package #:aitools.process.domain)

(defstruct (output-report (:copier nil))
  (head nil :type list :read-only t)
  (tail nil :type list :read-only t)
  (total-lines 0 :type (integer 0) :read-only t)
  (truncated nil :type boolean :read-only t)
  ;; NIL when no pattern was given; otherwise a list of (LINE-NUMBER . TEXT).
  (matches nil :type list :read-only t)
  (total-matches nil :type (or null (integer 0)) :read-only t)
  (redactions 0 :type (integer 0) :read-only t))

(defconstant +max-line-characters+ (* 1024 1024)
  "Per-line ceiling for a reported output line. A child that emits a huge run
of bytes with no newline would otherwise put its whole stream into a single
head line and inflate the envelope; past this a line is cut and the report is
marked truncated. Well past any realistic line, so ordinary output is
unchanged.")

(defun %cap-line (line)
  "(VALUES CAPPED-LINE CAPPED-P): LINE cut to +MAX-LINE-CHARACTERS+ characters."
  (if (> (length line) +max-line-characters+)
      (values (subseq line 0 +max-line-characters+) t)
      (values line nil)))

(defun output-report-grep-exceeded-p (report limit)
  "True when REPORT's pattern matched more lines than LIMIT, the condition
`run` reports with exit code 3."
  (let ((total (output-report-total-matches report)))
    (and total (> total limit))))

(defun summarize-output (text &key head-count tail-count strip-p pattern grep-limit)
  "Build the OUTPUT-REPORT for one captured stream TEXT. Lines are
normalized (NORMALIZE-TERMINAL-LINE) and redacted before anything else
looks at them. When the line count fits in HEAD-COUNT + TAIL-COUNT nothing
is dropped: HEAD holds the first HEAD-COUNT lines and TAIL the rest."
  (let* ((redactions 0)
         (line-capped nil)
         (lines (coerce (mapcar (lambda (raw)
                                  (multiple-value-bind (capped capped-p)
                                      (%cap-line (normalize-terminal-line raw strip-p))
                                    (when capped-p (setf line-capped t))
                                    (multiple-value-bind (masked count)
                                        (aitools.protocol.domain:redact-secrets capped)
                                      (incf redactions count)
                                      masked)))
                                (split-output-lines text))
                        'vector))
         (total (length lines))
         (head-end (min head-count total))
         (tail-start (max head-end (- total tail-count)))
         (matches '())
         (total-matches (and pattern 0)))
    (when pattern
      (loop for line across lines
            for number from 1
            when (line-pattern-matches-p pattern line)
              do (when (< total-matches grep-limit)
                   (push (cons number line) matches))
                 (incf total-matches)))
    (make-output-report :head (coerce (subseq lines 0 head-end) 'list)
                        :tail (coerce (subseq lines tail-start) 'list)
                        :total-lines total
                        :truncated (or (> tail-start head-end) line-capped)
                        :matches (nreverse matches)
                        :total-matches total-matches
                        :redactions redactions)))

(defun output-report-json (report)
  "REPORT as `run`'s stream object."
  (json-object-from-alist
   (append (list (cons "head" (output-report-head report))
                 (cons "tail" (output-report-tail report))
                 (cons "total_lines" (output-report-total-lines report))
                 (cons "truncated" (json-boolean (output-report-truncated report))))
           (when (output-report-total-matches report)
             (list (cons "matches"
                         (mapcar (lambda (match)
                                   (json-object "n" (car match) "text" (cdr match)))
                                 (output-report-matches report)))
                   (cons "total_matches" (output-report-total-matches report)))))))
