;;;; packages/feature/process/src/domain/log-slice.lisp
;;;;
;;;; `bg logs`: the caller, not aitools, holds the read position (a
;;;; server-side cursor loses data across agents and context resets), so
;;;; every slice reports `next_offset`, the byte position the next `--from`
;;;; should start at.
(in-package #:aitools.process.domain)

(defstruct (log-slice (:copier nil))
  (lines nil :type list :read-only t)
  (next-offset 0 :type (integer 0) :read-only t)
  (truncated nil :type boolean :read-only t)
  (redactions 0 :type (integer 0) :read-only t))

(defun %line-spans (octets final-p skip-first-partial-p)
  "(VALUES SPANS CONSUMED). SPANS lists (START END NEXT) per line: bytes
[START,END) without the LF, NEXT the index after it. A trailing line with
no LF is only a line when FINAL-P (the writer has stopped); otherwise it may
still grow and is left for the next read. CONSUMED is the NEXT of the last
span, or where the skipped prefix ended."
  (let ((length (length octets)) (start 0) (spans '()))
    (when skip-first-partial-p
      (let ((newline (position 10 octets)))
        (setf start (if newline (1+ newline) length))))
    (let ((consumed start))
      (loop while (< start length)
            do (let ((newline (position 10 octets :start start)))
                 (cond (newline
                        (push (list start newline (1+ newline)) spans)
                        (setf start (1+ newline) consumed start))
                       (final-p
                        (push (list start length length) spans)
                        (setf start length consumed length))
                       (t (return)))))
      (values (nreverse spans) consumed))))

(defun slice-log (octets base-offset &key count from-p final-p strip-p pattern
                                          omitted-before-p more-after-p)
  "Slice OCTETS, the log bytes starting at file offset BASE-OFFSET, into at
most COUNT reported lines. Without FROM-P (tail mode) the last COUNT lines
are kept; with FROM-P (paging forward from `--from`) the first COUNT, and
NEXT-OFFSET stops right after the last one kept so no line is skipped.
PATTERN, when given, keeps only matching lines (after normalization and
redaction). OMITTED-BEFORE-P says the caller skipped earlier bytes, and
MORE-AFTER-P that it did not read to the end of the file; either marks the
slice truncated. OMITTED-BEFORE-P also drops the first, partial line."
  (multiple-value-bind (spans consumed) (%line-spans octets final-p omitted-before-p)
    (let ((kept '()))
      ;; Each kept entry is (TEXT NEXT REDACTION-COUNT).
      (dolist (span spans)
        (destructuring-bind (start end next) span
          (multiple-value-bind (text redaction-count)
              (aitools.protocol.domain:redact-secrets
               (normalize-terminal-line (decode-output-octets octets :start start :end end) strip-p))
            (when (or (null pattern) (line-pattern-matches-p pattern text))
              (push (list text next redaction-count) kept)))))
      (setf kept (nreverse kept))
      (let* ((excess (> (length kept) count))
             (selected (cond ((not excess) kept)
                             (from-p (subseq kept 0 count))
                             (t (last kept count)))))
        (make-log-slice :lines (mapcar #'first selected)
                        :next-offset (+ base-offset
                                        (if (and from-p excess) (second (car (last selected))) consumed))
                        :truncated (or excess omitted-before-p more-after-p)
                        :redactions (reduce #'+ selected :key #'third))))))
