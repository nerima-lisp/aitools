;;;; packages/feature/env/src/domain/time-input.lisp
;;;;
;;;; `time convert`'s automatic input-format detection: one parser for every time value `time
;;;; convert` and `time diff` accept.
;;;;
;;;; - All ASCII digits (optionally with a `.fraction`): Unix epoch. Twelve or
;;;;   more integer digits means milliseconds, fewer means seconds; a
;;;;   12-digit second count would be past the year 5000, while a 12-digit
;;;;   millisecond count is 1973 onward. An all-digit value is never read as
;;;;   a basic-format date (`20260308`); write it with dashes instead.
;;;; - ISO 8601 extended (`2026-03-08`, `2026-03-08T01:30`,
;;;;   `2026-03-08 01:30:00.250+09:00`) and basic with a `T`
;;;;   (`20260308T013000Z`). Without an offset the value is a local time,
;;;;   resolved by the caller in the requested zone.
;;;;
;;;; A value starting with `-` never reaches here as a value, so there are no
;;;; negative epochs.
(in-package #:aitools.env.domain)

(define-condition time-syntax-error
  (error)
  ((text :initarg :text :reader time-syntax-error-text)
   (reason :initarg :reason :reader time-syntax-error-reason))
  (:report
   (lambda (condition stream)
     (format stream
             "cannot read ~S as a time: ~A"
             (time-syntax-error-text condition)
             (time-syntax-error-reason condition)))))

(defstruct (time-input (:constructor make-time-input (kind format milliseconds)) (:copier nil))
  "KIND is :INSTANT (MILLISECONDS is Unix epoch milliseconds) or :LOCAL
(MILLISECONDS is the wall-clock fields encoded as if they were UTC, still to
be resolved against a zone). FORMAT is \"epoch_s\", \"epoch_ms\", or
\"iso8601\"."
  (kind nil :type (member :instant :local) :read-only t)
  (format nil :type string :read-only t)
  (milliseconds 0 :type integer :read-only t))

(defparameter *maximum-time-input-length*
  64
  "Longer text is rejected before any parsing: every accepted spelling is
far shorter, and the bound keeps digit runs from building huge integers.")

(defun %time-syntax-error (text reason)
  (error 'time-syntax-error :text text :reason reason))

(defun %fraction-milliseconds (text start end)
  "Milliseconds from the fraction digits TEXT[START,END); digits past the
third are truncated."
  (let ((digits (min 3 (- end start))))
    (* (parse-ascii-integer text start (+ start digits)) (expt 10 (- 3 digits)))))

(defun %parse-epoch (text)
  (let* ((dot (position #\. text))
         (integer-end (or dot (length text))))
    ;; PARSE-TIME-INPUT passes only digits and dots, starting with a digit, so
    ;; only the fraction can be malformed: empty, or holding a second dot.
    (when (and dot (not (ascii-digits-p text :start (1+ dot))))
      (%time-syntax-error text "not an epoch number"))
    (when (> integer-end 16)
      (%time-syntax-error text "epoch value out of range"))
    (let* ((milliseconds-p (>= integer-end 12))
           (whole (parse-ascii-integer text 0 integer-end))
           (fraction (if dot (%fraction-milliseconds text (1+ dot) (length text)) 0))
           (value (if milliseconds-p
                      whole
                      (+ (* whole 1000) fraction))))
      (unless (epoch-milliseconds-in-range-p value)
        (%time-syntax-error text "epoch value out of range"))
      (make-time-input :instant (if milliseconds-p "epoch_ms" "epoch_s") value))))
