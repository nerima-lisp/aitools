;;;; packages/core/text/src/domain/codec-conditions.lisp
;;;;
;;;; Conditions of the byte-format codecs. Archives and compressed streams
;;;; are untrusted input (trust-boundaries: every decoder is its own
;;;; boundary), so every malformed-input path ends in ARCHIVE-ERROR rather
;;;; than an array-index error, and REASON is always a fixed description,
;;;; never input bytes.
(in-package #:aitools.text.domain)

(define-condition archive-error (error)
  ((reason :initarg :reason :reader archive-error-reason))
  (:report (lambda (condition stream)
             (format stream "malformed archive data: ~A" (archive-error-reason condition)))))

(define-condition archive-limit-exceeded (archive-error)
  ((limit :initarg :limit :reader archive-limit-exceeded-limit))
  (:report (lambda (condition stream)
             (format stream "decoded size exceeds the limit of ~D bytes"
                     (archive-limit-exceeded-limit condition)))))

(define-condition archive-unsupported (archive-error)
  ()
  (:report (lambda (condition stream)
             (format stream "unsupported archive feature: ~A" (archive-error-reason condition)))))

(defun %archive-fail (reason)
  (error 'archive-error :reason reason))

(defun %archive-unsupported (reason)
  (error 'archive-unsupported :reason reason))
