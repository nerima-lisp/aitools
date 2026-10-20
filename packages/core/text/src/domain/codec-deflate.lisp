;;;; packages/core/text/src/domain/codec-deflate.lisp
;;;;
;;;; Raw DEFLATE compatibility wrappers over cl-deflate-kit.
(in-package #:aitools.text.domain)

(defun inflate (octets &key (start 0) end max-output truncate-at size-hint)
  "Decode raw DEFLATE bytes and return the absolute index after the final block.
With TRUNCATE-AT, return the prefix and NIL as the index."
  (declare (type octets octets) (type fixnum start))
  (handler-case
      (multiple-value-bind (output consumed)
          (deflate-kit:inflate octets :start start :end end
                               :max-output-bytes max-output
                               :unbounded (null max-output)
                               :truncate-at truncate-at
                               :size-hint size-hint
                               :allow-trailing t)
        (values (coerce output 'octets)
                (and consumed (+ start consumed))))
    (deflate-kit:inflate-size-limit-exceeded (condition)
      (error 'archive-limit-exceeded
             :limit (deflate-kit:deflate-output-limit-limit condition)
             :reason "DEFLATE output limit"))
    (deflate-kit:inflate-invalid-data (condition)
      (%archive-fail (deflate-kit:deflate-error-message condition)))
    (deflate-kit:deflate-error ()
      (%archive-fail "invalid DEFLATE stream"))))

(defun deflate (octets &key (start 0) end)
  "Compress OCTETS[START,END) as raw DEFLATE bytes."
  (declare (type octets octets) (type fixnum start))
  (let* ((end (or end (length octets)))
         (input (if (and (zerop start) (= end (length octets)))
                    octets
                    (subseq octets start end))))
    (deflate-kit:deflate input :level 6)))
