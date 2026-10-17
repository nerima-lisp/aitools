;;;; Unix epoch conversions shared by contexts that expose Unix timestamps.
(in-package #:aitools.kernel.domain)

(defconstant +unix-epoch-universal-time+
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-time-to-unix-seconds (universal-time)
  (- universal-time +unix-epoch-universal-time+))

(defun unix-seconds-to-universal-time (unix-seconds)
  (+ unix-seconds +unix-epoch-universal-time+))
