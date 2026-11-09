(in-package #:aitools.data)

(defparameter *kernel-duration-unit-milliseconds*
  (quote
   (("ms" . 1) ("s" . 1000) ("m" . 60000) ("h" . 3600000) ("d" . 86400000)))
  "Longest suffix candidates are tried first in PARSE-DURATION so that `ms` does not get misread as the unit `m` plus a dangling `s`.")

(export (quote (*kernel-duration-unit-milliseconds*)))
