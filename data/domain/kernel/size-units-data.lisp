(in-package #:aitools.data)

(defparameter *kernel-size-unit-multipliers*
  (quote (("KiB" . 1024) ("MiB" . 1048576) ("GiB" . 1073741824))))

(export (quote (*kernel-size-unit-multipliers*)))
