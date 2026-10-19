(in-package #:aitools.process.infrastructure)

(defun nested-run (program)
  (process-kit:run program '() :options '(:timeout 1)))
(defun nested-communicate (process)
  (process-kit:communicate process :input '(:timeout 1)))
