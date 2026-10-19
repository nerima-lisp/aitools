(in-package #:aitools.process.infrastructure)

(defun direct-run (program)
  (process-kit:run program '() :timeout 1))
(defun direct-communicate (process)
  (process-kit:communicate process :timeout 1))
