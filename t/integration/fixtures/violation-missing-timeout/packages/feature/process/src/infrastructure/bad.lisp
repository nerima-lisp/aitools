(in-package #:aitools.process.infrastructure)

(defun bad-run (program)
  (process-kit:run program '()))
(defun bad-communicate (process)
  (process-kit:communicate process))
