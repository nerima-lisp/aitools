(in-package #:aitools.process.infrastructure)

(defun unapproved-spawn ()
  (process-kit:spawn-native "program" '()))
