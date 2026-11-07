(in-package #:aitools.e2e.test)

;;;;
;;;; Correspondence table row 40: external Git.
;;;; The shared TRIMMED helper is in t/e2e/process-env-cases.lisp.

(define-row-case (40 "git status runs through run" :foreign ("git") :git t) (ws)
  (put ws "f" "content")
  (let ((result (run-ok ws (list "run" "--" "git" "status" "--porcelain")))
        (expected (oracle ws (list "git") "git status --porcelain" (format nil "?? f~%"))))
    (expect (lines-text (jget result "stdout" "head")) :to-equal expected)))

