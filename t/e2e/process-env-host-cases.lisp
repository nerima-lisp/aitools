(in-package #:aitools.e2e.test)

;;;;
;;;; Correspondence table rows 40-41: external Git and host environment.
;;;; The shared TRIMMED helper is in t/e2e/process-env-cases.lisp.

(describe "aitools environment child-process trace span (e2e)"
  (it "records env.system-query with the command parent"
    (:tags (quote (:trace-positive)))
    (when (let ((directory (uiop:getenv "AITOOLS_E2E_BYTE_CAPTURE_DIR")))
            (and directory (plusp (length directory))))
      (cl-weave:skip "trace specs require a binary with --trace"))
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (let ((result (aitools ws '("--trace" "sys" "tools" "sh") :expect-trace t)))
         (expect (aitools-exit-code result) :to-be 0)
         (expect-child-process-span (aitools-envelope result) "env.system-query" "ok"))))))

(define-row-case (40 "git status runs through run" :foreign ("git") :git t) (ws)
  (put ws "f" "content")
  (let ((result (run-ok ws (list "run" "--" "git" "status" "--porcelain")))
        (expected (oracle ws (list "git") "git status --porcelain" (format nil "?? f~%"))))
    (expect (lines-text (jget result "stdout" "head")) :to-equal expected)))

