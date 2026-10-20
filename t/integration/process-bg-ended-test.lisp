(in-package #:aitools.process.integration-test)

;;;; Saved files exercise log rendering after the process has ended.
;;;; `invoke`, `value`, and `lines` are in t/integration/process-test.lisp;
;;;; the package and temporary-directory helpers are in
;;;; t/support/process-test-support.lisp.

(describe "aitools bg logs of an ended process (integration)"
  (it "keeps ANSI escapes only under --no-strip-ansi"
    (with-temporary-directory (state)
      (let ((bg (merge-pathnames "bg/" state))
            (colored (format nil "~C[31mred~C[0m" (code-char 27) (code-char 27))))
        (ensure-directories-exist bg)
        (flet ((put (name text)
                 (with-open-file (out (merge-pathnames name bg) :direction :output)
                   (write-string text out))))
          ;; The exit file makes the process ended, so its pid is never probed.
          (put "bg-1.json" (aitools.process.domain:serialize-bg-record
                            (aitools.process.domain:make-bg-record :id "bg-1" :argv '("printf" "x")
                                                                   :pid 2147483646
                                                                   :started "2026-01-01T00:00:00Z")))
          (put "bg-1.log" (format nil "~A~%" colored))
          (put "bg-1.exit" (format nil "0~%")))
        (multiple-value-bind (code envelope) (invoke (list "bg" "logs" "bg-1") :state-directory state)
          (expect code :to-be 0)
          (expect (value envelope "running") :to-be json-kit:+json-false+)
          (expect (lines envelope "lines") :to-equal '("red")))
        (multiple-value-bind (code envelope) (invoke (list "bg" "logs" "bg-1" "--no-strip-ansi") :state-directory state)
          (expect code :to-be 0)
          (expect (lines envelope "lines") :to-equal (list colored)))))))
