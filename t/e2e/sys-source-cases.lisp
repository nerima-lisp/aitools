(in-package #:aitools.e2e.test)

(progn
  (defvar *sys-source-e2e-binary* nil)

  (defun build-sys-source-e2e-binary ()
    (let* ((directory (scratch-path "sys-source-e2e-build/"))
           (target (merge-pathnames "aitools-sys-source-e2e" directory))
           (fasl-directory (merge-pathnames "fasl/" directory))
           (script (asdf:system-relative-pathname
                    "aitools" "t/e2e/fixtures/sys-cli-build.lisp"))
           (result
             (run-process (native sb-ext:*runtime-pathname*)
                          (list "--non-interactive" "--no-sysinit" "--no-userinit"
                                "--disable-debugger" "--load" (native script))
                          :environment
                          (environment-with
                           (list (cons "AITOOLS_E2E_SYS_CLI_OUTPUT" (native target))
                                 (cons "AITOOLS_E2E_SYS_CLI_FASL" (native fasl-directory))))
                          :timeout-seconds +build-timeout-seconds+)))
      (unless (and (eql (result-exit-code result) 0) (probe-file target))
        (fail "~A"
              (format nil "building the sys source e2e CLI failed (exit ~A): ~A~%~A"
                      (result-exit-code result)
                      (utf8 (result-stdout result))
                      (utf8 (result-stderr result)))))
      target))

  (defun sys-source-e2e-binary ()
    (or *sys-source-e2e-binary*
        (setf *sys-source-e2e-binary* (build-sys-source-e2e-binary))))

  (defun run-sys-source-e2e (workspace arguments mode)
    (let ((*binary* (sys-source-e2e-binary)))
      (aitools workspace arguments
               :env (list (cons "AITOOLS_E2E_SYS_SOURCE_MODE" mode)))))

  (eval-when (:load-toplevel)
    (unless (uiop:getenv "AITOOLS_E2E_BYTE_CAPTURE_DIR")
      (setf *sys-source-e2e-binary* (build-sys-source-e2e-binary)))))

(describe "aitools host metric source failures (e2e)"
  (it "returns unavailable without uptime seconds when the uptime source is missing"
    (when (uiop:getenv "AITOOLS_E2E_BYTE_CAPTURE_DIR")
      (cl-weave:skip "source-failure specs use a dedicated child image"))
    (call-with-workspace
     (lambda (ws)
       (let* ((result (run-sys-source-e2e ws '("sys" "info") "uptime-missing"))
              (envelope (aitools-envelope result)))
         (expect (aitools-exit-code result) :to-be 1)
         (expect (jget envelope "status") :to-equal "error")
         (expect (jget envelope "error" "code") :to-equal "environment.unavailable")
         (expect (nth-value 1 (gethash "uptime_seconds" envelope)) :to-be nil)
         (expect (realp (jget envelope "uptime_seconds")) :to-be nil)))))

  (it "returns unavailable without uptime seconds when the uptime source is corrupt"
    (when (uiop:getenv "AITOOLS_E2E_BYTE_CAPTURE_DIR")
      (cl-weave:skip "source-failure specs use a dedicated child image"))
    (call-with-workspace
     (lambda (ws)
       (let* ((result (run-sys-source-e2e ws '("sys" "info") "uptime-corrupt"))
              (envelope (aitools-envelope result)))
         (expect (aitools-exit-code result) :to-be 1)
         (expect (jget envelope "status") :to-equal "error")
         (expect (jget envelope "error" "code") :to-equal "environment.unavailable")
         (expect (nth-value 1 (gethash "uptime_seconds" envelope)) :to-be nil)
         (expect (realp (jget envelope "uptime_seconds")) :to-be nil)))))

  (it "returns unavailable without load averages when the load-average source is missing"
    (when (uiop:getenv "AITOOLS_E2E_BYTE_CAPTURE_DIR")
      (cl-weave:skip "source-failure specs use a dedicated child image"))
    (call-with-workspace
     (lambda (ws)
       (let* ((result (run-sys-source-e2e ws '("sys" "metrics") "load-average-missing"))
              (envelope (aitools-envelope result)))
         (expect (aitools-exit-code result) :to-be 1)
         (expect (jget envelope "status") :to-equal "error")
         (expect (jget envelope "error" "code") :to-equal "environment.unavailable")
         (expect (nth-value 1 (gethash "load_average" envelope)) :to-be nil)
         (expect (realp (jget envelope "load_average" "one_minute")) :to-be nil)
         (expect (realp (jget envelope "load_average" "five_minutes")) :to-be nil)
         (expect (realp (jget envelope "load_average" "fifteen_minutes")) :to-be nil)))))

  (it "returns unavailable without load averages when the load-average source is corrupt"
    (when (uiop:getenv "AITOOLS_E2E_BYTE_CAPTURE_DIR")
      (cl-weave:skip "source-failure specs use a dedicated child image"))
    (call-with-workspace
     (lambda (ws)
       (let* ((result (run-sys-source-e2e ws '("sys" "metrics") "load-average-corrupt"))
              (envelope (aitools-envelope result)))
         (expect (aitools-exit-code result) :to-be 1)
         (expect (jget envelope "status") :to-equal "error")
         (expect (jget envelope "error" "code") :to-equal "environment.unavailable")
         (expect (nth-value 1 (gethash "load_average" envelope)) :to-be nil)
         (expect (realp (jget envelope "load_average" "one_minute")) :to-be nil)
         (expect (realp (jget envelope "load_average" "five_minutes")) :to-be nil)
         (expect (realp (jget envelope "load_average" "fifteen_minutes")) :to-be nil))))))
