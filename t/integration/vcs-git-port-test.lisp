;;;; t/integration/vcs-git-port-test.lisp

(in-package #:aitools.vcs.integration-test)

(defun call-with-path (value thunk)
  "Call THUNK with the process's PATH set to VALUE."
  (let ((previous (sb-posix:getenv "PATH")))
    (unwind-protect (progn (sb-posix:setenv "PATH" value 1) (funcall thunk))
      (if previous (sb-posix:setenv "PATH" previous 1) (sb-posix:unsetenv "PATH")))))

(describe-run-if (git-available-p) "aitools git production port limits"
  (it "dispatches git diff --output stat"
    (with-temporary-directory (directory)
      (build-fixture directory)
      (multiple-value-bind (code envelope) (dispatch-in directory "git" "diff" "--output" "stat")
        (expect code :to-equal 0)
        (expect (gethash "mode" envelope) :to-equal "stat")
        (expect (gethash "mode" (aref (gethash "files" envelope) 0)) :to-equal "stat"))))

  (it "fails as environment.io when git prints more than the output limit"
    (with-temporary-directory (directory)
      (build-fixture directory)
      (let ((aitools.vcs.infrastructure::*git-output-limit* 10))
        (multiple-value-bind (kind fields)
            (run-flow #'aitools.vcs.application:git-show/k directory "HEAD:a.txt")
          (expect kind :to-be :error)
          (expect (getf fields :code) :to-equal "environment.io")
          (expect (getf fields :message) :to-equal "git cat-file printed more than 10 characters")))))

  (it "reports environment.unavailable when git cannot be started"
    (with-temporary-directory (directory)
      (build-fixture directory)
      (multiple-value-bind (kind fields)
          (call-with-path (uiop:native-namestring directory)
                          (lambda () (run-flow #'aitools.vcs.application:git-status/k directory)))
        (expect kind :to-be :error)
        (expect (getf fields :code) :to-equal "environment.unavailable")
        (expect (getf fields :message) :to-equal "git could not be started")))))

(describe "aitools git production port failure kinds"
  (it-each ((vcs-kit:git-exit-error (:command "git" :arguments ("log")) :exit "Git command failed: git log")
            (vcs-kit:git-launch-error (:command "git" :arguments ("log")) :missing "Git could not be launched: git log")
            (vcs-kit:vcs-error () :failed "VCS operation failed"))
      "maps ~S to ~*~S"
      (type initargs kind message)
    (expect (multiple-value-list (aitools.vcs.infrastructure::%failure (apply #'make-condition type initargs)))
            :to-equal (list kind message)))

  (it "treats a git that exits 0 without `true` from rev-parse as outside a work tree"
    ;; git before 2.25 printed `false` and exited 0 for --show-toplevel
    ;; outside a work tree instead of failing.
    (with-temporary-directory (directory)
      (let ((fake (merge-pathnames "git" directory)))
        (with-open-file (out fake :direction :output)
          (format out "#!/bin/sh~%printf 'false\\n/elsewhere\\n'~%"))
        (sb-posix:chmod (uiop:native-namestring fake) #o755)
        (multiple-value-bind (kind fields)
            (call-with-path (uiop:native-namestring directory)
                            (lambda () (run-flow #'aitools.vcs.application:git-status/k directory)))
          (expect kind :to-be :error)
          (expect (getf fields :code) :to-equal "environment.unavailable")
          (expect (getf fields :message) :to-equal "the working directory is not inside a git work tree"))))))

(describe "aitools.vcs.application git port construction"
  (it-each ((:probe) (:run) (:status) (:numstat) (:at))
      "refuses a port without ~S"
      (missing)
    (let ((slots (loop for key in '(:probe :run :status :numstat :at)
                       unless (eq key missing) append (list key #'identity))))
      (expect (handler-case (progn (apply #'aitools.vcs.application:make-git-port slots) :made)
                (error (condition) (princ-to-string condition)))
              :to-equal (format nil "MAKE-GIT-PORT requires :~A" missing)))))
