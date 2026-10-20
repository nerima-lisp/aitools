(in-package #:aitools.vcs.integration-test)

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
  (it "maps a launch failure from cl-vcs-kit to the failure continuation"
    (let* ((test-thread (cl-concurrent-kit:current-thread))
           (run-git #'vcs-kit:run-git/checked)
           (condition
             (make-condition 'vcs-kit:git-launch-error
                             :command "git"
                             :arguments '("log"))))
      (cl-weave:with-mocked-functions
          (((symbol-function 'vcs-kit:run-git/checked)
            (lambda (&rest arguments)
              (if (eq (cl-concurrent-kit:current-thread) test-thread)
                  (error condition)
                  (apply run-git arguments)))))
        (expect
         (multiple-value-list
          (aitools.vcs.infrastructure::%run nil "log" nil))
         :to-equal
         (list :on-failure
               (list :missing (princ-to-string condition)))))))
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
          (expect (getf fields :message) :to-equal "the working directory is not inside a git work tree")))))
  (it "maps a cl-vcs-kit status failure to the failure continuation"
    (let* ((test-thread (cl-concurrent-kit:current-thread))
           (status (symbol-function (find-symbol "GIT-STATUS" "VCS-KIT")))
           (condition (make-condition (find-symbol "VCS-ERROR" "VCS-KIT"))))
      (cl-weave:with-mocked-functions
          (((symbol-function (find-symbol "GIT-STATUS" "VCS-KIT"))
            (lambda (&rest arguments)
              (if (eq (cl-concurrent-kit:current-thread) test-thread)
                  (error condition)
                  (apply status arguments)))))
        (expect
         (multiple-value-list (aitools.vcs.infrastructure::%status nil))
         :to-equal (list :on-failure (list :failed "VCS operation failed"))))))
  (it "maps a cl-vcs-kit numstat failure to the failure continuation"
    (let* ((test-thread (cl-concurrent-kit:current-thread))
           (numstat (symbol-function (find-symbol "GIT-DIFF-NUMSTAT" "VCS-KIT")))
           (condition (make-condition (find-symbol "VCS-ERROR" "VCS-KIT"))))
      (cl-weave:with-mocked-functions
          (((symbol-function (find-symbol "GIT-DIFF-NUMSTAT" "VCS-KIT"))
            (lambda (&rest arguments)
              (if (eq (cl-concurrent-kit:current-thread) test-thread)
                  (error condition)
                  (apply numstat arguments)))))
        (expect
         (multiple-value-list (aitools.vcs.infrastructure::%numstat nil nil))
         :to-equal (list :on-failure (list :failed "VCS operation failed")))))))
(describe "aitools git probe errors"
  (it "preserves a non-exit VCS probe failure"
    (let* ((test-thread (cl-concurrent-kit:current-thread))
           (run-git #'vcs-kit:run-git/checked)
           (condition (make-condition (find-symbol "VCS-ERROR" "VCS-KIT"))))
      (cl-weave:with-mocked-functions
          (((symbol-function 'vcs-kit:run-git/checked)
            (lambda (&rest arguments)
              (if (eq (cl-concurrent-kit:current-thread) test-thread)
                  (error condition)
                  (apply run-git arguments)))))
        (multiple-value-bind (kind payload) (aitools.vcs.infrastructure::%probe-git nil)
          (expect kind :to-be :on-failure)
          (expect (first payload) :to-be :failed)
          (expect (second payload) :to-equal "VCS operation failed")))))

  (it "preserves a Git failure that does not mean outside a work tree"
    (with-temporary-directory (directory)
      (let ((fake (merge-pathnames "git" directory))
            (message "fatal: blocked by safe.directory"))
        (with-open-file (out fake :direction :output)
          (format out "#!/bin/sh~%echo 'fatal: blocked by safe.directory' >&2~%exit 128~%"))
        (sb-posix:chmod (uiop:native-namestring fake) #o755)
        (multiple-value-bind (kind fields)
            (call-with-path (uiop:native-namestring directory)
                            (lambda () (run-flow #'aitools.vcs.application:git-status/k directory)))
          (expect kind :to-be :error)
          (expect (getf fields :code) :to-equal "environment.io")
          (expect (getf fields :message) :to-equal message))))))

