(in-package #:aitools.vcs.integration-test)

(describe "aitools git failure continuation behavior"
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

  (it "classifies a translated outside-repository failure by exit code"
    (with-temporary-directory (directory)
      (let ((fake (merge-pathnames "git" directory)))
        (with-open-file (out fake :direction :output)
          (format out "#!/bin/sh~%echo '致命的: Git リポジトリではありません' >&2~%exit 128~%"))
        (sb-posix:chmod (uiop:native-namestring fake) #o755)
        (multiple-value-bind (kind fields)
            (call-with-path (uiop:native-namestring directory)
                            (lambda () (run-flow #'aitools.vcs.application:git-status/k directory)))
          (expect kind :to-be :error)
          (expect (getf fields :code) :to-equal "environment.unavailable")
          (expect (getf fields :message)
                  :to-equal "the working directory is not inside a git work tree")))))

  (it "preserves a Git failure that does not mean outside a work tree"
    (with-temporary-directory (directory)
      (ensure-directories-exist (merge-pathnames ".git/config" directory))
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

