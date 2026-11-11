(in-package #:aitools.env.test)

(describe "aitools env boundary contract"
  (it "registers every capability required by env application"
    (let ((boundaries (aitools.env.composition::make-env-dependencies)))
      (dolist (key '(:env-clock :env-environment :env-filesystem :env-host-info
                     :env-sleep-seconds :env-list-directory :env-read-link
                     :env-executable-p :env-run-program :env-file-system-space
                     :env-system-identity :env-user-id :env-workspace-root
                     #+darwin :env-cpu-counters #+darwin :env-sysctl-mib))
        (expect (cl-boundary-kit:boundary-context-require boundaries key)
                :to-be-truthy))))
  (it "reads wall-clock milliseconds from the production env clock"
    (let* ((boundaries (aitools.env.infrastructure:make-production-env-boundaries))
           (clock (cl-boundary-kit:boundary-context-require boundaries :env-clock)))
      (multiple-value-bind (before-seconds) (sb-ext:get-time-of-day)
        (let ((milliseconds (cl-boundary-kit:clock-now clock)))
          (multiple-value-bind (after-seconds) (sb-ext:get-time-of-day)
            (expect (integerp milliseconds) :to-be t)
            (expect (plusp milliseconds) :to-be t)
            (expect (<= (* before-seconds 1000)
                        milliseconds
                        (1- (* (1+ after-seconds) 1000)))
                    :to-be t))))))
  (it "contains no old env boundary symbols in production"
    (let ((sources
            (directory
             (merge-pathnames
              (make-pathname
               :directory '(:relative "packages" "feature" "env" "src" :wild-inferiors)
               :name :wild :type "lisp")
              (asdf:system-source-directory "aitools")))))
      (expect sources :to-be-truthy)
      (dolist (source sources)
        (expect (search
                    (concatenate (quote string) "env-"
                                 (coerce (quote (#\p #\o #\r #\t #\s)) (quote string)))
                    (uiop:read-file-string source) :test #'char-equal)
                :to-be nil)))))
