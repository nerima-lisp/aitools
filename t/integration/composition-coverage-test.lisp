(in-package #:aitools/test)

(describe "CLI composition coverage"
  (it "handles tx insertion without globals and with a parent option value"
    (multiple-value-bind (app registry) (aitools/cli:build-app)
      (declare (ignore registry))
      (expect (aitools/cli::%with-tx app (quote ("tx" "status")) "tx-coverage")
              :to-equal
              (quote ("tx" "status" "--tx" "tx-coverage"))))
    (let* ((leaf (cl-cli:make-command :name "leaf"))
           (group (cl-cli:make-command
                   :name "group"
                   :options (list (cl-cli:make-option :name "value" :kind :value))
                   :subcommands (list leaf)))
           (app (cl-cli:make-app :name "coverage" :commands (list group))))
      (expect (aitools/cli::%with-tx app
                                     (quote ("group" "--value" "x" "leaf"))
                                     "tx-coverage")
              :to-equal
              (quote ("group" "--value" "x" "leaf" "--tx" "tx-coverage")))))
  (it "keeps partial trace outcomes from being marked as errors"
    (let ((session (aitools/cli::%make-trace-session)))
      (let ((aitools/cli::*trace-session* session))
        (handler-case
            (aitools/cli::%call-with-trace-span
             :io "partial"
             (lambda () (error "partial trace failure"))
             :condition-outcome-function
             (lambda (condition)
               (declare (ignore condition))
               :partial))
          (simple-error () nil)))
      (let* ((record (first (aitools/cli::%trace-session-records session)))
             (span-id (observability-kit:span-record-span-id record)))
        (expect (gethash span-id (aitools/cli::%trace-session-outcomes session))
                :to-be :partial)))))

