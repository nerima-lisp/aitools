(in-package #:aitools.edit.test)

(describe "aitools edit command specs"
  (it "expands option groups while preserving command-specific options"
    (let* ((custom-option (list :key :tx :name "custom-tx" :kind :flag))
           (spec
             (list :name "test.command"
                   :include (list :write)
                   :options (list custom-option)
                   :mktemp-area :refuse))
           (expanded (aitools.edit.application::%expand-spec spec))
           (options (getf expanded :options)))
      (expect (mapcar (lambda (option) (getf option :key)) options)
              :to-equal (list :tx :dry-run))
      (expect (getf (first options) :name) :to-equal "custom-tx")
      (expect (getf (second options) :key) :to-be :dry-run)
      (expect (getf expanded :mktemp-area) :to-be :refuse)
      (expect (length (getf spec :options)) :to-be 1))))
