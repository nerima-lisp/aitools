(in-package #:aitools.text.test)

(describe "aitools.text.domain archive-entry constructor guards"
  (it "rejects a negative archive-entry size"
    (let ((invalid-size (1- (parse-integer "0"))))
      (signals type-error
        (aitools.text.domain::make-archive-entry :size invalid-size)))))
