(in-package #:aitools.kernel.test)
(describe "selector constructor macro expansion"
  (it "generates the requested constructor definition"
    (let ((expansion
            (macroexpand-1
             '(aitools.kernel.domain::define-selector-constructor
               make-sample-selector (value &optional option) :range
               value option))))
      (expect expansion :to-equal
              '(defun make-sample-selector
                 (value &optional option)
                 (aitools.kernel.domain::%make-selector :range value option))))))
