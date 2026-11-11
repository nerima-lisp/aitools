(in-package #:aitools.edit.test)

(describe "aitools edit input callbacks and recorded options"
  (it "raises when an injected stdin callback returns an unknown result"
    (call-with-edit-workspace (lambda () (put "a.txt" (format nil "keep~%"))
      (let ((before (snapshot)))
        (let ((*boundaries*
                (make-edit-test-boundaries
                 :edit-workspace-host
                 (cl-boundary-kit:boundary-context-require *boundaries* :edit-workspace-host)
                 :edit-open-store
                 (cl-boundary-kit:boundary-context-require *boundaries* :edit-open-store)
                 :edit-text-source
                 (cl-boundary-kit:boundary-context-require *boundaries* :edit-text-source)
                 :edit-read-stdin-octets
                 (lambda (limit)
                   (declare (ignore limit))
                   (values :unknown nil))
                 :edit-universal-now
                 (cl-boundary-kit:boundary-context-require *boundaries* :edit-universal-now))))
          (let ((failure
                  (handler-case
                      (progn (run "edit" (list "a.txt") :stdin t) nil)
                    (error (condition)
                      (princ-to-string condition)))))
            (expect (and failure
                         (search "Unknown standard-input result" failure)
                         (search ":UNKNOWN" failure))
                    :to-be-truthy)))
        (expect-unchanged before)))))
  (it "keeps the last single-value option while parsing recorded argv"
    (destructuring-bind (name positionals options)
        (parse-recorded-argv
         (list "edit" "a.txt" "--old" "x" "--new" "first" "--new" "last")
         :on-parsed (function list)
         :on-invalid (function identity))
      (expect (list name positionals options)
              :to-equal (list "edit"
                              (list "a.txt")
                              (list :old "x" :new "last")))))
  (it "reports an invalid line selector without changing its target"
    (call-with-edit-workspace (lambda () (put "a.txt" (format nil "keep~%"))
      (let ((before (snapshot)))
        (with-error (code)
            (run "transform" (list "a.txt") :op (list "sort") :match "(" :expect-count "1")
          (expect code :to-equal "input.syntax-error"))
        (expect-unchanged before))))))
