(in-package #:aitools.integration.dispatch-test)

(describe "dispatch group format repairs"
  (it "points a bare group subcommand name at every group form, keeping the operands"
    (aitools.test.support:with-workspace
      (*root* state-home)
      (declare (ignore state-home))
      (multiple-value-bind (code envelope) (run-aitools "get" "/a" "f.json")
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "argument.invalid")
        (expect (repair-commands envelope)
                :to-equal
                (list (format nil "aitools --root ~A json get /a f.json" *root*)
                      (format nil "aitools --root ~A toml get /a f.json" *root*)
                      (format nil "aitools --root ~A yaml get /a f.json" *root*))))
      (multiple-value-bind (code envelope) (run-plain "status")
        (expect code :to-be 1)
        (expect (repair-commands envelope) :to-equal '("aitools bg status" "aitools git status" "aitools tx status")))
      (multiple-value-bind (code envelope) (run-plain "uuid")
        (expect code :to-be 1)
        (expect (repair-commands envelope) :to-equal '("aitools util uuid"))))))
