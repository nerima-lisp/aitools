(in-package #:aitools.integration.dispatch-test)

(describe "aitools schema group names"
  (it "expands a group followed by a subcommand into full details"
    (aitools.test.support:with-workspace
      (*root* state-home)
      (declare (ignore state-home))
      (multiple-value-bind (code envelope stream)
          (run-aitools "schema" "json" "get" "read")
        (expect (list code stream) :to-equal (list 0 :stdout))
        (let ((commands (value envelope "commands")))
          (expect (map 'list (lambda (command) (value command "name")) commands)
                  :to-equal '("json get" "read"))
          (expect (every (lambda (command)
                           (and (stringp (value command "description"))
                                (vectorp (value command "args"))
                                (vectorp (value command "output_fields"))
                                (vectorp (value command "error_codes"))))
                         commands)
                  :to-be t))))))
