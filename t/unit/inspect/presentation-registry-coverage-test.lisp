(in-package #:aitools.inspect.test)

(describe "inspect command registry coverage"
  (it "expands command declarations with the requested group and schema table"
    (let* ((expansion
             (macroexpand-1
              '(aitools.inspect.presentation::define-inspect-command
                "fixture.command" "fixture" '%fixture-command 'fixture-table)))
           (declaration
             (second (second (second expansion))))
           (options (third declaration)))
      (expect (first expansion) :to-be 'let*)
      (expect (first declaration)
              :to-be 'aitools.protocol.application:define-command)
      (expect (getf options :group) :to-equal "fixture")
      (expect (first (getf options :command-builder)) :to-be 'symbol-function)
      (expect (second (getf options :command-builder))
              :to-equal '(quote %fixture-command))
      (expect (first (getf options :schema))
              :to-be 'aitools.inspect.presentation::%schema)
      (expect (third (getf options :schema))
              :to-equal '(quote fixture-table))))

  (it "rejects a command name missing from its schema table"
    (signals error
      (aitools.inspect.presentation::%schema-entry "not-registered" nil)))
  (it "defaults integer option minimum to one"
    (let ((option
            (aitools.inspect.presentation::integer-option
             "count" nil "Number of rows.")))
      (expect (cl-cli:option-value-min option)
              :to-be 1))))
