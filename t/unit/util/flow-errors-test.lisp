(in-package #:aitools.util.test)

(describe "aitools.util.application continuation errors"
  (it "forwards decode and redact input errors"
    (multiple-value-bind (kind fields)
        (run-flow (function util-decode-flow) (test-boundaries) "hex" (request))
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "argument.invalid")
      (expect (getf fields :message)
              :to-equal "no input given: pass one of --content, --content-file, or --stdin")
      (expect (getf fields :repairs) :not :to-be-falsy))
    (multiple-value-bind (kind fields)
        (run-flow (function util-redact-flow) (test-boundaries) (request))
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "argument.invalid")
      (expect (getf fields :message)
              :to-equal "no input given: pass one of --content, --content-file, or --stdin")
      (expect (getf fields :repairs) :not :to-be-falsy)))

  (it "forwards stdin-read and calculation errors"
    (let ((boundaries (test-boundaries :read-stdin-octets (outcome-reader :failure "boom"))))
      (multiple-value-bind (kind fields)
          (run-flow (function util-calc-flow) boundaries nil t 10)
        (expect kind :to-be :error)
        (expect (getf fields :code) :to-equal "environment.io")
        (expect (getf fields :message) :to-equal "cannot read standard input: boom")
        (expect (getf fields :repairs) :not :to-be-falsy)))
    (multiple-value-bind (kind fields)
        (run-flow (function util-calc-flow)
                  (test-boundaries :read-stdin-octets (stdin-reader "1 +"))
                  nil t 10)
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "input.syntax-error")
      (expect (getf fields :message)
              :to-equal "at character 3: unexpected end of expression")
      (expect (getf fields :repairs) :not :to-be-falsy))))

(describe "aitools.util.application input reader contract"
  (it "rejects an unknown file reader result"
    (let ((message
            (handler-case
                (progn
                  (run-flow (function util-encode-flow)
                            (test-boundaries :read-file-octets (outcome-reader :unexpected))
                            "hex" (request :content-file "f"))
                  nil)
              (error (condition) (princ-to-string condition)))))
      (expect message :to-equal "Unknown content-file result :UNEXPECTED.")))

  (it "rejects an unknown stdin reader result"
    (let ((message
            (handler-case
                (progn
                  (run-flow (function util-tokens-flow)
                            (test-boundaries :read-stdin-octets (outcome-reader :unexpected))
                            (request :stdin t))
                  nil)
              (error (condition) (princ-to-string condition)))))
      (expect message :to-equal "Unknown standard-input result :UNEXPECTED."))))
