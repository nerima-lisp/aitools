(in-package #:aitools.inspect.test)

(describe "inspect context boundary"
          (it "preserves root error details when candidates are empty"
              (multiple-value-bind (kind fields)
                  (run-flow #'info-flow
                            (make-test-ports :files nil)
                            "a.txt"
                            :root
                            "/missing")
                (expect kind :to-be :error)
                (expect (error-code fields) :to-equal "input.not-found")
                (expect (getf fields :message)
                        :to-equal
                        "workspace root /missing does not exist")
                (expect (getf fields :candidates) :to-be nil)
                (expect (getf (first (getf fields :repairs)) :command)
                        :to-equal
                        "aitools info ."))))

(describe "migrated Inspect flows preserve missing-root errors"
  (it "returns a missing-root error through check"
    (multiple-value-bind (kind fields)
        (run-flow (function check-flow) (make-test-ports :files nil)
                  "a.txt" :root "/missing")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (getf fields :message)
              :to-equal "workspace root /missing does not exist")
      (expect (getf fields :candidates) :to-be nil)
      (expect (getf (first (getf fields :repairs)) :command)
              :to-equal "aitools info .")))

  (it "returns a missing-root error through diff"
    (multiple-value-bind (kind fields)
        (run-flow (function diff-flow) (make-test-ports :files nil)
                  :a "a.txt" :b "b.txt" :root "/missing")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (getf fields :message)
              :to-equal "workspace root /missing does not exist")
      (expect (getf fields :candidates) :to-be nil)
      (expect (getf (first (getf fields :repairs)) :command)
              :to-equal "aitools info .")))

  (it "returns a missing-root error through snapshot create"
    (multiple-value-bind (kind fields)
        (run-flow (function snapshot-create-flow) (make-test-ports :files nil)
                  :root "/missing")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (getf fields :message)
              :to-equal "workspace root /missing does not exist")
      (expect (getf fields :candidates) :to-be nil)
      (expect (getf (first (getf fields :repairs)) :command)
              :to-equal "aitools info .")))

  (it "returns a missing-root error through snapshot diff"
    (multiple-value-bind (kind fields)
        (run-flow (function snapshot-diff-flow) (make-test-ports :files nil)
                  "snap-20231114T221320Z-0a1b2c3d" :root "/missing")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (getf fields :message)
              :to-equal "workspace root /missing does not exist")
      (expect (getf fields :candidates) :to-be nil)
      (expect (getf (first (getf fields :repairs)) :command)
              :to-equal "aitools info ."))))
