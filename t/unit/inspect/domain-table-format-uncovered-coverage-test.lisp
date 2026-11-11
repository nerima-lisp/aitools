(in-package #:aitools.inspect.test)

(describe "remaining table domain paths"
  (it "recognizes signed numeric spellings and extensionless paths"
    (expect (aitools.inspect.domain::%path-extension "README") :to-be nil)
    (expect (aitools.inspect.domain::%path-extension ".profile") :to-be nil)
    (expect (aitools.inspect.domain::%path-extension "report.CSV") :to-equal "csv")
    (expect (aitools.inspect.domain::%integer-text-p "+7") :to-be t)
    (expect (aitools.inspect.domain::%number-text-p "-.5e+2") :to-be t)
    (expect (aitools.inspect.domain::%number-text-p "1.") :to-be t)
    (expect (aitools.inspect.domain::%number-text-p "1e-") :to-be nil)
    (expect (aitools.inspect.domain::%number-value "-.5") :to-be-close-to -0.5d0)
    (expect (aitools.inspect.domain::%number-value "1.25") :to-be-close-to 1.25d0))

  (it "reports only present non-numeric cells in selected rows"
    (let* ((rows (list (vector "1")
                       (vector "bad")
                       (vector json-kit:+json-null+)
                       (vector "")))
           (table (aitools.inspect.domain::%make-table
                   :csv '("value") '("string") rows)))
      (expect (aitools.inspect.domain:non-numeric-cells table 0)
              :to-equal '((2 "bad")))
      (expect (aitools.inspect.domain:non-numeric-cells
               table 0 :rows (list (first rows)))
              :to-equal nil)))

  (it "aggregates mixed numeric strings while skipping text cells"
    (let* ((rows (list (vector "a" "1.5" "x")
                       (vector "a" "bad" "x")
                       (vector "b" "2" json-kit:+json-null+)))
           (table (aitools.inspect.domain::%make-table
                   :csv '("group" "amount" "tag")
                   '("string" "string" "string") rows))
           (groups (aitools.inspect.domain:aggregate-table
                    table :group-by '(0) :sum 1 :avg 1 :min 1 :max 1 :distinct 2))
           (first-group (first groups))
           (second-group (second groups)))
      (expect (mapcar #'aitools.inspect.domain:table-group-key groups)
              :to-equal '(("a") ("b")))
      (expect (aitools.inspect.domain:table-group-sum first-group)
              :to-be-close-to 1.5d0)
      (expect (aitools.inspect.domain:table-group-average first-group)
              :to-be-close-to 1.5d0)
      (expect (aitools.inspect.domain:table-group-min second-group) :to-be 2)
      (expect (aitools.inspect.domain:table-group-max second-group) :to-be 2)
      (expect (aitools.inspect.domain:table-group-distinct-count first-group) :to-be 1)
      (expect (aitools.inspect.domain:table-group-distinct-count second-group) :to-be 0))))

(describe "remaining TOML and YAML conversion paths"
  (it "converts finite floats and nested vectors in TOML helpers"
    (expect (aitools.inspect.domain::%toml-to-json 1.25d0 nil)
            :to-be-close-to 1.25d0)
    (expect (aitools.inspect.domain::%toml-to-json #(1 2.5d0) nil)
            :to-equalp #(1 2.5d0))
    (expect (aitools.inspect.domain::%toml-write-check-value
             1.25d0 nil 0 nil)
            :to-be nil)
    (expect (aitools.inspect.domain::%toml-write-check-value
             #(1 "two") nil 0 nil)
            :to-be nil))

  (it "converts finite YAML values and checks mapping and sequence nodes"
    (expect (aitools.inspect.domain::%yaml-to-json 1.25d0 nil)
            :to-be-close-to 1.25d0)
    (expect (aitools.inspect.domain::%yaml-to-json #(1 "two") nil)
            :to-equalp #(1 "two"))
    (expect (aitools.inspect.domain::%yaml-write-finite-float-p 1.25d0)
            :to-be t)
    (let ((node (first (yaml-kit:compose-all "items: [one, two]"))))
      (expect (aitools.inspect.domain::%yaml-write-check-node
               node nil (make-hash-table :test #'eq) 0 nil)
              :to-be nil)))

  (it "enforces the YAML write depth limit for mappings and nested sequences"
    (signals yaml-syntax-error
      (parse-yaml-write-document "value: one" :max-depth 0))
    (signals yaml-syntax-error
      (parse-yaml-write-document "items: [one]" :max-depth 1))))
