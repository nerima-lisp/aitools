(in-package #:aitools.inspect.test)

(describe "numeric helper branch boundaries"
  (it "rejects empty and malformed numeric spellings"
    (expect (aitools.inspect.domain::%integer-text-p "") :to-be nil)
    (expect (aitools.inspect.domain::%integer-text-p "8") :to-be t)
    (expect (aitools.inspect.domain::%integer-text-p "-") :to-be nil)
    (expect (aitools.inspect.domain::%number-text-p "") :to-be nil)
    (expect (aitools.inspect.domain::%number-text-p "8") :to-be t)
    (expect (aitools.inspect.domain::%number-text-p "8.5") :to-be t)
    (expect (aitools.inspect.domain::%number-text-p "8.x") :to-be nil)
    (expect (aitools.inspect.domain::%number-text-p ".") :to-be nil)
    (expect (aitools.inspect.domain::%number-value "true") :to-be nil)
    (expect (aitools.inspect.domain::%number-value "null") :to-be nil)))

(describe "format serialization boundaries"
  #+sbcl
  (it "rejects NaN in TOML and YAML read and write helpers"
    (let ((nan (sb-kernel:make-double-float #x7ff80000 0)))
      (sb-int:with-float-traps-masked (:invalid)
        (handler-case
            (progn
              (aitools.inspect.domain::%toml-to-json nan nil)
              (fail "expected a non-finite TOML read value"))
          (toml-unsupported-value (condition)
            (expect (toml-unsupported-reason condition)
                    :to-equal "non-finite-float")))
        (handler-case
            (progn
              (aitools.inspect.domain::%toml-write-check-value nan nil 0 nil)
              (fail "expected a non-finite TOML write value"))
          (toml-unsupported-value (condition)
            (expect (toml-unsupported-reason condition)
                    :to-equal "non-finite-float")))
        (handler-case
            (progn
              (aitools.inspect.domain::%yaml-to-json nan nil)
              (fail "expected a non-finite YAML read value"))
          (yaml-unsupported-value (condition)
            (expect (yaml-unsupported-reason condition)
                    :to-equal "non-finite-float")))
        (expect (aitools.inspect.domain::%yaml-write-finite-float-p nan)
                :to-be nil)
        (let ((node (first (yaml-kit:compose-all ".nan"))))
          (handler-case
              (progn
                (aitools.inspect.domain::%yaml-write-check-node
                 node nil (make-hash-table :test (function eq)) 0 nil)
                (fail "expected a non-finite YAML write value"))
            (yaml-unsupported-value (condition)
              (expect (yaml-unsupported-reason condition)
                      :to-equal "non-finite-float")))))))

  (it "accepts the YAML implicit tag at the write boundary"
    (let ((node (first (yaml-kit:compose-all "plain"))))
      (expect (aitools.inspect.domain::%yaml-write-explicit-tag node nil)
              :to-be nil)))

  (it "rejects a local explicit tag at the write boundary"
    (let ((node (first (yaml-kit:compose-all "!custom value"))))
      (handler-case
          (progn
            (aitools.inspect.domain::%yaml-write-explicit-tag node nil)
            (fail "expected an explicit YAML tag to be rejected"))
        (yaml-unsupported-value (condition)
          (expect (yaml-unsupported-reason condition)
                  :to-equal "explicit-tag")))))

  (describe "YAML write depth boundaries"
    (it "accepts containers exactly at the configured depth"
      (let ((node (first (yaml-kit:compose-all "items: [one]"))))
        (expect (aitools.inspect.domain::%yaml-write-check-node
                 node nil (make-hash-table :test (function eq)) 0 2)
                :to-be nil)))

    (it "rejects a nested container past the configured depth"
      (let ((node (first (yaml-kit:compose-all "items: [one]"))))
        (signals yaml-syntax-error
          (aitools.inspect.domain::%yaml-write-check-node
           node nil (make-hash-table :test (function eq)) 0 1))))))
(describe "format document reader sequences"
  (it "maps arrays returned by the TOML document parser"
    (let ((value (parse-toml-document "items = [1, 2]")))
      (expect (json-object-get value "items")
              :to-equalp #(1 2))))

  (it "maps a sequence returned by the YAML document parser"
    (expect (parse-yaml-document "[one, two]")
            :to-equalp #("one" "two"))))