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
    (expect (aitools.inspect.domain::%number-value "true") :to-be nil)))

(describe "non-finite format conversion branches"
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
                 node nil (make-hash-table :test #'eq) 0 nil)
                (fail "expected a non-finite YAML write value"))
            (yaml-unsupported-value (condition)
              (expect (yaml-unsupported-reason condition)
                      :to-equal "non-finite-float")))))))

  (it "accepts the YAML implicit tag at the write boundary"
    (let ((node (first (yaml-kit:compose-all "plain"))))
      (expect (aitools.inspect.domain::%yaml-write-explicit-tag node nil)
              :to-be nil)))

(describe "YAML write depth boundary"
  (it "accepts containers exactly at the configured depth"
    (let ((node (first (yaml-kit:compose-all "items: [one]"))))
      (expect (aitools.inspect.domain::%yaml-write-check-node
               node nil (make-hash-table :test #'eq) 0 2)
              :to-be nil)))
))