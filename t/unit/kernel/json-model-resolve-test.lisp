(in-package #:aitools.kernel.test)

(describe "aitools.kernel.domain JSON value models"
  (it "calls on-found after resolving every pointer token"
    (let* ((model aitools.edit.domain::*json-value-model*)
           (profile (json-object-with (cons "name" "Ada")))
           (document (json-object-with (cons "profile" profile)))
           (result nil))
      (aitools.kernel.domain::json-model-resolve/k
       model document (quote ("profile" "name"))
       :on-found (lambda (value) (setf result value))
       :on-missing (lambda (&rest values) (setf result values)))
      (expect result :to-equal "Ada")))

  (it "passes the prefix, current value, and missing token to on-missing"
    (let* ((model aitools.edit.domain::*json-value-model*)
           (profile (json-object-with (cons "name" "Ada")))
           (document (json-object-with (cons "profile" profile)))
           (result nil))
      (aitools.kernel.domain::json-model-resolve/k
       model document (quote ("profile" "age"))
       :on-found (lambda (value) (setf result (list :found value)))
       :on-missing (lambda (path value token)
                     (setf result (list path value token))))
      (expect (first result) :to-equal (quote ("profile")))
      (expect (second result) :to-be profile)
      (expect (third result) :to-equal "age")))

  (it-each ((1 2 ((:replace nil 1 2)))
            ("same" "same" nil)
            ((:object "removed" 1 "same" "old")
             (:object "same" "old" "added" 2)
             ((:remove ("removed") 1 nil)
              (:add ("added") nil 2)))
            ((:array "a" "b")
             (:array "a" "c" "d")
             ((:replace ("1") "b" "c")
              (:add ("2") nil "d"))))
      "diffs ~S and ~S as ~S"
      (left right expected)
    (expect (json-model-diff-ops (test-json-value-model) left right)
            :to-equal expected))

  (it "preserves nested pointer tokens in object differences"
    (let ((model (test-json-value-model)))
      (expect (json-model-diff-ops
               model
               (test-json-object "profile" (test-json-object "a/b" "old"))
               (test-json-object "profile" (test-json-object "a/b" "new")))
              :to-equal (quote ((:replace ("profile" "a/b") "old" "new"))))))

  (it-property "returns no diff operations for identical generated strings"
      ((value (gen-string :min-length 0 :max-length 40)))
    (expect (json-model-diff-ops (test-json-value-model) value value)
            :to-equal nil))

  (it-property "round-trips generated JSON Pointer tokens"
      ((tokens (gen-list (gen-string :min-length 0 :max-length 12 :alphabet "ab~/")
                         :min-length 0 :max-length 8)))
    (expect (parse-json-pointer (format-json-pointer tokens))
            :to-equal tokens))

  (it "recursively merges object patches and removes null members"
    (let* ((model (test-json-value-model))
           (target (test-json-object
                    "drop" "old"
                    "nested" (test-json-object "keep" 1 "replace" "old")))
           (patch (test-json-object
                   "drop" :null
                   "nested" (test-json-object "replace" "new" "add" "value")
                   "added" 2)))
      (expect (json-model-merge-patch model target patch)
              :to-equal (test-json-object
                         "nested" (test-json-object "keep" 1 "replace" "new" "add" "value")
                         "added" 2))
      (expect (json-model-merge-patch model target "replacement")
              :to-equal "replacement")
      (expect (json-model-merge-patch model "scalar" patch)
              :to-equal (test-json-object
                         "nested" (test-json-object "replace" "new" "add" "value")
                         "added" 2))))

  (it "updates object and array values through JSON pointers"
    (let* ((model (test-json-value-model))
           (object (test-json-object "name" "old"))
           (array (test-json-array "first" "second"))
           (root (test-json-object "profile" object "items" array)))
      (expect (json-model-pointer-get model root nil) :to-be root)
      (expect (json-model-pointer-get model root (quote ("profile" "name")))
              :to-equal "old")
      (expect (json-model-add model object (quote ("city")) "Tokyo")
              :to-equal (test-json-object "name" "old" "city" "Tokyo"))
      (expect (json-model-replace model object (quote ("name")) "new")
              :to-equal (test-json-object "name" "new"))
      (expect (json-model-remove model object (quote ("name")))
              :to-equal (test-json-object))
      (expect (json-model-add model array (quote ("1")) "middle")
              :to-equal (test-json-array "first" "middle" "second"))
      (expect (json-model-add model array (quote ("-")) "last")
              :to-equal (test-json-array "first" "second" "last"))
      (expect (json-model-add model array (quote ("1")) "replacement"
                              :array-mode :replace)
              :to-equal (test-json-array "first" "replacement"))
      (expect (json-model-remove model array (quote ("0")))
              :to-equal (test-json-array "second"))
      (expect (json-model-add model root nil :replacement) :to-be :replacement)
      (expect (json-model-replace model root nil :replacement) :to-be :replacement)))

  (it "exposes model children, child names, and container lengths"
    (let* ((model (test-json-value-model))
           (object (test-json-object "name" "Ada"))
           (array (test-json-array "one" "two")))
      (multiple-value-bind (value present) (json-model-child model object "name")
        (expect value :to-equal "Ada")
        (expect present :to-be-truthy))
      (multiple-value-bind (value present) (json-model-child model array "1")
        (expect value :to-equal "two")
        (expect present :to-be-truthy))
      (multiple-value-bind (value present) (json-model-child model :scalar "name")
        (expect value :to-be-falsy)
        (expect present :to-be-falsy))
      (expect (json-model-child-names model object) :to-equal (list "name"))
      (expect (json-model-child-names model array) :to-equal (list "0" "1"))
      (expect (json-model-child-names model :scalar) :to-equal nil)
      (expect (json-model-value-length model object) :to-be 1)
      (expect (json-model-value-length model array) :to-be 2)
      (expect (json-model-value-length model "Ada") :to-be 3)
      (expect (json-model-value-length model :scalar) :to-be-falsy)))

  (it "copies and moves array values through JSON Patch"
    (let* ((model (test-json-value-model))
           (root (test-json-object "items" (test-json-array "a" "b")))
           (operations
             (test-json-array
              (test-json-object "op" "copy" "from" "/items/0" "path" "/items/1")
              (test-json-object "op" "move" "from" "/items/2" "path" "/items/0")
              (test-json-object "op" "test" "path" "/items/0" "value" "b")))
           (same-path
             (test-json-array
              (test-json-object "op" "move" "from" "/items/0" "path" "/items/0"))))
      (expect (json-model-pointer-get model (json-model-apply-patch model root operations)
                                     (quote ("items" "0")))
              :to-equal "b")
      (expect (json-model-apply-patch model root same-path) :to-equal root)))

  (it "reports JSON Patch failures at the operation boundary"
    (let ((model (test-json-value-model))
          (root (test-json-object "name" "old")))
      (expect (model-error-message
               (lambda () (json-model-remove model root nil)))
              :to-equal "the root value cannot be removed")
      (expect (model-error-message
               (lambda () (json-model-apply-patch model root "not-an-array")))
              :to-equal "a JSON Patch document must be an array")
      (expect (model-error-message
               (lambda () (json-model-apply-patch model root (test-json-array "not-an-object"))))
              :to-equal "JSON Patch operation 0 is not an object")
      (expect (model-error-message
               (lambda ()
                 (json-model-apply-patch
                  model root
                  (test-json-array (test-json-object "op" "move" "from" "/name"
                                                     "path" "/name/child")))))
              :to-equal "JSON Patch cannot move a value into itself")
      (expect (model-error-message
               (lambda ()
                 (json-model-apply-patch
                  model root
                  (test-json-array (test-json-object "op" "test" "path" "/name"
                                                     "value" "different")))))
              :to-equal "JSON Patch test failed at \"/name\" (operation 0)")))

  (it-each (((:object "op" "add") "JSON Patch operation lacks \"path\"")
            ((:object "op" "add" "path" "/name")
             "JSON Patch operation lacks \"value\"")
            ((:object "op" "add" "path" 1 "value" "x")
             "JSON Patch member \"path\" must be a string")
            ((:object "op" "move" "path" "/name")
             "JSON Patch operation lacks \"from\"")
            ((:object "op" "other") "unknown JSON Patch op \"other\"")
            ((:object "path" "/name") "JSON Patch operation lacks \"op\"")
            ((:object "op" "add" "path" "/bad~2" "value" "x")
             "JSON pointer \"/bad~2\" must be empty or start with /, with valid ~ escapes"))
      "rejects operation ~S with ~A"
      (operation expected)
    (let ((model (test-json-value-model))
          (root (test-json-object "name" "old")))
      (expect (model-error-message
               (lambda ()
                 (json-model-apply-patch model root (test-json-array operation))))
              :to-equal expected)))
  (it-each (((:array "a" "b" "c") (:array "a" "b")
              ((:remove ("2") "c" nil)))
            ((:array "a" "b") (:array "a")
             ((:remove ("1") "b" nil))))
    "diffs the surplus array members from ~S to ~S"
    (left right expected)
  (expect (json-model-diff-ops (test-json-value-model) left right)
          :to-equal expected))
  (it "recurses through nested arrays and reports missing pointers"
  (let* ((model (test-json-value-model))
         (root (test-json-object
                "items" (test-json-array
                         (test-json-object "name" "old"))))
         (updated (test-json-object
                   "items" (test-json-array
                            (test-json-object "name" "new"))))
         (array (test-json-array "one" "two")))
    (expect (json-model-replace model root (quote ("items" "0" "name")) "new")
            :to-equal updated)
    (expect (model-error-message
             (lambda () (json-model-pointer-get model root (quote ("missing")))))
            :to-equal (format nil "no value at JSON pointer ~S" "/missing"))
    (expect (model-error-message
             (lambda () (json-model-pointer-get model root (quote ("items" "3")))))
            :to-equal (format nil "no value at JSON pointer ~S" "/items/3"))
    (expect (model-error-message
             (lambda () (json-model-add model :scalar (quote ("name")) "value")))
            :to-equal (format nil "no value at JSON pointer ~S" "/name"))
    (expect (model-error-message
             (lambda () (json-model-remove model array (quote ("4")))))
            :to-equal (format nil "no value at JSON pointer ~S" "/4"))
    (expect (model-error-message
             (lambda () (json-model-add model root (quote ("missing" "child")) "value")))
            :to-equal (format nil "no value at JSON pointer ~S" "/missing"))
    (expect (model-error-message
             (lambda () (json-model-add model array (quote ("9" "child")) "value")))
            :to-equal (format nil "no value at JSON pointer ~S" "/9"))
    (expect (model-error-message
             (lambda () (json-model-add model array (quote ("9")) "value")))
            :to-equal (format nil "no value at JSON pointer ~S" "/9"))
    (expect (model-error-message
             (lambda () (json-model-remove model root (quote ("missing")))))
            :to-equal (format nil "no value at JSON pointer ~S" "/missing"))
    (expect (model-error-message
             (lambda () (json-model-remove model :scalar (quote ("field")))))
            :to-equal (format nil "no value at JSON pointer ~S" "/field"))
    (expect (model-error-message
             (lambda ()
               (aitools.kernel.domain::%json-model-update-at
                model :scalar (quote ("field" "nested")) (function identity))))
            :to-equal (format nil "no value at JSON pointer ~S" "/field")))
  (let ((condition (make-condition
                    (find-symbol "JSON-MODEL-ERROR" "AITOOLS.KERNEL.DOMAIN")
                    :code "test" :message "test report")))
    (expect (princ-to-string condition) :to-equal "test report"))))
