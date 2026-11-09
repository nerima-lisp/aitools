(in-package #:aitools.kernel.test)

(describe "aitools.kernel.domain JSON value models"
  (it "applies pointer operations to the production JSON representation"
    (let* ((model aitools.edit.domain::*json-value-model*)
           (root (json-object-with
                  (cons "profile" (json-object-with (cons "name" "old")))
                  (cons "items" (vector "one" "two"))))
           (added (json-model-add model root (list "profile" "city") "Tokyo"))
           (replaced (json-model-replace model added (list "items" "1") "TWO"))
           (removed (json-model-remove model replaced (list "items" "0")))
           (removed-items (json-model-pointer-get model removed (list "items"))))
      (expect (json-model-pointer-get model added (list "profile" "city")) :to-equal "Tokyo")
      (expect (json-model-pointer-get model replaced (list "items" "1")) :to-equal "TWO")
      (expect (json-model-pointer-get model removed (list "items" "0")) :to-equal "TWO")
      (expect (and (vectorp removed-items) (not (stringp removed-items))) :to-be-truthy)))

  (it "applies JSON Patch operations through the production value model"
    (let* ((model aitools.edit.domain::*json-value-model*)
           (root (json-object-with
                  (cons "items" (vector "one" "two"))))
           (operations (vector
                        (json-object-with (cons "op" "add")
                                          (cons "path" "/items/1")
                                          (cons "value" "middle"))
                        (json-object-with (cons "op" "replace")
                                          (cons "path" "/items/0")
                                          (cons "value" "first"))
                        (json-object-with (cons "op" "test")
                                          (cons "path" "/items/1")
                                          (cons "value" "middle"))
                        (json-object-with (cons "op" "remove")
                                          (cons "path" "/items/2")))))
      (let ((equality-spy (cl-weave:spy-on (quote aitools.kernel.domain:json-model-equal))))
        (unwind-protect
             (let* ((result (json-model-apply-patch model root operations))
                    (result-items (json-model-pointer-get model result (list "items"))))
               (expect (json-model-pointer-get model result (list "items" "0")) :to-equal "first")
               (expect (json-model-pointer-get model result (list "items" "1")) :to-equal "middle")
               (expect (and (vectorp result-items) (not (stringp result-items))) :to-be-truthy)
               (expect (length result-items) :to-equal 2)
               (expect (length (cl-weave:mock-calls equality-spy)) :to-equal 1))
          (cl-weave:mock-restore equality-spy)
          (cl-weave:dispose-mock equality-spy)))))

  (it "preserves JSON Patch pointer errors and field validation order"
    (let ((model aitools.edit.domain::*json-value-model*)
          (root (json-object-with (cons "name" "old"))))
      (flet ((apply-one (operation)
               (json-model-apply-patch model root (vector operation))))
        (expect (model-error-message
                 (lambda ()
                   (apply-one (json-object-with (cons "op" "add")
                                                (cons "path" "/bad~2")
                                                (cons "value" "x")))))
                :to-equal "JSON pointer \"/bad~2\" must be empty or start with /, with valid ~ escapes")
        (expect (model-error-message
                 (lambda ()
                   (apply-one (json-object-with (cons "op" "add")
                                                (cons "value" "x")))))
                :to-equal "JSON Patch operation lacks \"path\"")
        (expect (model-error-message
                 (lambda ()
                   (apply-one (json-object-with (cons "op" "replace")
                                                (cons "value" "x")))))
                :to-equal "JSON Patch operation lacks \"path\"")
        (expect (model-error-message
                 (lambda ()
                   (apply-one (json-object-with (cons "op" "test")
                                                (cons "value" "x")))))
                :to-equal "JSON Patch operation lacks \"path\"")))))
