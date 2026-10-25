;;;;
;;;; The JSON pointer and equality rules shared by the inspect and edit
;;;; contexts. The test-json-equal cases use a representation private to this test
;;;; (plists for objects, lists for arrays, keywords for literals, and either
;;;; Lisp numbers or number text), which is the point: JSON-EQUAL knows only
;;;; the classifier it is given.
(in-package #:aitools.kernel.test)

(defun classify-test-value (value)
  (cond ((and (consp value) (eq (first value) :object))
         (values :object (loop for (key member) on (rest value) by #'cddr collect (cons key member))))
        ((and (consp value) (eq (first value) :array)) (values :array (rest value)))
        ((and (consp value) (eq (first value) :number)) (values :number (second value)))
        ((realp value) (values :number value))
        ((stringp value) (values :string value))
        (t (values value nil))))

(defun test-json-equal (a b)
  (json-equal a b #'classify-test-value))

(defun json-object-with (&rest members)
  (aitools.edit.domain:make-json-obj members))

(defun model-error-message (thunk)
  (handler-case (progn (funcall thunk) nil)
    (json-model-error (condition) (json-model-error-message condition))))

(describe "aitools.kernel.domain JSON pointers"
  (it "parses and formats RFC 6901 pointers with ~0 and ~1 escapes"
    (expect (parse-json-pointer "") :to-equal '())
    (expect (parse-json-pointer "/a~1b/~0c/0") :to-equal (list "a/b" "~c" "0"))
    (expect (parse-json-pointer "/") :to-equal (list ""))
    (expect (format-json-pointer (list "a/b" "~c" "0")) :to-equal "/a~1b/~0c/0"))

  (it "reports a malformed pointer as :INVALID"
    (expect (parse-json-pointer "a") :to-be :invalid)
    (expect (parse-json-pointer "/a~2") :to-be :invalid)
    (expect (parse-json-pointer "/a~") :to-be :invalid))

  (it "reads an array index without leading zeros, and `-` only when asked"
    (expect (json-pointer-array-index "0" 3) :to-be 0)
    (expect (json-pointer-array-index "2" 3) :to-be 2)
    (expect (json-pointer-array-index "3" 3) :to-be-falsy)
    (expect (json-pointer-array-index "01" 3) :to-be-falsy)
    (expect (json-pointer-array-index "-" 3) :to-be-falsy)
    (expect (json-pointer-array-index "-" 3 :allow-end t) :to-be 3)
    (expect (json-pointer-array-index "3" 3 :allow-end t) :to-be 3)
    (expect (json-pointer-array-index "１" 3) :to-be-falsy)))

(describe "aitools.kernel.domain json-equal"
  (it "compares numbers as the IEEE doubles their text parses to"
    (expect (test-json-equal '(:number "1.00000000000000001") 1) :to-be-truthy)
    (expect (test-json-equal '(:number "0.1") '(:number "0.10000000000000001")) :to-be-truthy)
    (expect (test-json-equal '(:number "1") '(:number "1.0e0")) :to-be-truthy)
    (expect (test-json-equal '(:number "10") '(:number "1E1")) :to-be-truthy)
    (expect (test-json-equal '(:number "-0") 0) :to-be-truthy)
    (expect (test-json-equal 0.1d0 '(:number "0.1")) :to-be-truthy)
    (expect (test-json-equal '(:number "9007199254740993") 9007199254740992) :to-be-truthy)
    (expect (test-json-equal '(:number "0.1") '(:number "0.2")) :to-be-falsy)
    (expect (test-json-equal '(:number "1") '(:number "-1")) :to-be-falsy))

  (it "treats numbers past the double range as signed infinity without expanding them"
    (expect (test-json-equal '(:number "1e400") '(:number "2e999999999")) :to-be-truthy)
    (expect (test-json-equal '(:number "1e400") '(:number "-1e400")) :to-be-falsy)
    (expect (test-json-equal '(:number "1e-999999999") 0) :to-be-truthy)
    (expect (test-json-equal '(:number "1e308") '(:number "1e309")) :to-be-falsy))

  (it "compares strings, literals, and arrays exactly"
    (expect (test-json-equal "a" "a") :to-be-truthy)
    (expect (test-json-equal "a" "A") :to-be-falsy)
    (expect (test-json-equal :null :null) :to-be-truthy)
    (expect (test-json-equal :true :false) :to-be-falsy)
    (expect (test-json-equal "1" 1) :to-be-falsy)
    (expect (test-json-equal '(:array 1 "x") '(:array 1.0d0 "x")) :to-be-truthy)
    (expect (test-json-equal '(:array 1 2) '(:array 2 1)) :to-be-falsy)
    (expect (test-json-equal '(:array 1) '(:array 1 1)) :to-be-falsy))

  (it "ignores object member order and lets a repeated key's last value win"
    (expect (test-json-equal '(:object "a" 1 "b" (:array 2)) '(:object "b" (:array 2) "a" 1)) :to-be-truthy)
    (expect (test-json-equal '(:object "a" 1 "a" 2) '(:object "a" 2)) :to-be-truthy)
    (expect (test-json-equal '(:object "a" 1) '(:object "a" 1 "b" 2)) :to-be-falsy)
    (expect (test-json-equal '(:object "a" 1) '(:array 1)) :to-be-falsy)))

(describe "aitools.kernel.domain JSON pointer edges"
  (it "keeps empty reference tokens and decodes ~01 as a literal ~1"
    (expect (parse-json-pointer "//") :to-equal (list "" ""))
    (expect (parse-json-pointer "/~01") :to-equal (list "~1"))
    (expect (format-json-pointer (list "~1" "")) :to-equal "/~01/"))

  (it "rejects an empty index and a non-ASCII digit"
    (expect (json-pointer-array-index "" 3) :to-be-falsy)
    (expect (json-pointer-array-index "١" 3) :to-be-falsy)))

(describe "aitools.kernel.domain scalar field formatters"
  (it-each ((0 "1900-01-01T00:00:00Z")
            (3913056000 "2024-01-01T00:00:00Z")
            (3913142399 "2024-01-01T23:59:59Z")
            (3960057600 "2025-06-28T00:00:00Z"))
      "renders universal time ~D as ~A"
      (universal-time expected)
    (expect (iso8601-utc universal-time) :to-equal expected))

  (it-each ((#o644 "0644")
            (#o100755 "0755")
            (#o4755 "4755")
            (#o7777 "7777")
            (0 "0000"))
      "renders mode ~O as ~A"
      (mode expected)
    (expect (octal-mode mode) :to-equal expected)))

(describe "aitools.kernel.domain JSON number edges"
  (it "reads a multi-digit array index"
    (expect (json-pointer-array-index "12" 20) :to-be 12))

  (it "treats an exact rational past the double range as signed infinity"
    (expect (test-json-equal (expt 10 400) '(:number "1e400")) :to-be-truthy)
    (expect (test-json-equal (- (expt 10 400)) '(:number "-1e400")) :to-be-truthy)
    (expect (test-json-equal (expt 10 400) '(:number "-1e400")) :to-be-falsy)
    (expect (test-json-equal 1/2 '(:number "0.5")) :to-be-truthy)))

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
