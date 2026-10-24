;;;; t/unit/inspect/toml-values-test.lisp
(in-package #:aitools.inspect.test)

(defun %assert-toml-unsupported (text reason pointer)
  (handler-case
      (progn
        (parse-toml-document text)
        (fail "expected an unsupported TOML value"))
    (toml-unsupported-value (condition)
      (expect (toml-unsupported-reason condition) :to-equal reason)
      (expect (toml-unsupported-pointer condition) :to-equal pointer))))

(describe "TOML value mapping"
  (it "maps values and preserves source member order"
    (let* ((value (parse-toml-document
                   (format nil "z = 1~%a = false~%fraction = 1.5~%[t]~%y = 3~%x = 4~%")))
           (table (json-object-get value "t")))
      (expect (mapcar (function car) (json-object-pairs value))
              :to-equal (list "z" "a" "fraction" "t"))
      (expect (json-object-get value "z") :to-be 1)
      (expect (json-false-value-p (json-object-get value "a")) :to-be t)
      (expect (= (json-object-get value "fraction") 1.5) :to-be t)
      (expect (mapcar (function car) (json-object-pairs table)) :to-equal (list "y" "x"))
      (expect (json-object-get table "x") :to-be 4)))

  (it "normalizes TOML datetime values into strings"
    (let ((value
            (parse-toml-document
             (format nil
                     "offset = 1979-05-27T07:32:00.123456789+00:00~%local_datetime = 1979-05-27T07:32:00.123456789~%local_date = 1979-05-27~%local_time = 07:32:00.123456789123~%"))))
      (expect (json-object-get value "offset")
              :to-equal "1979-05-27T07:32:00.123456789Z")
      (expect (json-object-get value "local_datetime")
              :to-equal "1979-05-27T07:32:00.123456789")
      (expect (json-object-get value "local_date") :to-equal "1979-05-27")
      (expect (json-object-get value "local_time")
              :to-equal "07:32:00.123456789"))
    (expect (aitools.inspect.domain::%toml-trim-fraction "1979-05-27T07:32:00.120000Z")
            :to-equal "1979-05-27T07:32:00.12Z")
    (expect (aitools.inspect.domain::%toml-trim-fraction "1979-05-27T07:32:00.000000Z")
            :to-equal "1979-05-27T07:32:00Z")
    (expect (aitools.inspect.domain::%toml-normalize-date-time "1979-05-27t07:32:00z")
            :to-equal "1979-05-27T07:32:00Z")
    (expect (aitools.inspect.domain::%toml-normalize-date-time "1979-05-27T07:32:00-00:00")
            :to-equal "1979-05-27T07:32:00Z"))

  (it "rejects non-finite numbers and TOML integers outside the parser range"
    (%assert-toml-unsupported "x = inf" "non-finite-float" "/x")
    (%assert-toml-unsupported "x = -inf" "non-finite-float" "/x")
    (%assert-toml-unsupported "x = nan" "non-finite-float" "/x")
    (signals toml-syntax-error
      (parse-toml-document "x = 9223372036854775808")))

  (it "maps malformed TOML to a syntax condition"
    (signals toml-syntax-error (parse-toml-document "x = [")))

  (cl-weave:it-property
   "round-trips generated strings through the TOML parser"
   ((value (cl-weave:gen-string
            :min-length 0
            :max-length 80
            :alphabet (format nil "aB09\"\\~%~C" #\Tab))))
   (let* ((text (format nil "value = ~A~%" (render-json value)))
          (object (parse-toml-document text)))
     (expect (json-object-get object "value") :to-equal value))))

(describe "TOML write validation"
  (it "validates JSON values and reports source comment spans"
    (multiple-value-bind (value comments)
        (parse-toml-write-document (format nil "# note~%items = [1, false]~%"))
      (let ((items (json-object-get value "items"))) (expect (length items) :to-be 2) (expect (aref items 0) :to-be 1) (expect (json-false-value-p (aref items 1)) :to-be t))
      (expect comments :to-be 1)))
  (it "rejects TOML date values and non-finite floats for writes"
    (labels ((assert-unsupported (text reason pointer)
               (handler-case
                   (progn
                     (parse-toml-write-document text)
                     (fail "expected unsupported TOML write value"))
                 (toml-unsupported-value (condition)
                   (expect (toml-unsupported-reason condition) :to-equal reason)
                   (expect (toml-unsupported-pointer condition) :to-equal pointer)))))
      (assert-unsupported "value = 1979-05-27T07:32:00+00:00" "offset-date-time" "/value")
      (assert-unsupported "value = 1979-05-27T07:32:00" "local-date-time" "/value")
      (assert-unsupported "value = 1979-05-27" "local-date" "/value")
      (assert-unsupported "value = 07:32:00" "local-time" "/value")
      (assert-unsupported "value = [1, inf]" "non-finite-float" "/value/1")))
  (it "maps malformed TOML to a syntax condition in write mode"
    (signals toml-syntax-error (parse-toml-write-document "value = ["))))

(describe "TOML write preflight edge cases"
  (it "rejects NaN values"
    (handler-case
        (progn
          (parse-toml-write-document "value = nan")
          (fail "expected unsupported TOML write value"))
      (toml-unsupported-value (condition)
        (expect (toml-unsupported-reason condition) :to-equal "non-finite-float")
        (expect (toml-unsupported-pointer condition) :to-equal "/value")))))
