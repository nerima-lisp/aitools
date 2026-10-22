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
                   (format nil "z = 1~%a = false~%[t]~%y = 3~%x = 4~%")))
           (table (json-object-get value "t")))
      (expect (mapcar #'car (json-object-pairs value)) :to-equal '("z" "a" "t"))
      (expect (json-object-get value "z") :to-be 1)
      (expect (json-false-value-p (json-object-get value "a")) :to-be t)
      (expect (mapcar #'car (json-object-pairs table)) :to-equal '("y" "x"))
      (expect (json-object-get table "x") :to-be 4)))

  (it "normalizes TOML datetime values into strings"
    (let ((value
            (parse-toml-document
             (format nil
                     (concatenate 'string
                                  "offset = 1979-05-27T07:32:00.123456789+00:00~%"
                                  "local_datetime = 1979-05-27T07:32:00.123456789~%"
                                  "local_date = 1979-05-27~%"
                                  "local_time = 07:32:00.123456789123~%")))))
      (expect (json-object-get value "offset")
              :to-equal "1979-05-27T07:32:00.123456789Z")
      (expect (json-object-get value "local_datetime")
              :to-equal "1979-05-27T07:32:00.123456789")
      (expect (json-object-get value "local_date") :to-equal "1979-05-27")
      (expect (json-object-get value "local_time")
              :to-equal "07:32:00.123456789")))

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
