;;;; t/unit/protocol/json-writer-test.lisp
;;;;
;;;; Redaction: the written envelope carries the number of masked regions as a
;;;; top-level `redactions`, and carries nothing extra when nothing was masked.
(in-package #:aitools.protocol.test)

(defun %written-envelope (fields)
  (with-output-to-string (out)
    (aitools.protocol.infrastructure:write-envelope
     (make-ok-envelope "read" fields) out)))

(describe "aitools.protocol.infrastructure write-envelope"
  (it "adds the top-level redactions count when a secret was masked"
    (let ((text (%written-envelope (list (cons "lines" (list "key=AKIAIOSFODNN7EXAMPLE"
                                                             "token ghp_1234567890abcdefABCDEF1234"))))))
      (expect text :to-contain "\"redactions\":2")
      (expect text :not :to-contain "AKIAIOSFODNN7EXAMPLE")))

  (it "adds to a redactions member the payload already carries instead of writing a second one"
    (let ((text (%written-envelope (list (cons "text" "PASSWORD=[REDACTED_SECRET] AKIAIOSFODNN7EXAMPLE")
                                         (cons "redactions" 1)))))
      (expect text :to-contain "\"redactions\":2")
      (expect (search "\"redactions\"" text :start2 (1+ (search "\"redactions\"" text))) :to-be-falsy)))

  (it "omits redactions when nothing was masked"
    (let ((text (%written-envelope (list (cons "lines" (list "ordinary text"))))))
      (expect text :not :to-contain "redactions"))))

(describe "aitools.protocol.infrastructure write-envelope output limit"
  (it "signals envelope-too-large and writes nothing when the envelope is past the limit"
    (let* ((out (make-string-output-stream))
           (condition (handler-case
                          (progn (aitools.protocol.infrastructure:write-envelope
                                  (make-ok-envelope "read" (list (cons "text" (make-string 16777216 :initial-element #\a))))
                                  out)
                                 nil)
                        (aitools.protocol.infrastructure:envelope-too-large (condition) condition))))
      (expect (aitools.protocol.infrastructure:envelope-too-large-limit condition) :to-be 16777216)
      (expect (length (get-output-stream-string out)) :to-be 0))))

(describe "aitools.protocol.infrastructure write-envelope JSON round trip"
  (it-property "preserves generated strings through JSON encoding and parsing"
      ((value (gen-string :min-length 0 :max-length 80
                          :alphabet (format nil "aB09\"\\~%~C" #\Tab))))
    (let* ((text (with-output-to-string (out)
                   (aitools.protocol.infrastructure:write-envelope
                    (make-ok-envelope "read" (list (cons "text" value))) out)))
           (envelope (json-kit:parse text)))
      (expect (json-alist-value envelope "schema_version") :to-be 2)
      (expect (json-alist-value envelope "status") :to-equal "ok")
      (expect (json-alist-value envelope "command") :to-equal "read")
      (expect (json-alist-value envelope "text") :to-equal value))))
