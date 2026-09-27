;;;; t/unit/protocol/redaction-flow-test.lisp
(in-package #:aitools.protocol.test)

(describe "aitools.protocol.application redact-json-value"
  (it "redacts a string leaf"
    (multiple-value-bind (value count) (redact-json-value "token=ghp_1234567890abcdefABCDEF1234")
      (expect count :to-be 1)
      (expect value :to-equal "token=[REDACTED_SECRET]")))

  (it "redacts inside a JSON object's values"
    (let* ((object (json-object-from-alist (list (cons "note" "AKIAIOSFODNN7EXAMPLE"))))
           (redacted (redact-json-value object)))
      (expect (json-alist-value redacted "note") :to-equal "[REDACTED_SECRET]")))

  (it "redacts a JSON object's keys"
    (multiple-value-bind (value count)
        (redact-json-value (json-object-from-alist (list (cons "ghp_1234567890abcdefABCDEF1234" 1))))
      (expect count :to-be 1)
      (expect (json-alist value) :to-equal (list (cons "[REDACTED_SECRET]" 1)))))

  (it "masks a PEM private key split into one string per line, counting the block once"
    (let ((envelope (json-object-from-alist
                     (list (cons "path" "id_ed25519")
                           (cons "lines" (list "-----BEGIN OPENSSH PRIVATE KEY-----"
                                               "b3BlbnNzaC1rZXktdjEAAAAABG5vbmU="
                                               "QyNTUxOQAAACDummyDummyDummyDummy"
                                               "-----END OPENSSH PRIVATE KEY-----"
                                               "after"))))))
      (multiple-value-bind (value count) (redact-json-value envelope)
        (expect count :to-be 1)
        (expect (json-alist-value value "path") :to-equal "id_ed25519")
        (expect (json-alist-value value "lines")
                :to-equal (list "[REDACTED_SECRET]" "[REDACTED_SECRET]" "[REDACTED_SECRET]"
                                "[REDACTED_SECRET]" "after")))))

  (it "masks the body lines after a PEM BEGIN line when the END line is not in the output"
    (multiple-value-bind (value count)
        (redact-json-value (list "before" "-----BEGIN RSA PRIVATE KEY-----" "MIIEowIBAAKCAQEAdummy"
                                 "Proc-Type: 4,ENCRYPTED" "c3VwZXJzZWNyZXQ="))
      (expect count :to-be 1)
      (expect value :to-equal (list "before" "[REDACTED_SECRET]" "[REDACTED_SECRET]"
                                    "[REDACTED_SECRET]" "[REDACTED_SECRET]"))))

  (it "masks the base64 lines before a PEM END line when the BEGIN line is not in the output"
    (multiple-value-bind (value count)
        (redact-json-value (list "src/key.pem" "MIIEowIBAAKCAQEAdummy" "c3VwZXJzZWNyZXQ="
                                 "-----END PRIVATE KEY-----" "tail text"))
      (expect count :to-be 1)
      (expect value :to-equal (list "src/key.pem" "[REDACTED_SECRET]" "[REDACTED_SECRET]"
                                    "[REDACTED_SECRET]" "tail text"))))

  (it "redacts every element of a list"
    (multiple-value-bind (value count) (redact-json-value (list "clean" "AKIAIOSFODNN7EXAMPLE"))
      (expect count :to-be 1)
      (expect (first value) :to-equal "clean")
      (expect (second value) :to-equal "[REDACTED_SECRET]")))

  (it "leaves non-string values untouched"
    (expect (redact-json-value 42) :to-be 42)
    (expect (redact-json-value nil) :to-be-falsy)
    (expect (redact-json-value t) :to-be t))

  (it "sums redactions across a nested tree"
    (let* ((inner (json-object-from-alist (list (cons "a" "AKIAIOSFODNN7EXAMPLE"))))
           (outer (json-object-from-alist (list (cons "b" "AKIAIOSFODNN7EXAMPLE") (cons "inner" inner)))))
      (multiple-value-bind (value count) (redact-json-value outer)
        (declare (ignore value))
        (expect count :to-be 2)))))

(describe "aitools.protocol.application redact-json-value containers"
  (it "redacts a hash table's string keys and values into a copy with the same test"
    (let ((table (make-hash-table :test 'equal)))
      (setf (gethash "ghp_1234567890abcdefABCDEF1234" table) "clean"
            (gethash "note" table) "AKIAIOSFODNN7EXAMPLE"
            (gethash 7 table) 8)
      (multiple-value-bind (value count) (redact-json-value table)
        (expect count :to-be 2)
        (expect (hash-table-test value) :to-be 'equal)
        (expect (hash-table-count value) :to-be 3)
        (expect (gethash "[REDACTED_SECRET]" value) :to-equal "clean")
        (expect (gethash "note" value) :to-equal "[REDACTED_SECRET]")
        (expect (gethash 7 value) :to-be 8)
        (expect (gethash "note" table) :to-equal "AKIAIOSFODNN7EXAMPLE"))))

  (it "redacts every string of a vector and keeps its other elements"
    (multiple-value-bind (value count) (redact-json-value (vector "AKIAIOSFODNN7EXAMPLE" 1 "clean"))
      (expect count :to-be 1)
      (expect (coerce value 'list) :to-equal (list "[REDACTED_SECRET]" 1 "clean"))))

  ;; Each array is its own PEM sequence, so the two halves are two masked
  ;; blocks: the BEGIN half runs unterminated to the end of its array, and
  ;; the END line with nothing before it in its array is a block of its own.
  (it "masks each array's part of a PEM private key split across nested containers"
    (multiple-value-bind (value count)
        (redact-json-value (json-object-from-alist
                            (list (cons "a" (vector "-----BEGIN PRIVATE KEY-----" "MIIEvQIBADANBgkqhkiG9w0B"))
                                  (cons "b" (list "-----END PRIVATE KEY-----" "tail")))))
      (expect count :to-be 2)
      (expect (coerce (json-alist-value value "a") 'list)
              :to-equal (list "[REDACTED_SECRET]" "[REDACTED_SECRET]"))
      (expect (json-alist-value value "b") :to-equal (list "[REDACTED_SECRET]" "tail")))))

(describe "aitools.protocol.application redact-json-value OpenPGP keys"
  (it "masks an OpenPGP private key block split into one string per line, counting it once"
    (multiple-value-bind (value count)
        (redact-json-value (list "-----BEGIN PGP PRIVATE KEY BLOCK-----" "" "lQOYBGXdummydummydummy" "=abcd"
                                 "-----END PGP PRIVATE KEY BLOCK-----" "after"))
      (expect count :to-be 1)
      (expect value :to-equal (list "[REDACTED_SECRET]" "" "[REDACTED_SECRET]" "[REDACTED_SECRET]"
                                    "[REDACTED_SECRET]" "after")))))

(describe "aitools.protocol.application redact-json-value PEM scope"
  (flet ((search-envelope (blocks)
           (json-object-from-alist
            (list (cons "blocks" (mapcar (lambda (block)
                                           (json-object-from-alist
                                            (list (cons "path" (first block)) (cons "lines" (rest block)))))
                                         blocks))
                  (cons "ignore_source" "builtin")))))
    (it "keeps a later field unmasked after a private key block truncated before its END line"
      (multiple-value-bind (value count)
          (redact-json-value (search-envelope
                              (list (list "key.pem" "-----BEGIN RSA PRIVATE KEY-----"
                                          "MIIEowIBAAKCAQEAdummy" "c3VwZXJzZWNyZXQ="))))
        (expect count :to-be 1)
        (expect (json-alist-value (first (json-alist-value value "blocks")) "lines")
                :to-equal (list "[REDACTED_SECRET]" "[REDACTED_SECRET]" "[REDACTED_SECRET]"))
        (expect (json-alist-value value "ignore_source") :to-equal "builtin")))

    (it "keeps another file's public key block unmasked after a truncated private key block"
      (let ((public (list "-----BEGIN PUBLIC KEY-----" "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8A"
                          "AMIIBCgKCAQEAxpublic" "-----END PUBLIC KEY-----")))
        (multiple-value-bind (value count)
            (redact-json-value (search-envelope
                                (list (list "key.pem" "-----BEGIN RSA PRIVATE KEY-----" "MIIEowIBAAKCAQEAdummy")
                                      (cons "pub.pem" public))))
          (expect count :to-be 1)
          (expect (json-alist-value (second (json-alist-value value "blocks")) "lines") :to-equal public)
          (expect (json-alist-value (second (json-alist-value value "blocks")) "path") :to-equal "pub.pem"))))))
