(in-package #:aitools.protocol.test)

(describe "aitools.protocol.domain PEM private key line handling"
  (it "keeps a non-key line inside a key block and masks the key lines around it"
    (multiple-value-bind (text count)
        (redact-secrets (format nil "-----BEGIN RSA PRIVATE KEY-----~%MIIEow~%src/key.pem:3 note~%: stray~%QUJD~%-----END RSA PRIVATE KEY-----~%tail"))
      (expect count :to-be 1)
      (expect text :to-equal (format nil "[REDACTED_SECRET]~%src/key.pem:3 note~%: stray~%[REDACTED_SECRET]~%tail"))))

  (it "leaves a BEGIN marker with no closing dashes alone"
    (multiple-value-bind (text count) (redact-secrets (format nil "-----BEGIN RSA PRIVATE KEY~%plain words"))
      (expect count :to-be 0)
      (expect text :to-equal (format nil "-----BEGIN RSA PRIVATE KEY~%plain words"))))

  (it "masks an unterminated quoted value that runs to the end of the text"
    (multiple-value-bind (text count) (redact-secrets "password='open sesame")
      (expect count :to-be 1)
      (expect text :to-equal "password='[REDACTED_SECRET]")))

  (it "leaves a separator at the very end of the text unmasked"
    (multiple-value-bind (text count) (redact-secrets "password=")
      (expect count :to-be 0)
      (expect text :to-equal "password="))))

(describe "aitools.protocol.domain shell quoting edges"
  (it-each (("'" "''\\'''")
            ("~" "'~'")
            ("a$b" "'a$b'")
            ("*.lisp" "'*.lisp'")
            ("日本" "'日本'"))
      "quotes ~S as ~A"
      (word expected)
    (expect (shell-quote word) :to-equal expected))

  (it "quotes an argument that holds a newline"
    (expect (shell-quote (format nil "a~%b")) :to-equal (format nil "'a~%b'")))

  (it "renders no words as an empty line and an empty word as ''"
    (expect (command-line '()) :to-equal "")
    (expect (command-line (list "")) :to-equal "''")
    (expect (command-line (list "a" "" "b")) :to-equal "a '' b")))

(describe "aitools.protocol.domain secret-key-name-p edges"
  (it-each (("") ("SECRETARY") ("tokens") ("xtoken") ("path"))
      "does not treat ~S as a secret key name"
      (name)
    (expect (secret-key-name-p name) :to-be-falsy))

  (it-each (("Api_Key") ("my-api_key") ("token_token") ("xtoken token") ("AWS_SECRET_ACCESS_KEY"))
      "treats ~S as a secret key name"
      (name)
    (expect (secret-key-name-p name) :to-be-truthy)))

(describe "aitools.protocol.domain redact-secrets edges"
  (it "masks an OpenPGP private key block"
    (multiple-value-bind (text count)
        (redact-secrets (format nil "-----BEGIN PGP PRIVATE KEY BLOCK-----~%~%lQOYBGXdummy~%=abcd~%-----END PGP PRIVATE KEY BLOCK-----~%tail"))
      (expect count :to-be 1)
      (expect text :to-equal (format nil "[REDACTED_SECRET]~%tail"))))

  (it "masks an OpenPGP private key block whose END line is not in the text"
    (multiple-value-bind (text count)
        (redact-secrets (format nil "head~%-----BEGIN PGP PRIVATE KEY BLOCK-----~%Version: GnuPG v2~%lQOYBGXdummy"))
      (expect count :to-be 1)
      (expect text :to-equal (format nil "head~%[REDACTED_SECRET]"))))

  (it "masks a whole BEGIN/END private key pair written on one line"
    (multiple-value-bind (text count)
        (redact-secrets "-----BEGIN RSA PRIVATE KEY----- MIIEow -----END RSA PRIVATE KEY----- after")
      (expect count :to-be 1)
      (expect text :to-equal "[REDACTED_SECRET]")))

  (it "leaves a BEGIN line whose label is not a private key alone"
    (multiple-value-bind (text count) (redact-secrets "-----BEGIN KEY-----")
      (expect count :to-be 0)
      (expect text :to-equal "-----BEGIN KEY-----")))

  ;; Redaction masks known secret formats only; a certificate is public, and
  ;; masking it would make a read-then-write round trip refuse the marker.
  (it-each (("-----BEGIN CERTIFICATE----- MIIC -----END CERTIFICATE----- after")
            ("-----BEGIN PUBLIC KEY-----
MIIBIjAN
-----END PUBLIC KEY-----"))
      "leaves a non-secret PEM block unmasked: ~S"
      (text)
    (multiple-value-bind (redacted count) (redact-secrets text)
      (expect count :to-be 0)
      (expect redacted :to-equal text)))

  ;; An AWS key id is the prefix plus exactly 16 uppercase letters or digits,
  ;; standing as its own word.
  (it-each (("AKIAIOSFODNN7EXAMPL")     ; 15 body characters
            ("AKIAIOSFODNN7EXAMPLEQ")   ; 17 body characters
            ("AKIAiosfodnn7example")    ; a lowercase body
            ("xAKIAIOSFODNN7EXAMPLE"))  ; glued to a preceding letter
      "does not mask the AWS-like ~S"
      (text)
    (multiple-value-bind (redacted count) (redact-secrets text)
      (expect count :to-be 0)
      (expect redacted :to-equal text)))

  (it "masks an AWS key id followed by non-ASCII text"
    (expect (redact-secrets "AKIAIOSFODNN7EXAMPLE café") :to-equal "[REDACTED_SECRET] café"))

  (it-each (("ghp_")         ; a literal prefix with no token after it
            ("Bearer ")      ; a scheme with no token after it
            ("password")     ; a key name with no assignment
            ("password = ")  ; an assignment with no value
            ("password=\"\"") ; an empty quoted value
            ("passwd x"))    ; a key name followed by a word, not `=` or `:`
      "leaves ~S unmasked"
      (text)
    (multiple-value-bind (redacted count) (redact-secrets text)
      (expect count :to-be 0)
      (expect redacted :to-equal text)))

  (it "masks an unterminated quoted value to the end of the line"
    (multiple-value-bind (text count) (redact-secrets (format nil "password: 'open sesame~%next"))
      (expect count :to-be 1)
      (expect text :to-equal (format nil "password: '[REDACTED_SECRET]~%next"))))

  (it "counts a token that two matchers both find as one redaction"
    (multiple-value-bind (text count) (redact-secrets "api_key=sk-abcdef123456 rest")
      (expect count :to-be 1)
      (expect text :to-equal "api_key=[REDACTED_SECRET] rest")))

  (it "matches a key name in any case with tabs around the separator"
    (multiple-value-bind (text count) (redact-secrets (format nil "Client_Secret~C=~Cabc def" #\Tab #\Tab))
      (expect count :to-be 1)
      (expect text :to-equal (format nil "Client_Secret~C=~C[REDACTED_SECRET] def" #\Tab #\Tab)))))
