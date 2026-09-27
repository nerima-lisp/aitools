;;;; t/unit/protocol/redaction-test.lisp
;;;;
;;;; Positive cases cover every masked secret format; negative cases are the
;;;; explicit non-goals: a git SHA, a UUID, and a
;;;; long identifier must never be masked.
(in-package #:aitools.protocol.test)

(describe "aitools.protocol.domain redact-secrets"
  (it "masks a PEM private key block"
    (multiple-value-bind (text count)
        (redact-secrets (format nil "-----BEGIN RSA PRIVATE KEY-----~%AAAA~%-----END RSA PRIVATE KEY-----~%tail"))
      (expect count :to-be 1)
      (expect text :to-contain "[REDACTED_SECRET]")
      (expect text :not :to-contain "AAAA")
      (expect text :to-contain "tail")))

  (it "masks an AWS access key"
    (multiple-value-bind (text count) (redact-secrets "key=AKIAIOSFODNN7EXAMPLE end")
      (expect count :to-be 1)
      (expect text :to-equal "key=[REDACTED_SECRET] end")))

  (it "masks a GitHub personal access token"
    (multiple-value-bind (text count) (redact-secrets "token ghp_1234567890abcdefABCDEF1234 done")
      (declare (ignore text))
      (expect count :to-be 1)))

  (it "masks an OpenAI-style sk- token"
    (multiple-value-bind (text count) (redact-secrets "OPENAI_API_KEY=sk-abcdEF1234567890abcdEF12")
      (declare (ignore text))
      (expect count :to-be 1)))

  (it "masks a Slack xox* token"
    (multiple-value-bind (text count) (redact-secrets "webhook uses xoxb-1234-5678-abcdefTOKEN")
      (declare (ignore text))
      (expect count :to-be 1)))

  (it "masks the token after Bearer, keeping the scheme name visible"
    (multiple-value-bind (text count) (redact-secrets "Authorization: Bearer abc123DEF456.ghi789")
      (expect count :to-be 1)
      (expect text :to-contain "Bearer [REDACTED_SECRET]")))

  (it "masks a quoted secret-key-name assignment"
    (multiple-value-bind (text count) (redact-secrets "{\"password\": \"sup3rSecr3t!\"}")
      (expect count :to-be 1)
      (expect text :to-contain "\"password\": \"[REDACTED_SECRET]\"")))

  (it "masks a bare secret-key-name assignment"
    (multiple-value-bind (text count) (redact-secrets "export SECRET=my-value-123 more")
      (expect count :to-be 1)
      (expect text :to-equal "export SECRET=[REDACTED_SECRET] more")))

  (it "does not mask sk- inside an ordinary word"
    (multiple-value-bind (text count) (redact-secrets "let's discuss the desk-lamp and task-1 items")
      (expect count :to-be 0)
      (expect text :to-equal "let's discuss the desk-lamp and task-1 items")))

  (it "does not mask a git commit SHA"
    (multiple-value-bind (text count)
        (redact-secrets "commit 4b825dc642cb6eb9a060e54bf8d69288fbee4904 is the empty tree")
      (expect count :to-be 0)
      (expect text :not :to-contain "[REDACTED_SECRET]")))

  (it "does not mask a UUID"
    (multiple-value-bind (text count) (redact-secrets "id=550e8400-e29b-41d4-a716-446655440000 ok")
      (expect count :to-be 0)
      (expect text :not :to-contain "[REDACTED_SECRET]")))

  (it "does not mask an unprefixed long identifier"
    (multiple-value-bind (text count)
        (redact-secrets "correlation_id=EXAMPLE0correlation0id0not0a0secret0EXAMPLE")
      (expect count :to-be 0)
      (expect text :not :to-contain "[REDACTED_SECRET]")))

  (it "does not mask an IANA time zone that starts with the AWS prefix letters"
    (multiple-value-bind (text count) (redact-secrets "{\"timezone\":\"Asia/Tokyo\"}")
      (expect count :to-be 0)
      (expect text :to-equal "{\"timezone\":\"Asia/Tokyo\"}")))

  (it "does not mask an uppercase word that starts with ASIA but is not a key id"
    (multiple-value-bind (text count) (redact-secrets "category ASIAN_FOOD_MENU_ITEMS_LIST")
      (expect count :to-be 0)
      (expect text :not :to-contain "[REDACTED_SECRET]")))

  (it "does not mask a lowercase-only prefix written in uppercase"
    (multiple-value-bind (text count) (redact-secrets "flag SK-LEVEL-3 and GHP_ENABLED")
      (expect count :to-be 0)
      (expect text :not :to-contain "[REDACTED_SECRET]")))

  (it "masks an ASIA temporary access key id"
    (multiple-value-bind (text count) (redact-secrets "aws ASIAIOSFODNN7EXAMPLE done")
      (expect count :to-be 1)
      (expect text :to-equal "aws [REDACTED_SECRET] done")))

  (it "returns the input unchanged and a zero count when nothing matches"
    (multiple-value-bind (text count) (redact-secrets "ordinary text")
      (expect text :to-equal "ordinary text")
      (expect count :to-be 0)))

  (it "masks exactly the token of each literal prefix and keeps the text after it"
    (expect (redact-secrets "use ghp_1234567890abcdefABCDEF1234 now") :to-equal "use [REDACTED_SECRET] now")
    (expect (redact-secrets "gho_16C7e42F292c6912E7710c838347Ae178B4a, then")
            :to-equal "[REDACTED_SECRET], then")
    (expect (redact-secrets "pat github_pat_11ABCDEFG0123456789_abcdefXYZ end")
            :to-equal "pat [REDACTED_SECRET] end")
    (expect (redact-secrets "key sk-abcdEF1234567890abcdEF12 next") :to-equal "key [REDACTED_SECRET] next")
    (expect (redact-secrets "slack xoxb-1234-5678-abcdefTOKEN (bot)") :to-equal "slack [REDACTED_SECRET] (bot)"))

  (it "masks a Bearer token whatever the case of the scheme"
    (multiple-value-bind (text count) (redact-secrets "authorization: bearer abc123DEF456 rest")
      (expect count :to-be 1)
      (expect text :to-equal "authorization: bearer [REDACTED_SECRET] rest")))

  (it "masks a quoted assignment value up to its closing quote"
    (multiple-value-bind (text count) (redact-secrets "password: \"correct horse battery\" next")
      (expect count :to-be 1)
      (expect text :to-equal "password: \"[REDACTED_SECRET]\" next")))

  (it "masks an unquoted assignment value up to the next whitespace"
    (multiple-value-bind (text count) (redact-secrets "password=hunter2,more tail")
      (expect count :to-be 1)
      (expect text :to-equal "password=[REDACTED_SECRET] tail")))

  (it "does not mask a key name that is only part of a longer word"
    (multiple-value-bind (text count) (redact-secrets "passwordless=true max_tokens=100")
      (expect count :to-be 0)
      (expect text :to-equal "passwordless=true max_tokens=100")))

  (it "does not count an assignment whose value is already the marker"
    (multiple-value-bind (text count) (redact-secrets "PASSWORD=[REDACTED_SECRET]")
      (expect count :to-be 0)
      (expect text :to-equal "PASSWORD=[REDACTED_SECRET]")))

  (it "masks a PEM private key whose END line is not in the text"
    (multiple-value-bind (text count)
        (redact-secrets (format nil "head~%-----BEGIN PRIVATE KEY-----~%MIIEvQIBADANBgkqhkiG9w0B~%QyNTUxOQAAACD="))
      (expect count :to-be 1)
      (expect text :to-equal (format nil "head~%[REDACTED_SECRET]"))))

  (it "masks the base64 lines before a PEM END line whose BEGIN line is not in the text"
    (multiple-value-bind (text count)
        (redact-secrets (format nil "note: x~%MIIEvQIBADANBgkqhkiG9w0B~%QyNTUxOQAAACD=~%-----END RSA PRIVATE KEY-----~%tail"))
      (expect count :to-be 1)
      (expect text :to-equal (format nil "note: x~%[REDACTED_SECRET]~%tail")))))

(describe "aitools.protocol.domain secret-key-name-p"
  (it "matches a secret key name as a whole underscore-separated word, case-insensitively"
    (expect (secret-key-name-p "GITHUB_TOKEN") :to-be-truthy)
    (expect (secret-key-name-p "db_password") :to-be-truthy)
    (expect (secret-key-name-p "TOKENIZER_PATH") :to-be-falsy)
    (expect (secret-key-name-p "passwordless") :to-be-falsy)))

(describe "aitools.protocol.domain shell quoting"
  (it "leaves safe words alone and single-quotes everything else"
    (expect (shell-quote "src/a-b_c.lisp") :to-equal "src/a-b_c.lisp")
    (expect (shell-quote "a,b=c:d@e%f+g") :to-equal "a,b=c:d@e%f+g")
    (expect (shell-quote "") :to-equal "''")
    (expect (shell-quote "café") :to-equal "'café'")
    (expect (shell-quote "a b") :to-equal "'a b'")
    (expect (shell-quote "it's") :to-equal "'it'\\''s'"))

  (it "joins a list of words into one command line"
    (expect (command-line (list "aitools" "read" "my file.txt" "--range" "1:5"))
            :to-equal "aitools read 'my file.txt' --range 1:5")))

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

(describe "aitools.protocol.domain secret-key-name-p edges"
  (it-each (("") ("SECRETARY") ("tokens") ("xtoken") ("path"))
      "does not treat ~S as a secret key name"
      (name)
    (expect (secret-key-name-p name) :to-be-falsy))

  (it-each (("Api_Key") ("my-api_key") ("token_token") ("xtoken token") ("AWS_SECRET_ACCESS_KEY"))
      "treats ~S as a secret key name"
      (name)
    (expect (secret-key-name-p name) :to-be-truthy)))

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
