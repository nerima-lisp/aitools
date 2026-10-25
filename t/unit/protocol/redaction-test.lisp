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

  (it "masks every configured literal and Slack prefix"
    (dolist (prefix (append aitools.data:*redaction-literal-prefixes*
                            aitools.data:*redaction-slack-prefixes*))
      (expect (redact-secrets (format nil "value ~Aabc123 end" prefix))
              :to-equal "value [REDACTED_SECRET] end")))

  (it "ignores empty configured patterns while retaining valid ones"
    (let ((aitools.data:*redaction-literal-prefixes*
            (cons "" aitools.data:*redaction-literal-prefixes*))
          (aitools.data:*redaction-slack-prefixes*
            (cons "" aitools.data:*redaction-slack-prefixes*))
          (aitools.data:*redaction-aws-key-prefixes*
            (cons "" aitools.data:*redaction-aws-key-prefixes*))
          (aitools.data:*redaction-secret-key-names*
            (cons "" aitools.data:*redaction-secret-key-names*)))
      (expect (redact-secrets "good ghp_abc AKIAIOSFODNN7EXAMPLE password=top-secret")
              :to-equal "good [REDACTED_SECRET] [REDACTED_SECRET] password=[REDACTED_SECRET]")))

  (it "dispatches a prefix added to the live configuration"
    (let ((aitools.data:*redaction-literal-prefixes*
            (copy-list aitools.data:*redaction-literal-prefixes*)))
      (expect (redact-secrets "value zzt-abc123 end")
              :to-equal "value zzt-abc123 end")
      (push "zzt-" aitools.data:*redaction-literal-prefixes*)
      (expect (redact-secrets "value zzt-abc123 end")
              :to-equal "value [REDACTED_SECRET] end")))

  (it "dispatches Unicode patterns added to the live configuration"
    (let ((aitools.data:*redaction-literal-prefixes*
            (cons "秘密-" aitools.data:*redaction-literal-prefixes*))
          (aitools.data:*redaction-secret-key-names*
            (cons "秘密" aitools.data:*redaction-secret-key-names*)))
      (expect (redact-secrets "秘密-abc123 秘密=top-secret")
              :to-equal "[REDACTED_SECRET] 秘密=[REDACTED_SECRET]")))

  (it-property "masking a literal token is idempotent for generated token bodies"
      ((body (gen-string :min-length 1 :max-length 30 :alphabet "abc123_-./+=")))
    (multiple-value-bind (once count)
        (redact-secrets (format nil "key sk-~A end" body))
      (multiple-value-bind (twice extra) (redact-secrets once)
        (expect count :to-be 1)
        (expect extra :to-be 0)
        (expect twice :to-equal once))))

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

