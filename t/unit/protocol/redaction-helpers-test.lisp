(in-package #:aitools.protocol.test)

(describe "aitools.protocol.domain shared value helpers"
  (it "maps NIL to JSON null and preserves non-NIL values"
    (expect (json-or-null nil) :to-equal (json-null))
    (expect (json-or-null "value") :to-equal "value"))

  (it "builds repair and schema-repair plists"
    (expect (repair "action" "detail" "command")
            :to-equal '(:action "action" :detail "detail" :command "command"))
    (expect (schema-repair "aitools schema read")
            :to-equal '(:action "inspect-schema"
                        :detail "Show this command's arguments and rules."
                        :command "aitools schema read"))))

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
            :to-equal "aitools read 'my file.txt' --range 1:5"))

  (it "splices list arguments, drops NIL, and quotes empty words"
    (expect (command-line "aitools" nil (list "read" "a b.txt") "")
            :to-equal "aitools read 'a b.txt' ''")))

(describe "aitools.protocol.domain redaction helpers"
  (it "matches secret key names as whole words, case-insensitively"
    (expect (secret-key-name-p "GITHUB_TOKEN") :to-be-truthy)
    (expect (secret-key-name-p "db_password") :to-be-truthy)
    (expect (secret-key-name-p "TOKENIZER_PATH") :to-be-falsy)
    (expect (secret-key-name-p "passwordless") :to-be-falsy))
  (it "ignores empty configured secret key names"
    (let ((aitools.data:*redaction-secret-key-names* '("")))
      (expect (secret-key-name-p "token") :to-be-falsy)))
  (it "classifies token characters and literal prefix boundaries"
    (let ((token-char-p
            (symbol-function 'aitools.protocol.domain::%token-char-p))
          (boundary-before-p
            (symbol-function 'aitools.protocol.domain::%boundary-before-p)))
      (expect (funcall token-char-p #\a) :to-be-truthy)
      (expect (funcall token-char-p #\Space) :to-be-falsy)
      (expect (funcall boundary-before-p "sk-token" 0) :to-be-truthy)
      (expect (funcall boundary-before-p " key" 1) :to-be-truthy)
      (expect (funcall boundary-before-p "desk-lamp" 2) :to-be-falsy))))
