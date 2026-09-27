;;;; data/domain/protocol/redaction-patterns-data.lisp
;;;;
;;;; The list of "known formats" redaction masks. Deliberately not a general
;;;; secret-entropy detector (only known formats are masked -- an
;;;; entropy-based detector was rejected precisely because it would mask a
;;;; git SHA, a UUID, or any other long identifier).
(in-package #:aitools.data)

(defparameter *redaction-literal-prefixes*
  '("ghp_" "gho_" "github_pat_" "sk-")
  "Prefixes that, once seen, mask themselves plus every following token
character (see TOKEN-CHAR-P in redaction.lisp). Matched case-sensitively:
issuers emit them in exactly this case.")

(defparameter *redaction-aws-key-prefixes*
  '("AKIA" "ASIA")
  "AWS access key id prefixes. Unlike *REDACTION-LITERAL-PREFIXES* these are
ordinary uppercase letters that also begin words (\"ASIAN\") and names
(\"Asia/Tokyo\"), so a match additionally requires the fixed id shape: the
prefix followed by exactly 16 uppercase letters or digits.")

(defparameter *redaction-aws-key-body-length* 16)

(defparameter *redaction-slack-prefixes*
  '("xoxb-" "xoxp-" "xoxa-" "xoxr-" "xoxs-" "xoxo-")
  "The `xox*` family of Slack token prefixes redaction masks; kept separate
from *REDACTION-LITERAL-PREFIXES* because the family shares a 3-character
stem (`xox`) with a variable 4th character, not one fixed string.")

(defparameter *redaction-secret-key-names*
  '("password" "passwd" "secret" "token" "api_key" "apikey" "access_key"
    "access_token" "auth_token" "client_secret" "secret_key" "private_key")
  "Key names redaction treats as naming a secret: when one of these
(case-insensitively) is immediately followed by `=` or `:` and a value, the
value is masked regardless of its own shape.")

(export '(*redaction-literal-prefixes* *redaction-aws-key-prefixes* *redaction-aws-key-body-length*
          *redaction-slack-prefixes* *redaction-secret-key-names*))
