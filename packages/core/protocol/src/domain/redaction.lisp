;;;; packages/core/protocol/src/domain/redaction.lisp
;;;;
(in-package #:aitools.protocol.domain)

(defun redact-secret-sequence (texts)
  "Return (VALUES REDACTED-TEXTS COUNT) for TEXTS, a list of the strings of
one text in order (e.g. the lines of one file). Each string is masked as by REDACT-SECRETS,
and a PEM private key split across several strings is masked in all of them
and counted once."
  (let ((vector (coerce texts 'simple-vector)))
    (multiple-value-bind (pem-spans total) (%pem-private-key-spans vector)
      (values (loop for text across vector
                    for spans across pem-spans
                    collect (multiple-value-bind (redacted count)
                                (%redact-patterns (if spans
                                                      (%replace-spans text (%merge-pem-spans text spans))
                                                      text))
                              (incf total count)
                              redacted))
              total))))

(defun redact-secrets (text)
  "Return (VALUES REDACTED-TEXT COUNT). COUNT is the number of masked regions,
each replaced by the literal string \"[REDACTED_SECRET]\"."
  (multiple-value-bind (texts count) (redact-secret-sequence (list text))
    (values (first texts) count)))

(defun secret-key-name-p (name)
  "True when NAME contains one of the redacted secret key names as a whole word,
case-insensitively; `_` and other non-alphanumerics separate words, so
GITHUB_TOKEN and DB_PASSWORD match and TOKENIZER_PATH does not."
  (loop for key in aitools.data:*redaction-secret-key-names*
        thereis (and (plusp (length key))
                     (loop with start = 0
                           for found = (search key name :start2 start :test #'char-equal)
                           while found
                           thereis (%word-boundary-match-p name found (length key))
                           do (setf start (1+ found))))))
