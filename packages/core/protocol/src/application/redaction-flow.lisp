;;;; packages/core/protocol/src/application/redaction-flow.lisp
;;;;
;;;; Streaming output and redaction together: json-kit's WRITE-JSON has no per-string
;;;; hook to mask a string as it is written, so
;;;; masking has to happen as a pre-pass over the value tree, strictly before
;;;; that tree reaches the streaming JSON writer -- never as a second pass
;;;; over already-written text, which would defeat the streaming rule, "write directly
;;;; to a buffered stream, never assemble the whole output as one string."
(in-package #:aitools.protocol.application)

(defvar *redaction-count* 0)

(defun redact-json-value (value)
  "Return (VALUES REDACTED-VALUE TOTAL-COUNT). REDACTED-VALUE is VALUE with
string values masked by AITOOLS.PROTOCOL.DOMAIN:REDACT-SECRET-SEQUENCE and
every object key masked by REDACT-SECRETS on its own. The string elements of
one array are one sequence, so a PEM key returned one line per element is
caught; every other string is a sequence by itself. A PEM block never
continues from one array or field into another: an unterminated block in a
search block's `lines` must not mask a later file's block or a scalar field.
TOTAL-COUNT is the number of masked regions."
  (let ((*redaction-count* 0))
    (values (%redact-value value) *redaction-count*)))

(defun %redact-sequence (strings)
  (multiple-value-bind (redacted count) (aitools.protocol.domain:redact-secret-sequence strings)
    (incf *redaction-count* count)
    redacted))

(defun %redact-elements (elements)
  "The list ELEMENTS with its strings masked as one sequence and every other
element redacted on its own."
  (let ((redacted (%redact-sequence (remove-if-not #'stringp elements))))
    (mapcar (lambda (element) (if (stringp element) (pop redacted) (%redact-value element)))
            elements)))

(defun %redact-key (key)
  (multiple-value-bind (text count) (aitools.protocol.domain:redact-secrets key)
    (incf *redaction-count* count)
    text))

(defun %redact-value (value)
  (etypecase value
    (string (first (%redact-sequence (list value))))
    ((satisfies aitools.protocol.domain:json-object-p)
     (aitools.protocol.domain:json-object-from-alist
      (mapcar (lambda (pair) (cons (%redact-key (car pair)) (%redact-value (cdr pair))))
              (aitools.protocol.domain:json-object-members value))))
    (hash-table
     (let ((copy (make-hash-table :test (hash-table-test value) :size (hash-table-count value))))
       (maphash (lambda (key entry)
                  (setf (gethash (if (stringp key) (%redact-key key) key) copy) (%redact-value entry)))
                value)
       copy))
    (cons (%redact-elements value))
    (vector (coerce (%redact-elements (coerce value 'list)) 'vector))
    (t value)))
