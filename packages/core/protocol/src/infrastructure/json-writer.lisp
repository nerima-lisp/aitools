;;;; packages/core/protocol/src/infrastructure/json-writer.lisp
;;;;
;;;; Streaming output: write directly to a buffered stream, never assemble the whole
;;;; output as a string first. This is why WRITE-ENVELOPE takes an actual
;;;; STREAM rather than going through cl-boundary-kit's string-oriented
;;;; console port (CONSOLE-WRITE takes a whole string, which is exactly the
;;;; "build it, then print it" shape this rules out) -- see
;;;; docs/src/reference/architecture.md (Placement decisions) for why the
;;;; infrastructure layer may call json-kit here.
;;;;
;;;; STREAM defaults to *STANDARD-OUTPUT*/*ERROR-OUTPUT* so a caller never has
;;;; to pass one for the ordinary case, and a test can bind
;;;; *STANDARD-OUTPUT*/*ERROR-OUTPUT* to a string stream instead of injecting
;;;; a port -- there is no meaningful "fake" for a byte-identical JSON
;;;; serializer to inject other than a real stream.
;;;;
;;;; Streaming must not leave a partial envelope behind when the writer
;;;; refuses a value halfway: each envelope is serialized twice, first into a
;;;; sink that keeps nothing (no string is assembled), then into STREAM.
(in-package #:aitools.protocol.infrastructure)

(defun %with-redaction-count (envelope count)
  "The `redactions` count: appended to the top-level object only when COUNT is
positive, so an output with nothing masked spends no tokens on it. A flow
that masked text itself (`run`, `bg logs`, `sys env`) already put its own
count there; COUNT is added to that member instead of writing a second one."
  (if (and (plusp count) (aitools.protocol.domain:json-object-p envelope))
      (let* ((members (aitools.protocol.domain:json-object-members envelope))
             (existing (assoc "redactions" members :test #'equal)))
        (aitools.protocol.domain:json-object-from-alist
         (if (and existing (integerp (cdr existing)))
             (mapcar (lambda (member)
                       (if (eq member existing) (cons "redactions" (+ (cdr existing) count)) member))
                     members)
             (append members (list (cons "redactions" count))))))
      envelope))

(defconstant +envelope-max-length+ 16777216
  "The most characters one envelope may serialize to: json-kit's default
MAX-OUTPUT-LENGTH, passed explicitly so the check and the write agree.")

(define-condition envelope-too-large (error)
  ((limit :initarg :limit :reader envelope-too-large-limit))
  (:report (lambda (condition stream)
             (format stream "the output would exceed the ~D-character JSON output limit"
                     (envelope-too-large-limit condition)))))

;;; json-kit writes only to a stream whose element type is CHARACTER. An
;;; empty broadcast stream discards everything but reports T; paired with a
;;; string input stream in a two-way stream it reports CHARACTER, and its
;;; output still goes to the broadcast stream.
(defun %discarding-character-stream ()
  (make-two-way-stream (make-string-input-stream "") (make-broadcast-stream)))

(defun %check-serializable (value)
  "Serialize VALUE into a sink that keeps nothing, so a value the writer
refuses fails here, before a byte reaches the real stream: signals
ENVELOPE-TOO-LARGE when only the output limit stands in the way, and lets
any other serialization error through."
  (let ((sink (%discarding-character-stream)))
    (handler-case (json-kit:write-json value sink :max-output-length +envelope-max-length+)
      (json-kit:json-serialization-error ()
        ;; A second pass without the limit tells the limit apart from every
        ;; other refusal without parsing the kit's message.
        (json-kit:write-json value sink :max-output-length nil)
        (error 'envelope-too-large :limit +envelope-max-length+)))))

(defun write-envelope (value stream)
  "Redact VALUE (a json-kit value tree, from AITOOLS.PROTOCOL.DOMAIN's
envelope constructors) via AITOOLS.PROTOCOL.APPLICATION:REDACT-JSON-VALUE,
write it as one line of JSON to STREAM with a top-level `redactions` count
when anything was masked, and flush STREAM. Returns the total redaction
count. The envelope is serialized once into a discarding sink first, so an
envelope past +ENVELOPE-MAX-LENGTH+ signals ENVELOPE-TOO-LARGE with nothing
written: STREAM never receives a partial envelope."
  (multiple-value-bind (redacted count) (aitools.protocol.application:redact-json-value value)
    (let ((envelope (%with-redaction-count redacted count)))
      (%check-serializable envelope)
      (json-kit:write-json envelope stream :max-output-length +envelope-max-length+))
    (terpri stream)
    (finish-output stream)
    count))
