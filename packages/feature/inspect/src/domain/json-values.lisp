;;;; packages/feature/inspect/src/domain/json-values.lisp
;;;;
;;;; The JSON value vocabulary of the inspect context. Output values and
;;;; parsed documents share one representation, the one json-kit writes:
;;;; ordered JSON-OBJECTs, vectors for arrays, strings, numbers, T for true,
;;;; and json-kit's opaque false and null. The application layer may not
;;;; name json-kit (docs/src/reference/architecture.md), so these wrappers are its only way to
;;;; build or inspect such values.
(in-package #:aitools.inspect.domain)

(defun json-false () json-kit:+json-false+)

(defun json-bool (value)
  (if value t json-kit:+json-false+))

(defun json-null-value-p (value)
  (eq value json-kit:+json-null+))

(defun json-false-value-p (value)
  (eq value json-kit:+json-false+))

(defun json-object-from-pairs (pairs)
  "An ordered JSON object from an alist of (STRING . VALUE)."
  (json-kit:make-json-object pairs))

(defun json-object-value-p (value)
  (json-kit:json-object-p value))

(defun json-object-pairs (object)
  "OBJECT's members as a fresh alist, in document order."
  (json-kit:json-object-members object))

(defun json-array-value-p (value)
  (and (vectorp value) (not (stringp value))))

(defun json-object-get (object key)
  "(VALUES value present-p) of KEY in the ordered OBJECT. With duplicate
keys, the last one wins, as JSON parsers commonly resolve them."
  (let ((pair (assoc key (reverse (json-object-pairs object)) :test #'string=)))
    (values (cdr pair) (and pair t))))

(defun render-json (value)
  "VALUE as compact JSON text."
  (json-kit:stringify value))

(defun parse-json-document/k (text &key on-value on-error)
  "Parse TEXT as one JSON document and call exactly one continuation:
ON-VALUE (value) or ON-ERROR (message line column), LINE and COLUMN 1-based.
Objects keep their key order; a repeated key keeps its last value."
  (declare (type function on-value on-error))
  (let ((value (handler-case
                   (json-kit:parse text :object-type :alist :array-type :vector
                                        :duplicate-key-policy :last
                                        :object-hook #'json-kit:make-json-object)
                 (json-kit:json-parse-error (condition)
                   (return-from parse-json-document/k
                     (funcall on-error
                              (format nil "expected ~A"
                                      (or (json-kit:json-parse-error-expected condition) "valid JSON"))
                              (json-kit:json-parse-error-line condition)
                              (json-kit:json-parse-error-column condition)))))))
    (funcall on-value value)))

(defun json-type-name (value)
  (cond ((json-object-value-p value) "object")
        ((json-array-value-p value) "array")
        ((stringp value) "string")
        ((numberp value) "number")
        ((or (eq value t) (json-false-value-p value)) "boolean")
        ((json-null-value-p value) "null")
        (t (error "not a JSON value: ~S" value))))

(defun %classify-json (value)
  "Map a json-kit VALUE to the (VALUES KIND PAYLOAD) that
AITOOLS.KERNEL.DOMAIN:JSON-EQUAL reads, the one canonical equality."
  (cond ((json-object-value-p value) (values :object (json-object-pairs value)))
        ((json-array-value-p value) (values :array value))
        ((stringp value) (values :string value))
        ((numberp value) (values :number value))
        ((eq value t) (values :true t))
        ((json-false-value-p value) (values :false nil))
        ((json-null-value-p value) (values :null nil))
        (t (values :unknown value))))

(defun json-equal (a b)
  "Structural JSON equality: object key order is ignored, numbers compare
by value (1 equals 1.0). The rule lives in AITOOLS.KERNEL.DOMAIN:JSON-EQUAL
(RFC 8259 numeric-by-value); this reaches json-kit values through
%CLASSIFY-JSON."
  (aitools.kernel.domain:json-equal a b #'%classify-json))
