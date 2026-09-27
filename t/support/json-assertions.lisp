;;;; t/support/json-assertions.lisp
;;;;
;;;; Small helpers for asserting on a json-kit value tree in tests, without
;;;; every test file re-deriving "walk a JSON-OBJECT's members" by hand.
(in-package #:aitools.test.support)

(defun json-alist (value)
  "VALUE's members as a fresh alist of (STRING . VALUE), whether VALUE is a
json-kit JSON-OBJECT or an ordinary HASH-TABLE."
  (etypecase value
    ((satisfies json-kit:json-object-p) (json-kit:json-object-members value))
    (hash-table (let (pairs) (maphash (lambda (k v) (push (cons k v) pairs)) value) pairs))))

(defun json-alist-value (value key &optional default)
  "The value under string KEY in VALUE's members, or DEFAULT if absent."
  (let ((pair (assoc key (json-alist value) :test #'string=)))
    (if pair (cdr pair) default)))

(defun string-bytes (string)
  "STRING as a UTF-8 octet vector, for feeding a byte-oriented function
(e.g. AITOOLS.KERNEL.DOMAIN:SHA256-HEX) a literal test string."
  (sb-ext:string-to-octets string :external-format :utf-8))
