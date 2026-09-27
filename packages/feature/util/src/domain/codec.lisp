;;;; packages/feature/util/src/domain/codec.lisp
;;;;
;;;; `util encode`/`util decode`'s three byte codecs. Every codec works on octets in both
;;;; directions, so `--content-file` input (raw bytes) and `--content` input
;;;; (the UTF-8 encoding of the text) take the same path, and a binary file
;;;; round-trips without passing through a character decoder.
;;;;
;;;; Decoders are strict: an input byte outside the scheme's grammar is
;;;; rejected with its offset rather than skipped, except ASCII whitespace
;;;; between base64/hex digits, which wrapped output (`base64 -w 76`,
;;;; `xxd -p`) always contains. URL decoding is RFC 3986 percent-decoding
;;;; only: `+` stays `+` (form encoding is a different scheme).
(in-package #:aitools.util.domain)

(defparameter +codec-schemes+ aitools.data:*util-codec-schemes*)

(defun codec-scheme-p (name)
  (and (member name +codec-schemes+ :test #'string=) t))

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defparameter +hex-digits+ "0123456789abcdef")

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(declaim (inline %ascii-whitespace-octet-p %hex-digit-value %base64-digit-value %url-unreserved-p))

(defun %ascii-whitespace-octet-p (octet)
  (member octet '(9 10 13 32)))

(defun %hex-digit-value (octet)
  (cond ((<= 48 octet 57) (- octet 48))
        ((<= 97 octet 102) (- octet 87))
        ((<= 65 octet 70) (- octet 55))))

(defun %base64-digit-value (octet)
  (cond ((<= 65 octet 90) (- octet 65))
        ((<= 97 octet 122) (- octet 71))
        ((<= 48 octet 57) (+ octet 4))
        ((= octet 43) 62)
        ((= octet 47) 63)))

(defun %url-unreserved-p (octet)
  (or (<= 65 octet 90) (<= 97 octet 122) (<= 48 octet 57) (member octet '(45 46 95 126))))

(defun utf-8-octets (string)
  "STRING as a fresh UTF-8 octet vector."
  (cl-codec-kit:string-to-octets string :encoding :utf-8 :errorp nil))

(defun octets->utf-8/k (octets &key on-text on-invalid)
  "Decode OCTETS strictly as UTF-8. Calls ON-TEXT with the string, or
ON-INVALID with the byte offset of the first invalid sequence."
  (declare (type function on-text on-invalid))
  (let ((text (handler-case (cl-codec-kit:octets-to-string octets :encoding :utf-8)
                (cl-codec-kit:decode-error (condition)
                  (return-from octets->utf-8/k
                    (funcall on-invalid (cl-codec-kit:decode-error-position condition)))))))
    (funcall on-text text)))

(defun octets->lenient-text (octets)
  "Decode OCTETS as UTF-8, replacing each invalid sequence with U+FFFD."
  (cl-codec-kit:octets-to-string octets :encoding :utf-8 :errorp nil))

(defun octets->hex (octets)
  (let ((out (make-string (* 2 (length octets)))))
    (loop for octet across octets
          for index from 0 by 2
          do (setf (char out index) (char +hex-digits+ (ash octet -4))
                   (char out (1+ index)) (char +hex-digits+ (logand octet 15))))
    out))

(defun %encode-base64 (octets)
  (let* ((length (length octets))
         (out (make-string (* 4 (ceiling length 3)) :initial-element #\=)))
    (loop for in from 0 below length by 3
          for pos from 0 by 4
          do (let* ((remaining (- length in))
                    (group (logior (ash (aref octets in) 16)
                                   (if (> remaining 1) (ash (aref octets (+ in 1)) 8) 0)
                                   (if (> remaining 2) (aref octets (+ in 2)) 0))))
               (setf (char out pos) (char +base64-alphabet+ (ldb (byte 6 18) group))
                     (char out (+ pos 1)) (char +base64-alphabet+ (ldb (byte 6 12) group)))
               (when (> remaining 1)
                 (setf (char out (+ pos 2)) (char +base64-alphabet+ (ldb (byte 6 6) group))))
               (when (> remaining 2)
                 (setf (char out (+ pos 3)) (char +base64-alphabet+ (ldb (byte 6 0) group))))))
    out))

(defun %encode-url (octets)
  (with-output-to-string (out)
    (loop for octet across octets
          do (if (%url-unreserved-p octet)
                 (write-char (code-char octet) out)
                 (progn (write-char #\% out)
                        (write-char (char-upcase (char +hex-digits+ (ash octet -4))) out)
                        (write-char (char-upcase (char +hex-digits+ (logand octet 15))) out))))))

(defun encode-octets (scheme octets)
  "Encode OCTETS with SCHEME (\"base64\", \"url\", or \"hex\") and return
the ASCII result string."
  (cond ((string= scheme "base64") (%encode-base64 octets))
        ((string= scheme "url") (%encode-url octets))
        ((string= scheme "hex") (octets->hex octets))))

(define-condition %invalid-encoding (error)
  ((offset :initarg :offset :reader %invalid-encoding-offset)
   (reason :initarg :reason :reader %invalid-encoding-reason)))

(defun %invalid (offset reason)
  (error '%invalid-encoding :offset offset :reason reason))

(defun %collect-octets (function)
  "Call FUNCTION with an emitter of one octet; return the emitted octets."
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (funcall function (lambda (octet) (vector-push-extend octet buffer)))
    (coerce buffer 'octets)))

(defun %decode-base64 (octets)
  (%collect-octets
   (lambda (emit)
     (let ((accumulator 0) (digits 0) (padding 0))
       (loop for octet across octets
             for offset from 0
             do (cond ((%ascii-whitespace-octet-p octet))
                      ((= octet 61)
                       (when (or (< digits 2) (>= padding 2))
                         (%invalid offset "misplaced '=' padding"))
                       (incf padding))
                      (t
                       (let ((value (%base64-digit-value octet)))
                         (unless value (%invalid offset "byte is not in the base64 alphabet"))
                         (when (plusp padding) (%invalid offset "data after '=' padding"))
                         (setf accumulator (logior (ash accumulator 6) value))
                         (incf digits)
                         (when (= digits 4)
                           (funcall emit (ldb (byte 8 16) accumulator))
                           (funcall emit (ldb (byte 8 8) accumulator))
                           (funcall emit (ldb (byte 8 0) accumulator))
                           (setf accumulator 0 digits 0))))))
       ;; No arm for 0 digits: `=` needs 2 digits before it and a digit after
       ;; it is rejected, so a complete final group never carries padding.
       (case digits
         (1 (%invalid (length octets) "truncated base64 group"))
         (2 (when (= padding 1) (%invalid (length octets) "truncated base64 padding"))
          (funcall emit (ldb (byte 8 4) accumulator)))
         (3 (when (= padding 2) (%invalid (length octets) "excess base64 padding"))
          (funcall emit (ldb (byte 8 10) accumulator))
          (funcall emit (ldb (byte 8 2) accumulator))))))))

(defun %decode-hex (octets)
  (%collect-octets
   (lambda (emit)
     (let ((high nil))
       (loop for octet across octets
             for offset from 0
             do (unless (%ascii-whitespace-octet-p octet)
                  (let ((value (%hex-digit-value octet)))
                    (unless value (%invalid offset "byte is not a hex digit"))
                    (if high
                        (progn (funcall emit (logior (ash high 4) value)) (setf high nil))
                        (setf high value)))))
       (when high (%invalid (length octets) "odd number of hex digits"))))))

(defun %decode-url (octets)
  (%collect-octets
   (lambda (emit)
     (let ((length (length octets)) (index 0))
       (loop while (< index length)
             do (let ((octet (aref octets index)))
                  (if (= octet 37)
                      (let ((high (and (< (+ index 2) length) (%hex-digit-value (aref octets (+ index 1)))))
                            (low (and (< (+ index 2) length) (%hex-digit-value (aref octets (+ index 2))))))
                        (unless (and high low) (%invalid index "'%' is not followed by two hex digits"))
                        (funcall emit (logior (ash high 4) low))
                        (incf index 3))
                      (progn (funcall emit octet) (incf index)))))))))

(defun decode-text/k (scheme octets &key on-decoded on-invalid)
  "Decode the encoded OCTETS with SCHEME. Calls ON-DECODED with the decoded
octet vector, or ON-INVALID with (OFFSET REASON): the input byte offset where
decoding failed and a fixed description."
  (declare (type function on-decoded on-invalid))
  (let ((decoded (handler-case
                     (cond ((string= scheme "base64") (%decode-base64 octets))
                           ((string= scheme "url") (%decode-url octets))
                           ((string= scheme "hex") (%decode-hex octets)))
                   (%invalid-encoding (condition)
                     (return-from decode-text/k
                       (funcall on-invalid (%invalid-encoding-offset condition)
                                (%invalid-encoding-reason condition)))))))
    (funcall on-decoded decoded)))
