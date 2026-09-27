;;;; packages/core/text/src/domain/charset.lisp
;;;;
;;;; `transcode` and `--encoding`: the six supported encodings.
;;;; UTF-8, UTF-16LE/BE, and ISO-8859-1 go through cl-codec-kit; Shift_JIS
;;;; (as CP932) and EUC-JP use the glibc-derived mapping tables in
;;;; data/domain/text/, built into hash tables at load time so the image
;;;; carries them ready and startup stays fast.
;;;;
;;;; Legacy multibyte decoding recovers WHATWG-style: an invalid sequence
;;;; becomes one U+FFFD, and an ASCII byte that failed as a trail byte is
;;;; re-read as itself rather than swallowed.
(in-package #:aitools.text.domain)

(defparameter *supported-encodings*
  '(:utf-8 :shift_jis :euc-jp :iso-8859-1 :utf-16le :utf-16be)
  "The supported encodings, in the order `schema` lists them.")

(defparameter *encoding-aliases*
  '(("utf-8" . :utf-8) ("utf8" . :utf-8)
    ("shift_jis" . :shift_jis) ("shift-jis" . :shift_jis) ("sjis" . :shift_jis)
    ("cp932" . :shift_jis) ("windows-31j" . :shift_jis) ("ms932" . :shift_jis)
    ("euc-jp" . :euc-jp) ("eucjp" . :euc-jp) ("euc_jp" . :euc-jp)
    ("iso-8859-1" . :iso-8859-1) ("iso8859-1" . :iso-8859-1) ("latin1" . :iso-8859-1)
    ("latin-1" . :iso-8859-1)
    ("utf-16le" . :utf-16le) ("utf16le" . :utf-16le)
    ("utf-16be" . :utf-16be) ("utf16be" . :utf-16be)))

(defun find-encoding (name)
  "The encoding keyword for NAME (case-insensitive, common aliases
accepted), or NIL when unsupported."
  (cdr (assoc name *encoding-aliases* :test #'string-equal)))

(defun encoding-name (encoding)
  "The canonical spelling `transcode` and `info` use for ENCODING."
  (ecase encoding
    (:utf-8 "utf-8") (:shift_jis "shift_jis") (:euc-jp "euc-jp")
    (:iso-8859-1 "iso-8859-1") (:utf-16le "utf-16le") (:utf-16be "utf-16be")))

(defstruct (%charmap (:constructor %make-charmap (decode encode)) (:copier nil))
  (decode nil :type hash-table :read-only t)
  (encode nil :type hash-table :read-only t))

(defun %build-charmap (triples)
  (let ((decode (make-hash-table :test 'eql :size (floor (length triples) 3)))
        (encode (make-hash-table :test 'eql :size (floor (length triples) 3))))
    (loop for i from 0 below (length triples) by 3
          do (let ((code (aref triples i))
                   (char (code-char (aref triples (+ i 1)))))
               (setf (gethash code decode) char)
               (when (= 1 (aref triples (+ i 2)))
                 (setf (gethash char encode) code))))
    (%make-charmap decode encode)))

(defparameter *cp932-charmap* (%build-charmap aitools.data:*cp932-mapping*))
(defparameter *euc-jp-charmap* (%build-charmap aitools.data:*euc-jp-mapping*))

(defun %sjis-sequence-length (lead)
  (if (or (<= #x81 lead #x9F) (<= #xE0 lead #xFC)) 2 1))

(defun %euc-sequence-length (lead)
  (cond ((= lead #x8F) 3)
        ((or (= lead #x8E) (<= #xA1 lead #xFE)) 2)
        (t 1)))

(defun %decode-legacy (octets start end charmap sequence-length replace on-invalid)
  "Decode with CHARMAP. Returns (VALUES STRING REPLACEMENTS); on the first
invalid sequence when REPLACE is false, returns the value of ON-INVALID
called with its byte offset instead."
  (declare (type octets octets) (type fixnum start end) (type function sequence-length))
  (let ((table (%charmap-decode charmap))
        (out (make-string-output-stream))
        (replacements 0)
        (i start))
    (declare (type fixnum i replacements))
    (loop while (< i end)
          do (let* ((lead (aref octets i))
                    (length (funcall sequence-length lead))
                    (char (and (<= (+ i length) end)
                               (gethash (loop with code = 0
                                              for k from i below (+ i length)
                                              do (setf code (+ (* code 256) (aref octets k)))
                                              finally (return code))
                                        table))))
               (cond
                 (char (write-char char out) (incf i length))
                 ((not replace) (return-from %decode-legacy (funcall on-invalid i)))
                 (t
                  (write-char (code-char #xFFFD) out)
                  (incf replacements)
                  ;; Swallow the lead plus following non-ASCII bytes of the
                  ;; attempted sequence; an ASCII byte is re-read as itself.
                  (incf i)
                  (loop repeat (1- length)
                        while (and (< i end) (>= (aref octets i) #x80))
                        do (incf i))))))
    (values (get-output-stream-string out) replacements)))

(defun %decode-codec-kit (octets start end encoding replace on-invalid)
  (if replace
      (let ((string (cl-codec-kit:octets-to-string octets :start start :end end
                                                          :encoding encoding :errorp nil)))
        (values string
                (if (eq encoding :iso-8859-1)
                    0
                    (count (code-char #xFFFD) string))))
      (handler-case (values (cl-codec-kit:octets-to-string octets :start start :end end
                                                                  :encoding encoding)
                            0)
        (cl-codec-kit:decode-error (condition)
          (return-from %decode-codec-kit
            (funcall on-invalid (cl-codec-kit:decode-error-position condition)))))))

(defun decode-octets/k (octets encoding &key (start 0) end replace on-decoded on-invalid)
  "Decode OCTETS[START,END) from ENCODING (a keyword of
*SUPPORTED-ENCODINGS*) and call exactly one continuation.

With REPLACE false, the first invalid sequence calls ON-INVALID with its byte
offset. With REPLACE true, each invalid sequence becomes U+FFFD and
ON-DECODED receives (STRING REPLACEMENTS). For :UTF-8 use DECODE-UTF8 when
genuine U+FFFD must not be counted; here REPLACEMENTS counts every U+FFFD
in the result for the Unicode encodings."
  (declare (type octets octets) (type function on-decoded))
  (let ((end (or end (length octets)))
        (on-invalid (or on-invalid (lambda (position) (declare (ignore position)) nil))))
    (multiple-value-bind (string replacements)
        (flet ((invalid (position) (return-from decode-octets/k (funcall on-invalid position))))
          (declare (dynamic-extent #'invalid))
          (ecase encoding
            (:shift_jis (%decode-legacy octets start end *cp932-charmap* #'%sjis-sequence-length
                                        replace #'invalid))
            (:euc-jp (%decode-legacy octets start end *euc-jp-charmap* #'%euc-sequence-length
                                     replace #'invalid))
            ((:utf-8 :utf-16le :utf-16be :iso-8859-1)
             (%decode-codec-kit octets start end encoding replace #'invalid))))
      (funcall on-decoded string replacements))))

(defun %encodable-by-codec-kit-p (char encoding)
  (let ((code (char-code char)))
    (case encoding
      (:iso-8859-1 (< code 256))
      (t (not (<= #xD800 code #xDFFF))))))

(defun encode-string/k (string encoding &key replace-unmappable on-encoded on-unmappable)
  "Encode STRING to ENCODING and call exactly one continuation.

A character ENCODING cannot represent calls ON-UNMAPPABLE with (INDEX CHAR),
unless REPLACE-UNMAPPABLE is true, in which case it is written as `?`.
ON-ENCODED receives (OCTETS REPLACED), REPLACED counting substituted
characters."
  (declare (type string string) (type function on-encoded))
  (let ((replaced 0)
        (on-unmappable (or on-unmappable (lambda (index char) (declare (ignore index char)) nil))))
    (flet ((unmappable (index char)
             (if replace-unmappable
                 (progn (incf replaced) nil)
                 (return-from encode-string/k (funcall on-unmappable index char)))))
      (let ((octets
              (ecase encoding
                ((:shift_jis :euc-jp)
                 (let ((table (%charmap-encode (if (eq encoding :shift_jis) *cp932-charmap* *euc-jp-charmap*)))
                       (out (make-array (length string) :element-type '(unsigned-byte 8)
                                                        :adjustable t :fill-pointer 0)))
                   (loop for char across string
                         for index from 0
                         do (let ((code (or (gethash char table)
                                            (progn (unmappable index char) (char-code #\?)))))
                              (cond ((< code #x100) (vector-push-extend code out))
                                    ((< code #x10000)
                                     (vector-push-extend (ldb (byte 8 8) code) out)
                                     (vector-push-extend (ldb (byte 8 0) code) out))
                                    (t
                                     (vector-push-extend (ldb (byte 8 16) code) out)
                                     (vector-push-extend (ldb (byte 8 8) code) out)
                                     (vector-push-extend (ldb (byte 8 0) code) out)))))
                   (coerce out 'octets)))
                ((:utf-8 :utf-16le :utf-16be :iso-8859-1)
                 (let ((index -1))
                   (flet ((checked-char (char)
                            (incf index)
                            (if (%encodable-by-codec-kit-p char encoding)
                                char
                                (progn (unmappable index char) #\?))))
                     (declare (dynamic-extent #'checked-char))
                     (coerce (cl-codec-kit:string-to-octets (map 'string #'checked-char string)
                                                            :encoding encoding)
                             'octets)))))))
        (funcall on-encoded octets replaced)))))
