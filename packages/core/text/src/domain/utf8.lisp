;;;; packages/core/text/src/domain/utf8.lisp
;;;;
;;;; The two UTF-8 policies over cl-codec-kit: reads replace each
;;;; invalid byte with U+FFFD and report how many replacements happened
;;;; (`encoding_errors`); text edits decode strictly and learn the first
;;;; invalid byte offset instead.
(in-package #:aitools.text.domain)

(defun %genuine-replacement-count (octets start end)
  "How many already-encoded U+FFFD sequences (EF BF BD) occur in
OCTETS[START,END). Lead byte EF can never be consumed as part of another
sequence, so each occurrence decodes to exactly one genuine U+FFFD."
  (declare (type octets octets) (type fixnum start end))
  (loop with count of-type fixnum = 0
        for i = (position #xEF octets :start start :end end)
          then (position #xEF octets :start (1+ i) :end end)
        while i
        do (when (and (< (+ i 2) end) (= (aref octets (1+ i)) #xBF) (= (aref octets (+ i 2)) #xBD))
             (incf count))
        finally (return count)))

(defun decode-utf8 (octets &key (start 0) end)
  "(VALUES STRING ENCODING-ERRORS): OCTETS[START,END) decoded with each
invalid or truncated sequence replaced by U+FFFD, and the number of such
replacements (genuine U+FFFD characters in the input are not counted)."
  (declare (type octets octets) (type fixnum start))
  (let* ((end (or end (length octets)))
         (string (cl-codec-kit:octets-to-string octets :start start :end end
                                                       :encoding :utf-8 :errorp nil)))
    (values string
            (if (find (code-char #xFFFD) string)
                (- (count (code-char #xFFFD) string) (%genuine-replacement-count octets start end))
                0))))

(defun decode-utf8-strict/k (octets &key (start 0) end on-decoded on-invalid)
  "Decode OCTETS[START,END) strictly and call exactly one continuation:
ON-DECODED with the string, or ON-INVALID with the byte offset (into OCTETS)
of the first invalid or truncated sequence."
  (declare (type octets octets) (type function on-decoded on-invalid))
  (let ((string (handler-case
                    (cl-codec-kit:octets-to-string octets :start start :end (or end (length octets))
                                                          :encoding :utf-8)
                  (cl-codec-kit:decode-error (condition)
                    (return-from decode-utf8-strict/k
                      (funcall on-invalid (cl-codec-kit:decode-error-position condition)))))))
    (funcall on-decoded string)))

(defun utf8-valid-p (octets &key (start 0) end)
  (flet ((decoded (string) (declare (ignore string)) t)
         (invalid (position) (declare (ignore position)) nil))
    (declare (dynamic-extent #'decoded #'invalid))
    (decode-utf8-strict/k octets :start start :end end :on-decoded #'decoded :on-invalid #'invalid)))

(defun encode-utf8 (string &key (start 0) end)
  "STRING[START,END) as UTF-8 bytes."
  (cl-codec-kit:string-to-octets string :start start :end end :encoding :utf-8))
