;;;; packages/core/text/src/domain/encoding-guess.lisp
;;;;
;;;; `info`'s `encoding_guess`: one of utf-8, shift_jis, euc-jp, utf-16le,
;;;; utf-16be, or unknown. The decision uses, in order: a byte-order mark,
;;;; the NUL-byte pattern of BOM-less UTF-16, UTF-8 validity (ASCII counts as
;;;; UTF-8), then which of Shift_JIS and EUC-JP decodes the sample without
;;;; error. When both do, the one yielding more hiragana, full-width katakana
;;;; and CJK ideographs wins: EUC-JP hiragana bytes read as Shift_JIS turn
;;;; into half-width katakana, which the score does not count.
(in-package #:aitools.text.domain)

(defparameter *encoding-guess-sample* 65536
  "Bytes examined; a longer input is judged by its prefix.")

(defun %utf16-without-bom (octets start end)
  (let ((pairs (floor (- end start) 2)) (even 0) (odd 0))
    (when (>= pairs 2)
      (loop for i from start below (- end 1) by 2
            do (when (zerop (aref octets i)) (incf even))
               (when (zerop (aref octets (1+ i))) (incf odd)))
      (cond ((and (>= (* odd 10) (* pairs 3)) (< (* even 10) pairs)) :utf-16le)
            ((and (>= (* even 10) (* pairs 3)) (< (* odd 10) pairs)) :utf-16be)))))

(defun %japanese-score (string)
  (count-if (lambda (char)
              (let ((code (char-code char)))
                (or (<= #x3040 code #x30FF) (<= #x4E00 code #x9FFF))))
            string))

(defun %legacy-candidate (octets start end encoding truncated)
  "The Japanese score of decoding OCTETS[START,END) as ENCODING, or NIL
when it is invalid. When the sample was TRUNCATED, an error in the last two
bytes is a cut sequence, not evidence against the encoding."
  (flet ((decoded (string replacements)
           (declare (ignore replacements))
           (%japanese-score string))
         (invalid (position)
           (when (and truncated (>= position (- end 2)))
             (%legacy-candidate octets start position encoding nil))))
    (declare (dynamic-extent #'decoded #'invalid))
    (decode-octets/k octets encoding :start start :end end :on-decoded #'decoded :on-invalid #'invalid)))

(defun guess-encoding (octets &key (start 0) end)
  "The `info` encoding guess for OCTETS[START,END) as a keyword: :UTF-8,
:SHIFT_JIS, :EUC-JP, :UTF-16LE, :UTF-16BE, or :UNKNOWN."
  (declare (type octets octets))
  (let* ((full-end (or end (length octets)))
         (end (min full-end (+ start *encoding-guess-sample*)))
         (truncated (< end full-end)))
    (flet ((starts-with (&rest bytes)
             (and (<= (+ start (length bytes)) end)
                  (loop for byte in bytes for i from start always (= (aref octets i) byte))))
           (utf8-decoded (string) (declare (ignore string)) t)
           (utf8-invalid (position) (and truncated (>= position (- end 3)))))
      (declare (dynamic-extent #'utf8-decoded #'utf8-invalid))
      (cond
        ((starts-with #xEF #xBB #xBF) :utf-8)
        ((starts-with #xFF #xFE) :utf-16le)
        ((starts-with #xFE #xFF) :utf-16be)
        ((position 0 octets :start start :end end)
         (or (%utf16-without-bom octets start end) :unknown))
        ((decode-utf8-strict/k octets :start start :end end
                                      :on-decoded #'utf8-decoded :on-invalid #'utf8-invalid)
         :utf-8)
        (t
         (let ((sjis (%legacy-candidate octets start end :shift_jis truncated))
               (euc (%legacy-candidate octets start end :euc-jp truncated)))
           (cond ((and sjis euc) (if (> euc sjis) :euc-jp :shift_jis))
                 (sjis :shift_jis)
                 (euc :euc-jp)
                 (t :unknown))))))))
