;;;; packages/core/text/src/domain/layout.lisp
;;;;
;;;; The three layout facts a write must preserve: a leading UTF-8 BOM,
;;;; the line ending (LF or CRLF), and whether the file ends with a newline.
;;;; All three are read from raw bytes, before any decoding.
(in-package #:aitools.text.domain)

(defun utf8-bom-length (octets)
  "3 when OCTETS starts with the UTF-8 BOM (EF BB BF), else 0. Line
indexing starts after it, so the BOM never appears in line text."
  (declare (type octets octets))
  (if (and (>= (length octets) 3)
           (= (aref octets 0) #xEF) (= (aref octets 1) #xBB) (= (aref octets 2) #xBF))
      3
      0))

(defun line-ending-style (octets &key (start 0) end)
  ":LF or :CRLF when every line feed in OCTETS[START,END) uses that style,
:MIXED when both occur, :NONE when there is no line feed. A lone CR is not
a line ending."
  (declare (type octets octets) (type fixnum start))
  (let ((end (or end (length octets))) (lf 0) (crlf 0))
    (declare (type fixnum end lf crlf))
    (loop for i = (position 10 octets :start start :end end)
            then (position 10 octets :start (1+ i) :end end)
          while i
          do (if (and (> i start) (= (aref octets (1- i)) 13)) (incf crlf) (incf lf)))
    (cond ((and (zerop lf) (zerop crlf)) :none)
          ((zerop lf) :crlf)
          ((zerop crlf) :lf)
          (t :mixed))))

(defun final-newline-p (octets &key (start 0) end)
  "True when OCTETS[START,END) is non-empty and ends with a line feed."
  (declare (type octets octets) (type fixnum start))
  (let ((end (or end (length octets))))
    (and (> end start) (= (aref octets (1- end)) 10))))

(defstruct (text-layout (:constructor %make-text-layout (bom-p line-ending final-newline-p))
                        (:copier nil))
  (bom-p nil :type boolean :read-only t)
  (line-ending :none :type (member :lf :crlf :mixed :none) :read-only t)
  (final-newline-p nil :type boolean :read-only t))

(defun detect-text-layout (octets)
  "The TEXT-LAYOUT of a whole file's OCTETS."
  (declare (type octets octets))
  (let ((start (utf8-bom-length octets)))
    (%make-text-layout (plusp start)
                       (line-ending-style octets :start start)
                       (final-newline-p octets :start start))))
