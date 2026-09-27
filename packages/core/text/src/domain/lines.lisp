;;;; packages/core/text/src/domain/lines.lisp
;;;;
;;;; Whole-file UTF-8 bytes to line strings: the UTF-8 BOM never
;;;; appears in line 1, invalid UTF-8 becomes U+FFFD and is counted, and a
;;;; CRLF terminator's CR is dropped unless the caller needs to see it. These
;;;; are core text operations the inspect and vcs contexts both read a file's
;;;; lines through, so they live in the text domain rather than in one
;;;; feature.
(in-package #:aitools.text.domain)

(defun split-text-lines (string &key keep-cr)
  "STRING's lines as a simple-vector: split at LF, the final LF not starting
an extra empty line; the CR before an LF dropped unless KEEP-CR."
  (declare (type string string))
  (let ((lines '()) (start 0) (length (length string)))
    (loop for newline = (position #\Newline string :start start)
          while newline
          do (let ((end (if (and (not keep-cr) (> newline start)
                                 (char= (char string (1- newline)) #\Return))
                            (1- newline)
                            newline)))
               (push (subseq string start end) lines)
               (setf start (1+ newline))))
    (when (< start length)
      (push (subseq string start) lines))
    (coerce (nreverse lines) 'simple-vector)))

(defun decode-text-lines (octets &key keep-cr)
  "(VALUES lines layout encoding-errors) for a whole UTF-8 file: LINES a
simple-vector of strings (BOM removed), LAYOUT the TEXT-LAYOUT, and
ENCODING-ERRORS the count of U+FFFD substitutions."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (multiple-value-bind (string errors) (decode-utf8 octets :start (utf8-bom-length octets))
    (values (split-text-lines string :keep-cr keep-cr) (detect-text-layout octets) errors)))
