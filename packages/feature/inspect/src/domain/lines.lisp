;;;; packages/feature/inspect/src/domain/lines.lisp
;;;;
;;;; File bytes to line strings: the UTF-8 BOM never appears in
;;;; line 1, invalid UTF-8 becomes U+FFFD and is counted, and a CRLF
;;;; terminator's CR is dropped unless the caller needs to see it (`diff`
;;;; without `--ignore-eol`, `read --escape-invisible`). Decoding happens
;;;; once per requested region, not per line.
(in-package #:aitools.inspect.domain)

(deftype octet-vector () '(simple-array (unsigned-byte 8) (*)))

;;; SPLIT-TEXT-LINES and DECODE-TEXT-LINES are core text operations shared by
;;; the inspect and vcs contexts; they live in AITOOLS.TEXT.DOMAIN (see
;;; packages/core/text/src/domain/lines.lisp) and are imported here.

(defun %utf8-cut (octets start cut)
  "CUT moved back to the start of the UTF-8 character it would split, but
never before START and at most three bytes (an invalid run is cut as is)."
  (let ((limit (max start (- cut 3))))
    (loop while (and (> cut limit) (= (logand (aref octets cut) #xC0) #x80))
          do (decf cut))
    cut))

(defun decode-line-range (octets index first last &key keep-cr max-line-bytes)
  "(VALUES lines encoding-errors cuts) for lines FIRST..LAST (1-based,
inclusive, within INDEX's count) of OCTETS, decoding only those bytes. With
MAX-LINE-BYTES, a longer line is decoded only up to that many bytes (backed
off to a character boundary), and CUTS lists (line . offset) for each such
line, OFFSET being the byte where its text stops; the bytes past it are not
decoded."
  (declare (type octet-vector octets))
  (cond
    ((> first last) (values (vector) 0 '()))
    ((null max-line-bytes)
     (let ((start (line-index-bounds index octets first)))
       (multiple-value-bind (last-start last-end) (line-index-bounds index octets last)
         (declare (ignore last-start))
         (multiple-value-bind (string errors) (decode-utf8 octets :start start :end last-end)
           (values (split-text-lines string :keep-cr keep-cr) errors '())))))
    (t
     (let ((lines (make-array (1+ (- last first)))) (errors 0) (cuts '()))
       (loop for line from first to last
             for slot from 0
             do (multiple-value-bind (start end) (line-index-bounds index octets line)
                  (when (and keep-cr (< end (length octets)) (= (aref octets end) 13))
                    (incf end))
                  (let ((stop (if (> (- end start) max-line-bytes)
                                  (%utf8-cut octets start (+ start max-line-bytes))
                                  end)))
                    (when (< stop end) (push (cons line stop) cuts))
                    (multiple-value-bind (string line-errors) (decode-utf8 octets :start start :end stop)
                      (setf (svref lines slot) string)
                      (incf errors line-errors)))))
       (values lines errors (nreverse cuts))))))


(defun decode-charset-lines/k (octets encoding &key keep-cr on-decoded)
  "Decode OCTETS from ENCODING (a text-context encoding keyword) with
replacement and call ON-DECODED (lines replacements). A leading U+FEFF is
dropped, as the BOM is for UTF-8."
  (declare (type function on-decoded))
  (decode-octets/k octets encoding
                   :replace t
                   :on-decoded (lambda (string replacements)
                                 (let ((string (if (and (plusp (length string))
                                                        (char= (char string 0) (code-char #xFEFF)))
                                                   (subseq string 1)
                                                   string)))
                                   (funcall on-decoded (split-text-lines string :keep-cr keep-cr)
                                            replacements)))))
