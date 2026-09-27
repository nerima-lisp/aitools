;;;; packages/feature/inspect/src/domain/read-render.lisp
;;;;
;;;; The alternative views `read` gives of a file: `--escape-invisible` (the `cat -A`
;;;; replacement), `--as hex` rows, and `--as strings` runs.
(in-package #:aitools.inspect.domain)

(defun invisible-char-p (char)
  "Characters `--escape-invisible` spells out: C0 and C1 controls (tab
included, as `cat -A` shows it), DEL, NBSP, soft hyphen, zero-width and
bidirectional format characters, the BOM, and the ideographic space."
  (let ((code (char-code char)))
    (or (< code #x20) (<= #x7F code #x9F)
        (= code #xA0) (= code #xAD)
        (<= #x200B code #x200F) (<= #x202A code #x202E) (<= #x2060 code #x2064)
        (<= #x2066 code #x2069) (= code #xFEFF) (= code #x3000))))

(defun escape-invisible (line)
  "LINE with each invisible character written as `\\u{XXXX}`. A CR kept at
the end of LINE (a CRLF terminator) is shown the same way."
  (if (notany #'invisible-char-p line)
      line
      (with-output-to-string (out)
        (loop for char across line
              do (if (invisible-char-p char)
                     (format out "\\u{~4,'0X}" (char-code char))
                     (write-char char out))))))

(defconstant +hex-row-bytes+ 16)

(defconstant +hex-mask-byte+ #x2A
  "The byte (`*`) a secret's bytes are shown as in `read --as hex` rows.")

(defun %latin1-line (octets start end)
  "OCTETS[START,END) as a string, one char per byte (latin-1), so char
offsets are byte offsets and the ASCII-anchored secret patterns match
the bytes exactly."
  (let ((string (make-string (- end start))))
    (loop for index from start below end
          for out from 0
          do (setf (char string out) (code-char (aref octets index))))
    string))

(defun %line-widen (octets start end)
  "(VALUES wide-start wide-end): [START,END) grown to whole lines of OCTETS,
so a secret straddling the window edge is still seen whole."
  (let ((wide-start start) (wide-end end) (length (length octets)))
    (loop while (and (plusp wide-start) (/= (aref octets (1- wide-start)) 10)) do (decf wide-start))
    (loop while (and (< wide-end length) (/= (aref octets wide-end) 10)) do (incf wide-end))
    (values wide-start wide-end)))

(defun %redacted-span (original redacted)
  "(VALUES start end) of the region of ORIGINAL that REDACTED changed (the
common prefix and suffix bound it), or NIL when they are equal. Needs no
knowledge of the mask marker; several secrets on one line collapse to one
span, which over-masks the literal text between them (safe for a hex dump)."
  (unless (string= original redacted)
    (let* ((olen (length original))
           (rlen (length redacted))
           (prefix (loop for index from 0 below (min olen rlen)
                         while (char= (char original index) (char redacted index))
                         finally (return index)))
           (suffix (loop for index from 0
                         while (and (< (+ prefix index) olen) (< (+ prefix index) rlen)
                                    (char= (char original (- olen 1 index)) (char redacted (- rlen 1 index))))
                         finally (return index))))
      (values prefix (- olen suffix)))))

(defun redact-hex-octets (octets start end)
  "A copy of OCTETS with the bytes of any redactable secret overlapping the lines
of [START,END) overwritten with +HEX-MASK-BYTE+, or OCTETS unchanged when
there is none. `read --as hex` shows raw bytes the per-string envelope
redaction never sees, so it masks them here; lines are redacted as one
sequence so a PEM key spanning them is caught."
  (multiple-value-bind (wide-start wide-end) (%line-widen octets start end)
    (let ((ranges '()) (pos wide-start))
      (loop while (< pos wide-end)
            do (let ((newline (position 10 octets :start pos :end wide-end)))
                 (push (cons pos (or newline wide-end)) ranges)
                 (setf pos (if newline (1+ newline) wide-end))))
      (setf ranges (nreverse ranges))
      (let* ((strings (mapcar (lambda (range) (%latin1-line octets (car range) (cdr range))) ranges))
             (redacted (aitools.protocol.domain:redact-secret-sequence strings))
             (copy nil))
        (loop for (line-start . nil) in ranges
              for original in strings
              for masked in redacted
              do (multiple-value-bind (span-start span-end) (%redacted-span original masked)
                   (when span-start
                     (unless copy (setf copy (copy-seq octets)))
                     (loop for index from (+ line-start span-start) below (+ line-start span-end)
                           do (setf (aref copy index) +hex-mask-byte+)))))
        (or copy octets)))))

(defun hex-rows (octets start end &key (base 0))
  "OCTETS[START,END) as {offset,hex} objects of 16 bytes each, HEX being
space-separated lowercase byte pairs. OCTETS[0] is byte BASE of the file, so
each offset is BASE plus the row's index."
  (declare (type octet-vector octets))
  (loop for row-start from start below end by +hex-row-bytes+
        collect (json-object "offset" (+ base row-start)
                             "hex" (with-output-to-string (out)
                                     (loop for index from row-start below (min end (+ row-start +hex-row-bytes+))
                                           for first = t then nil
                                           do (unless first (write-char #\Space out))
                                              (format out "~(~2,'0X~)" (aref octets index)))))))

(defun %utf8-sequence (octets index end)
  "(VALUES char length) for a well-formed UTF-8 sequence at INDEX, or NIL."
  (let* ((lead (aref octets index))
         (length (cond ((< lead #x80) 1) ((<= #xC2 lead #xDF) 2) ((<= #xE0 lead #xEF) 3)
                       ((<= #xF0 lead #xF4) 4) (t 0))))
    (when (and (plusp length) (<= (+ index length) end)
               (loop for offset from 1 below length
                     always (= (logand (aref octets (+ index offset)) #xC0) #x80)))
      (let ((code (if (= length 1)
                      lead
                      (let ((value (logand lead (ash #xFF (- (1+ length))))))
                        (loop for offset from 1 below length
                              do (setf value (logior (ash value 6) (logand (aref octets (+ index offset)) #x3F))))
                        value))))
        (when (and (< code char-code-limit)
                   (not (<= #xD800 code #xDFFF))
                   (> code (case length (2 #x7F) (3 #x7FF) (4 #xFFFF) (t -1))))
          (values (code-char code) length))))))

(defun %printable-for-strings-p (char)
  (or (char= char #\Tab)
      (and (not (invisible-char-p char)) (graphic-char-p char))))

(defstruct (strings-scan (:constructor make-strings-scan (&key (min-length 4) max-text-length emit))
                         (:copier nil))
  "An `--as strings` walk carried across successive chunks of one file. EMIT
is called with (offset text cut-offset) for each run of at least MIN-LENGTH
printable characters; a run longer than MAX-TEXT-LENGTH characters keeps only
its first MAX-TEXT-LENGTH, and CUT-OFFSET is then the byte offset where the
kept text stops (else NIL). EMIT returning :STOP ends the walk."
  (min-length 4 :read-only t)
  (max-text-length nil :read-only t)
  (emit nil :type function :read-only t)
  (run-start nil)
  (run (make-string-output-stream) :read-only t)
  (run-length 0)
  (cut-offset nil))

(defun scan-strings-chunk (scan octets base &key (start 0) end final)
  "Continue SCAN over OCTETS[START,END), OCTETS[0] being byte BASE of the
file. Returns the index of the first byte not consumed: END when FINAL, which
also ends the last run; otherwise up to three trailing bytes that may begin a
character the next chunk completes are left for the caller to pass again at
the head of the next chunk. Returns NIL once EMIT has returned :STOP."
  (declare (type octet-vector octets))
  (let* ((end (or end (length octets)))
         (limit (if final end (max start (- end 3))))
         (index start)
         (max-text-length (strings-scan-max-text-length scan)))
    (flet ((flush ()
             (let ((text (get-output-stream-string (strings-scan-run scan)))
                   (run-start (strings-scan-run-start scan)))
               (when (and run-start (>= (strings-scan-run-length scan) (strings-scan-min-length scan)))
                 (when (eq (funcall (strings-scan-emit scan) run-start text (strings-scan-cut-offset scan)) :stop)
                   (return-from scan-strings-chunk nil))))
             (setf (strings-scan-run-start scan) nil
                   (strings-scan-run-length scan) 0
                   (strings-scan-cut-offset scan) nil)))
      (loop while (< index limit)
            do (multiple-value-bind (char length) (%utf8-sequence octets index end)
                 (if (and char (%printable-for-strings-p char))
                     (progn (unless (strings-scan-run-start scan)
                              (setf (strings-scan-run-start scan) (+ base index)))
                            (cond ((or (null max-text-length) (< (strings-scan-run-length scan) max-text-length))
                                   (write-char char (strings-scan-run scan)))
                                  ((null (strings-scan-cut-offset scan))
                                   (setf (strings-scan-cut-offset scan) (+ base index))))
                            (incf (strings-scan-run-length scan))
                            (incf index length))
                     (progn (flush) (incf index)))))
      (when final (flush))
      index)))

(defun extract-strings (octets &key (min-length 4) (start 0) end emit)
  "Call EMIT (offset text) for each run of at least MIN-LENGTH printable
characters (ASCII or well-formed UTF-8) in OCTETS[START,END), the `strings`
replacement. EMIT returning :STOP ends the walk."
  (declare (type octet-vector octets) (type function emit))
  (scan-strings-chunk (make-strings-scan :min-length min-length
                                         :emit (lambda (offset text cut-offset)
                                                 (declare (ignore cut-offset))
                                                 (funcall emit offset text)))
                      octets 0 :start start :end end :final t)
  nil)
