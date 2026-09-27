;;;; packages/feature/process/src/domain/terminal-text.lisp
;;;;
;;;; `run` and `bg logs` drop ANSI escape sequences and keep
;;;; only the last redraw of a `\r`-overwritten progress line, unless
;;;; `--no-strip-ansi` is given. Both rules work on one already-split line.
(in-package #:aitools.process.domain)

(defconstant +escape-code+ 27)
(defconstant +c1-csi-code+ #x9b)

(declaim (inline %code-in-p))
(defun %code-in-p (char low high)
  (<= low (char-code char) high))

(defun %skip-csi (line index)
  "INDEX is just past a CSI introducer. Return the index past the sequence:
parameter bytes 0x30-0x3F, intermediate bytes 0x20-0x2F, one final byte
0x40-0x7E (ECMA-48 5.4). An unterminated sequence consumes the line."
  (let ((length (length line)))
    (loop while (and (< index length) (%code-in-p (char line index) #x30 #x3f)) do (incf index))
    (loop while (and (< index length) (%code-in-p (char line index) #x20 #x2f)) do (incf index))
    (if (and (< index length) (%code-in-p (char line index) #x40 #x7e))
        (1+ index)
        length)))

(defun %skip-string-sequence (line index)
  "INDEX is just past an OSC/DCS/SOS/PM/APC introducer. Return the index past
its terminator: BEL, or ST (`ESC \\`). An unterminated string consumes the
line."
  (let ((length (length line)))
    (loop while (< index length)
          do (let ((code (char-code (char line index))))
               (cond ((= code 7) (return-from %skip-string-sequence (1+ index)))
                     ((and (= code +escape-code+) (< (1+ index) length)
                           (char= (char line (1+ index)) #\\))
                      (return-from %skip-string-sequence (+ index 2)))
                     (t (incf index)))))
    length))

(defun %skip-escape (line index)
  "INDEX is at an ESC. Return the index past the whole escape sequence."
  (let ((length (length line)) (next (1+ index)))
    (if (>= next length)
        length
        (let ((char (char line next)))
          (cond ((char= char #\[) (%skip-csi line (1+ next)))
                ((find char "]PX^_") (%skip-string-sequence line (1+ next)))
                (t
                 ;; nF/Fp/Fe/Fs: intermediates 0x20-0x2F, then one final byte.
                 (loop while (and (< next length) (%code-in-p (char line next) #x20 #x2f))
                       do (incf next))
                 (min length (1+ next))))))))

(defun strip-ansi-escapes (line)
  "LINE without ECMA-48 escape sequences (7-bit ESC forms and the 8-bit C1
CSI)."
  (if (not (find-if (lambda (char) (member (char-code char) (list +escape-code+ +c1-csi-code+))) line))
      line
      (with-output-to-string (out)
        (let ((index 0) (length (length line)))
          (loop while (< index length)
                do (let ((code (char-code (char line index))))
                     (cond ((= code +escape-code+) (setf index (%skip-escape line index)))
                           ((= code +c1-csi-code+) (setf index (%skip-csi line (1+ index))))
                           (t (write-char (char line index) out) (incf index)))))))))

(defun collapse-carriage-returns (line)
  "The text a terminal would last have drawn on LINE: trailing CRs (from a
CRLF line end or a final redraw) are dropped, then everything up to the last
remaining CR is discarded."
  (let* ((trimmed-end (1+ (or (position-if-not (lambda (char) (char= char #\Return)) line
                                                :from-end t)
                               -1)))
         (last-cr (position #\Return line :from-end t :end trimmed-end)))
    (cond ((and (null last-cr) (= trimmed-end (length line))) line)
          ((null last-cr) (subseq line 0 trimmed-end))
          (t (subseq line (1+ last-cr) trimmed-end)))))

(defun normalize-terminal-line (line strip-p)
  "LINE as `run`/`bg logs` report it: escape sequences removed and CR
redraws collapsed when STRIP-P, else LINE unchanged."
  (if strip-p
      (collapse-carriage-returns (strip-ansi-escapes line))
      line))

(defun split-output-lines (text)
  "TEXT split on LF. A final LF ends the last line rather than starting an
empty one, so \"a\\nb\\n\" and \"a\\nb\" both have two lines and \"\" has none."
  (let ((lines '()) (start 0) (length (length text)))
    (loop for newline = (position #\Newline text :start start)
          while newline
          do (push (subseq text start newline) lines)
             (setf start (1+ newline)))
    (when (< start length)
      (push (subseq text start) lines))
    (nreverse lines)))

(defun decode-output-octets (octets &key (start 0) (end (length octets)))
  "OCTETS[START,END) as UTF-8, with every malformed sequence replaced by
U+FFFD (the lenient UTF-8 reading rule): a child's output is reported, never refused."
  (values (decode-utf8 octets :start start :end end)))
