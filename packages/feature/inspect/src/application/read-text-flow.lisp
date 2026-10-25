(in-package #:aitools.inspect.application)

(defconstant +max-line-bytes+
  16384
  "The per-line text limit and maximum string-run length. UTF-8 lines are
capped in bytes; lines decoded with other encodings are capped in characters
because their byte offsets are unknown. `--as strings` runs are capped in
characters. A cut gets an `--as hex` continuation only when its byte offset
is known.")

(defun %text-approx-tokens (strings)
  (approx-token-count (+ (length strings) (reduce #'+ strings :key #'length))))

(defstruct
    (text-view-options (:constructor make-text-view-options) (:copier nil) (:predicate nil))
  "The text-mode knobs shared by `read` and `archive read`. COMMAND-WORDS
rebuild the command for `next_commands` (without selectors and limits);
EXTRA-WORDS follow the selector (e.g. `--encoding X`). HEX-WORDS, given
(start end) byte offsets, returns the command words that dump those bytes
with `--as hex`, the continuation named for a cut line."
  (selector nil :read-only t)
  (tail nil :read-only t)
  (max-lines 80 :read-only t)
  (escape-invisible nil :read-only t)
  (encoding nil :read-only t)
  (path "" :read-only t)
  (command-words '() :read-only t)
  (extra-words '() :read-only t)
  (hex-words nil :read-only t))

(defun %continue-command (context options &rest words)
  (command-line context
                (append (text-view-options-command-words options)
                        words
                        (text-view-options-extra-words options))))

(defun %cut-decoded-lines (octets options numbers texts)
  "(VALUES texts cuts): the decoded lines TEXTS (line NUMBERS of OCTETS) with
every line over +MAX-LINE-BYTES+ cut as the byte path cuts it, CUTS being
(line . offset) per cut line. Under a non-UTF-8 `--encoding` the byte
offsets are unknown, so a line is cut after +MAX-LINE-BYTES+ characters and
its offset is NIL."
  (let ((encoding (text-view-options-encoding options))
        (index nil)
        (cuts '()))
    (values
     (map 'vector
          (lambda (number text)
            (cond
              ((and encoding (not (eq encoding :utf-8)))
               (if (> (length text) +max-line-bytes+)
                   (progn (push (cons number nil) cuts) (subseq text 0 +max-line-bytes+))
                   text))
              ;; A UTF-8 character is at most 4 bytes, so only a line of more
              ;; than a quarter of the cap in characters can be over it.
              ((<= (length text) (floor +max-line-bytes+ 4)) text)
              (t
               (unless index (setf index (build-line-index octets :start (utf8-bom-length octets))))
               (multiple-value-bind (lines errors line-cuts)
                   (decode-line-range octets index number number
                                      :keep-cr (text-view-options-escape-invisible options)
                                      :max-line-bytes +max-line-bytes+)
                 (declare (ignore errors))
                 (if line-cuts
                     (progn (push (first line-cuts) cuts) (svref lines 0))
                     text)))))
          numbers texts)
     (nreverse cuts))))

(aitools.kernel.domain:define-flow
 (%decode-all/k octets options &key on-decoded)
 "Decode OCTETS for whole-file selectors and call ON-DECODED with lines and encoding errors."
 (declare (type function on-decoded))
 (let ((keep-cr (text-view-options-escape-invisible options))
       (encoding (text-view-options-encoding options)))
   (if (and encoding (not (eq encoding :utf-8)))
       (aitools.kernel.domain:flow-call
        (decode-charset-lines/k octets encoding :keep-cr keep-cr)
        (:on-decoded (lines errors)
                     (aitools.kernel.domain:flow-continue :on-decoded
                                                          lines
                                                          errors)))
       (multiple-value-bind (lines layout errors)
           (decode-text-lines octets :keep-cr keep-cr)
         (declare (ignore layout))
         (aitools.kernel.domain:flow-continue :on-decoded lines errors)))))

(defun %shown-window (start end max-lines tail)
  "(VALUES first last truncated) of the lines to show from START..END."
  (if (<= (1+ (- end start)) max-lines)
      (values start end nil)
      (if tail
          (values (1+ (- end max-lines)) end t)
          (values start (+ start max-lines -1) t))))
