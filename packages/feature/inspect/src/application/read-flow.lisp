;;;; packages/feature/inspect/src/application/read-flow.lisp
;;;;
;;;; `read`. Three modes (`--as text|hex|strings`); text mode takes
;;;; one selector or `--tail`, always capped by `--max-lines`, and names the
;;;; next `--range` whenever it stopped before the end of the file. Only the
;;;; lines shown are decoded unless a content selector needs them all
;;;; (so a large file costs only the lines shown). The line-and-view logic
;;;; is shared with `archive read`, whose output has the same shape.
(in-package #:aitools.inspect.application)

(defconstant +max-line-bytes+ 16384
  "The most bytes of one line `read` and `archive read` return in text mode
(characters, with `--encoding`), and the most characters of one `--as
strings` run. The rest is cut and named by a `--as hex` next command, so
one long line cannot push the envelope past the JSON writer's output limit:
80 cut lines stay far below it even when every byte is escaped.")

(defun %text-approx-tokens (strings)
  (approx-token-count (+ (length strings) (reduce #'+ strings :key #'length))))

(defstruct (text-view-options (:constructor make-text-view-options) (:copier nil))
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
  (command-line context (append (text-view-options-command-words options) words
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


(defun %decode-all/k (octets options continuation)
  "CONTINUATION (lines encoding-errors) over the whole file."
  (let ((keep-cr (text-view-options-escape-invisible options))
        (encoding (text-view-options-encoding options)))
    (if (and encoding (not (eq encoding :utf-8)))
        (decode-charset-lines/k octets encoding :keep-cr keep-cr :on-decoded continuation)
        (multiple-value-bind (lines layout errors) (decode-text-lines octets :keep-cr keep-cr)
          (declare (ignore layout))
          (funcall continuation lines errors)))))

(defun %shown-window (start end max-lines tail)
  "(VALUES first last truncated) of the lines to show from START..END."
  (if (<= (1+ (- end start)) max-lines)
      (values start end nil)
      (if tail
          (values (1+ (- end max-lines)) end t)
          (values start (+ start max-lines -1) t))))

(defun text-view/k (context octets options &key on-ok on-partial on-error)
  "Render OCTETS as `read`'s text mode under OPTIONS (see
TEXT-VIEW-OPTIONS). Calls ON-OK or ON-PARTIAL with the field alist that
follows `mode`/`path`, or ON-ERROR for a failed selection."
  (declare (type function on-ok on-partial on-error))
  (let ((selector (text-view-options-selector options))
        (max-lines (text-view-options-max-lines options))
        (escape (text-view-options-escape-invisible options))
        (path (text-view-options-path options))
        (hash (content-hash octets)))
    (labels ((present (lines) (if escape (map 'list #'escape-invisible lines) (coerce lines 'list)))
             (emit (first shown truncated total errors next &key line-numbers cuts)
               ;; A cut line makes the result partial like a cut window, and
               ;; its continuation is the `--as hex` dump of the bytes after
               ;; the cut (the first cut line's, when it has an offset).
               (let* ((texts (present shown))
                      (hex-words (text-view-options-hex-words options))
                      (offset (cdr (first cuts)))
                      (truncated (or truncated (and cuts t)))
                      (next (append next
                                    (when (and hex-words offset)
                                      (list (command-line context (funcall hex-words offset (+ offset 256)))))))
                      (fields (append (list (cons "start_line" first)
                                            (cons "lines" texts))
                                      (when line-numbers (list (cons "line_numbers" line-numbers)))
                                      (when cuts (list (cons "cut_lines" (mapcar #'car cuts))))
                                      (list (cons "total_lines" total)
                                            (cons "hash" hash)
                                            (cons "truncated" (json-bool truncated))
                                            (cons "encoding_errors" errors))
                                      (when (text-view-options-encoding options)
                                        (list (cons "encoding" (encoding-name (text-view-options-encoding options)))))
                                      (list (cons "approx_tokens" (%text-approx-tokens texts)))
                                      (when next (list (cons "next_commands" next))))))
                 (funcall (if truncated on-partial on-ok) fields)))
             (next-range (first last total tail truncated)
               (cond
                 ((and tail truncated)
                  (list (%continue-command context options "--range"
                                           (format nil "~D:~D" (max 1 (- first max-lines)) (1- first)))))
                 ((< last total)
                  (list (%continue-command context options "--range"
                                           (format nil "~D:~D" (1+ last) (min total (+ last max-lines))))))))
             (contiguous (lines total start end errors tail)
               (multiple-value-bind (first last truncated) (%shown-window start end max-lines tail)
                 (multiple-value-bind (shown cuts)
                     (%cut-decoded-lines octets options (loop for number from first to last collect number)
                                         (subseq lines (1- first) last))
                   (emit first shown truncated total errors
                         (next-range first last total tail truncated)
                         :cuts cuts))))
             (selected (lines total ranges errors)
               (if (eq (selector-kind selector) :match)
                   (let* ((numbers (mapcar #'car ranges))
                          (shown (subseq numbers 0 (min max-lines (length numbers))))
                          (truncated (< (length shown) (length numbers))))
                     (multiple-value-bind (texts cuts)
                         (%cut-decoded-lines octets options shown
                                             (map 'vector (lambda (number) (svref lines (1- number))) shown))
                     (emit (if shown (first shown) 1)
                           texts
                           truncated total errors
                           (and truncated
                                (list (command-line context
                                                    (append (text-view-options-command-words options)
                                                            (list "--match" (selector-match-pattern selector))
                                                            (when (selector-invert selector) (list "--invert"))
                                                            (list "--max-lines" (princ-to-string (length numbers)))
                                                            (text-view-options-extra-words options)))))
                           :line-numbers shown :cuts cuts)))
                   (destructuring-bind ((start . end)) ranges
                     (if (> start end)
                         (emit start (vector) nil total errors nil)
                         (contiguous lines total start end errors nil))))))
      (if (or (and selector (not (eq (selector-kind selector) :range)))
              (text-view-options-encoding options) escape)
          (%decode-all/k
           octets options
           (lambda (lines errors)
             (let ((total (length lines)))
               (cond
                 ((null selector)
                  (let ((tail (text-view-options-tail options)))
                    (if (zerop total)
                        (emit 1 (vector) nil 0 errors nil)
                        (contiguous lines total (if tail (max 1 (- total tail -1)) 1) total errors tail))))
                 (t
                  (resolve-selector/k lines selector
                                      :path path
                                      :on-selected (lambda (ranges) (selected lines total ranges errors))
                                      :on-no-match (lambda (candidates)
                                                     (selection-error on-error context :no-match candidates :path path))
                                      :on-ambiguous (lambda (candidates)
                                                      (selection-error on-error context :ambiguous candidates :path path))
                                      :on-invalid (lambda (code message)
                                                    (selection-error on-error context :invalid nil
                                                                     :path path :code code :message message))))))))
          (let* ((bom (utf8-bom-length octets))
                 (index (build-line-index octets :start bom))
                 (total (line-index-count index))
                 (tail (text-view-options-tail options)))
            (flet ((window (start end tail)
                     (multiple-value-bind (first last truncated) (%shown-window start end max-lines tail)
                       (multiple-value-bind (shown errors cuts)
                           (decode-line-range octets index first last :max-line-bytes +max-line-bytes+)
                         (emit first shown truncated total errors
                               (next-range first last total tail truncated)
                               :cuts cuts)))))
              (cond
                ((zerop total)
                 (if selector
                     (selection-error on-error context :no-match '() :path path)
                     (emit 1 (vector) nil 0 0 nil)))
                ((null selector) (window (if tail (max 1 (- total tail -1)) 1) total tail))
                (t
                 (let ((start (selector-range-start selector))
                       (end (min total (or (selector-range-end selector) total))))
                   (if (> start total)
                       (selection-error on-error context :no-match
                                        (multiple-value-bind (lines)
                                            (decode-line-range octets index total total
                                                               :max-line-bytes +max-line-bytes+)
                                          (list (json-object "line" total "text" (svref lines 0))))
                                        :path path)
                       (window start end nil)))))))))))

(defun %parse-byte-span (text size)
  "(VALUES start end) for `--bytes S:E` or `S:` (0-based, END exclusive)
clamped to SIZE, or NIL when TEXT is malformed."
  (let ((colon (position #\: text)))
    (flet ((number (string) (and (plusp (length string))
                                 (every (lambda (char) (char<= #\0 char #\9)) string)
                                 (parse-integer string))))
      (when colon
        (let ((start (number (subseq text 0 colon)))
              (end (if (= colon (1- (length text))) size (number (subseq text (1+ colon))))))
          (when (and start end (<= start end))
            (values (min start size) (min end size))))))))

(defun %hex-next-command (context target end size truncated)
  "The `--bytes` continuation after byte END, or NIL at the end of file."
  (when (< end size)
    (list (command-line context (list "read" (file-target-argument target) "--as" "hex" "--bytes"
                                      (format nil "~D:~D" end (if truncated size (min size (+ end 256)))))))))

(defconstant +hex-redaction-margin+ 4096
  "Bytes read on each side of a `--as hex` window, so the secret mask sees
the whole lines around it without reading the file. A line running past
the margin is masked as if it began or ended there.")

(defun %hex-view (context target bytes max-lines on-ok on-partial on-error)
  (multiple-value-bind (start end) (if bytes (%parse-byte-span bytes most-positive-fixnum) (values 0 256))
    (if (null start)
        (fail on-error "argument.invalid" (format nil "--bytes ~S is not S:E or S: (0-based byte offsets)" bytes)
              :repairs (list (repair "fix-bytes" "Give a byte span such as 0:256."
                                     (command-line context (list "read" (file-target-argument target)
                                                                 "--as" "hex" "--bytes" "0:256")))))
        (let ((low (max 0 (- start +hex-redaction-margin+))))
          (multiple-value-bind (octets size-or-problem)
              (read-target-range context target low
                                 (+ (min end (+ start (* 16 max-lines))) +hex-redaction-margin+))
            (if (null octets)
                (fail-target-read context target on-error size-or-problem)
                (let* ((size size-or-problem)
                       (low (min low size))
                       (start (min start size))
                       (end (min end size))
                       (shown-end (min end (+ start (* 16 max-lines))))
                       (truncated (< shown-end end))
                       (next (%hex-next-command context target shown-end size truncated))
                       (masked (redact-hex-octets octets (- start low) (- shown-end low))))
                  (funcall (if truncated on-partial on-ok)
                           (append (list (cons "mode" "hex")
                                         (cons "path" (target-display-path target))
                                         (cons "size" size)
                                         (cons "start" start)
                                         (cons "end" shown-end)
                                         (cons "rows" (hex-rows masked (- start low) (- shown-end low) :base low))
                                         (cons "truncated" (json-bool truncated))
                                         (cons "approx_tokens" (approx-token-count (* 3 (- shown-end start)))))
                                   (when next (list (cons "next_commands" next))))))))))))

(defun %strings-view (context target min-length max-lines on-ok on-partial on-error)
  (let* ((items '()) (total 0) (first-cut nil)
         (scan (make-strings-scan
                :min-length min-length :max-text-length +max-line-bytes+
                :emit (lambda (offset text cut-offset)
                        (when (< total max-lines)
                          (push (if cut-offset
                                    (json-object "offset" offset "text" text "cut" t)
                                    (json-object "offset" offset "text" text))
                                items)
                          (when (and cut-offset (null first-cut)) (setf first-cut cut-offset)))
                        (incf total)
                        nil)))
         (carry nil)
         (position 0))
    ;; A chunk's last bytes may begin a character the next chunk completes:
    ;; they are carried over and scanned again at the head of the next one.
    (multiple-value-bind (size problem)
        (map-target-chunks context target
                           (lambda (chunk)
                             (let* ((octets (if carry (concatenate '(simple-array (unsigned-byte 8) (*)) carry chunk) chunk))
                                    (base (- position (length carry)))
                                    (consumed (scan-strings-chunk scan octets base)))
                               (setf carry (and (< consumed (length octets)) (subseq octets consumed))
                                     position (+ position (length chunk))))))
      (if (null size)
          (fail-target-read context target on-error problem)
          (progn
            (scan-strings-chunk scan (or carry (make-array 0 :element-type '(unsigned-byte 8)))
                                (- position (length carry)) :final t)
            (let* ((truncated (or (> total max-lines) first-cut))
                   (items (nreverse items))
                   (texts (mapcar (lambda (item) (json-object-get item "text")) items))
                   (next (append (when (> total max-lines)
                                   (list (command-line context (list "read" (file-target-argument target) "--as" "strings"
                                                                     "--min-length" (princ-to-string min-length)
                                                                     "--max-lines" (princ-to-string total)))))
                                 (when first-cut
                                   (list (command-line context (list "read" (file-target-argument target) "--as" "hex"
                                                                     "--bytes" (format nil "~D:~D" first-cut
                                                                                       (min size (+ first-cut 256))))))))))
              (funcall (if truncated on-partial on-ok)
                       (append (list (cons "mode" "strings")
                                     (cons "path" (target-display-path target))
                                     (cons "size" size)
                                     (cons "strings" items)
                                     (cons "total" total)
                                     (cons "truncated" (json-bool truncated))
                                     (cons "approx_tokens" (%text-approx-tokens texts)))
                               (when next (list (cons "next_commands" next)))))))))))


(defun %binary-fields (target prefix size)
  (list (cons "mode" "text")
        (cons "path" (target-display-path target))
        (cons "binary" t)
        (cons "size" size)
        (cons "mime" (guess-mime prefix :path (file-target-absolute target)))))

(defun read-flow (ports path &key root tx lock-timeout range symbol kind between exclusive match invert
                                  tail (as "text") bytes (min-length 4) escape-invisible encoding (max-lines 80)
                                  on-ok on-partial on-error)
  "`read`. Calls exactly one of ON-OK, ON-PARTIAL (fields) or ON-ERROR."
  (declare (type function on-ok on-partial on-error))
  (let ((mode (cond ((string= as "hex") :hex) ((string= as "strings") :strings) (t :text)))
        (charset (and encoding (find-encoding encoding))))
    (flet ((invalid (message)
             (fail on-error "argument.invalid" message
                   :repairs (list (repair "read-file" "Read the file with the defaults."
                                          (format nil "aitools read ~A" (shell-quote path))))))
           (continue-with (selector)
             (%read-with-selector ports path selector mode
                                  :root root :tx tx :lock-timeout lock-timeout :tail tail :bytes bytes
                                  :min-length min-length :escape-invisible escape-invisible :charset charset
                                  :max-lines max-lines :on-ok on-ok :on-partial on-partial :on-error on-error)))
      (cond
        ((and encoding (null charset))
         (fail on-error "input.unsupported-format"
               (format nil "unknown encoding ~S; supported: ~{~A~^, ~}" encoding
                       (mapcar #'encoding-name *supported-encodings*))
               :repairs (list (repair "guess-encoding" "Ask info for the encoding guess."
                                      (format nil "aitools info ~A" (shell-quote path))))))
        ((and bytes (not (eq mode :hex))) (invalid "--bytes needs --as hex"))
        ((and (member mode '(:hex :strings)) (or encoding escape-invisible))
         (invalid "--encoding and --escape-invisible apply to --as text only"))
        (t
         (parse-selector-options/k
          :read :range range :symbol symbol :kind kind :between between :exclusive exclusive
                :match match :invert invert
                :extra-exclusive (list (cons "--tail" tail)
                                       (cons "--as hex" (eq mode :hex))
                                       (cons "--as strings" (eq mode :strings)))
                :on-invalid (lambda (message repairs) (fail on-error "argument.invalid" message :repairs repairs))
                :on-none (lambda () (continue-with nil))
                :on-selector #'continue-with))))))

(defun %text-options (target selector tail max-lines escape-invisible charset)
  (make-text-view-options
   :selector selector :tail tail :max-lines max-lines
   :escape-invisible escape-invisible :encoding charset
   :path (target-display-path target)
   :command-words (list "read" (file-target-argument target))
   :extra-words (append (when escape-invisible (list "--escape-invisible"))
                        (when charset (list "--encoding" (encoding-name charset))))
   :hex-words (lambda (start end)
                (list "read" (file-target-argument target) "--as" "hex" "--bytes" (format nil "~D:~D" start end)))))

(defun %read-text-mode (context target options charset on-ok on-partial on-error)
  (flet ((with-head (continuation)
           (lambda (fields)
             (funcall continuation (list* (cons "mode" "text")
                                          (cons "path" (target-display-path target))
                                          fields)))))
    (flet ((render (octets)
             (text-view/k context octets options
                          :on-ok (with-head on-ok) :on-partial (with-head on-partial) :on-error on-error))
           (missing () (fail-missing context target on-error))
           (unreadable () (fail-unreadable context target on-error)))
      (if charset
          (multiple-value-bind (octets problem) (read-target-octets context target)
            (if octets (render octets) (fail-target-read context target on-error problem)))
          (call-with-target-text/k context target
                                   :on-text #'render
                                   :on-binary (lambda (prefix size) (funcall on-ok (%binary-fields target prefix size)))
                                   :on-missing #'missing
                                   :on-unreadable #'unreadable)))))

(defun %read-target (context target selector mode &key tail bytes min-length escape-invisible charset max-lines
                                                       on-ok on-partial on-error)
  (if (eq mode :text)
      (%read-text-mode context target (%text-options target selector tail max-lines escape-invisible charset)
                       charset on-ok on-partial on-error)
      (if (eq mode :hex)
          (%hex-view context target bytes max-lines on-ok on-partial on-error)
          (%strings-view context target min-length max-lines on-ok on-partial on-error))))

(defun %read-with-selector (ports path selector mode &rest keys
                            &key root tx lock-timeout tail bytes min-length escape-invisible charset max-lines
                                 on-ok on-partial on-error)
  (declare (ignore tail bytes min-length escape-invisible charset max-lines on-ok on-partial))
  (call-with-inspect-file/k
   ports path :root root :tx tx :lock-timeout lock-timeout :record t :on-error on-error
   :on-file (lambda (context target)
              (apply #'%read-target context target selector mode
                     (%without-keys keys '(:root :tx :lock-timeout))))))

(defun %without-keys (plist keys)
  (loop for (key value) on plist by #'cddr
        unless (member key keys) append (list key value)))
