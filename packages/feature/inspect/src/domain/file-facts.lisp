;;;; packages/feature/inspect/src/domain/file-facts.lisp
;;;;
;;;; The content fields of `info` (`wc`, `file`, `stat` replacements) and
;;;; the format choice of `check`.
(in-package #:aitools.inspect.domain)

(defun %whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed) :test #'char=))

(defun %decoded-text-counts (octets start)
  "The DECODE-TEXT-LINES-based reference count of OCTETS[START:], scanned in
place with no per-line strings. Used when the bytes are not valid UTF-8, so
U+FFFD substitution is counted exactly as DECODE-TEXT-LINES counts it."
  (declare (type octet-vector octets) (type fixnum start))
  (let* ((string (decode-utf8 octets :start start))
         (length (length string))
         (line-count 0)
         (words 0)
         (max-chars 0)
         (sum-line-length 0)
         (line-start 0))
    (declare (type fixnum length line-count words max-chars sum-line-length line-start)
             (type string string))
    (labels ((tally (start end)
               ;; One line spanning STRING[START,END); split-text-lines has
               ;; already dropped a CRLF's CR from END; one further trailing CR is
               ;; dropped from the char count below.
               (declare (type fixnum start end))
               (incf line-count)
               (incf sum-line-length (- end start))
               (let ((text-end (if (and (> end start) (char= (char string (1- end)) #\Return))
                                   (1- end)
                                   end)))
                 (when (> (the fixnum (- text-end start)) max-chars)
                   (setf max-chars (- text-end start))))
               (loop with in-word = nil
                     for i of-type fixnum from start below end
                     for char = (char string i)
                     do (let ((word-char (not (or (%whitespace-char-p char)
                                                  (char= char (code-char #x3000))))))
                          (when (and word-char (not in-word)) (incf words))
                          (setf in-word word-char)))))
      (loop for newline = (position #\Newline string :start line-start)
            while newline
            do (let ((end (if (and (> newline line-start)
                                   (char= (char string (1- newline)) #\Return))
                              (1- newline)
                              newline)))
                 (tally line-start end)
                 (setf line-start (the fixnum (1+ newline)))))
      (when (< line-start length)
        (tally line-start length)))
    (values line-count words max-chars (+ sum-line-length (max 0 (1- line-count))))))

(defun %valid-utf8-text-counts (octets start)
  "(VALUES lines words max-line-chars characters T) for OCTETS[START:] counted
directly on the bytes as strict UTF-8 (RFC 3629), or (VALUES 0 0 0 0 NIL) when
a byte is not part of a valid sequence. A codepoint is one column; U+3000 (the
only multibyte whitespace COUNT-WORDS recognizes) is E3 80 80; LF, CR, and the
other whitespace are single bytes, so line splitting and the CR-drop rules run
on bytes exactly as SPLIT-TEXT-LINES + LINE-TEXT-LENGTH run on characters."
  (declare (type octet-vector octets) (type fixnum start))
  (let ((end (length octets)) (i start)
        (line-count 0) (words 0) (max-chars 0) (characters 0)
        (cur-len 0) (cr-run 0) (in-word nil))
    (declare (type fixnum end i line-count words max-chars characters cur-len cr-run))
    (macrolet ((invalid () '(return-from %valid-utf8-text-counts (values 0 0 0 0 nil))))
      (labels ((cont-p (j) (and (< j end) (<= #x80 (aref octets j) #xBF)))
               (end-line (chars-len max-len)
                 (declare (type fixnum chars-len max-len))
                 (incf line-count)
                 (incf characters chars-len)
                 (when (> max-len max-chars) (setf max-chars max-len)))
               (add (whitespace cr)
                 (incf cur-len)
                 (setf cr-run (if cr (the fixnum (1+ cr-run)) 0))
                 (let ((word-char (not whitespace)))
                   (when (and word-char (not in-word)) (incf words))
                   (setf in-word word-char))))
        (loop while (< i end) do
          (let ((b (aref octets i)))
            (declare (type (unsigned-byte 8) b))
            (cond
              ((< b #x80)
               (cond
                 ((= b #x0A)
                  (let* ((chars-len (- cur-len (if (>= cr-run 1) 1 0)))
                         (max-len (- chars-len (if (>= cr-run 2) 1 0))))
                    (end-line chars-len max-len))
                  (setf cur-len 0 cr-run 0 in-word nil)
                  (incf i))
                 ((= b #x0D) (add t t) (incf i))
                 ((or (= b #x09) (= b #x0C) (= b #x20)) (add t nil) (incf i))
                 (t (add nil nil) (incf i))))
              ((<= #xC2 b #xDF)
               (if (cont-p (1+ i)) (progn (add nil nil) (incf i 2)) (invalid)))
              ((<= #xE0 b #xEF)
               (let ((b1 (if (< (1+ i) end) (aref octets (1+ i)) -1)))
                 (declare (type fixnum b1))
                 (if (and (cond ((= b #xE0) (<= #xA0 b1 #xBF))
                                ((= b #xED) (<= #x80 b1 #x9F))
                                (t (<= #x80 b1 #xBF)))
                          (cont-p (+ i 2)))
                     (progn (add (and (= b #xE3) (= b1 #x80) (= (aref octets (+ i 2)) #x80)) nil)
                            (incf i 3))
                     (invalid))))
              ((<= #xF0 b #xF4)
               (let ((b1 (if (< (1+ i) end) (aref octets (1+ i)) -1)))
                 (declare (type fixnum b1))
                 (if (and (cond ((= b #xF0) (<= #x90 b1 #xBF))
                                ((= b #xF4) (<= #x80 b1 #x8F))
                                (t (<= #x80 b1 #xBF)))
                          (cont-p (+ i 2)) (cont-p (+ i 3)))
                     (progn (add nil nil) (incf i 4))
                     (invalid))))
              (t (invalid)))))
        (when (> cur-len 0)
          (end-line cur-len (- cur-len (if (>= cr-run 1) 1 0))))
        (values line-count words max-chars (+ characters (max 0 (1- line-count))) t)))))

(defun text-content-counts (octets)
  "(VALUES lines words max-line-chars characters) for OCTETS as a whole UTF-8
file: the `wc` counts `info` reports for text content. Byte-identical to
counting over (DECODE-TEXT-LINES OCTETS) with COUNT-WORDS, MAX-LINE-CHARS, and
the character total, but valid UTF-8 (the common case) is counted directly on
the bytes, materializing no line strings and no decoded string at all.
Only genuinely invalid UTF-8 falls back to a single decode so its U+FFFD
substitution stays counted exactly as before."
  (declare (type octet-vector octets))
  (let ((start (utf8-bom-length octets)))
    (multiple-value-bind (lines words max-chars characters valid)
        (%valid-utf8-text-counts octets start)
      (if valid
          (values lines words max-chars characters)
          (%decoded-text-counts octets start)))))

(defun line-ending-name (style)
  (ecase style (:lf "lf") (:crlf "crlf") (:mixed "mixed") (:none "none")))

(defun format-file-mode (mode)
  "MODE's permission bits as a four-digit octal string, as `chmod --mode`
takes them."
  (format nil "~4,'0O" mode))

(defun iso8601-from-unix (seconds)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (aitools.kernel.domain:unix-seconds-to-universal-time seconds) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" year month day hour minute second)))

(defun check-format-for-path (path)
  "(VALUES format dialect) chosen from PATH for `check`: :JSON, or :LISP
with its scanner dialect, or NIL when the extension names neither."
  (let* ((name (subseq path (1+ (or (position #\/ path :from-end t) -1))))
         (dot (position #\. name :from-end t))
         (extension (and dot (string-downcase (subseq name (1+ dot))))))
    (if (member extension '("json" "jsonc" "geojson" "webmanifest") :test #'equal)
        (values :json nil)
        (let* ((language (language-for-path path))
               (dialect (and language (lisp-dialect-for-language (language-name language)))))
          (if dialect (values :lisp dialect) (values nil nil))))))

(defun json-diagnostics (text)
  "NIL when TEXT is one valid JSON document, else a one-element list
(line col message)."
  (parse-json-document/k text
                         :on-value (lambda (value) (declare (ignore value)) nil)
                         :on-error (lambda (message line column) (list (list line column message)))))
