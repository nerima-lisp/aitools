;;;; packages/feature/search/src/domain/code.lisp
;;;;
;;;; `code outline`, `code defs`, and `code refs` over the text context's
;;;; language table (data/domain/text/language-data.lisp). Every language's definition
;;;; patterns are compiled once, at load time, into a MATCHER, so they live
;;;; in the saved image (startup compiles nothing) and reuse the search matcher's
;;;; candidate-line strategy. Patterns are anchored at the line start, so
;;;; MAP-MATCHES' "leftmost, ties to the lower index" rule picks, per line,
;;;; the first table entry that matches: table order is priority order.
;;;;
;;;; A definition's end line is an estimate by the language's :EXTENT rule
;;;; (balanced parentheses, balanced braces, indentation, or the next heading
;;;; of the same or a higher level), as `--symbol` is described.
(in-package #:aitools.search.domain)

(defstruct (language-index (:constructor %make-language-index (language matcher kinds name-groups
                                                                        level-groups identifier))
                           (:copier nil))
  (language nil :read-only t)
  (matcher nil :type matcher :read-only t)
  ;; Per definition pattern: its kind string, the index of its `name`
  ;; group, and of its `level` group (or NIL).
  (kinds #() :type simple-vector :read-only t)
  (name-groups #() :type simple-vector :read-only t)
  (level-groups #() :type simple-vector :read-only t)
  ;; A byte regex matching one identifier character.
  (identifier nil :read-only t))

(defstruct (definition (:constructor %make-definition (line end-line kind name)) (:copier nil))
  (line 0 :type fixnum :read-only t)
  (end-line 0 :type fixnum :read-only t)
  (kind "" :type string :read-only t)
  (name "" :type string :read-only t))

(defun %build-language-index (language)
  (let ((definitions (language-definitions language)))
    (build-matcher/k (mapcar #'second definitions)
                     :on-built
                     (lambda (matcher)
                       (let ((programs (matcher-programs matcher)))
                         (%make-language-index
                          language matcher
                          (map 'simple-vector #'first definitions)
                          (map 'simple-vector (lambda (program)
                                                (cl-regex-kit:regex-group-index (program-regex program) "name"))
                                programs)
                          (map 'simple-vector (lambda (program)
                                                (cl-regex-kit:regex-group-index (program-regex program) "level"))
                                programs)
                          (cl-regex-kit:compile-byte-regex
                           (format nil "(?:~A)" (language-identifier language))))))
                     :on-syntax-error
                     (lambda (index pattern message)
                       (error "language ~A: definition pattern ~D ~S does not compile: ~A"
                              (language-name language) index pattern message)))))

(defparameter *language-indexes*
  (mapcar (lambda (name) (%build-language-index (aitools.text.domain:find-language name)))
          (aitools.text.domain:language-names))
  "One LANGUAGE-INDEX per language of the text context's table, built when
this file loads.")

(defun language-index-for-path (path)
  "The LANGUAGE-INDEX of PATH's language, or NIL for an unsupported file."
  (let ((language (language-for-path path)))
    (and language (find language *language-indexes* :key #'language-index-language))))

;;; ------------------------------------------------------------ extents

(defun %line-of (octets position)
  (1+ (count-newlines octets 0 position)))

(defun %starts-with-p (octets position marker)
  (and marker
       (<= (+ position (length marker)) (length octets))
       (loop for i from 0 below (length marker)
             always (= (aref octets (+ position i)) (char-code (char marker i))))))

(defun %sexp-end (octets start block-comment)
  "Offset of the parenthesis closing the first one at or after START,
skipping strings, `;` comments, BLOCK-COMMENT, and backslash escapes; the
last byte when it never closes."
  (let ((open (position 40 octets :start start))
        (length (length octets))
        (depth 0))
    (when (null open) (return-from %sexp-end start))
    (let ((i open))
      (loop while (< i length)
            do (let ((byte (aref octets i)))
                 (cond
                   ((= byte 92) (incf i))
                   ((= byte 34)
                    (loop do (incf i)
                          while (< i length)
                          do (case (aref octets i)
                               (92 (incf i))
                               (34 (return)))))
                   ((= byte 59) (setf i (1- (next-line-start octets i))))
                   ((%starts-with-p octets i (first block-comment))
                    (let ((close (search (map 'octets #'char-code (second block-comment)) octets
                                         :start2 (+ i (length (first block-comment))))))
                      (setf i (if close (+ close (length (second block-comment)) -1) length))))
                   ((= byte 40) (incf depth))
                   ((= byte 41)
                    (decf depth)
                    (when (zerop depth) (return-from %sexp-end i)))))
               (incf i)))
    (max start (1- length))))

(defun %blank-line-p (octets start end)
  (loop for i from start below end always (member (aref octets i) '(32 9 13))))

(defun %first-content-byte (octets start end)
  (loop for i from start below end
        unless (member (aref octets i) '(32 9 13)) return (aref octets i)))

(defun %brace-end (octets start line-comment block-comment)
  "Offset ending a brace-delimited definition that starts at START: the
brace closing its first `{`, or, before any `{`, a `;` at bracket depth
zero, or a line end whose next non-blank line does not open with `{`."
  (let ((length (length octets)) (depth 0) (brackets 0) (opened nil) (i start))
    (loop while (< i length)
          do (let ((byte (aref octets i)))
               (cond
                 ((= byte 34)
                  (loop do (incf i)
                        while (< i length)
                        do (case (aref octets i)
                             (92 (incf i))
                             (34 (return))
                             (10 (return)))))
                 ((%starts-with-p octets i line-comment)
                  (setf i (1- (next-line-start octets i))))
                 ((%starts-with-p octets i (first block-comment))
                  (let ((close (search (map 'octets #'char-code (second block-comment)) octets
                                       :start2 (+ i (length (first block-comment))))))
                    (setf i (if close (+ close (length (second block-comment)) -1) length))))
                 ((= byte 123) (setf opened t) (incf depth))
                 ((= byte 125)
                  (decf depth)
                  (when (and opened (<= depth 0)) (return-from %brace-end i)))
                 ((member byte '(40 91)) (incf brackets))
                 ((member byte '(41 93)) (decf brackets))
                 ((and (not opened) (= byte 59) (<= brackets 0)) (return-from %brace-end i))
                 ((and (not opened) (= byte 10) (<= brackets 0))
                  (let ((next (loop for pos = (1+ i) then (next-line-start octets pos)
                                    while (< pos length)
                                    unless (%blank-line-p octets pos (line-content-end octets pos))
                                      return (%first-content-byte octets pos (line-content-end octets pos)))))
                    (unless (eql next 123) (return-from %brace-end (max start (1- i))))))))
             (incf i))
    (max start (1- length))))

(defun %indentation (octets start end)
  (loop for i from start below end
        while (member (aref octets i) '(32 9))
        count t))

(defun %indent-end-line (octets line line-start)
  "The last non-blank line of the block that LINE (starting at LINE-START)
opens: lines after it until one indented no deeper than LINE."
  (let ((indent (%indentation octets line-start (line-content-end octets line-start)))
        (last line) (number line) (pos (next-line-start octets line-start)) (length (length octets)))
    (loop while (< pos length)
          do (incf number)
             (let ((end (line-content-end octets pos)))
               (unless (%blank-line-p octets pos end)
                 (when (<= (%indentation octets pos end) indent) (return))
                 (setf last number))
               (setf pos (next-line-start octets pos))))
    last))

(defun %fence-lines (octets)
  "Line numbers inside Markdown fenced code blocks (``` or ~~~), fences
included: headings there are not headings."
  (let ((lines (make-hash-table)) (inside nil) (number 0) (pos 0) (length (length octets)))
    (loop while (< pos length)
          do (incf number)
             (let* ((end (line-content-end octets pos))
                    (content (loop for i from pos below end
                                   unless (member (aref octets i) '(32 9)) return i))
                    (fence (and content (or (%starts-with-p octets content "```")
                                            (%starts-with-p octets content "~~~")))))
               (when (or inside fence) (setf (gethash number lines) t))
               (when fence (setf inside (not inside)))
               (setf pos (next-line-start octets pos))))
    lines))

;;; ------------------------------------------------------------ definitions

(defun %group-text (octets hit group)
  (let ((start (and group (hit-group-start hit group))))
    (if start (%decode octets start (hit-group-end hit group)) "")))

(defun file-definitions (index octets)
  "Every definition in OCTETS (a whole file, BOM included or not) as
DEFINITIONs in line order."
  (declare (type octets octets))
  (let* ((octets (strip-bom octets))
         (language (language-index-language index))
         (extent (language-extent language))
         (fences (and (eq extent :heading) (%fence-lines octets)))
         (found '()))
    (flet ((on-match (pattern result line line-start)
             (unless (and fences (gethash line fences))
               (push (list line line-start pattern result) found))
             nil))
      (declare (dynamic-extent #'on-match))
      (map-matches #'on-match (language-index-matcher index) octets :line-mode t))
    (setf found (nreverse found))
    (loop for (line line-start pattern result) in found
          collect (%make-definition
                   line
                   (ecase extent
                     (:sexp (%line-of octets (%sexp-end octets (hit-start result)
                                                        (aitools.text.domain:language-block-comment language))))
                     (:brace (%line-of octets (%brace-end octets line-start
                                                          (aitools.text.domain:language-line-comment language)
                                                          (aitools.text.domain:language-block-comment language))))
                     (:indent (%indent-end-line octets line line-start))
                     (:heading
                      (let* ((level-group (svref (language-index-level-groups index) pattern))
                             (level (length (%group-text octets result level-group)))
                             (next (find-if (lambda (entry)
                                              (let ((group (svref (language-index-level-groups index) (third entry))))
                                                (<= (length (%group-text octets (fourth entry) group)) level)))
                                            (rest (member line found :key #'first)))))
                        (if next (1- (first next)) (count-lines octets)))))
                   (svref (language-index-kinds index) pattern)
                   (%group-text octets result (svref (language-index-name-groups index) pattern))))))

(defun line-definition (index octets line-start line-end)
  "(VALUES kind name) of the definition the line [LINE-START, LINE-END) of
OCTETS opens, or NIL."
  (let ((programs (matcher-programs (language-index-matcher index))))
    (loop for program across programs
          for pattern from 0
          do (let ((result (scan-line (program-regex program) octets line-start line-end)))
               (when result
                 (return (values (svref (language-index-kinds index) pattern)
                                 (%group-text octets result (svref (language-index-name-groups index) pattern)))))))))

;;; ------------------------------------------------------------ references

(defun %char-bounds-before (octets position)
  "(VALUES start end) of the UTF-8 character ending at POSITION, or NIL."
  (when (plusp position)
    (let ((start (1- position)))
      (loop while (and (plusp start) (= (logand (aref octets start) #xC0) #x80))
            do (decf start))
      (values start position))))

(defun %char-bounds-at (octets position)
  (when (< position (length octets))
    (values position (next-char-boundary octets position))))

(defun %identifier-char-p (index octets &optional start end)
  (and start (cl-regex-kit:full-match-p (language-index-identifier index) (subseq octets start end))))

(defun map-word-occurrences (function index name octets)
  "Call FUNCTION with (LINE LINE-START LINE-END) once for each line of
OCTETS (BOM already removed) holding NAME (octets) with no identifier
character of INDEX's language right before or after it: `code refs`'s
word-bounded reference."
  (declare (type function function) (type octets name octets))
  (let ((pos 0) (line 1) (line-pos 0))
    (loop
      (let ((hit (octets-find name octets pos)))
        (unless hit (return))
        (let ((after (+ hit (length name))))
          (if (and (not (multiple-value-call #'%identifier-char-p index octets (%char-bounds-before octets hit)))
                   (not (multiple-value-call #'%identifier-char-p index octets (%char-bounds-at octets after))))
              (progn
                (incf line (count-newlines octets line-pos hit))
                (setf line-pos hit)
                (funcall function line (line-start-at octets hit) (line-content-end octets hit))
                (setf pos (next-line-start octets hit)))
              (setf pos (1+ hit))))))))
