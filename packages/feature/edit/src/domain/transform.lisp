;;;; packages/feature/edit/src/domain/transform.lisp
;;;;
;;;; `transform --op` line operations. Each op maps a list of line
;;;; strings to a new list; the whole-file ops (eol-*, *final-newline,
;;;; strip-bom) act on the document and live with the flow. Pure value
;;;; computations, so no continuations.
(in-package #:aitools.edit.domain)

(defparameter +line-transform-ops+
  '("sort" "sort-numeric" "sort-version" "reverse" "shuffle" "unique" "delete-blank" "squeeze-blank"
    "strip-trailing" "indent" "dedent" "tabs-to-spaces" "spaces-to-tabs" "upper" "lower" "nfc" "nfkc"
    "wrap" "reflow" "comment" "uncomment"))

(defparameter +whole-file-transform-ops+
  '("eol-lf" "eol-crlf" "final-newline" "no-final-newline" "strip-bom"))

(defun %blank-p (line)
  (zerop (length (trim-whitespace line))))

(defun %field (line key delimiter)
  "Field KEY (1-based) of LINE split by DELIMITER (a compiled regex), or
LINE itself when KEY is NIL."
  (if (null key)
      line
      (let ((fields (remove "" (cl-regex-kit:split delimiter (string-left-trim '(#\Space #\Tab) line))
                            :test #'string=)))
        (or (nth (1- key) fields) ""))))

(defun %leading-number (string)
  "The number a line starts with (after whitespace), as GNU `sort -n` reads
it; 0 when there is none."
  (let* ((text (string-left-trim '(#\Space #\Tab) string))
         (end (or (position-if-not (lambda (c) (or (%ascii-digit-p c) (member c '(#\. #\- #\+)))) text)
                  (length text)))
         (candidate (subseq text 0 end))
         (sign (if (and (plusp (length candidate)) (char= (char candidate 0) #\-)) -1 1))
         (body (string-left-trim "+-" candidate))
         (dot (position #\. body))
         (whole (subseq body 0 dot))
         (fraction (if dot (remove-if-not #'%ascii-digit-p (subseq body (1+ dot))) "")))
    (if (and (every #'%ascii-digit-p whole) (or (plusp (length whole)) (plusp (length fraction))))
        (* sign (+ (if (plusp (length whole)) (parse-integer whole) 0)
                   (if (plusp (length fraction)) (/ (parse-integer fraction) (expt 10 (length fraction))) 0)))
        0)))

(defun %version-parts (string)
  (loop with position = 0 and length = (length string)
        while (< position length)
        collect (let* ((digit (%ascii-digit-p (char string position)))
                       (end (or (position-if (lambda (c) (if digit (not (%ascii-digit-p c)) (%ascii-digit-p c)))
                                             string :start position)
                                length)))
                  (prog1 (if digit (parse-integer string :start position :end end) (subseq string position end))
                    (setf position end)))))

(defun version< (a b)
  "True when version string A sorts before B: digit runs compare as numbers
(1.9 < 1.10), other runs as strings, a number before a non-number."
  (loop for xs = (%version-parts a) then (rest xs)
        for ys = (%version-parts b) then (rest ys)
        do (cond ((null ys) (return nil))
                 ((null xs) (return t))
                 (t (let ((x (first xs)) (y (first ys)))
                      (cond ((and (integerp x) (integerp y)) (when (/= x y) (return (< x y))))
                            ((integerp x) (return t))
                            ((integerp y) (return nil))
                            ((string/= x y) (return (string< x y)))))))))

;;; A seeded SplitMix64 generator, so `shuffle --seed N` produces the same
;;; order on every host and SBCL version (output must be byte-identical);
;;; CL:RANDOM's algorithm is implementation-defined.

(defun %splitmix64 (state)
  "(values next-state output)"
  (let* ((next (ldb (byte 64 0) (+ state #x9E3779B97F4A7C15)))
         (z next))
    (setf z (ldb (byte 64 0) (* (logxor z (ash z -30)) #xBF58476D1CE4E5B9)))
    (setf z (ldb (byte 64 0) (* (logxor z (ash z -27)) #x94D049BB133111EB)))
    (values next (logxor z (ash z -31)))))

(defun seeded-shuffle (list seed)
  "LIST in a Fisher-Yates order drawn from SplitMix64 seeded with SEED."
  (let ((vector (coerce list 'simple-vector))
        (state (ldb (byte 64 0) seed)))
    (loop for index from (1- (length vector)) downto 1
          do (multiple-value-bind (next output) (%splitmix64 state)
               (setf state next)
               (rotatef (svref vector index) (svref vector (mod output (1+ index))))))
    (coerce vector 'list)))

(defun %expand-tabs (line width)
  (with-output-to-string (out)
    (let ((column 0))
      (loop for char across line
            do (if (char= char #\Tab)
                   (let ((spaces (- width (mod column width))))
                     (dotimes (i spaces) (write-char #\Space out))
                     (incf column spaces))
                   (progn (write-char char out) (incf column)))))))

(defun %leading-columns (line width)
  "(values columns length) of LINE's leading whitespace, tabs expanded."
  (let ((column 0) (index 0))
    (loop while (< index (length line))
          do (case (char line index)
               (#\Space (incf column))
               (#\Tab (incf column (- width (mod column width))))
               (t (loop-finish)))
             (incf index))
    (values column index)))

(defun %wrap-line (line columns)
  "LINE broken at spaces into lines of at most COLUMNS characters where
possible; continuation lines repeat LINE's indentation."
  (if (<= (length line) columns)
      (list line)
      (let* ((indent (leading-whitespace line))
             (words (remove "" (%split-on #\Space (subseq line (length indent))) :test #'string=))
             (lines '())
             (current nil))
        (dolist (word words)
          (if (and current (> (+ (length current) 1 (length word)) columns))
              (progn (push current lines) (setf current (concatenate 'string indent word)))
              (setf current (if current
                                (concatenate 'string current " " word)
                                (concatenate 'string indent word)))))
        (when current (push current lines))
        (nreverse lines))))

(defun %fence-line-p (line)
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (or (and (>= (length trimmed) 3) (string= trimmed "```" :end1 3))
        (and (>= (length trimmed) 3) (string= trimmed "~~~" :end1 3)))))

(defun %map-outside-fences (lines function)
  "Split LINES into runs outside and inside Markdown code fences; FUNCTION
maps each outside run (a list) to its replacement, fenced runs (fence lines
included) are kept verbatim."
  (let ((result '()) (run '()) (in-fence nil))
    (flet ((flush () (when run (setf result (append (reverse (funcall function (nreverse run))) result)) (setf run '()))))
      (dolist (line lines)
        (cond
          ((%fence-line-p line)
           (unless in-fence (flush))
           (push line result)
           (setf in-fence (not in-fence)))
          (in-fence (push line result))
          (t (push line run))))
      (flush))
    (nreverse result)))

(defun %reflow (lines columns)
  (let ((result '()) (paragraph '()))
    (flet ((flush ()
             (when paragraph
               (let* ((ordered (nreverse paragraph))
                      (indent (leading-whitespace (first ordered)))
                      (joined (format nil "~A~{~A~^ ~}" indent (mapcar #'trim-whitespace ordered))))
                 (setf result (append (reverse (%wrap-line joined columns)) result)))
               (setf paragraph '()))))
      (dolist (line lines)
        (if (%blank-p line)
            (progn (flush) (push line result))
            (push line paragraph)))
      (flush))
    (nreverse result)))

(defun %comment-lines (lines language)
  (let ((marker (aitools.text.domain:language-line-comment language))
        (block (aitools.text.domain:language-block-comment language))
        (column (loop for line in lines unless (%blank-p line) minimize (length (leading-whitespace line)))))
    (mapcar (lambda (line)
              (cond
                ((%blank-p line) line)
                (marker (concatenate 'string (subseq line 0 column) marker " " (subseq line column)))
                (t (concatenate 'string (subseq line 0 column) (first block) " " (subseq line column)
                                " " (second block)))))
            lines)))

(defun %strip-prefix (string prefix)
  (and (>= (length string) (length prefix)) (string= string prefix :end1 (length prefix))
       (subseq string (length prefix))))

(defun %uncomment-lines (lines language)
  (let ((marker (aitools.text.domain:language-line-comment language))
        (block (aitools.text.domain:language-block-comment language)))
    (mapcar (lambda (line)
              (let* ((indent (leading-whitespace line))
                     (body (subseq line (length indent))))
                (flet ((drop-space (text) (if (and (plusp (length text)) (char= (char text 0) #\Space))
                                              (subseq text 1) text)))
                  (cond
                    ((and marker (%strip-prefix body marker))
                     (concatenate 'string indent (drop-space (%strip-prefix body marker))))
                    ((and block (%strip-prefix body (first block))
                          (let ((trimmed (string-right-trim '(#\Space #\Tab) body)))
                            (and (>= (length trimmed) (+ (length (first block)) (length (second block))))
                                 (string= (second block) trimmed :start2 (- (length trimmed) (length (second block)))))))
                     (let* ((trimmed (string-right-trim '(#\Space #\Tab) body))
                            (inner (subseq trimmed (length (first block)) (- (length trimmed) (length (second block))))))
                       (concatenate 'string indent (string-right-trim " " (drop-space inner)))))
                    (t line)))))
            lines)))

(defun transform-lines (op lines &key width key delimiter (columns 80) seed language)
  "LINES (a list of strings) after line operation OP (a string of
+LINE-TRANSFORM-OPS+). KEY/DELIMITER select the sort key field; DELIMITER is
a compiled regex. LANGUAGE (a text-domain LANGUAGE) is required by
comment/uncomment, SEED by shuffle."
  (flet ((keyed (function) (lambda (line) (funcall function (%field line key delimiter)))))
    (cond
      ((string= op "sort") (stable-sort (copy-list lines) #'string< :key (keyed #'identity)))
      ((string= op "sort-numeric") (stable-sort (copy-list lines) #'< :key (keyed #'%leading-number)))
      ((string= op "sort-version") (stable-sort (copy-list lines) #'version< :key (keyed #'identity)))
      ((string= op "reverse") (reverse lines))
      ((string= op "shuffle") (seeded-shuffle lines seed))
      ((string= op "unique")
       (let ((seen (make-hash-table :test 'equal)))
         (remove-if (lambda (line)
                      (let ((field (%field line key delimiter)))
                        (prog1 (gethash field seen) (setf (gethash field seen) t))))
                    lines)))
      ((string= op "delete-blank") (remove-if #'%blank-p lines))
      ((string= op "squeeze-blank")
       (loop for line in lines
             for previous-blank = nil then blank
             for blank = (%blank-p line)
             unless (and blank previous-blank) collect line))
      ((string= op "strip-trailing") (mapcar (lambda (line) (string-right-trim '(#\Space #\Tab) line)) lines))
      ((string= op "indent")
       (let ((prefix (make-string (or width 2) :initial-element #\Space)))
         (mapcar (lambda (line) (if (%blank-p line) line (concatenate 'string prefix line))) lines)))
      ((string= op "dedent")
       (let ((remove (or width
                         (loop for line in lines unless (%blank-p line)
                               minimize (nth-value 0 (%leading-columns line 8))))))
         (mapcar (lambda (line)
                   (multiple-value-bind (column end) (%leading-columns line 8)
                     (if (%blank-p line)
                         line
                         (concatenate 'string (make-string (max 0 (- column (or remove 0))) :initial-element #\Space)
                                      (subseq line end)))))
                 lines)))
      ((string= op "tabs-to-spaces") (mapcar (lambda (line) (%expand-tabs line (or width 8))) lines))
      ((string= op "spaces-to-tabs")
       (let ((width (or width 8)))
         (mapcar (lambda (line)
                   (multiple-value-bind (column end) (%leading-columns line width)
                     (concatenate 'string (make-string (floor column width) :initial-element #\Tab)
                                  (make-string (mod column width) :initial-element #\Space)
                                  (subseq line end))))
                 lines)))
      ((string= op "upper") (mapcar #'string-upcase lines))
      ((string= op "lower") (mapcar #'string-downcase lines))
      ((string= op "nfc") (mapcar (lambda (line) (aitools.text.domain:normalize-text line :nfc)) lines))
      ((string= op "nfkc") (mapcar (lambda (line) (aitools.text.domain:normalize-text line :nfkc)) lines))
      ((string= op "wrap")
       (%map-outside-fences lines (lambda (run) (mapcan (lambda (line) (%wrap-line line columns)) run))))
      ((string= op "reflow") (%map-outside-fences lines (lambda (run) (%reflow run columns))))
      ((string= op "comment") (%comment-lines lines language))
      ((string= op "uncomment") (%uncomment-lines lines language))
      ;; OP is one of +LINE-TRANSFORM-OPS+, validated by the transform flow
      ;; before it reaches here; this guard only catches a future op added to
      ;; the list but not to this dispatch.
      (t (error "unknown transform op ~S" op)))))
