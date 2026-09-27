;;;; packages/feature/inspect/src/domain/table.lisp
;;;;
;;;; `table read`: the `cut`/`awk` column replacement. Every format
;;;; becomes one TABLE of named, typed columns: csv (RFC 4180 quoting), tsv,
;;;; jsonl, json (an array of objects under a pointer), ws (awk semantics:
;;;; split each line on runs of whitespace independently, so rows may differ
;;;; in width; the opt-in WS-COLUMNS bound keeps the rest of the line in the
;;;; last column), sep (a fixed delimiter string), and
;;;; lines (one `line` column). Delimited cells are typed by column: a column
;;;; is integer, number, or boolean only when every non-empty cell is.
;;;; This file holds the TABLE struct, detection, splitting, and typing;
;;;; building and PARSE-TABLE/K are in table-build.lisp, and column and
;;;; `--where` queries in table-query.lisp.
(in-package #:aitools.inspect.domain)

(defstruct (table (:constructor %make-table (format columns types rows)) (:copier nil))
  "FORMAT a keyword; COLUMNS a list of names; TYPES the matching list of
type names; ROWS a list of simple-vectors of JSON values, one per column."
  (format :csv :read-only t)
  (columns '() :read-only t)
  (types '() :read-only t)
  (rows '() :read-only t))

(defparameter *table-formats* '("csv" "tsv" "jsonl" "json" "ws" "sep" "lines"))

;;; ------------------------------------------------------------ detection

(defun %path-extension (path)
  (let* ((name (subseq path (1+ (or (position #\/ path :from-end t) -1))))
         (dot (position #\. name :from-end t)))
    (and dot (plusp dot) (string-downcase (subseq name (1+ dot))))))

(defun detect-table-format (path lines)
  "The format of PATH's LINES from its extension, then its content; NIL
when neither decides. ws, sep, and lines are never detected."
  (let ((by-extension (cdr (assoc (%path-extension path)
                                  '(("csv" . :csv) ("tsv" . :tsv) ("tab" . :tsv) ("jsonl" . :jsonl)
                                    ("ndjson" . :jsonl) ("json" . :json))
                                  :test #'equal))))
    (or by-extension
        (let ((first (find-if-not #'%blank-line-p lines)))
          (when first
            (let ((trimmed (string-left-trim '(#\Space #\Tab) first)))
              (cond ((char= (char trimmed 0) #\[) :json)
                    ((char= (char trimmed 0) #\{)
                     (if (> (count-if-not #'%blank-line-p lines) 1) :jsonl :json))
                    ((find #\Tab first) :tsv)
                    ((find #\, first) :csv)
                    (t nil))))))))

;;; ------------------------------------------------------------ splitting

(defun %csv-records/k (text delimiter on-records on-error)
  "RFC 4180 records of TEXT (lists of strings). ON-ERROR (message line) for
a quoted field left open."
  (let ((records '()) (record '()) (field (make-string-output-stream))
        (index 0) (length (length text)) (line 1) (quoted nil) (quote-line 1) (any nil))
    (flet ((end-field () (push (get-output-stream-string field) record) (setf any t))
           (end-record ()
             (push (nreverse record) records)
             (setf record '() any nil)))
      (loop while (< index length)
            do (let ((char (char text index)))
                 (cond
                   (quoted
                    (cond ((and (char= char #\") (< (1+ index) length) (char= (char text (1+ index)) #\"))
                           (write-char #\" field) (incf index 2))
                          ((char= char #\") (setf quoted nil) (incf index))
                          (t (when (char= char #\Newline) (incf line))
                             (write-char char field) (incf index))))
                   ((char= char #\")
                    (setf quoted t quote-line line any t) (incf index))
                   ((char= char delimiter) (end-field) (incf index))
                   ((char= char #\Newline)
                    (end-field) (end-record) (incf line) (incf index))
                   ((and (char= char #\Return) (< (1+ index) length) (char= (char text (1+ index)) #\Newline))
                    (incf index))
                   (t (write-char char field) (setf any t) (incf index)))))
      (when quoted
        (return-from %csv-records/k (funcall on-error "unterminated quoted field" quote-line)))
      (when (or any record)
        (end-field)
        (end-record))
      (funcall on-records (nreverse records)))))

(defun %split-on (line delimiter)
  "LINE split at every occurrence of the non-empty string DELIMITER."
  (loop with start = 0
        for position = (search delimiter line :start2 start)
        collect (subseq line start (or position (length line)))
        while position
        do (setf start (+ position (length delimiter)))))

(defun %whitespace-p (char) (member char '(#\Space #\Tab)))

(defun %split-whitespace (line count)
  "LINE's whitespace-separated fields (awk semantics: leading whitespace
skipped, runs collapsed). COUNT NIL splits the whole line; a COUNT bounds the
fields, the last holding the rest of the line verbatim (the --ws-columns opt-in)."
  (let ((fields '()) (index (or (position-if-not #'%whitespace-p line) (length line))))
    (loop while (< index (length line))
          do (if (and count (= (length fields) (1- count)))
                 (progn (push (subseq line index) fields) (setf index (length line)))
                 (let ((end (or (position-if #'%whitespace-p line :start index) (length line))))
                   (push (subseq line index end) fields)
                   (setf index (or (position-if-not #'%whitespace-p line :start end) (length line))))))
    (nreverse fields)))

;;; ------------------------------------------------------------ typing

(defun %ascii-digits-p (string start end)
  (and (< start end) (loop for index from start below end always (char<= #\0 (char string index) #\9))))

(defun %integer-text-p (text)
  (let ((start (if (and (plusp (length text)) (find (char text 0) "+-")) 1 0)))
    (%ascii-digits-p text start (length text))))

(defun %number-text-p (text)
  "A decimal with an optional sign, fraction, and exponent (ASCII only)."
  (let* ((start (if (and (plusp (length text)) (find (char text 0) "+-")) 1 0))
         (exponent (position-if (lambda (char) (char-equal char #\e)) text :start start))
         (mantissa-end (or exponent (length text)))
         (dot (position #\. text :start start :end mantissa-end)))
    (and (if dot
             (or (%ascii-digits-p text start dot) (%ascii-digits-p text (1+ dot) mantissa-end))
             (%ascii-digits-p text start mantissa-end))
         (or (null dot) (= (1+ dot) mantissa-end) (%ascii-digits-p text (1+ dot) mantissa-end))
         (or (null dot) (= dot start) (%ascii-digits-p text start dot))
         (or (null exponent)
             (let ((digits (if (and (< (1+ exponent) (length text)) (find (char text (1+ exponent)) "+-"))
                               (+ exponent 2)
                               (1+ exponent))))
               (and (%ascii-digits-p text digits (length text)) (<= (- (length text) digits) 4)))))))

(defun %number-value (text)
  "The number TEXT spells, through the JSON number reader, or NIL."
  (let* ((unsigned (string-left-trim "+" text))
         (negative (and (plusp (length unsigned)) (char= (char unsigned 0) #\-)))
         (body (if negative (subseq unsigned 1) unsigned))
         (body (if (char= (char body 0) #\.) (concatenate 'string "0" body) body))
         (dot (position #\. body))
         (body (if (and dot (or (= (1+ dot) (length body)) (char-equal (char body (1+ dot)) #\e)))
                   (concatenate 'string (subseq body 0 (1+ dot)) "0" (subseq body (1+ dot)))
                   body))
         (body (string-left-trim "0" body))
         (body (if (or (zerop (length body)) (not (char<= #\0 (char body 0) #\9)))
                   (concatenate 'string "0" body)
                   body)))
    (parse-json-document/k (if negative (concatenate 'string "-" body) body)
                           :on-value (lambda (value) (and (numberp value) value))
                           :on-error (lambda (message line column) (declare (ignore message line column)) nil))))

(defun %cell-class (text)
  (cond ((zerop (length text)) :null)
        ((%integer-text-p text) :integer)
        ((%number-text-p text) :number)
        ((member text '("true" "false") :test #'string=) :boolean)
        (t :string)))

(defun %column-type (classes)
  (let ((present (remove :null classes)))
    (cond ((null present) "null")
          ((every (lambda (class) (eq class :integer)) present) "integer")
          ((every (lambda (class) (member class '(:integer :number))) present) "number")
          ((every (lambda (class) (eq class :boolean)) present) "boolean")
          (t "string"))))

(defun %typed-cell (text type)
  (cond ((zerop (length text)) (if (string= type "string") "" (json-null)))
        ((string= type "integer") (parse-integer text))
        ((string= type "number") (or (%number-value text) text))
        ((string= type "boolean") (json-bool (string= text "true")))
        (t text)))

(defun %json-cell-type (values)
  (let ((present (remove-if (lambda (value) (or (null value) (json-null-value-p value))) values)))
    (cond ((null present) "null")
          ((every #'integerp present) "integer")
          ((every #'numberp present) "number")
          ((every (lambda (value) (or (eq value t) (json-false-value-p value))) present) "boolean")
          ((every #'stringp present) "string")
          (t "mixed"))))
