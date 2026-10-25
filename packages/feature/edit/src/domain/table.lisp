;;;;
;;;; `table set` over CSV and TSV: the file is scanned into records
;;;; of field spans, and only the addressed field's span is rewritten, so
;;;; every other byte (quoting style, line endings, trailing newline) stays
;;;; as it was.
(in-package #:aitools.edit.domain)

(defun %quoted-field-end (text position length)
  (loop while (< position length)
        do (if (char= (char text position) #\")
               (if (and (< (1+ position) length) (char= (char text (1+ position)) #\"))
                   (incf position 2)
                   (progn
                     (incf position)
                     (return)))
               (incf position)))
  position)

(defun %field-end (text start delimiter length)
  (let ((position
         (if (and (< start length) (char= (char text start) #\"))
             (%quoted-field-end text (1+ start) length)
             start)))
    (loop while (and (< position length) (not (member (char text position) (list delimiter #\Newline #\Return))))
          do (incf position))
    position))

(defun %scan-records (text delimiter)
  "Records of TEXT as field spans, preserving the quotes around quoted fields.
A final empty line is not a record."
  (let ((records nil)
        (fields nil)
        (position 0)
        (length (length text)))
    (loop (let* ((start position)
                 (end (%field-end text start delimiter length)))
            (push (cons start end) fields)
            (setf position end)
            (cond
              ((>= position length)
                (unless (and (null (rest fields)) (= start position))
                  (push (nreverse fields) records))
                (return))
              ((char= (char text position) delimiter) (incf position))
              (t
                (when
                    (and (char= (char text position) #\Return)
                         (< (1+ position) length)
                         (char= (char text (1+ position)) #\Newline))
                  (incf position))
                (incf position)
                (push (nreverse fields) records)
                (setf fields nil)
                (when (>= position length)
                  (return))))))
    (nreverse records)))

(defun %field-text (text span)
  (let ((raw (subseq text (car span) (cdr span))))
    (if (and (>= (length raw) 2) (char= (char raw 0) #\"))
        (let ((close (position #\" raw :from-end t)))
          (with-output-to-string (out)
            (loop with index = 1
                  while (< index close)
                  do (write-char (char raw index) out) (incf index
                                                             (if (and (char= (char raw index) #\") (< (1+ index) close))
                                                                 2
                                                                 1)))))
        raw)))

(defun %encode-field (value delimiter)
  (if (find-if
       (lambda (char)
         (member char (list delimiter #\" #\Newline #\Return)))
       value)
      (with-output-to-string (out)
        (write-char #\" out)
        (loop for char across value
              do (when (char= char #\")
                   (write-char #\" out)) (write-char char out))
        (write-char #\" out))
      value))

(defun table-delimiter-for-path (path)
  "#\\, for .csv, #\\Tab for .tsv, else NIL."
  (let ((dot (position #\. path :from-end t)))
    (and dot
         (let ((extension (string-downcase (subseq path (1+ dot)))))
           (cond
             ((string= extension "csv") #\,)
             ((string= extension "tsv") #\Tab))))))

(defun table-set-cell (text delimiter row column value)
  "(values new-text previous-value): TEXT with data row ROW (1-based, after
the header) and COLUMN (a header name, or a 1-based index when no header
has that name) set to VALUE. Signals EDIT-REFUSAL input.not-found."
  (let* ((records (%scan-records text delimiter))
         (header
          (mapcar
           (lambda (span)
             (%field-text text span))
           (first records)))
         (index
          (or (position column header :test #'string=)
              (and (plusp (length column))
                   (every #'%ascii-digit-p column)
                   (let ((number (parse-integer column)))
                     (and (<= 1 number (length header)) (1- number))))))
         (record (and (plusp row) (nth row records))))
    (cond
      ((null index) (refuse "input.not-found" "no column ~S (columns: ~{~A~^, ~})" column header))
      ((null record) (refuse "input.not-found" "no data row ~D (the table has ~D)" row (max 0 (1- (length records)))))
      ((>= index (length record)) (refuse "input.not-found" "row ~D has no column ~S" row column))
      (t
       (let ((span (nth index record)))
         (values
          (concatenate 'string (subseq text 0 (car span)) (%encode-field value delimiter) (subseq text (cdr span)))
          (%field-text text span)))))))
