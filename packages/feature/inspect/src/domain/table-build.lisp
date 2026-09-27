;;;; packages/feature/inspect/src/domain/table-build.lisp
;;;;
;;;; `table read`: build a TABLE from split string records (csv, tsv,
;;;; sep, ws) or parsed JSON values (jsonl, json), and PARSE-TABLE/K, the
;;;; entry point that dispatches on the format. The TABLE struct, format
;;;; detection, splitting, and cell typing live in table.lisp.
(in-package #:aitools.inspect.domain)

;;; ------------------------------------------------------------ building

(defun %header-row-p (row)
  "Whether ROW looks like a header: non-empty, unique, non-numeric cells."
  (and row
       (every (lambda (cell) (eq (%cell-class cell) :string)) row)
       (= (length row) (length (remove-duplicates row :test #'string=)))))

(defun %numbered-columns (count)
  (loop for index from 1 to count collect (princ-to-string index)))

(defconstant +max-table-columns+ 8192
  "Columns a delimited/whitespace row may have before `table read` refuses it
as input.syntax-error, so a pathological line cannot allocate without bound.")

(defun %row-vector (row width)
  "ROW (a list of cell strings) as a WIDTH-long simple-vector, cells past the
row's end left NIL so a short row's missing cells stay distinct from an empty
cell. Vectorizing once turns the later per-column work from O(cols^2) (via
NTH) into O(cols)."
  (let ((vector (make-array width :initial-element nil)))
    (loop for cell in row
          for index from 0 below width
          do (setf (svref vector index) cell))
    vector))

(defun %text-table (format records no-header)
  "A TABLE from string RECORDS (lists), the first taken as the header when
it looks like one and NO-HEADER is false."
  (let* ((header (and (not no-header) (%header-row-p (first records)) (first records)))
         (data (if header (rest records) records))
         (width (max (length header) (reduce #'max data :key #'length :initial-value 0)))
         (columns (if header
                      (append header (nthcdr (length header) (%numbered-columns width)))
                      (%numbered-columns width)))
         (rows (mapcar (lambda (row) (%row-vector row width)) data))
         (types (loop for index below width
                      collect (%column-type (mapcar (lambda (row) (%cell-class (or (svref row index) ""))) rows))))
         (type-vector (coerce types 'simple-vector)))
    (%make-table format columns types
                 (mapcar (lambda (row)
                           (let ((out (make-array width)))
                             (dotimes (index width out)
                               (let ((cell (svref row index)))
                                 (setf (svref out index)
                                       (if cell (%typed-cell cell (svref type-vector index)) (json-null)))))))
                         rows))))

(defun %text-table/k (format records no-header on-table on-error)
  "Build a text TABLE from RECORDS, or ON-ERROR (message line) when a row has
more than +MAX-TABLE-COLUMNS+ columns."
  (let ((width (reduce #'max records :key #'length :initial-value 0)))
    (if (> width +max-table-columns+)
        (funcall on-error (format nil "a row has ~D columns, past the ~D-column limit" width +max-table-columns+) 1)
        (funcall on-table (%text-table format records no-header)))))

(defun %object-table (format objects)
  "A TABLE from JSON values: object keys become columns in first-appearance
order; a non-object element fills a `value` column."
  (let ((columns '()))
    (dolist (object objects)
      (if (json-object-value-p object)
          (dolist (pair (json-object-pairs object))
            (pushnew (car pair) columns :test #'string=))
          (pushnew "value" columns :test #'string=)))
    (let* ((columns (nreverse columns))
           (rows (mapcar (lambda (object)
                           (coerce (loop for column in columns
                                         collect (multiple-value-bind (value present)
                                                     (if (json-object-value-p object)
                                                         (json-object-get object column)
                                                         (if (string= column "value") (values object t) (values nil nil)))
                                                   (if present value (json-null))))
                                   'simple-vector))
                         objects)))
      (%make-table format columns
                   (loop for index from 0 below (length columns)
                         collect (%json-cell-type (mapcar (lambda (row) (svref row index)) rows)))
                   rows))))

(defun %jsonl-objects/k (lines on-objects on-error)
  (let ((objects '()))
    (loop for line across lines
          for number from 1
          unless (%blank-line-p line)
            do (parse-json-document/k line
                                      :on-value (lambda (value) (push value objects))
                                      :on-error (lambda (message line-in column)
                                                  (declare (ignore line-in column))
                                                  (return-from %jsonl-objects/k
                                                    (funcall on-error (format nil "invalid JSON: ~A" message) number)))))
    (funcall on-objects (nreverse objects))))

(defun %json-array-table/k (lines pointer on-table on-error)
  (let ((tokens (parse-json-pointer (or pointer ""))))
    (if (eq tokens :invalid)
        (funcall on-error (format nil "~S is not a JSON pointer" pointer) 1)
        (parse-json-document/k
         (format nil "~{~A~^~%~}" (coerce lines 'list))
         :on-error (lambda (message line column)
                     (declare (ignore column))
                     (funcall on-error (format nil "invalid JSON: ~A" message) line))
         :on-value (lambda (document)
                     (resolve-json-pointer/k
                      document tokens
                      :on-missing (lambda (parent-tokens parent token)
                                    (declare (ignore parent-tokens parent token))
                                    (funcall on-error (format nil "pointer ~A does not exist" pointer) 1))
                      :on-found (lambda (value)
                                  (if (json-array-value-p value)
                                      (funcall on-table (%object-table :json (coerce value 'list)))
                                      (funcall on-error (format nil "~A is not an array; give --pointer to one"
                                                                (if (plusp (length (or pointer ""))) pointer "the document"))
                                               1)))))))))

(defun parse-table/k (lines format &key delimiter pointer no-header ws-columns on-table on-error)
  "Parse LINES (a vector of decoded line strings) as FORMAT (a keyword) and
call ON-TABLE (table) or ON-ERROR (message line). WS-COLUMNS, for :WS, bounds
the field count and puts the rest of the line in the last column; NIL
splits every line fully, awk-style."
  (declare (type function on-table on-error))
  (let ((rows (remove-if #'%blank-line-p (coerce lines 'list))))
    (ecase format
      (:csv (%csv-records/k (format nil "~{~A~^~%~}" (coerce lines 'list)) #\,
                            (lambda (records)
                              (%text-table/k :csv (remove '("") records :test #'equal) no-header on-table on-error))
                            on-error))
      (:tsv (%text-table/k :tsv (mapcar (lambda (line) (%split-on line (string #\Tab))) rows) no-header on-table on-error))
      (:sep (%text-table/k :sep (mapcar (lambda (line) (%split-on line delimiter)) rows) no-header on-table on-error))
      (:ws (%text-table/k :ws (mapcar (lambda (line) (%split-whitespace line ws-columns)) rows) no-header
                          on-table on-error))
      (:lines (funcall on-table (%make-table :lines '("line") '("string")
                                             (map 'list (lambda (line) (vector line)) lines))))
      (:jsonl (%jsonl-objects/k lines (lambda (objects) (funcall on-table (%object-table :jsonl objects))) on-error))
      (:json (%json-array-table/k lines pointer on-table on-error)))))
