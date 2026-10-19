;;;; packages/feature/inspect/src/domain/table-text-build.lisp
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
(aitools.kernel.domain:define-flow
 (%text-table/k format records no-header &key on-table on-error)
 "Build a text TABLE from RECORDS, or ON-ERROR (message line) when a row has
more than +MAX-TABLE-COLUMNS+ columns."
 (declare (type function on-table on-error))
 (let ((width (reduce #'max records :key #'length :initial-value 0)))
   (if (> width +max-table-columns+)
       (aitools.kernel.domain:flow-continue
        :on-error
        (format nil "a row has ~D columns, past the ~D-column limit"
                width +max-table-columns+)
        1)
       (aitools.kernel.domain:flow-continue
        :on-table
        (%text-table format records no-header)))))
