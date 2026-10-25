(in-package #:aitools.inspect.application)
(aitools.kernel.domain:define-flow
 (%numeric-check/k context path options table rows aggregates command-words
                   &key on-valid on-error)
 "ON-VALID when each numeric aggregate has no non-numeric cells."
 (declare (type function on-valid on-error))
 (let ((problem nil))
   (loop for (flag . index) in aggregates
         while (null problem)
         when (and index
                   (not (member (nth index (table-types table))
                                '("integer" "number" "null")
                                :test #'string=)))
         do (let* ((cells (non-numeric-cells table index :rows rows))
                   (column (nth index (table-columns table))))
              (when cells
                (setf problem (list flag column
                                    (nth index (table-types table))
                                    cells)))))
   (if problem
       (destructuring-bind (flag column type cells) problem
         (aitools.kernel.domain:flow-continue
          :on-error
          "argument.invalid"
          (format nil "~A needs a numeric column; ~A is ~A"
                  flag column type)
          :diagnostics
          (mapcar (lambda (cell)
                    (json-object "row" (first cell)
                                 "column" column
                                 "value" (second cell)))
                  cells)
          :repairs
          (list
           (repair
            "filter-rows"
            "Drop the non-numeric rows with --where."
            (command-line
             context
             (append (list "table" "agg" path)
                     (%table-option-words options)
                     command-words
                     (list "--where"
                           (%where-exclude-text
                            column (second (first cells)))))))
           (repair
            "table-options"
            "See the table options and aggregates."
            "aitools schema table agg"))))
     (aitools.kernel.domain:flow-continue
      :on-valid))))
(aitools.kernel.domain:define-flow
 (%aggregate-indexes/k table specs &key on-indexes on-error)
 "Map each requested aggregate and column spec to its column index."
 (declare (type function on-indexes on-error))
 (let ((indexes '())
       (missing nil))
   (dolist (entry specs)
     (let* ((name (car entry))
            (spec (cdr entry))
            (index (and spec (table-column-index table spec))))
       (push (cons name index) indexes)
       (when (and spec (null index) (null missing))
         (setf missing spec))))
   (if missing
       (aitools.kernel.domain:flow-continue
        :on-error
        "argument.invalid"
        (format nil "no column ~S; columns: ~{~A~^, ~}"
                missing (table-columns table))
        :candidates
        (mapcar (lambda (name)
                  (json-object "column" name))
                (rank-similar missing (table-columns table)))
        :repairs
        (list
         (repair
          "table-options"
          "Name columns as listed, or by 1-based number."
          "aitools schema table read")))
       (aitools.kernel.domain:flow-continue
        :on-indexes
        (nreverse indexes)))))
