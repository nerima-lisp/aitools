;;;; packages/feature/inspect/src/domain/table-agg.lisp
;;;;
;;;; `table agg`: declarative grouping and aggregation, the
;;;; replacement for `awk '{s+=$2}'` and `sort | uniq -c`. Numeric
;;;; aggregates (sum, avg, min, max) apply to numeric columns only
;;;; (docs/src/reference/commands.md#table-agg); NON-NUMERIC-CELLS names the rows that break that.
(in-package #:aitools.inspect.domain)

(defstruct (table-group (:constructor %make-table-group (key)) (:copier nil))
  "KEY is the list of group-by cell values; the rest accumulate."
  (key '() :read-only t)
  (count 0)
  (sum 0)
  (avg-sum 0)
  (avg-count 0)
  (min nil)
  (max nil)
  (distinct nil))

(defun %present-p (value)
  (not (or (null value) (json-null-value-p value))))

(defun %numeric-cell-p (value)
  "True when VALUE counts as numeric for an aggregate: a number, or a string
that spells a JSON number. A mixed column is typed `string` so its numeric
cells are stored as their raw text (table.lisp %typed-cell); the numeric check classifies
each cell from that text rather than trusting the degraded column type."
  (or (numberp value)
      (and (stringp value) (member (%cell-class value) '(:integer :number)))))

(defun non-numeric-cells (table index &key (limit 20))
  "(row value) for the first LIMIT data rows (1-based) whose cell at INDEX is
present and non-blank but does not read as a number."
  (loop for row in (table-rows table)
        for number from 1
        for value = (svref row index)
        when (and (%present-p value)
                  (not (and (stringp value) (zerop (length value))))
                  (not (%numeric-cell-p value)))
          collect (list number value) into found
        when (>= (length found) limit) return found
        finally (return found)))

(defun %key-less-p (a b)
  (loop for x in a
        for y in b
        do (cond ((json-value-less-p x y) (return t))
                 ((json-value-less-p y x) (return nil)))
        finally (return nil)))

(defun aggregate-table (table &key (rows (table-rows table)) group-by sum avg min max distinct)
  "TABLE-GROUPs of ROWS (default: TABLE's rows) keyed by the GROUP-BY column indexes, in
key order. SUM/AVG/MIN/MAX/DISTINCT are single column indexes or NIL."
  (let ((groups (make-hash-table :test 'equal))
        (order '())
        (extreme-min min)
        (extreme-max max))
    (dolist (row rows)
      (let* ((key (mapcar (lambda (index) (svref row index)) group-by))
             (fingerprint (render-json (coerce key 'vector)))
             (group (or (gethash fingerprint groups)
                        (let ((new (%make-table-group key)))
                          (push new order)
                          (setf (gethash fingerprint groups) new)))))
        (incf (table-group-count group))
        (when sum
          (let ((value (svref row sum)))
            (when (numberp value) (incf (table-group-sum group) value))))
        (when avg
          (let ((value (svref row avg)))
            (when (numberp value)
              (incf (table-group-avg-sum group) value)
              (incf (table-group-avg-count group)))))
        (when extreme-min
          (let ((value (svref row extreme-min)))
            (when (and (numberp value) (or (null (table-group-min group)) (< value (table-group-min group))))
              (setf (table-group-min group) value))))
        (when extreme-max
          (let ((value (svref row extreme-max)))
            (when (and (numberp value) (or (null (table-group-max group)) (> value (table-group-max group))))
              (setf (table-group-max group) value))))
        (when distinct
          (let ((value (svref row distinct)))
            (when (%present-p value)
              (pushnew (render-json value) (table-group-distinct group) :test #'string=))))))
    (stable-sort (nreverse order) #'%key-less-p :key #'table-group-key)))

(defun table-group-average (group)
  (and (plusp (table-group-avg-count group))
       (float (/ (table-group-avg-sum group) (table-group-avg-count group)) 1d0)))

(defun table-group-distinct-count (group)
  (length (table-group-distinct group)))
