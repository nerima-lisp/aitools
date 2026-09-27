;;;; packages/feature/inspect/src/application/table-agg-flows.lisp
;;;;
;;;; `table agg`: group the rows of a parsed table and compute
;;;; count/sum/avg/min/max/distinct per group, refusing a numeric aggregate
;;;; over a non-numeric cell. The shared table options, parsing, and row
;;;; filtering live in table-flows.lisp.
(in-package #:aitools.inspect.application)

;;; ------------------------------------------------------------ table agg

(defparameter *aggregate-outputs* '("count" "sum" "avg" "min" "max" "distinct"))

(defun %where-exclude-text (column value)
  "A `--where` condition excluding the offending VALUE of COLUMN. A string
value is compared as its text; anything else by its rendered JSON."
  (format nil "~A!=~A" column (if (stringp value) value (render-json value))))

(defun %numeric-check/k (context path options table rows aggregates on-valid on-error)
  "ON-VALID () when every numeric aggregate column (an alist of (flag .
index)) is numeric; else argument.invalid naming the offending rows and a
concrete --where that drops the first offending value."
  (declare (ignore rows))
  (loop for (flag . index) in aggregates
        when (and index (not (member (nth index (table-types table)) '("integer" "number" "null") :test #'string=)))
          do (let* ((cells (non-numeric-cells table index))
                    (column (nth index (table-columns table))))
               (return-from %numeric-check/k
                 (fail on-error "argument.invalid"
                       (format nil "~A needs a numeric column; ~A is ~A" flag column
                               (nth index (table-types table)))
                       :diagnostics (mapcar (lambda (cell)
                                              (json-object "row" (first cell)
                                                           "column" column
                                                           "value" (second cell)))
                                            cells)
                       :repairs (list (repair "filter-rows" "Drop the non-numeric rows with --where."
                                              (command-line context
                                                            (append (list "table" "agg" path)
                                                                    (%table-option-words options)
                                                                    (list "--where"
                                                                          (%where-exclude-text
                                                                           column (second (first cells)))))))
                                      (repair "table-options" "See the table options and aggregates."
                                              "aitools schema table agg"))))))
  (funcall on-valid))

(defun %group-json (table group group-by aggregates count)
  (json-object-from-pairs
   (append (list (cons "key" (json-object-from-pairs
                              (loop for index in group-by
                                    for value in (table-group-key group)
                                    collect (cons (nth index (table-columns table)) value)))))
           (when count (list (cons "count" (table-group-count group))))
           (loop for (name . index) in aggregates
                 when index
                   collect (cons name (json-or-null
                                       (cond ((string= name "sum") (table-group-sum group))
                                             ((string= name "avg") (table-group-average group))
                                             ((string= name "min") (table-group-min group))
                                             ((string= name "max") (table-group-max group))
                                             (t (table-group-distinct-count group)))))))))

(defun %sort-groups (jsons sort desc)
  (flet ((key (json) (multiple-value-bind (value present) (json-object-get json sort)
                       (if present value (json-object-get (json-object-get json "key") sort)))))
    (cond (sort (stable-sort jsons (if desc
                                       (lambda (a b) (json-value-less-p (key b) (key a)))
                                       (lambda (a b) (json-value-less-p (key a) (key b))))))
          (desc (reverse jsons))
          (t jsons))))

(defun %agg-fields (context path table options groups group-by aggregates count min-count sort desc limit)
  "(VALUES fields truncated)."
  (let* ((kept (if min-count (remove-if (lambda (group) (< (table-group-count group) min-count)) groups) groups))
         (jsons (%sort-groups (mapcar (lambda (group) (%group-json table group group-by aggregates count)) kept)
                              sort desc))
         (shown (subseq jsons 0 (min limit (length jsons))))
         (truncated (< (length shown) (length jsons))))
    (values (append (list (cons "path" path)
                          (cons "format" (string-downcase (symbol-name (table-format table))))
                          (cons "groups" shown)
                          (cons "total_groups" (length jsons))
                          (cons "truncated" (json-bool truncated)))
                    (when truncated
                      (list (cons "next_commands"
                                  (list (command-line context (append (list "table" "agg" path)
                                                                      (%table-option-words options)
                                                                      (list "--limit" (princ-to-string (length jsons))))))))))
            truncated)))

(defun %aggregate-indexes/k (table specs continuation on-error)
  "CONTINUATION (alist) mapping each (name . spec) of SPECS to its column
index, NIL for an absent spec."
  (let ((indexes '()))
    (loop for (name . spec) in specs
          do (if (null spec)
                 (push (cons name nil) indexes)
                 (let ((index (table-column-index table spec)))
                   (if index
                       (push (cons name index) indexes)
                       (return-from %aggregate-indexes/k
                         (%column-indexes/k table (list spec) #'identity on-error))))))
    (funcall continuation (nreverse indexes))))

(defun %aggregate (context path table rows options group-indexes aggregates flags on-ok on-partial)
  (let ((index (lambda (name) (cdr (assoc name aggregates :test #'string=)))))
    (multiple-value-bind (fields truncated)
        (%agg-fields context path table options
                     (aggregate-table table :rows rows :group-by group-indexes
                                            :sum (funcall index "sum") :avg (funcall index "avg")
                                            :min (funcall index "min") :max (funcall index "max")
                                            :distinct (funcall index "distinct"))
                     group-indexes aggregates
                     (getf flags :count) (getf flags :min-count) (getf flags :sort) (getf flags :desc)
                     (getf flags :limit))
      (funcall (if truncated on-partial on-ok) fields))))

(defun table-agg-flow (ports path &key root tx lock-timeout format delimiter pointer no-header ws-columns where
                                      encoding group-by count sum avg min max distinct min-count sort desc (limit 50)
                                      on-ok on-partial on-error)
  "`table agg`. GROUP-BY and WHERE are lists; SUM, AVG, MIN, MAX, DISTINCT are
column names or numbers. COUNT is implied when no other aggregate is asked
for. Calls ON-OK or ON-PARTIAL (fields), or ON-ERROR."
  (declare (type function on-ok on-partial on-error))
  (let ((options (make-table-options :format format :delimiter delimiter :pointer pointer :no-header no-header
                                     :ws-columns ws-columns :where where :encoding encoding))
        (flags (list :count (or count (not (or sum avg min max distinct)))
                     :min-count min-count :sort sort :desc desc :limit limit))
        (specs (list (cons "sum" sum) (cons "avg" avg) (cons "min" min) (cons "max" max) (cons "distinct" distinct))))
    (if (and sort (not (member sort *aggregate-outputs* :test #'string=)) (not (member sort group-by :test #'string=)))
        (%table-invalid on-error (format nil "--sort ~S names no output column (~{~A~^, ~} or a --group-by column)"
                                         sort *aggregate-outputs*)
                        "aitools schema table agg")
        (%call-with-table/k
         ports path options :root root :tx tx :lock-timeout lock-timeout :on-error on-error
         :on-table
         (lambda (context target table rows)
           (declare (ignore target))
           (flet ((with-aggregates (group-indexes aggregates)
                    (%numeric-check/k context path options table rows
                                      (loop for (name . index) in aggregates
                                            unless (string= name "distinct")
                                              collect (cons (concatenate 'string "--" name) index))
                                      (lambda ()
                                        (%aggregate context path table rows options group-indexes aggregates flags
                                                    on-ok on-partial))
                                      on-error)))
             (%column-indexes/k table group-by
                                (lambda (group-indexes)
                                  (%aggregate-indexes/k table specs
                                                        (lambda (aggregates)
                                                          (with-aggregates (if group-by group-indexes '()) aggregates))
                                                        on-error))
                                on-error)))))))
