;;;; packages/feature/inspect/src/presentation/table-commands.lisp
;;;;
;;;; `table read` and `table agg`.
(in-package #:aitools.inspect.presentation)

(defun %table-summary (name)
  (command-summary name aitools.data:*inspect-format-command-schemas*))

(defun %table-read-options ()
  (list (value-option "format" "csv, tsv, jsonl, json, ws, sep, or lines."
                      :choices '("csv" "tsv" "jsonl" "json" "ws" "sep" "lines"))
        (value-option "delimiter" "With --format sep: the separator." :value-name "TEXT")
        (value-option "pointer" "With --format json: pointer to the rows." :value-name "POINTER")
        (flag-option "no-header" "The first row is data.")
        (integer-option "ws-columns" nil "With --format ws: split into at most N columns, the last keeping the rest of the line.")
        (%repeated-option "where" "Condition <column><op><value>." :value-name "COND")
        (value-option "encoding" "Decode from this encoding." :value-name "NAME")
        (tx-option)))

(defun %table-read-arguments (invocation)
  (list :format (option-value invocation :format)
        :delimiter (option-value invocation :delimiter)
        :pointer (option-value invocation :pointer)
        :no-header (option-value invocation :no-header)
        :ws-columns (option-value invocation :ws-columns)
        :where (option-value invocation :where)
        :encoding (option-value invocation :encoding)))

(defun %table-read-command (ports)
  (make-command
   :name "read" :description (%table-summary "table.read")
   :positionals (list (make-positional :key :path :name "path" :required-p t))
   :options (append (%table-read-options)
                    (list (make-option :name "columns" :kind :value :value-delimiter #\, :value-name "A,B"
                                       :description "Columns by name or 1-based number.")
                          (value-option "range" "Rows S:E, S:, or N." :value-name "S:E")
                          (integer-option "limit" 50 "Maximum rows returned.")))
   :handler (lambda (invocation)
              (apply #'run-flow #'aitools.inspect.application:table-read-flow ports
                     (positional-value invocation :path)
                     :columns (option-value invocation :columns)
                     :range (option-value invocation :range)
                     :limit (option-value invocation :limit)
                     (append (%table-read-arguments invocation) (context-arguments invocation))))))

(defun %table-agg-command (ports)
  (make-command
   :name "agg" :description (%table-summary "table.agg")
   :positionals (list (make-positional :key :path :name "path" :required-p t))
   :options (append (%table-read-options)
                    (list (%repeated-option "group-by" "Grouping column." :value-name "COLUMN")
                          (flag-option "count" "Rows per group.")
                          (value-option "sum" "Column to sum." :value-name "COLUMN")
                          (value-option "avg" "Column to average." :value-name "COLUMN")
                          (value-option "min" "Column minimum." :value-name "COLUMN")
                          (value-option "max" "Column maximum." :value-name "COLUMN")
                          (value-option "distinct" "Column whose distinct values are counted." :value-name "COLUMN")
                          (integer-option "min-count" nil "Only groups with at least N rows.")
                          (value-option "sort" "Output column to order by." :value-name "NAME")
                          (flag-option "desc" "Descending order.")
                          (integer-option "limit" 50 "Maximum groups returned.")))
   :handler (lambda (invocation)
              (apply #'run-flow #'aitools.inspect.application:table-agg-flow ports
                     (positional-value invocation :path)
                     :group-by (option-value invocation :group-by)
                     :count (option-value invocation :count)
                     :sum (option-value invocation :sum)
                     :avg (option-value invocation :avg)
                     :min (option-value invocation :min)
                     :max (option-value invocation :max)
                     :distinct (option-value invocation :distinct)
                     :min-count (option-value invocation :min-count)
                     :sort (option-value invocation :sort)
                     :desc (option-value invocation :desc)
                     :limit (option-value invocation :limit)
                     (append (%table-read-arguments invocation) (context-arguments invocation))))))

(define-inspect-command "table.read" "table" '%table-read-command aitools.data:*inspect-format-command-schemas*)
(define-inspect-command "table.agg" "table" '%table-agg-command aitools.data:*inspect-format-command-schemas*)
