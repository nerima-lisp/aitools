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

(defun %agg-fields
    (context path table options groups group-by aggregates
     count min-count sort desc limit command-words)
  "(VALUES fields truncated)."
  (let* ((kept
           (if min-count
               (remove-if
                (lambda (group)
                  (< (table-group-count group) min-count))
                groups)
               groups))
         (jsons
           (%sort-groups
            (mapcar
             (lambda (group)
               (%group-json table group group-by aggregates count))
             kept)
            sort desc))
         (shown (subseq jsons 0 (min limit (length jsons))))
         (truncated (< (length shown) (length jsons)))
         (next-command
           (when truncated
             (command-line
              context
              (append (list "table" "agg" path)
                      (%table-option-words options)
                      command-words
                      (list "--limit"
                            (princ-to-string (length jsons)))))))
         (fields
           (append
            (list (cons "path" path)
                  (cons "format" (string-downcase (symbol-name (table-format table))))
                  (cons "groups" shown)
                  (cons "total_groups" (length jsons))
                  (cons "truncated" (json-bool truncated)))
            (when next-command
              (list (cons "next_commands" (list next-command)))))))
    (values fields truncated)))

(aitools.kernel.domain:define-flow
 (%aggregate context path table rows options group-indexes aggregates flags
             &key on-ok on-partial)
 (declare (type function on-ok on-partial))
 (let ((index
         (lambda (name)
           (cdr (assoc name aggregates :test #'string=)))))
   (multiple-value-bind (fields truncated)
       (%agg-fields
        context path table options
        (aggregate-table
         table
         :rows rows
         :group-by group-indexes
         :sum (funcall index "sum")
         :avg (funcall index "avg")
         :min (funcall index "min")
         :max (funcall index "max")
         :distinct (funcall index "distinct"))
        group-indexes aggregates
        (getf flags :count)
        (getf flags :min-count)
        (getf flags :sort)
        (getf flags :desc)
        (getf flags :limit)
        (getf flags :command-words))
     (if truncated
         (aitools.kernel.domain:flow-continue
          :on-partial fields)
         (aitools.kernel.domain:flow-continue
          :on-ok fields)))))

(aitools.kernel.domain:define-flow
 (table-agg-flow ports path
                 &key root tx lock-timeout format delimiter pointer no-header
                   ws-columns where encoding group-by count sum avg min max
                   distinct min-count sort desc (limit 50)
                   on-ok on-partial on-error)
 "The table agg command. GROUP-BY and WHERE are lists; aggregate columns
are names or 1-based numbers. COUNT is implied when no aggregate is asked
for. Calls ON-OK or ON-PARTIAL (fields), or ON-ERROR."
 (declare (type function on-ok on-partial on-error))
 (let* ((command-words
          (append
           (loop for column in group-by
                 append (list "--group-by" column))
           (when count (list "--count"))
           (loop for (name . column)
                   in (list (cons "sum" sum)
                            (cons "avg" avg)
                            (cons "min" min)
                            (cons "max" max)
                            (cons "distinct" distinct))
                 when column
                   append (list (concatenate 'string "--" name) column))
           (when min-count
             (list "--min-count" (princ-to-string min-count)))
           (when sort (list "--sort" sort))
           (when desc (list "--desc"))))
        (options
          (make-table-options
           :format format
           :delimiter delimiter
           :pointer pointer
           :no-header no-header
           :ws-columns ws-columns
           :where where
           :encoding encoding))
        (flags
          (list :count (or count (not (or sum avg min max distinct)))
                :min-count min-count
                :sort sort
                :desc desc
                :limit limit
                :command-words command-words))
        (specs
          (list (cons "sum" sum)
                (cons "avg" avg)
                (cons "min" min)
                (cons "max" max)
                (cons "distinct" distinct))))
   (if (and sort
            (not (member sort *aggregate-outputs* :test #'string=))
            (not (member sort group-by :test #'string=)))
       (aitools.kernel.domain:flow-call
           (%table-invalid
            (format nil
                    "--sort ~S names no output column (~{~A~^, ~} or a --group-by column)"
                    sort *aggregate-outputs*)
            :command "aitools schema table agg")
         (:on-error (code message &key repairs)
           (aitools.kernel.domain:flow-continue
            :on-error code message :repairs repairs)))
       (aitools.kernel.domain:flow-call
           (%call-with-table/k
            ports path options
            :root root
            :tx tx
            :lock-timeout lock-timeout)
         (:on-error
          (code message &key candidates diagnostics conflicts repairs)
           (aitools.kernel.domain:flow-continue
            :on-error
            code message
            :candidates candidates
            :diagnostics diagnostics
            :conflicts conflicts
            :repairs repairs))
         (:on-table (context target table rows)
           (declare (ignore target))
           (aitools.kernel.domain:flow-call
               (%column-indexes/k table group-by)
             (:on-error (code message &key candidates repairs)
               (aitools.kernel.domain:flow-continue
                :on-error
                code message
                :candidates candidates
                :repairs repairs))
             (:on-indexes (group-indexes)
               (aitools.kernel.domain:flow-call
                   (%aggregate-indexes/k table specs)
                 (:on-error (code message &key candidates repairs)
                   (aitools.kernel.domain:flow-continue
                    :on-error
                    code message
                    :candidates candidates
                    :repairs repairs))
                 (:on-indexes (aggregates)
                   (let ((numeric-aggregates
                           (loop for (name . index) in aggregates
                                 unless (string= name "distinct")
                                   collect
                                   (cons (concatenate 'string "--" name)
                                         index))))
                     (aitools.kernel.domain:flow-call
                         (%numeric-check/k
                          context path options table rows numeric-aggregates
                          (append command-words
                                  (list "--limit"
                                        (princ-to-string limit))))
                       (:on-error (code message &key diagnostics repairs)
                         (aitools.kernel.domain:flow-continue
                          :on-error
                          code message
                          :diagnostics diagnostics
                          :repairs repairs))
                       (:on-valid ()
                         (aitools.kernel.domain:flow-call
                             (%aggregate
                              context path table rows options (if group-by group-indexes '())
                              aggregates flags)
                           (:on-ok (fields)
                             (aitools.kernel.domain:flow-continue
                              :on-ok fields))
                           (:on-partial (fields)
                             (aitools.kernel.domain:flow-continue
                              :on-partial fields)))))))))))))))
