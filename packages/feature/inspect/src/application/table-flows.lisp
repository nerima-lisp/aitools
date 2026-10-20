;;;; packages/feature/inspect/src/application/table-flows.lisp
;;;;
;;;; `table read` and `table agg`: one parse of the file into
;;;; a typed table, then row filtering (`--where`), column choice
;;;; (`--columns`), a row window (`--range`, `--limit`), or grouping.
;;;; Both join a tx's read set (`table read` is a single-file read;
;;;; `agg` reads the same way). This file holds the shared options and
;;;; parsing plus `table read`; the `table agg` flow is in
;;;; table-agg-flows.lisp.
(in-package #:aitools.inspect.application)

(aitools.kernel.domain:define-flow
 (%parse-target-table/k context target options &key on-table on-error)
 (declare (type function on-table on-error))
 (multiple-value-bind (octets problem)
     (read-target-octets context target)
   (let ((path (file-target-argument target)))
     (if (null octets)
         (fail-target-read
          context target
          (lambda (code message &key candidates diagnostics conflicts repairs)
            (aitools.kernel.domain:flow-continue
             :on-error
             code message
             :candidates candidates
             :diagnostics diagnostics
             :conflicts conflicts
             :repairs repairs))
          problem)
         (aitools.kernel.domain:flow-call
             (%table-lines/k octets options)
           (:on-lines (lines)
             (let ((format
                     (if (table-options-format options)
                         (intern
                          (string-upcase (table-options-format options))
                          :keyword)
                         (detect-table-format
                          (file-target-absolute target)
                          lines))))
               (if (null format)
                   (aitools.kernel.domain:flow-continue
                    :on-error
                    "input.unsupported-format"
                    (format nil
                            "cannot tell the table format of ~A; give --format"
                            path)
                    :repairs
                    (list
                     (repair
                      "whitespace-columns"
                      "Split lines at whitespace runs."
                      (command-line
                       context
                       (list "table" "read" path "--format" "ws")))
                     (repair
                      "whole-lines"
                      "Read each line as one column."
                      (command-line
                       context
                       (list "table" "read" path "--format" "lines"))))
                   (aitools.kernel.domain:flow-call
                       (parse-table/k
                        lines format
                        :delimiter (table-options-delimiter options)
                        :pointer (table-options-pointer options)
                        :no-header (table-options-no-header options)
                        :ws-columns (table-options-ws-columns options))
                     (:on-error (message line)
                       (aitools.kernel.domain:flow-continue
                        :on-error
                        "input.syntax-error"
                        (format nil "~A: ~A at line ~D"
                                path message line)
                        :diagnostics
                        (list (json-object
                               "line" line "col" 1 "message" message))
                        :repairs
                        (list
                         (repair
                          "read-around"
                          "Read the lines around the problem."
                          (command-line
                           context
                           (list "read" path "--range"
                                 (format nil "~D:~D"
                                         (max 1 (- line 3))
                                         (+ line 3))))))))
                     (:on-table (table)
                       (aitools.kernel.domain:flow-continue
                        :on-table table))))))))))))

(aitools.kernel.domain:define-flow
 (%filter-rows/k table where &key on-rows on-error)
 "ON-ROWS (rows) with TABLE's rows satisfying every WHERE text."
 (declare (type function on-rows on-error))
 (let ((comparisons '())
       (problem nil))
   (loop for text in where
         while (null problem)
         do (aitools.kernel.domain:flow-call
                (table-comparison/k table text)
              (:on-comparison (comparison)
                (push comparison comparisons))
              (:on-error (message)
                (setf problem message))))
   (if problem
       (aitools.kernel.domain:flow-call
           (%table-invalid problem)
         (:on-error (code message &key repairs)
           (aitools.kernel.domain:flow-continue
            :on-error code message :repairs repairs)))
       (let ((comparisons (nreverse comparisons)))
         (aitools.kernel.domain:flow-continue
          :on-rows
          (remove-if-not
           (lambda (row)
             (table-row-matches-p row comparisons))
           (table-rows table)))))))

(aitools.kernel.domain:define-flow
 (%call-with-table/k ports path options
                     &key root tx lock-timeout on-table on-error)
 "Validate OPTIONS, read and parse PATH, filter by --where, and call
ON-TABLE (context target table rows)."
 (declare (type function on-table on-error))
 (aitools.kernel.domain:flow-call
     (%validate-table-options options)
   (:on-error (code message &key repairs)
     (aitools.kernel.domain:flow-continue
      :on-error code message :repairs repairs))
   (:on-valid ()
     (aitools.kernel.domain:flow-call
         (call-with-inspect-file/k
          ports path
          :root root
          :tx tx
          :lock-timeout lock-timeout
          :record t)
       (:on-error
        (code message &key candidates diagnostics conflicts repairs)
         (aitools.kernel.domain:flow-continue
          :on-error
          code message
          :candidates candidates
          :diagnostics diagnostics
          :conflicts conflicts
          :repairs repairs))
       (:on-file (context target)
         (aitools.kernel.domain:flow-call
             (%parse-target-table/k context target options)
           (:on-error
            (code message &key candidates diagnostics conflicts repairs)
             (aitools.kernel.domain:flow-continue
              :on-error
              code message
              :candidates candidates
              :diagnostics diagnostics
              :conflicts conflicts
              :repairs repairs))
           (:on-table (table)
             (aitools.kernel.domain:flow-call
                 (%filter-rows/k table (table-options-where options))
               (:on-error (code message &key repairs)
                 (aitools.kernel.domain:flow-continue
                  :on-error code message :repairs repairs))
               (:on-rows (rows)
                 (aitools.kernel.domain:flow-continue
                  :on-table context target table rows))))))))))

(aitools.kernel.domain:define-flow
 (%column-indexes/k table specs &key on-indexes on-error)
 "Indexes for SPECS (names or 1-based numbers), all columns when SPECS is NIL."
 (declare (type function on-indexes on-error))
 (if (null specs)
     (aitools.kernel.domain:flow-continue
      :on-indexes
      (loop for index below (length (table-columns table))
            collect index))
     (let* ((indexes
              (mapcar (lambda (spec)
                        (table-column-index table spec))
                      specs))
            (missing (position nil indexes)))
       (if missing
           (let ((spec (nth missing specs)))
             (aitools.kernel.domain:flow-continue
              :on-error
              "argument.invalid"
              (format nil "no column ~S; columns: ~{~A~^, ~}"
                      spec (table-columns table))
              :candidates
              (mapcar (lambda (name)
                        (json-object "column" name))
                      (rank-similar spec (table-columns table)))
              :repairs
              (list
               (repair
                "table-options"
                "Name columns as listed, or by 1-based number."
                "aitools schema table read"))))
           (aitools.kernel.domain:flow-continue
            :on-indexes indexes)))))

;;; ------------------------------------------------------------ table read

