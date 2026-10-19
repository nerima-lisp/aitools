;;;; packages/feature/inspect/src/domain/table-build.lisp
;;;;
;;;; `table read`: build a TABLE from split string records (csv, tsv,
;;;; sep, ws) or parsed JSON values (jsonl, json), and PARSE-TABLE/K, the
;;;; entry point that dispatches on the format. The TABLE struct, format
;;;; detection, splitting, and cell typing live in table.lisp.
(in-package #:aitools.inspect.domain)

;;; ------------------------------------------------------------ building

(defun %object-table (format objects)
  "A TABLE from JSON values: object keys become columns in first-appearance
order; a non-object element fills the value column."
  (let ((columns '()))
    (dolist (object objects)
      (if (json-object-value-p object)
          (dolist (pair (json-object-pairs object))
            (pushnew (car pair) columns :test #'string=))
          (pushnew "value" columns :test #'string=)))
    (let* ((columns (nreverse columns))
           (rows
             (mapcar
              (lambda (object)
                (coerce
                 (loop for column in columns
                       collect
                       (multiple-value-bind (value present)
                           (if (json-object-value-p object)
                               (json-object-get object column)
                               (if (string= column "value")
                                   (values object t)
                                   (values nil nil)))
                         (if present value (json-null))))
                 'simple-vector))
              objects)))
      (%make-table format columns
                   (loop for index from 0 below (length columns)
                         collect
                         (%json-cell-type
                          (mapcar (lambda (row)
                                    (svref row index))
                                  rows)))
                   rows))))

(aitools.kernel.domain:define-flow
 (%jsonl-objects/k lines &key on-objects on-error)
 (declare (type function on-objects on-error))
 (let ((objects '())
       (problem nil))
   (loop for line across lines
         for number from 1
         while (null problem)
         unless (%blank-line-p line)
           do (aitools.kernel.domain:flow-call
                  (parse-json-document/k line)
                (:on-value (value)
                  (push value objects))
                (:on-error (message line-in column)
                  (declare (ignore line-in column))
                  (setf problem (cons number message)))))
   (if problem
       (aitools.kernel.domain:flow-continue
        :on-error
        (format nil "invalid JSON: ~A" (cdr problem))
        (car problem))
       (aitools.kernel.domain:flow-continue
        :on-objects
        (nreverse objects)))))

(aitools.kernel.domain:define-flow
 (%json-array-table/k lines pointer &key on-table on-error)
 (declare (type function on-table on-error))
 (let ((tokens (parse-json-pointer (or pointer ""))))
   (if (eq tokens :invalid)
       (aitools.kernel.domain:flow-continue
        :on-error
        (format nil "~S is not a JSON pointer" pointer)
        1)
       (aitools.kernel.domain:flow-call
           (parse-json-document/k
            (format nil "~{~A~^~%~}" (coerce lines 'list)))
         (:on-error (message line column)
           (declare (ignore column))
           (aitools.kernel.domain:flow-continue
            :on-error
            (format nil "invalid JSON: ~A" message)
            line))
         (:on-value (document)
           (aitools.kernel.domain:flow-call
               (resolve-json-pointer/k document tokens)
             (:on-missing (parent-tokens parent token)
               (declare (ignore parent-tokens parent token))
               (aitools.kernel.domain:flow-continue
                :on-error
                (format nil "pointer ~A does not exist" pointer)
                1))
             (:on-found (value)
               (if (json-array-value-p value)
                   (aitools.kernel.domain:flow-continue
                    :on-table
                    (%object-table :json (coerce value 'list)))
                   (aitools.kernel.domain:flow-continue
                    :on-error
                    (format nil "~A is not an array; give --pointer to one"
                            (if (plusp (length (or pointer "")))
                                pointer
                                "the document"))
                    1)))))))))

(aitools.kernel.domain:define-flow
 (parse-table/k lines format &key delimiter pointer no-header ws-columns
                on-table on-error)
 "Parse LINES (a vector of decoded line strings) as FORMAT (a keyword) and
call ON-TABLE (table) or ON-ERROR (message line). WS-COLUMNS, for :WS, bounds
the field count and puts the rest of the line in the last column; NIL
splits every line fully, awk-style."
 (declare (type function on-table on-error))
 (let ((rows (remove-if #'%blank-line-p (coerce lines 'list))))
   (ecase format
     (:csv
      (aitools.kernel.domain:flow-call
          (%csv-records/k
           (format nil "~{~A~^~%~}" (coerce lines 'list))
           #\,)
        (:on-error (message line)
          (aitools.kernel.domain:flow-continue
           :on-error message line))
        (:on-records (records)
          (aitools.kernel.domain:flow-call
              (%text-table/k :csv (remove '("") records :test #'equal)
                             no-header)
            (:on-error (message line)
              (aitools.kernel.domain:flow-continue
               :on-error message line))
            (:on-table (table)
              (aitools.kernel.domain:flow-continue
               :on-table table))))))
     (:tsv
      (aitools.kernel.domain:flow-call
          (%text-table/k :tsv
                         (mapcar (lambda (line)
                                   (%split-on line (string #\Tab)))
                                 rows)
                         no-header)
        (:on-error (message line)
          (aitools.kernel.domain:flow-continue
           :on-error message line))
        (:on-table (table)
          (aitools.kernel.domain:flow-continue
           :on-table table))))
     (:sep
      (aitools.kernel.domain:flow-call
          (%text-table/k :sep
                         (mapcar (lambda (line)
                                   (%split-on line delimiter))
                                 rows)
                         no-header)
        (:on-error (message line)
          (aitools.kernel.domain:flow-continue
           :on-error message line))
        (:on-table (table)
          (aitools.kernel.domain:flow-continue
           :on-table table))))
     (:ws
      (aitools.kernel.domain:flow-call
          (%text-table/k :ws
                         (mapcar (lambda (line)
                                   (%split-whitespace line ws-columns))
                                 rows)
                         no-header)
        (:on-error (message line)
          (aitools.kernel.domain:flow-continue
           :on-error message line))
        (:on-table (table)
          (aitools.kernel.domain:flow-continue
           :on-table table))))
     (:lines
      (aitools.kernel.domain:flow-continue
       :on-table
       (%make-table :lines '("line") '("string")
                    (map 'list
                         (lambda (line)
                           (vector line))
                         lines))))
     (:jsonl
      (aitools.kernel.domain:flow-call
          (%jsonl-objects/k lines)
        (:on-error (message line)
          (aitools.kernel.domain:flow-continue
           :on-error message line))
        (:on-objects (objects)
          (aitools.kernel.domain:flow-continue
           :on-table
           (%object-table :jsonl objects)))))
     (:json
      (aitools.kernel.domain:flow-call
          (%json-array-table/k lines pointer)
        (:on-error (message line)
          (aitools.kernel.domain:flow-continue
           :on-error message line))
        (:on-table (table)
          (aitools.kernel.domain:flow-continue
           :on-table table)))))))
