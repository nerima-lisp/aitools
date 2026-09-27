;;;; packages/feature/inspect/src/domain/table-query.lisp
;;;;
;;;; Queries over a parsed TABLE shared by `table read` and `table agg`:
;;;; column lookup by name or 1-based number, `--where` comparisons typed by
;;;; column, row matching, and row projection.
(in-package #:aitools.inspect.domain)

;;; ------------------------------------------------------------ queries

(defun table-column-index (table spec)
  "The 0-based index of column SPEC (a name, else a 1-based number), or NIL."
  (or (position spec (table-columns table) :test #'string=)
      (and (%integer-text-p spec) (not (find (char spec 0) "+-"))
           (let ((number (parse-integer spec)))
             (and (<= 1 number (length (table-columns table))) (1- number))))))

(defun table-comparison/k (table text &key on-comparison on-error)
  "Parse `--where <column><op><value>` against TABLE and call ON-COMPARISON
(comparison) keyed by the column index, or ON-ERROR (message). A string
column compares with the value's text as typed."
  (declare (type function on-comparison on-error))
  (multiple-value-bind (left operator right) (split-comparison text)
    (let ((index (and left (table-column-index table left))))
      (cond
        ((null left) (funcall on-error (format nil "--where ~S has no operator (= != < <= > >= ~~)" text)))
        ((null index) (funcall on-error (format nil "--where names no column ~S; columns: ~{~A~^, ~}"
                                                left (table-columns table))))
        (t
         (make-comparison/k index operator right
                            :on-error on-error
                            :on-comparison
                            (lambda (comparison)
                              (funcall on-comparison
                                       (if (and (string= (nth index (table-types table)) "string")
                                                (not (eq (comparison-operator comparison) :match)))
                                           (%make-comparison index (comparison-operator comparison) right nil)
                                           comparison)))))))))

(defun table-row-matches-p (row comparisons)
  (every (lambda (comparison)
           (let ((value (svref row (comparison-key comparison))))
             (comparison-holds-p comparison value (not (json-null-value-p value)))))
         comparisons))

(defun table-row-json (row indexes)
  "ROW's cells at INDEXES as a list (a JSON array)."
  (mapcar (lambda (index) (svref row index)) indexes))
