(in-package #:aitools.org.infrastructure)

(defun %table-positive-index-p (value)
  (and (integerp value) (plusp value)))

(defun %table-node-at (engine index)
  (let ((tables nil))
    (org-kit:map-nodes
     (lambda (node)
       (when (eq (org-kit:node-type node) :table)
         (push node tables)))
     (org-kit:document-root engine))
    (nth (1- index) (nreverse tables))))

(defun %table-argument-failure (message)
  (%failure :argument message))

(defun %table-choice-keyword (value choices)
  (cond
    ((stringp value) (cdr (assoc value choices :test #'string=)))
    ((keywordp value)
     (when (member value (mapcar #'cdr choices))
       value))))

(defun %table-sort-options (options)
  (let* ((column (getf options :column))
         (type
          (%table-choice-keyword (or (getf options :type) "lexical")
                                 '(("lexical" . :lexical)
                                   ("numeric" . :numeric))))
         (order
          (%table-choice-keyword (or (getf options :order) "ascending")
                                 '(("ascending" . :ascending)
                                   ("descending" . :descending))))
         (start (getf options :start))
         (end (getf options :end)))
    (cond
      ((not (%table-positive-index-p column))
       (values nil
               nil
               nil
               nil
               nil
               (%table-argument-failure
                "--column must be a positive column number")))
      ((not type)
       (values nil
               nil
               nil
               nil
               nil
               (%table-argument-failure "--type must be lexical or numeric")))
      ((not order)
       (values nil
               nil
               nil
               nil
               nil
               (%table-argument-failure
                "--order must be ascending or descending")))
      ((not (eql (null start) (null end)))
       (values nil
               nil
               nil
               nil
               nil
               (%table-argument-failure
                "--start and --end must be provided together")))
      ((or (and start (not (%table-positive-index-p start)))
           (and end (not (%table-positive-index-p end))))
       (values nil
               nil
               nil
               nil
               nil
               (%table-argument-failure
                "--start and --end must be positive row numbers")))
      (t (values column type order start end nil)))))

(defun %table-error-failure (condition operation)
  (let ((reason (org-kit:org-table-error-reason condition)))
    (case reason
      (:unsupported-table-type
       (%failure :unsupported "Only Org pipe tables support this operation"))
      ((:unsupported-mode :unknown-function)
       (%failure :unsupported "Org table formula uses unsupported syntax"))
      ((:invalid-number :invalid-value :invalid-index :invalid-range :ambiguous-range)
       (if (eq operation :table-sort)
           (%failure :argument "Org table options or cell values are invalid")
           (%failure :syntax "Org table formula could not be evaluated")))
      ((:division-by-zero :non-finite-result :invalid-function-result :empty-range
        :unknown-column-name :ambiguous-column-name :invalid-reference
        :duplicate-target :unknown-property :unknown-constant :unknown-remote-table
        :cyclic-formula :cyclic-remote-reference)
       (%failure :syntax "Org table formula could not be evaluated"))
      (otherwise (%failure :internal "Org table edit failed")))))

(defun %table-edit-error-failure (condition)
  (case (org-kit:org-edit-error-reason condition)
    ((:invalid-value :target-not-found :wrong-node-type :ambiguous-target)
     (%failure :argument "Org table options are invalid"))
    (:uneditable-structure
     (%failure :unsupported "Org table structure cannot be edited"))
    (otherwise (%failure :internal "Org table edit failed"))))

(defun %apply-org-table-edit (engine operation
                                     table-number
                                     table
                                     column
                                     type
                                     order
                                     start
                                     end)
  (handler-case (let* ((result
                        (case operation
                          (:table-align (org-kit:table-align engine table))
                          (:table-sort
                           (org-kit:table-sort-rows engine
                                                    table
                                                    column
                                                    :type
                                                    type
                                                    :order
                                                    order
                                                    :start
                                                    start
                                                    :end
                                                    end))
                          (:table-recalculate
                           (org-kit:table-recalculate engine table))))
                       (updated (org-kit:edit-result-updated-document result)))
                  (values updated (list (cons "table" table-number)) nil))
    (org-kit:org-resource-limit-error ()
      (values nil
              nil
              (%failure :too-large "Org table edit exceeded a resource limit")))
    (org-kit:org-table-unsupported-syntax ()
      (values nil
              nil
              (%failure :unsupported
                        "Org table formula uses unsupported syntax")))
    (org-kit:org-table-error (condition)
      (values nil nil (%table-error-failure condition operation)))
    (org-kit:org-edit-error (condition)
      (values nil nil (%table-edit-error-failure condition)))
    (org-kit:org-kit-error ()
      (values nil nil (%failure :internal "Org table edit failed")))))

(defun edit-org-table-document (engine operation options)
  (let ((table-number (getf options :table)))
    (unless (%table-positive-index-p table-number)
      (return-from edit-org-table-document
        (values nil
                nil
                (%table-argument-failure
                 "--table must be a positive table number"))))
    (let ((table (%table-node-at engine table-number)))
      (unless table
        (return-from edit-org-table-document
          (values nil
                  nil
                  (%table-argument-failure
                   "--table does not select a table in the document"))))
      (case operation
        (:table-align
         (%apply-org-table-edit engine
                                operation
                                table-number
                                table
                                nil
                                nil
                                nil
                                nil
                                nil))
        (:table-recalculate
         (%apply-org-table-edit engine
                                operation
                                table-number
                                table
                                nil
                                nil
                                nil
                                nil
                                nil))
        (:table-sort
         (multiple-value-bind (column type order start end failure)
             (%table-sort-options options)
           (if failure
               (values nil nil failure)
               (%apply-org-table-edit engine
                                      operation
                                      table-number
                                      table
                                      column
                                      type
                                      order
                                      start
                                      end))))
        (otherwise
         (values nil nil (%failure :internal "Unknown Org table operation")))))))
