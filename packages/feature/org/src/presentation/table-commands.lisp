(in-package #:aitools.org.presentation)

(defun %table-option ()
  (%value-option "table" "N" "One-based table number in the document."
                 :type :integer :min 1 :required-p t))

(defun %table-align-command (ports)
  (make-command
   :name "align" :description (%summary "org.table.align")
   :positionals (list (%path-positional))
   :options (append (list (%table-option)) (%org-write-options))
   :handler (lambda (invocation)
              (%org-run-write #'aitools.org.application:org-table-align/k
                              ports invocation
                              (list :table (option-value invocation :table))))))

(defun %table-sort-command (ports)
  (make-command
   :name "sort" :description (%summary "org.table.sort")
   :positionals (list (%path-positional))
   :options (append
             (list (%table-option)
                   (%value-option "column" "N" "One-based column to sort by."
                                  :type :integer :min 1 :required-p t)
                   (%value-option "type" "TYPE" "Cell comparison type."
                                  :choices '("lexical" "numeric")
                                  :default "lexical")
                   (%value-option "order" "ORDER" "Sort direction."
                                  :choices '("ascending" "descending")
                                  :default "ascending")
                   (%value-option "start" "N" "First one-based standard row to sort."
                                  :type :integer :min 1)
                   (%value-option "end" "N" "Last one-based standard row to sort."
                                  :type :integer :min 1))
             (%org-write-options))
   :handler (lambda (invocation)
              (%org-run-write #'aitools.org.application:org-table-sort/k
                              ports invocation
                              (list :table (option-value invocation :table)
                                    :column (option-value invocation :column)
                                    :type (option-value invocation :type)
                                    :order (option-value invocation :order)
                                    :start (option-value invocation :start)
                                    :end (option-value invocation :end))))))

(defun %table-recalculate-command (ports)
  (make-command
   :name "recalculate" :description (%summary "org.table.recalculate")
   :positionals (list (%path-positional))
   :options (append (list (%table-option)) (%org-write-options))
   :handler (lambda (invocation)
              (%org-run-write #'aitools.org.application:org-table-recalculate/k
                              ports invocation
                              (list :table (option-value invocation :table))))))

(defun %org-table-declarations ()
  (list
   (aitools.protocol.application:define-command
    "org.table.align"
    (:group "org.table" :command-builder #'%table-align-command
     :schema (%schema "org.table.align")))
   (aitools.protocol.application:define-command
    "org.table.sort"
    (:group "org.table" :command-builder #'%table-sort-command
     :schema (%schema "org.table.sort")))
   (aitools.protocol.application:define-command
    "org.table.recalculate"
    (:group "org.table" :command-builder #'%table-recalculate-command
     :schema (%schema "org.table.recalculate")))))
