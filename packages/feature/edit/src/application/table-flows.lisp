(in-package #:aitools.edit.application)

(define-write-command "table.set"
                      (ports env positionals options on-plan fail)
                      (let ((path (first positionals)))
                      (flet ((plan (row column value record-options inputs)
                             (let ((delimiter (table-delimiter-for-path path))
                                   (row-number (parse-count row)))
                             (cond
                             ((null delimiter)
                              (funcall fail
                                       "input.unsupported-format"
                                       (format nil
                                               "table set edits .csv and .tsv files, not ~A"
                                               path)))
                             ((not (and row-number (plusp row-number)))
                              (funcall fail
                                       "argument.invalid"
                                       (format nil
                                               "--row ~S must be a positive integer"
                                               row)))
                             ((not
                               (and (stringp column)
                                    (plusp (length column))
                                    (stringp value)))
                              (funcall fail
                                       "argument.invalid"
                                       "table set needs --row, --column and --value"))
                             (t
                              (funcall on-plan
                                       (make-write-plan :command
                                                        "table.set"
                                                        :targets
                                                        (list
                                                         (make-write-target
                                                          path))
                                                        :inputs
                                                        inputs
                                                        :guard-requirements
                                                        (list
                                                         (list :expect-hash
                                                               path))
                                                        :expect-hashes
                                                        (getf options
                                                              :expect-hash)
                                                        :plan
                                                        (lambda
                                                        (context commit reject)
                                                        (read-document/k context
                                                                         (context-path
                                                                          context)
                                                                         reject
                                                                         (lambda
                                                                         (document)
                                                                         (multiple-value-bind (text
                                                                                               previous)
                                                                         (table-set-cell
                                                                          (document-text
                                                                           document)
                                                                          delimiter
                                                                          row-number
                                                                          column
                                                                          value)
                                                                         (commit-document
                                                                          context
                                                                          (context-path
                                                                           context)
                                                                          (make-text-document
                                                                           text
                                                                           :bom-p
                                                                           (text-document-bom-p
                                                                            document)
                                                                           :eol
                                                                           (text-document-eol
                                                                            document))
                                                                          commit
                                                                          (list
                                                                           (cons
                                                                            "previous"
                                                                            previous)))))))
                                                        :record-options
                                                        record-options
                                                        :record-positionals
                                                        (lambda (paths)
                                                        (list (first paths))))))))))
                      (cond
                      ((or (null path) (rest positionals))
                       (funcall fail
                                "argument.invalid"
                                "table set takes exactly one PATH"))
                      ((or (getf options :stdin) (getf options :stdin-data))
                       (read-stdin-json/k ports
                                          options
                                          :on-error
                                          fail
                                          :on-json
                                          (lambda (input text)
                                          (let ((row
                                                 (cdr
                                                  (%json-member input "row")))
                                                (column
                                                 (cdr
                                                  (%json-member input "column")))
                                                (value
                                                 (cdr
                                                  (%json-member input "value"))))
                                          (plan
                                           (if (json-num-p row)
                                           (json-num-text row)
                                           row)
                                           (if (json-num-p column)
                                           (json-num-text column)
                                           column)
                                           (if (json-num-p value)
                                           (json-num-text value)
                                           value)
                                           (inline-stdin-options options text)
                                           (list text))))))
                      (t
                       (plan (getf options :row)
                             (getf options :column)
                             (getf options :value)
                             options
                             (list (or (getf options :value) ""))))))))

