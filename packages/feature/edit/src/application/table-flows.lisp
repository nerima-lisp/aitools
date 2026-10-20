(in-package #:aitools.edit.application)

(define-write-command "table.set"
                      (ports env positionals options &key on-plan on-error)
                      (let ((path (first positionals)))
                        (flet ((plan (row column value record-options inputs)
                                 (let ((delimiter
                                        (table-delimiter-for-path path))
                                       (row-number (parse-count row)))
                                   (cond
                                     ((null delimiter)
                                      (flow-continue :on-error
                                                     "input.unsupported-format"
                                                     (format nil
                                                             "table set edits .csv and .tsv files, not ~A"
                                                             path)))
                                     ((not (and row-number (plusp row-number)))
                                      (flow-continue :on-error
                                                     "argument.invalid"
                                                     (format nil
                                                             "--row ~S must be a positive integer"
                                                             row)))
                                     ((not
                                       (and (stringp column)
                                            (plusp (length column))
                                            (stringp value)))
                                      (flow-continue :on-error
                                                     "argument.invalid"
                                                     "table set needs --row, --column and --value"))
                                     (t
                                      (flow-continue :on-plan
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
                                                                       (list
                                                                        :expect-hash
                                                                        path))
                                                                      :expect-hashes
                                                                      (getf
                                                                       options
                                                                       :expect-hash)
                                                                      :plan
                                                                      (lambda
                                                                          (context
                                                                           &key
                                                                           on-committed
                                                                           on-rejected)
                                                                        (aitools.kernel.domain:flow-call
                                                                         (read-document/k
                                                                          context
                                                                          (context-path
                                                                           context))
                                                                         (:on-error
 (code message
       &key
       candidates
       diagnostics
       conflicts
       repairs
       path)
 (declare (ignore path))
 (funcall on-rejected
          code
          message
          :candidates
          candidates
          :diagnostics
          diagnostics
          :conflicts
          conflicts
          :repairs
          repairs))

                                                                         (:on-document
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
                                                                            (aitools.kernel.domain:flow-call
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
                                                                              :extra
                                                                              (list
                                                                               (cons
                                                                                "previous"
                                                                                previous)))
                                                                             (:on-committed
                                                                              (requests
                                                                               &optional
                                                                               extra)
                                                                              (funcall
                                                                               on-committed
                                                                               requests
                                                                               extra)))))
                                                                         (:on-filtered
                                                                          ())))
                                                                      :record-options
                                                                      record-options
                                                                      :record-positionals
                                                                      (lambda
                                                                          (paths)
                                                                        (list
                                                                         (first
                                                                          paths))))))))))
                          (cond
                            ((or (null path) (rest positionals))
                             (flow-continue :on-error
                                            "argument.invalid"
                                            "table set takes exactly one PATH"))
                            ((or (getf options :stdin)
                                 (getf options :stdin-data))
                             (aitools.kernel.domain:flow-call
                              (read-stdin-json/k ports options)
                              (:on-error (code message)
                                         (flow-continue :on-error code message))
                              (:on-json (input text)
                                        (let ((row
                                               (cdr (%json-member input "row")))
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
