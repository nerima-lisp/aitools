(in-package #:aitools.edit.test)

(describe "Org write flow preconditions"
          (it
           "rejects an empty path and missing write capabilities"
           (let ((boundaries (cl-boundary-kit:make-boundary-context
 :org-workspace-host nil
 :org-text-source nil
 :org-max-input-bytes nil
 :org-parse nil
 :org-outline nil
 :org-query nil
 :org-export nil
 :org-open-store nil
 :org-write nil
 :org-edit nil
 :org-agenda nil
 :org-clock-report nil)))
             (expect
              (org-flow-error-code
               (function aitools.org.application:org-todo-set/k)
               boundaries
               :path
               "")
              :to-equal
              "argument.invalid")
             (expect
              (org-flow-error-code
               (function aitools.org.application:org-todo-set/k)
               boundaries
               :path
               "missing.org")
              :to-equal
              "internal.unexpected"))))

(describe "Org write flow targets"
          (it
           "rejects missing and non-file targets without changing nearby files"
           (call-with-edit-workspace (lambda () (let ((boundaries (make-org-write-flow-boundaries)))
                                  (expect
                                   (org-flow-error-code
                                    (function
                                     aitools.org.application:org-todo-set/k)
                                    boundaries
                                    :path
                                    "missing.org"
                                    :root
                                    *root*)
                                   :to-equal
                                   "input.not-found")
                                  (put "folder/entry.org"
                                       (format nil "* TODO Entry~%"))
                                  (let ((before (octets-of "folder/entry.org")))
                                    (expect
                                     (org-flow-error-code
                                      (function
                                       aitools.org.application:org-todo-set/k)
                                      boundaries
                                      :path
                                      "folder"
                                      :root
                                      *root*)
                                     :to-equal
                                     "refusal.not-a-file")
                                    (expect (octets-of "folder/entry.org")
                                            :to-equalp
                                            before)))))))

(describe "Org write flow input rejection"
          (it "rejects malformed UTF-8 before changing the source bytes"
              (call-with-edit-workspace (lambda () (let* ((boundaries (make-org-write-flow-boundaries))
                                          (original
                                           (octet-vector 42
                                                         32
                                                         84
                                                         79
                                                         68
                                                         79
                                                         32
                                                         255
                                                         10)))
                                     (put "invalid.org" original)
                                     (expect
                                      (org-flow-error-code
                                       (function
                                        aitools.org.application:org-todo-set/k)
                                       boundaries
                                       :path
                                       "invalid.org"
                                       :root
                                       *root*
                                       :match
                                       "/TODO"
                                       :state
                                       "DONE"
                                       :at
                                       "2026-10-19T09:00:00"
                                       :tz
                                       "UTC")
                                      :to-equal
                                      "input.not-utf8")
                                     (expect (octets-of "invalid.org")
                                             :to-equalp
                                             original)))))
          (it "rejects NUL bytes before editing and preserves source bytes"
              (call-with-edit-workspace (lambda () (let* ((boundaries (make-org-write-flow-boundaries))
                       (original (octet-vector 42 32 84 79 68 79 32 0 10)))
                  (put "nul.org" original)
                  (expect
                   (org-flow-error-code
                    (function
                     aitools.org.application:org-todo-set/k)
                    boundaries
                    :path "nul.org"
                    :root *root*
                    :match "/TODO"
                    :state "DONE"
                    :at "2026-10-19T09:00:00"
                    :tz "UTC")
                   :to-equal "input.unsupported-format")
                  (expect (octets-of "nul.org") :to-equalp original))))))

(describe "Org table sort arguments"
          (it
           "rejects an unknown sort type through the application flow without changing the table"
           (call-with-edit-workspace (lambda () (let* ((boundaries (make-org-write-flow-boundaries))
                                       (source
                                        (format nil
                                                "| A | B |~%|---+---|~%| 1 | 2 |~%")))
                                  (put "table.org" source)
                                  (let ((before (octets-of "table.org")))
                                    (expect
                                     (org-flow-error-code
                                      (function
                                       aitools.org.application:org-table-sort/k)
                                      boundaries
                                      :path
                                      "table.org"
                                      :table
                                      1
                                      :column
                                      1
                                      :type
                                      "unknown"
                                      :order
                                      "ascending"
                                      :root
                                      *root*)
                                     :to-equal
                                     "argument.invalid")
                                    (expect (octets-of "table.org")
                                            :to-equalp
                                            before)))))))
(describe "Org table sort arguments"
          (it
           "rejects an unknown sort type through the application flow without changing the table"
           (call-with-edit-workspace (lambda () (let* ((boundaries (make-org-write-flow-boundaries))
                                       (source
                                        (format nil
                                                "| A | B |~%|---+---|~%| 1 | 2 |~%")))
                                  (put "table.org" source)
                                  (let ((before (octets-of "table.org")))
                                    (expect
                                     (org-flow-error-code
                                      (function
                                       aitools.org.application:org-table-sort/k)
                                      boundaries
                                      :path
                                      "table.org"
                                      :table
                                      1
                                      :column
                                      1
                                      :type
                                      "unknown"
                                      :order
                                      "ascending"
                                      :root
                                      *root*)
                                     :to-equal
                                     "argument.invalid")
                                    (expect (octets-of "table.org")
                                            :to-equalp
                                            before)))))))
