(in-package #:aitools.edit.test)
(defmacro %with-org-write-adapter-condition (function condition &body body)
  `(cl-weave:with-mocked-functions
       (((symbol-function (quote ,function))
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (error ,condition))))
     ,@body))
(describe "Org write adapter edge and error paths"
  (it "validates table indexes, choices, and row bounds"
    (let ((choices (list (cons "lexical" :lexical)
                         (cons "numeric" :numeric)
                         (cons "ascending" :ascending))))
      (expect (aitools.org.infrastructure::%table-positive-index-p 1)
              :to-be t)
      (expect (aitools.org.infrastructure::%table-positive-index-p 0)
              :to-be nil)
      (expect (aitools.org.infrastructure::%table-positive-index-p "1")
              :to-be nil)
      (expect (aitools.org.infrastructure::%table-choice-keyword "numeric" choices)
              :to-be :numeric)
      (expect (aitools.org.infrastructure::%table-choice-keyword :numeric choices)
              :to-be :numeric)
      (expect (aitools.org.infrastructure::%table-choice-keyword :descending choices)
              :to-be nil)
      (expect (aitools.org.infrastructure::%table-choice-keyword 1 choices)
              :to-be nil)
      (dolist (options (list (list :column "1")
                             (list :column 1 :start 1 :end 0)))
        (multiple-value-bind (column type order start end failure)
            (aitools.org.infrastructure::%table-sort-options options)
          (declare (ignore column type order start end))
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :argument)))))
  (it "maps table edit failures from cl-org-kit"
    (let* ((document
             (%org-coverage-document
              (format nil "| A | B |~%| 1 | 2 |~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (table (aitools.org.infrastructure::%table-node-at engine 1)))
      (%with-org-write-adapter-condition
          org-kit:table-recalculate
          (make-condition (quote org-kit:org-resource-limit-error)
                          :message "table limit" :limit-name :max-output-length
                          :limit 1 :actual 2)
        (multiple-value-bind (updated fields failure)
            (aitools.org.infrastructure::%apply-org-table-edit
             engine :table-recalculate 1 table nil nil nil nil nil)
          (expect updated :to-be nil)
          (expect fields :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :too-large)))
      (%with-org-write-adapter-condition
          org-kit:table-recalculate
          (make-condition (quote org-kit:org-table-unsupported-syntax)
                          :message "formula unsupported")
        (multiple-value-bind (updated fields failure)
            (aitools.org.infrastructure::%apply-org-table-edit
             engine :table-recalculate 1 table nil nil nil nil nil)
          (expect updated :to-be nil)
          (expect fields :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :unsupported)))
      (%with-org-write-adapter-condition
          org-kit:table-recalculate
          (make-condition (quote org-kit:org-table-error)
                          :message "unknown formula" :reason :unknown-function)
        (multiple-value-bind (updated fields failure)
            (aitools.org.infrastructure::%apply-org-table-edit
             engine :table-recalculate 1 table nil nil nil nil nil)
          (expect updated :to-be nil)
          (expect fields :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :unsupported)))
      (%with-org-write-adapter-condition
          org-kit:table-sort-rows
          (make-condition (quote org-kit:org-table-error)
                          :message "invalid sort value" :reason :invalid-number)
        (multiple-value-bind (updated fields failure)
            (aitools.org.infrastructure::%apply-org-table-edit
             engine :table-sort 1 table 1 :numeric :ascending nil nil)
          (expect updated :to-be nil)
          (expect fields :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :argument)))
      (%with-org-write-adapter-condition
          org-kit:table-recalculate
          (make-condition (quote org-kit:org-edit-error)
                          :message "table structure refused"
                          :reason :uneditable-structure)
        (multiple-value-bind (updated fields failure)
            (aitools.org.infrastructure::%apply-org-table-edit
             engine :table-recalculate 1 table nil nil nil nil nil)
          (expect updated :to-be nil)
          (expect fields :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :unsupported)))
      (%with-org-write-adapter-condition
          org-kit:table-recalculate
          (make-condition (quote org-kit:org-kit-error) :message "table failed")
        (multiple-value-bind (updated fields failure)
            (aitools.org.infrastructure::%apply-org-table-edit
             engine :table-recalculate 1 table nil nil nil nil nil)
          (expect updated :to-be nil)
          (expect fields :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :internal)))))
  (it "maps writer resource and kit failures"
    (let* ((document (%org-coverage-document "* TODO Work"))
           (engine (aitools.org.domain:org-document-engine-value document)))
      (%with-org-write-adapter-condition
          org-kit:write-org
          (make-condition (quote org-kit:org-resource-limit-error)
                          :message "output limit" :limit-name :max-output-length
                          :limit 1 :actual 2)
        (multiple-value-bind (octets failure)
            (aitools.org.infrastructure::write-org-octets engine)
          (expect octets :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :too-large)))
      (%with-org-write-adapter-condition
          org-kit:write-org
          (make-condition (quote org-kit:org-kit-error) :message "writer failed")
        (multiple-value-bind (octets failure)
            (aitools.org.infrastructure::write-org-octets engine)
          (expect octets :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :internal))))))
