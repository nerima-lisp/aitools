(in-package #:aitools.org.test)

(describe "aitools.org infrastructure parser and source codec"
          (it-property "preserves generated UTF-8 paragraph bytes"
                       ((text
                         (gen-string :min-length
                                     0
                                     :max-length
                                     80
                                     :alphabet
                                     "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,.-_あ")))
                       (let ((octets (aitools.test.support:string-bytes text)))
                         (multiple-value-bind (document failure)
                             (parse-org-octets octets
                                               :source-name
                                               "generated.org")
                           (expect failure :to-be nil)
                           (expect (aitools.org.domain::org-document-p document)
                                   :to-be
                                   t)
                           (expect
                            (aitools.org.domain::org-document-source-name
                             document)
                            :to-equal
                            "generated.org")
                           (expect
                            (aitools.org.domain::org-document-octets document)
                            :to-equalp
                            octets)))))

(cl-weave:describe "aitools.org infrastructure query diagnostics"
                   (cl-weave:it
                    "maps query offset boundaries to clamped one-based positions"
                    (let ((match (format nil "one~%bad")))
                      (dolist
                          (case (quote ((0 1 1) (4 2 1) (7 2 4) (10 2 4))))
                        (destructuring-bind (offset line column) case
                          (let ((failure
                                 (aitools.org.infrastructure::%query-syntax-failure
                                  match
                                  (make-condition 'org-kit:org-query-error
                                                  :position
                                                  offset))))
                            (expect
                             (aitools.org.domain:org-failure-line failure)
                             :to-be
                             line)
                            (expect
                             (aitools.org.domain:org-failure-column failure)
                             :to-be
                             column)
                            (expect
                             (aitools.org.domain:org-failure-offset failure)
                             :to-be
                             offset)))))))

(cl-weave:describe "Org domain records"
  (cl-weave:it "constructs a document with its source, octets, and parsed value"
    (let* ((octets #(65 66))
           (engine-value (list :parsed))
           (document
             (aitools.org.domain:make-org-document
              "record.org" octets engine-value)))
      (expect (aitools.org.domain::org-document-p document) :to-be t)
      (expect (aitools.org.domain::org-document-source-name document)
              :to-equal "record.org")
      (expect (aitools.org.domain::org-document-octets document)
              :to-equalp octets)
      (expect (eq (aitools.org.domain:org-document-engine-value document)
                  engine-value)
              :to-be t)))
  (cl-weave:it "constructs failures with the default message and absent locations"
    (let ((failure (aitools.org.domain:make-org-failure)))
      (expect (aitools.org.domain:org-failure-p failure) :to-be t)
      (expect (aitools.org.domain:org-failure-kind failure) :to-be nil)
      (expect (aitools.org.domain:org-failure-message failure)
              :to-equal "Org operation failed")
      (expect (aitools.org.domain:org-failure-line failure) :to-be nil)
      (expect (aitools.org.domain:org-failure-column failure) :to-be nil)
      (expect (aitools.org.domain:org-failure-offset failure) :to-be nil)))
  (cl-weave:it "preserves failure kind, message, and source location"
    (let ((failure
            (aitools.org.domain:make-org-failure
             :kind :syntax :message "bad heading" :line 3 :column 4 :offset 18)))
      (expect (aitools.org.domain:org-failure-kind failure) :to-be :syntax)
      (expect (aitools.org.domain:org-failure-message failure)
              :to-equal "bad heading")
      (expect (aitools.org.domain:org-failure-line failure) :to-be 3)
      (expect (aitools.org.domain:org-failure-column failure) :to-be 4)
      (expect (aitools.org.domain:org-failure-offset failure) :to-be 18)))
  (cl-weave:it "rejects invalid failure field types"
    (cl-weave:signals type-error
      (aitools.org.domain:make-org-failure :message 9))
    (cl-weave:signals type-error
      (aitools.org.domain:make-org-failure :line "3"))
    (cl-weave:signals type-error
      (aitools.org.domain:make-org-failure :column "4"))
    (cl-weave:signals type-error
      (aitools.org.domain:make-org-failure :offset "18")))
  (cl-weave:it "constructs outline entries with defaults and supplied fields"
    (let ((default (aitools.org.domain:make-org-outline-entry))
          (entry
            (aitools.org.domain:make-org-outline-entry
             :kind :heading :name "Heading" :level 2 :line 5)))
      (expect (aitools.org.domain:org-outline-entry-kind default) :to-be nil)
      (expect (aitools.org.domain:org-outline-entry-name default) :to-equal "")
      (expect (aitools.org.domain:org-outline-entry-level default) :to-be nil)
      (expect (aitools.org.domain:org-outline-entry-line default) :to-be 1)
      (expect (aitools.org.domain:org-outline-entry-kind entry) :to-be :heading)
      (expect (aitools.org.domain:org-outline-entry-name entry)
              :to-equal "Heading")
      (expect (aitools.org.domain:org-outline-entry-level entry) :to-be 2)
      (expect (aitools.org.domain:org-outline-entry-line entry) :to-be 5)))
  (cl-weave:it "enforces outline entry field types"
    (cl-weave:signals type-error
      (aitools.org.domain:make-org-outline-entry :name nil))
    (cl-weave:signals type-error
      (aitools.org.domain:make-org-outline-entry :level "2"))
    (cl-weave:signals type-error
      (aitools.org.domain:make-org-outline-entry :line nil)))
  (cl-weave:it "constructs heading path entries with defaults and supplied fields"
    (let ((default (aitools.org.domain:make-org-heading-path-entry))
          (entry
            (aitools.org.domain:make-org-heading-path-entry
             :title "Root" :index 2)))
      (expect (aitools.org.domain:org-heading-path-entry-title default)
              :to-equal "")
      (expect (aitools.org.domain:org-heading-path-entry-index default)
              :to-be 1)
      (expect (aitools.org.domain:org-heading-path-entry-title entry)
              :to-equal "Root")
      (expect (aitools.org.domain:org-heading-path-entry-index entry)
              :to-be 2)))
  (cl-weave:it "enforces heading path entry field types"
    (cl-weave:signals type-error
      (aitools.org.domain:make-org-heading-path-entry :title nil))
    (cl-weave:signals type-error
      (aitools.org.domain:make-org-heading-path-entry :index nil)))
  (cl-weave:it "constructs query matches with defaults and supplied fields"
    (let ((default (aitools.org.domain:make-org-query-match))
          (match
            (aitools.org.domain:make-org-query-match
             :line 4 :level 2 :title "Task" :todo "TODO"
             :tags '("work" "urgent") :heading-path '("Root" "Task"))))
      (expect (aitools.org.domain:org-query-match-line default) :to-be 1)
      (expect (aitools.org.domain:org-query-match-level default) :to-be 1)
      (expect (aitools.org.domain:org-query-match-title default) :to-equal "")
      (expect (aitools.org.domain:org-query-match-todo default) :to-be nil)
      (expect (aitools.org.domain:org-query-match-tags default) :to-equal nil)
      (expect (aitools.org.domain:org-query-match-heading-path default)
              :to-equal nil)
      (expect (aitools.org.domain:org-query-match-line match) :to-be 4)
      (expect (aitools.org.domain:org-query-match-level match) :to-be 2)
      (expect (aitools.org.domain:org-query-match-title match) :to-equal "Task")
      (expect (aitools.org.domain:org-query-match-todo match) :to-equal "TODO")
      (expect (aitools.org.domain:org-query-match-tags match)
              :to-equal '("work" "urgent"))
      (expect (aitools.org.domain:org-query-match-heading-path match)
              :to-equal '("Root" "Task"))))
  (cl-weave:it "enforces query match field types"
    (cl-weave:signals type-error
      (aitools.org.domain:make-org-query-match :line nil))
    (cl-weave:signals type-error
      (aitools.org.domain:make-org-query-match :level nil))
    (cl-weave:signals type-error
      (aitools.org.domain:make-org-query-match :title nil))
    (cl-weave:signals type-error
      (aitools.org.domain:make-org-query-match :todo 1))
    (cl-weave:signals type-error
      (aitools.org.domain:make-org-query-match :tags :not-a-list))
    (cl-weave:signals type-error
      (aitools.org.domain:make-org-query-match :heading-path :not-a-list))))
