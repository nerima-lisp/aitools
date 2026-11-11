(in-package #:aitools.edit.test)
(defun %org-coverage-document (source)
  (multiple-value-bind (document failure)
      (aitools.org.infrastructure:parse-org-octets
       (sb-ext:string-to-octets source :external-format :utf-8))
    (expect failure :to-be nil)
    document))
(describe "Org infrastructure coverage"
  (it "maps parsed outlines, query matches, and supported exports"
    (let ((document
            (%org-coverage-document
             (format nil "#+TODO: TODO | DONE~%* TODO Parent :work:~%** TODO Child~%"))))
      (multiple-value-bind (outline failure)
          (aitools.org.infrastructure::outline-org-document document 1)
        (expect failure :to-be nil)
        (expect (length (getf outline :entries)) :to-be 1)
        (expect (getf outline :truncated) :to-be t))
      (multiple-value-bind (matches failure)
          (aitools.org.infrastructure::query-org-document
           document "LEVEL=2" "2026-10-19T09:00:00" "UTC" "compatible")
        (expect failure :to-be nil)
        (expect (length matches) :to-be 1)
        (let ((match (first matches)))
          (expect (aitools.org.domain:org-query-match-title match)
                  :to-equal "Child")
          (expect (length
                   (aitools.org.domain:org-query-match-heading-path match))
                  :to-be 2)))
      (multiple-value-bind (content failure)
          (aitools.org.infrastructure::export-org-document document :gfm)
        (expect failure :to-be nil)
        (expect (stringp content) :to-be t))))
  (it "reports parse, export, and agenda outcomes"
    (multiple-value-bind (document failure)
        (aitools.org.infrastructure:parse-org-octets #(255))
      (expect document :to-be nil)
      (expect (aitools.org.domain:org-failure-kind failure) :to-be :invalid-utf8))
    (multiple-value-bind (document failure)
        (aitools.org.infrastructure:parse-org-octets
         (sb-ext:string-to-octets "#+TODO:" :external-format :utf-8))
      (expect document :to-be nil)
      (expect (aitools.org.domain:org-failure-kind failure) :to-be :syntax))
    (let ((aitools.org.infrastructure::*org-kit-limits* (org-kit:make-limits :max-input-length 1)))
      (multiple-value-bind (document failure)
          (aitools.org.infrastructure:parse-org-octets
           (sb-ext:string-to-octets "* A" :external-format :utf-8))
        (expect document :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :too-large)))
    (let ((document
            (%org-coverage-document
             (format nil "* Link~%[[file:target.org][internal]]~%"))))
      (multiple-value-bind (content failure)
          (aitools.org.infrastructure::export-org-document document :gfm)
        (expect content :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :internal-link)))
    (let ((document
            (%org-coverage-document
             (format nil "#+TODO: TODO | DONE~%* TODO Work~%SCHEDULED: <2026-10-19 Mon>~%"))))
      (dolist (repeat-mode (list nil "all" "next" "none"))
        (multiple-value-bind (agenda failure)
            (aitools.org.infrastructure::agenda-org-document
             document "2026-10-19" "2026-10-19" "2026-10-19T09:00:00"
             "UTC" "compatible" repeat-mode)
          (expect failure :to-be nil)
          (expect (getf agenda :count) :to-be 1)
          (expect (getf (first (getf agenda :entries)) :kind) :to-be-truthy)))
      (expect (aitools.org.infrastructure::%agenda-repeat-mode "sometimes")
              :to-be nil)
      (multiple-value-bind (agenda failure)
          (aitools.org.infrastructure::agenda-org-document
           document "2026-10-19" "2026-10-19" "2026-10-19T09:00:00"
           "UTC" "compatible" "sometimes")
        (expect agenda :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument))))
  (it "reports and edits clock data"
    (let ((document
            (%org-coverage-document
             (format nil "#+TODO: TODO | DONE~%* TODO Work~%:LOGBOOK:~%CLOCK: [2026-10-19 Mon 09:00]--[2026-10-19 Mon 09:30] =>  0:30~%:END:~%"))))
      (multiple-value-bind (report failure)
          (aitools.org.infrastructure::clock-report-org-document
           document "/TODO" "2026-10-19T09:00:00" "2026-10-19T10:00:00"
           "2026-10-19T09:00:00" "UTC" "compatible")
        (expect failure :to-be nil)
        (expect (getf report :total-minutes) :to-be 30)
        (expect (length (getf report :rows)) :to-be 1)))
    (let* ((document
             (%org-coverage-document
              (format nil "#+TODO: TODO | DONE~%* TODO Work~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (options (list :match "/TODO" :at "2026-10-19T09:00:00"
                          :tz "UTC" :dst-policy "compatible")))
      (multiple-value-bind (clocked fields failure)
          (aitools.org.infrastructure::%org-clock-edit engine options :in)
        (expect failure :to-be nil)
        (expect fields :to-be-truthy)
        (multiple-value-bind (closed interval close-failure)
            (aitools.org.infrastructure::%org-clock-edit
             clocked (list :match "/TODO" :at "2026-10-19T09:30:00" :tz "UTC" :dst-policy "compatible") :out)
          (expect close-failure :to-be nil)
          (expect interval :to-be-truthy)
          (multiple-value-bind (octets write-failure)
              (aitools.org.infrastructure::write-org-octets closed)
            (expect write-failure :to-be nil)
            (expect (plusp (length octets)) :to-be t)))))
    (multiple-value-bind (range failure)
        (aitools.org.infrastructure::%clock-report-range
         "2026-10-19T10:00:00" "2026-10-19T09:00:00")
      (expect range :to-be nil)
      (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)))
  (it "applies planning and table edits"
    (let* ((document
             (%org-coverage-document
              (format nil "#+TODO: TODO | DONE~%* TODO Release~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (options (list :match "/TODO" :at "2026-10-19T09:00:00"
                          :tz "UTC" :dst-policy "compatible")))
      (multiple-value-bind (planned fields failure)
          (aitools.org.infrastructure::edit-org-document
           engine :planning-set
           (append options (list :kind "scheduled" :timestamp "<2026-10-20>")))
        (expect failure :to-be nil)
        (expect (cdr (assoc "kind" fields :test (function string=)))
                :to-equal "scheduled")
        (multiple-value-bind (removed fields failure)
            (aitools.org.infrastructure::edit-org-document
             planned :planning-delete
             (append options (list :kind "scheduled")))
          (expect failure :to-be nil)
          (expect removed :to-be-truthy)
          (expect (cdr (assoc "kind" fields :test (function string=)))
                  :to-equal "scheduled")))
      (multiple-value-bind (unchanged fields failure)
          (aitools.org.infrastructure::edit-org-document
           engine :property-delete
           (append options (list :name "OWNER")))
        (expect failure :to-be nil)
        (expect unchanged :to-be engine)
        (expect fields :to-be-truthy)))
    (let ((document
            (%org-coverage-document
             (format nil "| Name | Count |~%|------+-------|~%| pear | 2 |~%| apple | 1 |~%"))))
      (let ((engine (aitools.org.domain:org-document-engine-value document)))
        (multiple-value-bind (sorted fields failure)
            (aitools.org.infrastructure::edit-org-table-document
             engine :table-sort
             (list :table 1 :column 2 :type "numeric" :order "ascending"))
          (expect failure :to-be nil)
          (expect (cdr (assoc "table" fields :test (function string=))) :to-be 1)
          (expect sorted :to-be-truthy))
        (multiple-value-bind (aligned fields failure)
            (aitools.org.infrastructure::edit-org-table-document
             engine :table-align (list :table 1))
          (expect failure :to-be nil)
          (expect (cdr (assoc "table" fields :test (function string=))) :to-be 1)
          (expect aligned :to-be-truthy))
        (multiple-value-bind (updated fields failure)
            (aitools.org.infrastructure::edit-org-table-document
             engine :table-align (list :table 0))
          (expect updated :to-be nil)
          (expect fields :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument))))))