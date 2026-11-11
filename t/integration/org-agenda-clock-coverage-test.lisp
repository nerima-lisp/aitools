(in-package #:aitools.edit.test)
(defmacro %with-org-adapter-condition (function condition &body body)
  `(cl-weave:with-mocked-functions
       (((symbol-function (quote ,function))
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (error ,condition))))
     ,@body))
(describe "Org agenda and clock adapter paths"
  (it "maps diary entries and validates agenda inputs"
    (let ((document
            (%org-coverage-document
             "%%(diary-date 1 2 2025) Personal entry")))
      (multiple-value-bind (agenda failure)
          (aitools.org.infrastructure::agenda-org-document
           document "2025-01-01" "2025-01-03" "2025-01-01T12:00:00"
           "UTC" "compatible" nil)
        (expect failure :to-be nil)
        (expect (getf agenda :count) :to-be 1)
        (let ((entry (first (getf agenda :entries))))
          (expect (getf entry :date) :to-equal "2025-01-02")
          (expect (getf entry :kind) :to-equal "timestamp")
          (expect (getf entry :line) :to-be 1)
          (expect (getf entry :title) :to-equal " Personal entry")
          (expect (getf entry :todo) :to-be nil)
          (expect (getf entry :tags) :to-be nil)))
      (expect
       (aitools.org.infrastructure::%agenda-entry-kind-name
        (org-kit:make-agenda-entry :kind "SCHEDULED"))
       :to-equal "scheduled")
      (expect
       (aitools.org.infrastructure::%agenda-entry-kind-name
        (org-kit:make-agenda-entry :kind 42))
       :to-equal "unknown")
      (expect
       (aitools.org.infrastructure::%agenda-entry-line
        (org-kit:make-agenda-entry))
       :to-be nil)
      (dolist (arguments
               (list
                (list "bad" "2025-01-03" "2025-01-01T12:00:00"
                      "UTC" "compatible" nil)
                (list "2025-01-01" "bad" "2025-01-01T12:00:00"
                      "UTC" "compatible" nil)
                (list "2025-01-01" "2025-01-03" "bad"
                      "UTC" "compatible" nil)
                (list "2025-01-01" "2025-01-03" "2025-01-01T12:00:00"
                      "UTC" "compatible" "sometimes")))
        (multiple-value-bind (agenda failure)
            (apply
             (function aitools.org.infrastructure::agenda-org-document)
             (cons document arguments))
          (expect agenda :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :argument)))))
  (it "maps clock edit selection, state, and engine failures"
    (let* ((document
             (%org-coverage-document
              (format nil "#+TODO: TODO | DONE~%* TODO Work~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (options (list :match "/TODO" :at "2026-10-19T09:00:00"
                          :tz "UTC" :dst-policy "compatible")))
      (multiple-value-bind (updated fields failure)
          (aitools.org.infrastructure::%org-clock-edit
           engine (list :match "/Missing" :at "2026-10-19T09:00:00"
                        :tz "UTC" :dst-policy "compatible") :in)
        (expect updated :to-be nil)
        (expect fields :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure)
                :to-be :selection-no-match))
      (multiple-value-bind (updated fields failure)
          (aitools.org.infrastructure::%org-clock-edit engine options :out)
        (expect updated :to-be nil)
        (expect fields :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure)
                :to-be :selection-no-match))
      (multiple-value-bind (clocked fields failure)
          (aitools.org.infrastructure::%org-clock-edit engine options :in)
        (expect failure :to-be nil)
        (expect (caar fields) :to-equal "timestamp")
        (multiple-value-bind (updated fields duplicate-failure)
            (aitools.org.infrastructure::%org-clock-edit clocked options :in)
          (expect updated :to-be nil)
          (expect fields :to-be nil)
          (expect (aitools.org.domain:org-failure-kind duplicate-failure)
                  :to-be :argument))
        (multiple-value-bind (closed fields close-failure)
            (aitools.org.infrastructure::%org-clock-edit clocked options :out)
          (expect close-failure :to-be nil)
          (expect (caar fields) :to-equal "interval")
          (multiple-value-bind (updated fields stopped-failure)
              (aitools.org.infrastructure::%org-clock-edit closed options :out)
            (expect updated :to-be nil)
            (expect fields :to-be nil)
            (expect (aitools.org.domain:org-failure-kind stopped-failure)
                    :to-be :selection-no-match))))
      (%with-org-adapter-condition
          org-kit:clock-in
          (make-condition 'org-kit:org-time-error :message "bad clock time")
        (multiple-value-bind (updated fields failure)
            (aitools.org.infrastructure::%org-clock-edit engine options :in)
          (expect updated :to-be nil)
          (expect fields :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :argument)))
      (%with-org-adapter-condition
          org-kit:clock-in
          (make-condition 'org-kit:org-resource-limit-error
                           :message "clock limit" :limit-name :max-output-length
                           :limit 1 :actual 2)
        (multiple-value-bind (updated fields failure)
            (aitools.org.infrastructure::%org-clock-edit engine options :in)
          (expect updated :to-be nil)
          (expect fields :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :too-large)))
      (%with-org-adapter-condition
          org-kit:clock-in
          (make-condition 'org-kit:org-kit-error :message "clock failed")
        (multiple-value-bind (updated fields failure)
            (aitools.org.infrastructure::%org-clock-edit engine options :in)
          (expect updated :to-be nil)
          (expect fields :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :internal)))))
  (it "maps clock report selection, range, and engine failures"
    (let* ((document
             (%org-coverage-document
              (format nil
                      "#+TODO: TODO | DONE~%* TODO Work~%:LOGBOOK:~%CLOCK: [2026-10-19 Mon 09:00]--[2026-10-19 Mon 09:30] =>  0:30~%:END:~%")))
           (at "2026-10-19T09:00:00")
           (from "2026-10-19T09:00:00")
           (through "2026-10-19T10:00:00"))
      (multiple-value-bind (report failure)
          (aitools.org.infrastructure::clock-report-org-document
           document "/Missing" from through at "UTC" "compatible")
        (expect report :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure)
                :to-be :selection-no-match))
      (multiple-value-bind (report failure)
          (aitools.org.infrastructure::clock-report-org-document
           document "/TODO" through from at "UTC" "compatible")
        (expect report :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure)
                :to-be :argument))
      (%with-org-adapter-condition
          org-kit:clocktable-data
          (make-condition 'org-kit:org-clock-error
                           :reason :clock-not-running :message "clock report failed")
        (multiple-value-bind (report failure)
            (aitools.org.infrastructure::clock-report-org-document
             document "/TODO" from through at "UTC" "compatible")
          (expect report :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :selection-no-match)))
      (%with-org-adapter-condition
          org-kit:clocktable-data
          (make-condition 'org-kit:org-time-error :message "invalid report time")
        (multiple-value-bind (report failure)
            (aitools.org.infrastructure::clock-report-org-document
             document "/TODO" from through at "UTC" "compatible")
          (expect report :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :argument)))
      (%with-org-adapter-condition
          org-kit:clocktable-data
          (make-condition 'org-kit:org-resource-limit-error
                           :message "report limit" :limit-name :max-output-length
                           :limit 1 :actual 2)
        (multiple-value-bind (report failure)
            (aitools.org.infrastructure::clock-report-org-document
             document "/TODO" from through at "UTC" "compatible")
          (expect report :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :too-large)))
      (%with-org-adapter-condition
          org-kit:clocktable-data
          (make-condition 'org-kit:org-query-error
                           :query "/TODO" :position 1 :message "bad query")
        (multiple-value-bind (report failure)
            (aitools.org.infrastructure::clock-report-org-document
             document "/TODO" from through at "UTC" "compatible")
          (expect report :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :query-syntax)))
      (%with-org-adapter-condition
          org-kit:clocktable-data
          (make-condition 'org-kit:org-kit-error :message "report failed")
        (multiple-value-bind (report failure)
            (aitools.org.infrastructure::clock-report-org-document
             document "/TODO" from through at "UTC" "compatible")
          (expect report :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :internal))))))
