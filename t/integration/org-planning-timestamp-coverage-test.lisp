(in-package #:aitools.edit.test)
(describe "Org planning timestamp adapter coverage"
  (it "counts timestamp tokens and AST components"
    (expect (aitools.org.infrastructure::%timestamp-content-tokens "<   >")
            :to-equal nil)
    (expect (aitools.org.infrastructure::%timestamp-content-tokens
             "< 2026-10-19   +1d >")
            :to-equal (list "2026-10-19" "+1d"))
    (dolist (case (list (list "<2026-10-19>" 1)
                        (list "<2026-10-19 09:00>" 2)
                        (list "<2026-10-19 +1d>" 2)
                        (list "<2026-10-19 -2d>" 2)
                        (list "<2026-10-19 +1d -2d>" 3)))
      (multiple-value-bind (view failure)
          (aitools.org.infrastructure::validate-planning-timestamp
           "scheduled" (first case))
        (expect failure :to-be nil)
        (expect (aitools.org.infrastructure::%timestamp-component-count view)
                :to-be (second case)))))
  (it "parses one timestamp and maps malformed timestamp documents"
    (multiple-value-bind (node failure)
        (aitools.org.infrastructure::%planning-timestamp-node "<2026-10-19>")
      (expect (org-kit:node-type node) :to-be :timestamp)
      (expect failure :to-be nil))
    (expect
     (aitools.org.domain:org-failure-kind
      (nth-value 1
       (aitools.org.infrastructure::%planning-timestamp-node
        "<2026-10-19> <2026-10-20>")))
     :to-be :argument)
    (dolist (timestamp (list "tomorrow" "#+TODO:"
                             (format nil "bad~C" (code-char 0))))
      (expect
       (aitools.org.domain:org-failure-kind
        (nth-value 1
         (aitools.org.infrastructure::%planning-timestamp-node timestamp)))
       :to-be :argument)))
  (it "compares calendar dates and accepts one-node time ranges"
    (let ((start (org-kit:make-org-time-value "2026-10-19T09:00:00"))
          (same-day (org-kit:make-org-time-value "2026-10-19T10:00:00"))
          (next-day (org-kit:make-org-time-value "2026-10-20T09:00:00")))
      (expect (aitools.org.infrastructure::%same-calendar-date-p start same-day)
              :to-be t)
      (expect (aitools.org.infrastructure::%same-calendar-date-p start next-day)
              :to-be nil))
    (dolist (case (list (list "scheduled" "<2026-10-19 09:00-10:00>")
                        (list "closed" "[2026-10-19 09:00-10:00]")))
      (multiple-value-bind (view failure)
          (aitools.org.infrastructure::validate-planning-timestamp
           (first case) (second case))
        (expect view :to-be-truthy)
        (expect (org-kit:org-timestamp-view-end view) :to-be-truthy)
        (expect failure :to-be nil))))
  (it "validates timestamp shape, kind, and date ranges"
    (dolist (timestamp (list "<2026-10-19>"
                             "<2026-10-19 09:00-10:00>"
                             "<2026-10-19 +1d>"
                             "<2026-10-19 -2d>"))
      (expect
       (nth-value 0
        (aitools.org.infrastructure::validate-planning-timestamp
         "scheduled" timestamp))
       :to-be-truthy))
    (expect
     (nth-value 0
      (aitools.org.infrastructure::validate-planning-timestamp
       "closed" "[2026-10-19]"))
     :to-be-truthy)
    (dolist (case (list (list "invalid" "<2026-10-19>")
                        (list "scheduled" "[2026-10-19]")
                        (list "closed" "<2026-10-19>")
                        (list "scheduled" "<2026-10-19 10:00-09:00>")
                        (list "scheduled" "<2026-10-19 Mon +1d +2w>")
                        (list "scheduled" "<2026-02-30>")
                        (list "scheduled" nil)
                        (list "scheduled" 7)
                        (list "scheduled" "")
                        (list "scheduled" "x")
                        (list "scheduled" "<>")
                        (list "scheduled" (format nil "<2026-10-19~%>"))
                        (list "scheduled"
                              (format nil "<2026-10-19~C>" #\Return))))
      (expect
       (aitools.org.domain:org-failure-kind
        (nth-value 1
         (aitools.org.infrastructure::validate-planning-timestamp
          (first case) (second case))))
       :to-be :argument)))
  (it "counts absent, indented, and repeated planning markers"
    (dolist (case
              (list (list (format nil "* Empty~%") 0)
                    (list (format nil "* Entry~%   Note~%") 0)
                    (list (format nil
                                  "* Entry~%   SCHEDULED: <2026-10-20> SCHEDULED: <2026-10-22>~%")
                          2)))
      (let* ((document (%org-coverage-document (first case)))
             (engine (aitools.org.domain:org-document-engine-value document))
             (heading
               (find :headline
                     (org-kit:node-children (org-kit:document-root engine))
                     :key (function org-kit:node-type))))
        (expect
         (aitools.org.infrastructure::%planning-marker-count
          engine heading "SCHEDULED:")
         :to-be (second case))))))


