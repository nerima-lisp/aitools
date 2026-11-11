(in-package #:aitools.edit.test)
(describe "Org planning timestamp adapter coverage"
  (it "counts timestamp tokens and AST components"
    (expect (aitools.org.infrastructure::%timestamp-content-tokens "<   >")
            :to-equal nil)
    (expect (aitools.org.infrastructure::%timestamp-content-tokens
             "< 2026-10-19   +1d >")
            :to-equal (list "2026-10-19" "+1d"))
    (expect
     (aitools.org.domain:org-failure-kind
      (nth-value 1
       (aitools.org.infrastructure::%planning-timestamp-node
        "<2026-10-19> <2026-10-20>")))
     :to-be :argument)
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
     :to-be-truthy))
  (it "rejects invalid timestamp shapes, kinds, and cross-date ranges"
    (expect
     (aitools.org.domain:org-failure-kind
      (nth-value 1
       (aitools.org.infrastructure::validate-planning-timestamp
        "scheduled" "<2026-10-19 23:00>--<2026-10-20 01:00>")))
     :to-be :argument)
    (expect
     (aitools.org.domain:org-failure-kind
      (nth-value 1
       (aitools.org.infrastructure::validate-planning-timestamp
        "scheduled" "[2026-10-19]")))
     :to-be :argument)
    (dolist (case (list (list "invalid" "<2026-10-19>")
                        (list "scheduled" "<2026-02-30>")
                        (list "scheduled" nil)
                        (list "scheduled" 7)
                        (list "scheduled" "")
                        (list "scheduled" (format nil "<2026-10-19>~%"))
                        (list "scheduled" (format nil "<2026-10-19>~C>" #\Return))))
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
