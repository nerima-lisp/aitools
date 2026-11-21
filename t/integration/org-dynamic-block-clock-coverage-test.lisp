(in-package #:aitools.edit.test)

(describe "Org dynamic block warnings through the dispatcher"
  (it "reports unparseable CLOCK lines with their workspace path, line, and source"
    (call-with-edit-workspace
     (lambda ()
       (put "tasks/bad.org" (format nil "* A~%~A~%~A~%" +dyn-clock+ +dyn-bad-clock+))
       (put "tasks/twin.org" (format nil "* A~%~A~%~A~%" +dyn-clock+ +dyn-bad-clock+))
       (put "tasks/shifted.org" (format nil "* A~%~A~%~A~%~A~%" +dyn-clock+ +dyn-clock+ +dyn-bad-clock+))
       ;; Same line number as the bad CLOCKs, different text.
       (put "tasks/other.org" (format nil "* A~%~A~%~A~%" +dyn-clock+ +dyn-clock+))
       ;; The line holds the text, but as a heading rather than a CLOCK.
       (put "tasks/heading.org" (format nil "* A~%~A~%* ~A~%" +dyn-clock+ +dyn-bad-clock+))
       (%dyn-agenda-report ":scope agenda :tstart \"2026-10-01\" :tend \"2026-11-01\"")
       (let* ((before (list (octets-of "tasks/bad.org") (octets-of "tasks/shifted.org")))
              (envelope (%dyn-ok (%dyn-update-arguments
                                  "report.org" 1 "--agenda-glob" "tasks/*.org")
                                 "org dynamic-block update"))
              (warnings (%dyn-warnings envelope)))
         (expect (sort (copy-list warnings) #'string< :key #'first)
                 :to-equal (list (list "tasks/bad.org" 3 +dyn-bad-clock+)
                                 (list "tasks/shifted.org" 4 +dyn-bad-clock+)
                                 (list "tasks/twin.org" 3 +dyn-bad-clock+)))
         (expect (search "File: tasks/bad.org" (text "report.org")) :to-be-truthy)
         (expect (list (octets-of "tasks/bad.org") (octets-of "tasks/shifted.org"))
                 :to-equalp before)))))
  (it "attributes a file-scope warning to the target document"
    (call-with-edit-workspace
     (lambda ()
       (put "task/other.org" (format nil "* A~%~A~%" +dyn-bad-clock+))
       (put "report.org"
            (format nil "#+BEGIN: clocktable :scope file~%old~%#+END:~%* A~%~A~%" +dyn-bad-clock+))
       (let ((envelope (%dyn-ok (%dyn-update-arguments "./report.org" 1)
                                "org dynamic-block update")))
         (expect (%dyn-warnings envelope)
                 :to-equal (list (list "report.org" 5 +dyn-bad-clock+))))))))

(describe "Org clock report warnings"
  (it "reports ignored CLOCK lines through the dispatcher"
    (call-with-edit-workspace
     (lambda ()
       (put "notes/report.org"
            (format nil "* Work~%~A~%~A~%" +dyn-clock+ +dyn-bad-clock+))
       (let ((envelope
               (%dyn-ok '("org" "clock" "report" "notes/report.org" "--match" "LEVEL=1"
                          "--from" "2026-10-01T00:00:00" "--through" "2026-10-02T00:00:00"
                          "--at" "2026-10-15T12:00:00" "--tz" "UTC")
                        "org clock report")))
         (expect (aitools.test.support:envelope-value envelope "total_minutes") :to-be 60)
         (expect (%dyn-warnings envelope)
                 :to-equal (list (list "notes/report.org" 3 +dyn-bad-clock+)))))))
  (it "returns an empty warnings vector when every CLOCK line parses"
    (let ((document
            (%org-coverage-document (format nil "* Work~%~A~%" +dyn-clock+))))
      (multiple-value-bind (report failure)
          (aitools.org.infrastructure::clock-report-org-document
           document "LEVEL=1" "2026-10-01T00:00:00" "2026-10-02T00:00:00"
           "2026-10-15T12:00:00" "UTC" "compatible" nil "report.org")
        (expect failure :to-be nil)
        (expect (getf report :total-minutes) :to-be 60)
        (expect (getf report :warnings) :to-equalp #()))))
  (it "returns one warning per ignored CLOCK line in source order"
    (let ((document
            (%org-coverage-document
             (format nil "* Work~%~A~%~A~%~A~%" +dyn-bad-clock+ +dyn-clock+
                     "CLOCK: [2025-02-01 Sat]--[2025-02-02 Sun]"))))
      (multiple-value-bind (report failure)
          (aitools.org.infrastructure::clock-report-org-document
           document "LEVEL=1" "2026-10-01T00:00:00" "2026-10-02T00:00:00"
           "2026-10-15T12:00:00" "UTC" "compatible" nil "report.org")
        (expect failure :to-be nil)
        (expect (getf report :total-minutes) :to-be 60)
        (expect (%dyn-warning-entries (getf report :warnings))
                :to-equal (list (list "report.org" 2 +dyn-bad-clock+)
                                (list "report.org" 4 "CLOCK: [2025-02-01 Sat]--[2025-02-02 Sun]")))))))
