(in-package #:aitools.edit.test)
(describe "Org read-flow coverage"
  (it "serializes outline and query results"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "read.org"
              (format nil "#+TODO: TODO | DONE~%* TODO Parent :work:~%** TODO Child~%"))
         (let ((outline
                 (org-flow-ok
                  (function aitools.org.application:org-outline/k) boundaries
                  :path "read.org" :root *root* :limit 1)))
           (expect (> (field outline "total") 1) :to-be t)
           (expect (length (field outline "entries")) :to-be 1)
           (expect (field outline "truncated") :to-be t))
         (let ((query
                 (org-flow-ok
                  (function aitools.org.application:org-query/k) boundaries
                  :path "read.org" :root *root* :match "LEVEL=1"
                  :at "2026-10-19T09:00:00" :tz "UTC"
                  :dst-policy "compatible")))
           (expect (field query "count") :to-be 1)
           (let ((match (elt (field query "matches") 0)))
             (expect (json-field match "title") :to-equal "Parent")
             (expect (length (json-field match "heading_path")) :to-be 1)))))))
  (it "serializes agenda, clock report, and export results"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "agenda.org"
              (format nil "#+TODO: TODO | DONE~%* TODO Work~%SCHEDULED: <2026-10-19 Mon>~%"))
         (let ((agenda
                 (org-flow-ok
                  (function aitools.org.application:org-agenda/k) boundaries
                  :path "agenda.org" :root *root* :from "2026-10-19"
                  :through "2026-10-19" :future-repeats "all"
                  :at "2026-10-19T09:00:00" :tz "UTC"
                  :dst-policy "compatible")))
           (expect (field agenda "count") :to-be 1)
           (expect (json-field (elt (field agenda "entries") 0) "title")
                   :to-equal "Work"))
         (put "clock.org"
              (format nil "#+TODO: TODO | DONE~%* TODO Work~%:LOGBOOK:~%CLOCK: [2026-10-19 Mon 09:00]--[2026-10-19 Mon 09:30] =>  0:30~%:END:~%"))
         (let ((report
                 (org-flow-ok
                  (function aitools.org.application:org-clock-report/k)
                  boundaries :path "clock.org" :root *root* :match "/TODO"
                  :from "2026-10-19T09:00:00" :through "2026-10-19T10:00:00"
                  :at "2026-10-19T09:00:00" :tz "UTC"
                  :dst-policy "compatible")))
           (expect (field report "total_minutes") :to-be 30)
           (expect (json-field (elt (field report "rows") 0) "title")
                   :to-equal "Work"))
         (put "export.org" (format nil "* Export~%A paragraph.~%"))
         (let ((export
                 (org-flow-ok
                  (function aitools.org.application:org-export/k) boundaries
                  :path "export.org" :root *root* :backend :gfm)))
           (expect (field export "backend") :to-equal "gfm")
           (expect (stringp (field export "content")) :to-be t)))))))
