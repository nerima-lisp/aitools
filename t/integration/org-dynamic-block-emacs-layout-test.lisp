(in-package #:aitools.edit.test)

;;; The expected documents were produced by Org 9.8.10 in `emacs --batch -Q`
;;; with the clock pinned to 2026-11-22 10:00 in the zone under test.

(defparameter +layout-clock+
  "CLOCK: [2026-10-01 Thu 10:00]--[2026-10-01 Thu 11:00] =>  1:00")

(defun %layout-update (source zone &rest options)
  "Update block 1 of SOURCE at 2026-11-22T10:00:00 in ZONE and return the new text."
  (put "report.org" source)
  (multiple-value-bind (code envelope)
      (aitools.test.support:run-aitools
       *root*
       (append (list "org" "dynamic-block" "update" "report.org" "--block" "1"
                     "--at" "2026-11-22T10:00:00" "--tz" zone)
               options))
    (expect (list code (aitools.test.support:envelope-value envelope "status"))
            :to-equal (list 0 "ok"))
    (text "report.org")))

(defun %layout-lines (&rest lines)
  (format nil "~{~A~%~}" lines))

(defparameter +layout-file-source+
  (%layout-lines "* Task" +layout-clock+
                 "#+BEGIN: clocktable :scope file" "#+END:"))

(defparameter +layout-file-expected+
  (%layout-lines "* Task" +layout-clock+
                 "#+BEGIN: clocktable :scope file"
                 "#+CAPTION: Clock summary at [2026-11-22 Sun 10:00]"
                 "| Headline     | Time   |"
                 "|--------------+--------|"
                 "| *Total time* | *1:00* |"
                 "|--------------+--------|"
                 "| Task         | 1:00   |"
                 "#+END:"))

(defparameter +layout-agenda-expected+
  (%layout-lines "#+TITLE: Report"
                 "#+BEGIN: clocktable :scope agenda"
                 "#+CAPTION: Clock summary at [2026-11-22 Sun 10:00]"
                 "| File      | Headline         | Time   |"
                 "|-----------+------------------+--------|"
                 "|           | ALL *Total time* | *1:00* |"
                 "|-----------+------------------+--------|"
                 "| empty.org | *File time*      | *0:00* |"
                 "|-----------+------------------+--------|"
                 "| work.org  | *File time*      | *1:00* |"
                 "|           | Task             | 1:00   |"
                 "#+END:"))

(describe "Org dynamic block update writes the Emacs clocktable layout"
  (it "writes one captioned table with a File column over the agenda files"
    (call-with-edit-workspace
     (lambda ()
       (put "task/work.org" (%layout-lines "* Task" +layout-clock+))
       (put "task/empty.org" (%layout-lines "* Idea"))
       (expect (%layout-update
                (%layout-lines "#+TITLE: Report" "#+BEGIN: clocktable :scope agenda" "#+END:")
                "Asia/Tokyo" "--agenda-glob" "task/*.org")
               :to-equal +layout-agenda-expected+))))
  (it "writes the caption for a file scope and an all-time table without a range in any zone"
    (call-with-edit-workspace
     (lambda ()
       (dolist (zone '("UTC" "Asia/Tokyo" "America/New_York"))
         (expect (list zone (%layout-update +layout-file-source+ zone))
                 :to-equal (list zone +layout-file-expected+)))))))
