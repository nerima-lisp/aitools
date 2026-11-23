(in-package #:aitools.edit.test)

;;; Planning lines that start with CLOSED: (a DONE heading Emacs closed) must archive, and
;;; the kit writes planning keywords in Emacs order. These specs need cl-org-kit 0.3.3.

(defparameter +closed-source+
  (format nil "#+TODO: TODO | DONE~%* Cat~%** DONE one~%CLOSED: [2026-11-20 Fri 10:00]~%"))

(defparameter +closed-scheduled-source+
  (format nil "#+TODO: TODO | DONE~%* Cat~%** DONE one~%CLOSED: [2026-11-20 Fri 10:00] SCHEDULED: <2026-11-18 Wed>~%"))

(defun %closed-engine (text)
  (aitools.org.domain:org-document-engine-value (%org-coverage-document text)))

(defun %closed-archive (source)
  "The (values source-text target-text failure) of archiving heading `one` of SOURCE."
  (multiple-value-bind (source-result target-result failure)
      (aitools.org.infrastructure::archive-org-documents
       (%closed-engine source) (%closed-engine (format nil "* Archived Tasks~%"))
       (list :match "ITEM=\"one\"" :target-heading "Archived Tasks"
             :at "2026-11-22T10:00:00" :tz "Asia/Tokyo" :dst-policy "compatible")
       nil)
    (values (and source-result (org-kit:document-text source-result))
            (and target-result (org-kit:document-text target-result))
            failure)))

(defun %closed-run (&rest arguments)
  "Run ARGUMENTS in the edit workspace with a fixed time context; return the envelope."
  (multiple-value-bind (code envelope)
      (aitools.test.support:run-aitools
       *root* (append arguments '("--at" "2026-11-22T10:00:00" "--tz" "Asia/Tokyo")))
    (expect code :to-be 0)
    envelope))

(describe "Org planning lines that start with CLOSED"
  (it "archives a heading whose planning line is only CLOSED"
    (multiple-value-bind (source target failure) (%closed-archive +closed-source+)
      (expect failure :to-be nil)
      (expect source :to-equal (format nil "#+TODO: TODO | DONE~%* Cat~%"))
      (expect (search (format nil "** DONE one~%CLOSED: [2026-11-20 Fri 10:00]~%:PROPERTIES:~%:ARCHIVE_TIME: 2026-11-22 Sun 10:00~%")
                      target)
              :to-be-truthy)))
  (it "archives a heading whose planning line is CLOSED followed by SCHEDULED"
    (multiple-value-bind (source target failure) (%closed-archive +closed-scheduled-source+)
      (expect failure :to-be nil)
      (expect source :to-equal (format nil "#+TODO: TODO | DONE~%* Cat~%"))
      (expect (search (format nil "** DONE one~%CLOSED: [2026-11-20 Fri 10:00] SCHEDULED: <2026-11-18 Wed>~%:PROPERTIES:~%:ARCHIVE_TIME: 2026-11-22 Sun 10:00~%")
                      target)
              :to-be-truthy)))
  (it "archives CLOSED-first headings through the CLI into another file"
    (call-with-edit-workspace
     (lambda ()
       (dolist (source (list +closed-source+ +closed-scheduled-source+))
         (put "source.org" source)
         (put "archive.org" (format nil "* Archived Tasks~%"))
         (%closed-run "org" "archive" "source.org" "--to" "archive.org"
                      "--target-heading" "Archived Tasks" "--match" "ITEM=\"one\"")
         (expect (text "source.org") :to-equal (format nil "#+TODO: TODO | DONE~%* Cat~%"))
         (expect (search (format nil "** DONE one~%CLOSED: [2026-11-20 Fri 10:00]")
                         (text "archive.org"))
                 :to-be-truthy)))))
  (it "writes CLOSED before an existing SCHEDULED when a heading is completed"
    (call-with-edit-workspace
     (lambda ()
       (put "todo.org" (format nil "#+TODO: TODO | DONE~%#+STARTUP: logdone~%* TODO Rel~%SCHEDULED: <2026-10-20 Tue>~%"))
       (%closed-run "org" "todo" "set" "todo.org" "--match" "/TODO" "--state" "DONE")
       (expect (text "todo.org")
               :to-equal (format nil "#+TODO: TODO | DONE~%#+STARTUP: logdone~%* DONE Rel~%CLOSED: [2026-11-22 Sun 10:00] SCHEDULED: <2026-10-20 Tue>~%")))))
  (it "writes CLOSED on an indented planning line and keeps its indentation"
    ;; Both expected lines match Org 9.8.10 `org-todo` with `org-log-done` set to `time`.
    (call-with-edit-workspace
     (lambda ()
       (dolist (case '(("  SCHEDULED: <2026-11-20 Fri>" "" "  CLOSED: [2026-11-22 Sun 10:00] SCHEDULED: <2026-11-20 Fri>")
                       ("   SCHEDULED: <2026-11-20 Fri>" "body" "   CLOSED: [2026-11-22 Sun 10:00] SCHEDULED: <2026-11-20 Fri>")))
         (destructuring-bind (planning body expected) case
           (put "indent.org" (format nil "#+TODO: TODO | DONE~%#+STARTUP: logdone~%* TODO H~%~A~%~A" planning (if (string= body "") "" (format nil "~A~%" body))))
           (%closed-run "org" "todo" "set" "indent.org" "--match" "/TODO" "--state" "DONE")
           (expect (text "indent.org")
                   :to-equal (format nil "#+TODO: TODO | DONE~%#+STARTUP: logdone~%* DONE H~%~A~%~A"
                                     expected (if (string= body "") "" (format nil "~A~%" body)))))))))
  (it "keeps the blank line after a planning line that planning delete empties"
    (call-with-edit-workspace
     (lambda ()
       (put "delete.org" (format nil "* TODO A~%SCHEDULED: <2026-11-30 Mon>~%~%body~%"))
       (%closed-run "org" "planning" "delete" "delete.org" "--match" "/TODO" "--kind" "scheduled")
       (expect (text "delete.org") :to-equal (format nil "* TODO A~%~%body~%"))))))
