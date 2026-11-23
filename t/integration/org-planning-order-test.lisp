(in-package #:aitools.edit.test)

;;; Keyword order of `org planning set`, run through the CLI flow in process. The kit
;;; (0.3.3) puts the keyword being set first on the planning line and moves a keyword that
;;; is set again to the front. 0.3.2 appended a new keyword and replaced an old one in place.

(defun %planning-order (source kind timestamp)
  "The file text after `org planning set --kind KIND --timestamp TIMESTAMP` on SOURCE."
  (put "order.org" source)
  (%closed-run "org" "planning" "set" "order.org" "--match" "/TODO"
               "--kind" kind "--timestamp" timestamp)
  (text "order.org"))

(describe "Org planning keyword order"
  (it "puts the keyword being set first, as Emacs org-add-planning-info does"
    ;; Each expected line was produced by Org 9.8.10 `org-schedule` / `org-deadline`
    ;; on the same input (batch Emacs, temp buffer).
    (call-with-edit-workspace
     (lambda ()
       (dolist (case '(("SCHEDULED: <2026-11-18 Wed>" "deadline" "<2026-11-30>"
                        "DEADLINE: <2026-11-30 Mon> SCHEDULED: <2026-11-18 Wed>")
                       ("DEADLINE: <2026-11-30 Mon>" "scheduled" "<2026-11-18>"
                        "SCHEDULED: <2026-11-18 Wed> DEADLINE: <2026-11-30 Mon>")
                       ("DEADLINE: <2026-11-30 Mon> SCHEDULED: <2026-11-18 Wed>" "scheduled" "<2026-11-20>"
                        "SCHEDULED: <2026-11-20 Fri> DEADLINE: <2026-11-30 Mon>")
                       ("SCHEDULED: <2026-11-18 Wed> DEADLINE: <2026-11-30 Mon>" "deadline" "<2026-12-01>"
                        "DEADLINE: <2026-12-01 Tue> SCHEDULED: <2026-11-18 Wed>")
                       ("SCHEDULED: <2026-11-18 Wed> DEADLINE: <2026-11-30 Mon>" "scheduled" "<2026-11-20>"
                        "SCHEDULED: <2026-11-20 Fri> DEADLINE: <2026-11-30 Mon>")))
         (destructuring-bind (planning kind timestamp expected) case
           (expect (%planning-order (format nil "* TODO A~%~A~%" planning) kind timestamp)
                   :to-equal (format nil "* TODO A~%~A~%" expected)))))))
  (it "keeps CLOSED, which Emacs org-schedule and org-deadline remove"
    ;; aitools behavior, not Emacs: the UI commands drop CLOSED, `org planning set` does not.
    (call-with-edit-workspace
     (lambda ()
       (dolist (case '(("CLOSED: [2026-11-20 Fri 10:00]" "deadline" "<2026-11-30>"
                        "DEADLINE: <2026-11-30 Mon> CLOSED: [2026-11-20 Fri 10:00]")
                       ("CLOSED: [2026-11-20 Fri 10:00]" "scheduled" "<2026-11-30>"
                        "SCHEDULED: <2026-11-30 Mon> CLOSED: [2026-11-20 Fri 10:00]")
                       ("CLOSED: [2026-11-20 Fri 10:00] SCHEDULED: <2026-11-18 Wed>" "deadline" "<2026-11-30>"
                        "DEADLINE: <2026-11-30 Mon> CLOSED: [2026-11-20 Fri 10:00] SCHEDULED: <2026-11-18 Wed>")
                       ("CLOSED: [2026-11-20 Fri 10:00] SCHEDULED: <2026-11-18 Wed>" "scheduled" "<2026-11-30>"
                        "SCHEDULED: <2026-11-30 Mon> CLOSED: [2026-11-20 Fri 10:00]")))
         (destructuring-bind (planning kind timestamp expected) case
           (expect (%planning-order (format nil "* TODO A~%~A~%" planning) kind timestamp)
                   :to-equal (format nil "* TODO A~%~A~%" expected))))))))
