(in-package #:aitools.e2e.test)

(describe "aitools e2e org --heading-path writes and reports"
  (define-org-case ("org property, planning, and clock writes accept --heading-path" ws)
    (put ws "hp.org" (hp-text +hp-lines+))
    (let* ((original (file-octets ws "hp.org"))
           (second-requirement (hp-path-json ws "hp.org" +hp-tres +hp-requirement 2))
           (first-requirement (hp-path-json ws "hp.org" +hp-tres +hp-requirement 1))
           (implement (hp-path-json ws "hp.org" +hp-tres "実装する" 1))
           (property-text (hp-edit 8 "** TODO 要件を確認する" ":PROPERTIES:" ":OWNER: A" ":END:")))
      (let ((set (org-ok ws (org-write-time-args
                             (append (list "org" "property" "set" "hp.org")
                                     (hp-heading-path second-requirement)
                                     '("--name" "OWNER" "--value" "A")))
                         "org property set")))
        (expect (jget set "changed") :to-be t)
        (expect (file-octets ws "hp.org") :to-equalp (octets property-text))
        (let ((deleted (org-ok ws (org-write-time-args
                                   (append (list "org" "property" "delete" "hp.org")
                                           (hp-heading-path second-requirement)
                                           '("--name" "OWNER")))
                               "org property delete")))
          (expect (jget deleted "changed") :to-be t)
          (expect (file-octets ws "hp.org") :to-equalp original)))
      (let ((set (org-ok ws (org-write-time-args
                             (append (list "org" "planning" "set" "hp.org")
                                     (hp-heading-path first-requirement)
                                     '("--kind" "scheduled" "--timestamp" "<2026-10-20>")))
                         "org planning set")))
        (expect (jget set "changed") :to-be t)
        (expect (file-octets ws "hp.org")
                :to-equalp (octets (hp-edit 6 "** TODO 要件を確認する" "SCHEDULED: <2026-10-20 Tue>")))
        (let ((deleted (org-ok ws (org-write-time-args
                                   (append (list "org" "planning" "delete" "hp.org")
                                           (hp-heading-path first-requirement)
                                           '("--kind" "scheduled")))
                               "org planning delete")))
          (expect (jget deleted "changed") :to-be t)
          (expect (file-octets ws "hp.org") :to-equalp original)))
      (let ((clock-in (org-ok ws (org-write-time-args
                                  (append (list "org" "clock" "in" "hp.org")
                                          (hp-heading-path implement)))
                              "org clock in")))
        (expect (jget clock-in "timestamp") :to-equal "[2026-10-19 Mon 09:00]")
        (expect (file-octets ws "hp.org")
                :to-equalp (octets (hp-edit 7 "** TODO 実装する" ":LOGBOOK:"
                                            "CLOCK: [2026-10-19 Mon 09:00]" ":END:"))))
      (let ((clock-out (org-ok ws (org-write-time-args
                                   (append (list "org" "clock" "out" "hp.org")
                                           (hp-heading-path implement))
                                   "2026-10-19T09:30:00")
                               "org clock out")))
        (expect (jget clock-out "interval")
                :to-equal "[2026-10-19 Mon 09:00]--[2026-10-19 Mon 09:30] => 0:30")
        (expect (file-octets ws "hp.org")
                :to-equalp (octets (hp-edit 7 "** TODO 実装する" ":LOGBOOK:"
                                            "CLOCK: [2026-10-19 Mon 09:00]--[2026-10-19 Mon 09:30] =>  0:30"
                                            ":END:"))))))

  (define-org-case ("org clock report --heading-path reads only the addressed heading" ws)
    (put ws "hp-clock.org"
         (hp-text (list "* Tres: 監視(Sentry)"
                        "** TODO 要件を確認する"
                        ":LOGBOOK:"
                        "CLOCK: [2026-10-19 Mon 09:00]--[2026-10-19 Mon 09:30] =>  0:30"
                        ":END:"
                        "** TODO 要件を確認する"
                        ":LOGBOOK:"
                        "CLOCK: [2026-10-19 Mon 10:00]--[2026-10-19 Mon 10:15] =>  0:15"
                        ":END:")))
    (let ((before (file-octets ws "hp-clock.org")))
      (loop for (index minutes) in '((1 30) (2 15))
            do (let ((report (org-ok ws (append
                                         (list "org" "clock" "report" "hp-clock.org")
                                         (hp-heading-path
                                          (hp-path-json ws "hp-clock.org" +hp-tres +hp-requirement index))
                                         '("--from" "2026-10-19T09:00:00" "--through" "2026-10-19T11:00:00"
                                           "--at" "2026-10-19T11:00:00" "--tz" "UTC"))
                                     "org clock report")))
                 (expect (jget report "total_minutes") :to-be minutes)
                 (expect (jget report "rows" 0 "title") :to-equal +hp-requirement)
                 (expect (file-octets ws "hp-clock.org") :to-equalp before)))
      (org-error ws (list "org" "clock" "report" "hp-clock.org"
                          "--heading-path" (hp-json +hp-tres 1 +hp-requirement 3)
                          "--from" "2026-10-19T09:00:00" "--through" "2026-10-19T11:00:00"
                          "--at" "2026-10-19T11:00:00" "--tz" "UTC")
                 "org clock report" "selection.no-match")))

  (define-org-case ("org query heading_path round-trips to the same heading for every match" ws)
    (put ws "hp.org" (hp-text +hp-lines+))
    (let* ((before (file-octets ws "hp.org"))
           (matches (hp-query ws "hp.org" "LEVEL=2")))
      (expect (length matches) :to-be 6)
      (dolist (match matches)
        (let ((dry-run (org-ok ws (append (hp-todo-args
                                           "hp.org"
                                           (hp-heading-path (json-kit:stringify (jget match "heading_path"))))
                                          '("--dry-run"))
                               "org todo set")))
          (expect (hp-removed-lines (jget dry-run "changes" 0 "diff"))
                  :to-equal (list (jget match "line")))
          (expect (file-octets ws "hp.org") :to-equalp before))))))
