(in-package #:aitools.e2e.test)
(describe "aitools e2e org command edge cases"
  (define-org-write-case ("org planning accepts a time range with repeater and warning" ws)
    (put ws "planning-range.org" (format nil "* TODO Release~%"))
    (let* ((timestamp "<2026-10-20 09:00-10:00 +1w -2d>")
           (result
             (org-ok ws
                     (org-write-time-args
                      (list "org" "planning" "set" "planning-range.org" "--match" "/TODO"
                            "--kind" "scheduled" "--timestamp" timestamp))
                     "org.planning.set")))
      (expect (jget result "timestamp") :to-equal timestamp)
      (expect (jget result "changed") :to-be t)
      (expect (file-text ws "planning-range.org")
              :to-equal (format nil "* TODO Release~%SCHEDULED: ~A~%" timestamp))))

  (define-org-write-case ("org agenda distinguishes next and disabled future repeats" ws)
    (put ws "repeat.org"
         (format nil "* TODO Weekly~%<2026-10-16 Fri +1w>~%"))
    (let* ((base '("org" "agenda" "repeat.org" "--from" "2026-10-19"
                   "--through" "2026-11-05" "--at" "2026-10-19T09:00:00" "--tz" "UTC"))
           (next (org-ok ws (append base '("--future-repeats" "next")) "org.agenda"))
           (entry (jget next "entries" 0))
           (none (org-ok ws (append base '("--future-repeats" "none")) "org.agenda")))
      (expect (jget next "count") :to-be 1)
      (expect (jget entry "date") :to-equal "2026-10-23")
      (expect (jget entry "kind") :to-equal "timestamp")
      (expect (jget entry "title") :to-equal "Weekly")
      (expect (jget none "count") :to-be 0)
      (expect (jlist (jget none "entries")) :to-equal nil)))

  (define-org-write-case ("org clock rejects a second running clock and an earlier end time" ws)
    (put ws "already-running.org" (format nil "* TODO Work~%"))
    (org-ok ws (org-write-time-args '("org" "clock" "in" "already-running.org" "--match" "/!"))
            "org.clock.in")
    (let ((before (file-octets ws "already-running.org")))
      (org-error ws
                 (org-write-time-args '("org" "clock" "in" "already-running.org" "--match" "/!")
                                      "2026-10-19T09:15:00")
                 "org.clock.in" "argument.invalid")
      (expect (file-octets ws "already-running.org") :to-equalp before))
    (put ws "earlier-clock-out.org"
         (format nil "* TODO Work~%:LOGBOOK:~%CLOCK: [2026-10-19 Mon 09:00]--~%:END:~%"))
    (let ((before (file-octets ws "earlier-clock-out.org")))
      (org-error ws
                 (org-write-time-args '("org" "clock" "out" "earlier-clock-out.org" "--match" "/!")
                                      "2026-10-19T08:00:00")
                 "org.clock.out" "argument.invalid")
      (expect (file-octets ws "earlier-clock-out.org") :to-equalp before)))

  (define-org-write-case ("org clock report validates both range endpoints" ws)
    (let ((source (format nil "* TODO Work~%")))
      (put ws "bad-clock-range.org" source)
      (dolist (arguments
               (list '("org" "clock" "report" "bad-clock-range.org" "--match" "/!"
                      "--from" "2026-02-30T09:00:00" "--through" "2026-10-20T09:00:00"
                      "--at" "2026-10-19T09:00:00" "--tz" "UTC")
                     '("org" "clock" "report" "bad-clock-range.org" "--match" "/!"
                      "--from" "2026-10-19T09:00:00" "--through" "2026-02-30T09:00:00"
                      "--at" "2026-10-19T09:00:00" "--tz" "UTC")))
        (let ((before (file-octets ws "bad-clock-range.org")))
          (org-error ws arguments "org.clock.report" "argument.invalid")
          (expect (file-octets ws "bad-clock-range.org") :to-equalp before)))))

  (define-org-write-case ("org table sort validates paired positive bounds and table selection" ws)
    (put ws "sort-bounds.org"
         (format nil "| Name | Score |~%|------+-------|~%| Beta | 2 |~%| Alpha | 1 |~%"))
    (let ((before (file-octets ws "sort-bounds.org")))
      (org-error ws '("org" "table" "sort" "sort-bounds.org" "--table" "1"
                      "--column" "1" "--start" "1")
                 "org.table.sort" "argument.invalid")
      (org-error ws '("org" "table" "sort" "sort-bounds.org" "--table" "1"
                      "--column" "1" "--start" "0" "--end" "2")
                 "org.table.sort" "argument.invalid")
      (org-error ws '("org" "table" "sort" "sort-bounds.org" "--table" "2"
                      "--column" "1")
                 "org.table.sort" "argument.invalid")
      (expect (file-octets ws "sort-bounds.org") :to-equalp before)))

  (define-org-write-case ("org numeric table sort rejects a nonnumeric cell without editing" ws)
    (put ws "numeric-sort.org"
         (format nil "| 10 |~%| unknown |~%"))
    (let ((before (file-octets ws "numeric-sort.org")))
      (org-error ws '("org" "table" "sort" "numeric-sort.org" "--table" "1"
                      "--column" "1" "--type" "numeric" "--order" "ascending"
                      "--start" "1" "--end" "2")
                 "org.table.sort" "argument.invalid")
      (expect (file-octets ws "numeric-sort.org") :to-equalp before))))
