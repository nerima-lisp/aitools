(in-package #:aitools.e2e.test)

(describe "aitools e2e org agenda"
  (define-org-read-case ("org agenda returns inclusive deadline entries" ws)
    (put ws "agenda.org" (format nil "* TODO Ship~%DEADLINE: <2026-10-19 Mon>~%"))
    (let* ((before (file-octets ws "agenda.org"))
           (envelope (org-ok ws
                             '("org" "agenda" "agenda.org" "--from" "2026-10-19"
                               "--through" "2026-10-19" "--at" "2026-10-19T09:00:00"
                               "--tz" "UTC")
                             "org.agenda"))
           (entry (jget envelope "entries" 0)))
      (expect (jget envelope "from") :to-equal "2026-10-19")
      (expect (jget envelope "through") :to-equal "2026-10-19")
      (expect (jget envelope "count") :to-be 1)
      (expect (jget entry "date") :to-equal "2026-10-19")
      (expect (jget entry "kind") :to-equal "deadline")
      (expect (jget entry "line") :to-be 1)
      (expect (jget entry "title") :to-equal "Ship")
      (expect (jget entry "todo") :to-equal "TODO")
      (expect (file-octets ws "agenda.org") :to-equalp before)
      (org-error ws '("org" "agenda" "agenda.org" "--from" "2026-10-20"
                      "--through" "2026-10-19" "--at" "2026-10-19T09:00:00"
                      "--tz" "UTC")
                 "org.agenda" "argument.invalid")))
)