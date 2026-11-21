(in-package #:aitools.e2e.test)

(describe "aitools e2e org agenda"
  (define-org-case ("org agenda returns inclusive deadline entries" ws)
    (put ws "agenda.org" (format nil "* TODO Ship~%DEADLINE: <2026-10-19 Mon>~%"))
    (let* ((before (file-octets ws "agenda.org"))
           (envelope (org-ok ws
                             '("org" "agenda" "agenda.org" "--from" "2026-10-19"
                               "--through" "2026-10-19" "--at" "2026-10-19T09:00:00"
                               "--tz" "UTC") "org agenda"))
           (entry (jget envelope "entries" 0)))
      (expect (jget envelope "from") :to-equal "2026-10-19")
      (expect (jget envelope "through") :to-equal "2026-10-19")
      (expect (jget envelope "count") :to-be 1)
      (expect (jget entry "date") :to-equal "2026-10-19")
      (expect (jget entry "kind") :to-equal "deadline")
      (expect (jget entry "line") :to-be 1)
      (expect (jget entry "title") :to-equal "Ship")
      (expect (jget entry "todo") :to-equal "TODO")
      (expect (nth-value 1 (gethash "path" entry)) :to-be nil)
      (expect (file-octets ws "agenda.org") :to-equalp before)
      (org-error ws '("org" "agenda" "agenda.org" "--from" "2026-10-20"
                      "--through" "2026-10-19" "--at" "2026-10-19T09:00:00"
                      "--tz" "UTC") "org agenda" "argument.invalid")))
  (define-org-case ("org agenda merges files in path order and keeps each file's date order" ws)
    (put ws "task/z.org" (format nil "* TODO Z~%DEADLINE: <2026-10-18 Sun>~%"))
    (put ws "task/sub/a.org"
         (format nil "* TODO Late~%DEADLINE: <2026-10-20 Tue>~%* TODO Early~%DEADLINE: <2026-10-19 Mon>~%"))
    (put ws "inbox.org" (format nil "* TODO Inbox~%DEADLINE: <2026-10-21 Wed>~%"))
    (let* ((envelope (org-ok ws
                             '("org" "agenda" "task/z.org" "--from" "2026-10-18"
                               "task" "inbox.org" "task/z.org" "--through" "2026-10-21"
                               "--at" "2026-10-22T09:00:00" "--tz" "UTC") "org agenda"))
           (entries (jlist (jget envelope "entries"))))
      (expect (jget envelope "from") :to-equal "2026-10-18")
      (expect (jget envelope "through") :to-equal "2026-10-21")
      (expect (jget envelope "count") :to-be 4)
      (expect (mapcar (lambda (entry)
                        (list (jget entry "path") (jget entry "date")
                              (jget entry "line") (jget entry "title")))
                      entries)
              :to-equal '(("inbox.org" "2026-10-21" 1 "Inbox")
                          ("task/sub/a.org" "2026-10-19" 3 "Early")
                          ("task/sub/a.org" "2026-10-20" 1 "Late")
                          ("task/z.org" "2026-10-18" 1 "Z")))))
  (define-org-case ("org agenda returns an empty array and validates dates when no Org files are selected" ws)
    (ensure-directories-exist (ws-path ws "empty/"))
    (put ws "encrypted/secret.org.gpg" #(#xFF))
    (flet ((arguments (paths from)
             (append '("org" "agenda") paths
                     (list "--from" from "--through" "2026-10-19"
                           "--at" "2026-10-19T09:00:00" "--tz" "UTC"))))
      (dolist (paths '(("empty") ("encrypted") ("encrypted/secret.org.gpg")))
        (let ((envelope (org-ok ws (arguments paths "2026-10-19") "org agenda")))
          (expect (jget envelope "count") :to-be 0)
          (expect (jget envelope "entries") :to-equalp #())))
      (org-error ws (arguments '("empty") "2026-10-20") "org agenda" "argument.invalid")
      (org-error ws (arguments nil "2026-10-19") "org agenda" "argument.invalid")))
  (define-org-case ("org agenda fails the whole request when one file cannot be parsed" ws)
    (put ws "task/a.org" (format nil "* TODO Valid~%DEADLINE: <2026-10-19 Mon>~%"))
    (put ws "task/z.org" #(#xFF))
    (let ((envelope (org-error ws
                               '("org" "agenda" "task" "--from" "2026-10-19"
                                 "--through" "2026-10-19" "--at" "2026-10-19T09:00:00"
                                 "--tz" "UTC") "org agenda" "input.not-utf8")))
      (expect (nth-value 1 (gethash "entries" envelope)) :to-be nil)
      (expect (nth-value 1 (gethash "count" envelope)) :to-be nil))))