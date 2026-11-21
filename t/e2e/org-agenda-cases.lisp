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
      (expect (jget entry "path") :to-equal "agenda.org")
      (expect (file-octets ws "agenda.org") :to-equalp before)
      (org-error ws '("org" "agenda" "agenda.org" "--from" "2026-10-20"
                      "--through" "2026-10-19" "--at" "2026-10-19T09:00:00"
                      "--tz" "UTC") "org agenda" "argument.invalid")))
  (define-org-case ("org agenda merges files in path order and keeps each file's date order" ws)
    (put ws "task/z.org" (format nil "* TODO Z~%DEADLINE: <2026-10-18 Sun>~%"))
    (put ws "task/sub/a.org"
         (format nil "* TODO Late~%DEADLINE: <2026-10-20 Tue>~%* TODO Early~%DEADLINE: <2026-10-19 Mon>~%"))
    (put ws "inbox.org" (format nil "* TODO Inbox~%DEADLINE: <2026-10-21 Wed>~%"))
    (put ws ".gitignore" (format nil "task/hidden.org~%"))
    (put ws "task/hidden.org" (format nil "* TODO Hidden~%DEADLINE: <2026-10-21 Wed>~%"))
    (put ws "encrypted.org.gpg" #(#xFF))
    (put ws "encrypted-upper.org.GPG" #(#xFF))
    (let* ((envelope (org-ok ws
                             '("org" "agenda" "task/z.org" "--from" "2026-10-18"
                               "encrypted.org.gpg" "task" "inbox.org" "task/z.org"
                               "encrypted-upper.org.GPG" "--through" "2026-10-21"
                               "--at" "2026-10-22T09:00:00" "--tz" "UTC") "org agenda"))
           (entries (jlist (jget envelope "entries"))))
      (expect (jget envelope "from") :to-equal "2026-10-18")
      (expect (jget envelope "through") :to-equal "2026-10-21")
      (expect (jget envelope "count") :to-be 5)
      (expect (mapcar (lambda (entry)
                        (list (jget entry "path") (jget entry "date")
                              (jget entry "line") (jget entry "title")))
                      entries)
              :to-equal '(("inbox.org" "2026-10-21" 1 "Inbox")
                          ("task/hidden.org" "2026-10-21" 1 "Hidden")
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
      (expect (nth-value 1 (gethash "count" envelope)) :to-be nil)))
  (define-org-case ("org agenda reports an explicit file once when given twice" ws)
    (put ws "agenda.org" (format nil "* TODO Ship~%DEADLINE: <2026-10-19 Mon>~%"))
    (let ((envelope (org-ok ws
                            '("org" "agenda" "agenda.org" "agenda.org" "--from" "2026-10-19"
                              "--through" "2026-10-19" "--at" "2026-10-19T09:00:00"
                              "--tz" "UTC") "org agenda")))
      (expect (jget envelope "count") :to-be 1)
      (expect (jget envelope "entries" 0 "path") :to-equal "agenda.org")))
  (define-org-case ("org agenda skips discovered links and checks explicit paths against the workspace" ws)
    (let ((content (format nil "* TODO Ship~%DEADLINE: <2026-10-19 Mon>~%")))
      (put ws "task/list/active.org" content)
      (put ws "task/target.org" content)
      (put ws "outside.org" content))
    (sb-posix:symlink "../target.org" (native (ws-path ws "task/list/alias.org")))
    (sb-posix:symlink "../../outside.org" (native (ws-path ws "task/list/escape.org")))
    (sb-posix:symlink "../../outside.org" (native (ws-path ws "task/list/escape.org.GPG")))
    (flet ((arguments (&rest paths)
             (append '("--root" "task" "org" "agenda") paths
                     '("--from" "2026-10-19" "--through" "2026-10-19"
                       "--at" "2026-10-19T09:00:00" "--tz" "UTC"))))
      (let ((envelope (org-ok ws (arguments "task/list") "org agenda")))
        (expect (jget envelope "count") :to-be 1)
        (expect (jget envelope "entries" 0 "path") :to-equal "list/active.org"))
      (let ((envelope (org-ok ws (arguments "task/list/alias.org") "org agenda")))
        (expect (jget envelope "count") :to-be 1)
        (expect (jget envelope "entries" 0 "path") :to-equal "target.org"))
      (dolist (path '("task/list/escape.org" "task/list/escape.org.GPG" "outside.org"))
        (org-error ws (arguments path) "org agenda" "refusal.outside-workspace"))))
  (define-org-case ("org agenda reports missing, special, and binary files with one path" ws)
    (put ws "task/valid.org" (format nil "* TODO Ship~%DEADLINE: <2026-10-19 Mon>~%"))
    (put ws "binary.org" #(#x00 #xFF))
    (sb-posix:mkfifo (native (ws-path ws "pipe.org")) #o600)
    (dolist (test '(("task/missing.org" "input.not-found")
                    ("pipe.org" "refusal.not-a-file")
                    ("binary.org" "input.unsupported-format")))
      (destructuring-bind (path code) test
        (let* ((envelope (org-error ws
                                    (list "org" "agenda" "task" path "--from" "2026-10-19"
                                          "--through" "2026-10-19" "--at" "2026-10-19T09:00:00"
                                          "--tz" "UTC") "org agenda" code))
               (message (jget envelope "error" "message")))
          (expect (loop for offset = (search path message)
                          then (search path message :start2 (1+ offset))
                        while offset count offset)
                  :to-be 1)))))
  (define-org-case ("org agenda validates options before reading files" ws)
    (put ws "broken.org" #(#xFF))
    (dolist (path '("broken.org" "missing.org"))
      (let ((envelope (org-error ws
                                 (list "org" "agenda" path "--from" "2026-10-20"
                                       "--through" "2026-10-19" "--at" "2026-10-19T09:00:00"
                                       "--tz" "UTC") "org agenda" "argument.invalid")))
        (expect (search path (jget envelope "error" "message")) :to-be nil)))))