;;;; t/e2e/org-write-cases.lisp
(in-package #:aitools.e2e.test)

(defmacro define-org-write-case ((name workspace) &body body)
  `(it ,name
     (with-progress (,name)
       (let ((*current-case* (list 0 ,name)))
         (aitools-binary)
         (call-with-workspace (lambda (,workspace) ,@body))))))

(defparameter +org-write-time-context+
  '("--at" "2026-10-19T09:00:00" "--tz" "UTC"))

(defun org-write-time-args (arguments &optional at) (append arguments (if at (list "--at" at "--tz" "UTC") +org-write-time-context+)))

(defun expect-org-write-and-undo (workspace relative-path original-octets expected-text write-envelope)
  (let ((op-id (jget write-envelope "op_id")))
    (expect (stringp op-id) :to-be t)
    (expect (file-octets workspace relative-path)
            :to-equalp (octets expected-text))
    (run-ok workspace (list "undo" op-id))
    (expect (file-octets workspace relative-path)
            :to-equalp original-octets)))

(defun org-replace-first (source old new)
  (let ((start (search old source)))
    (unless start (error "fixture does not contain ~A" old))
    (concatenate 'string (subseq source 0 start) new
                 (subseq source (+ start (length old))))))

(defparameter +org-byte-fixture+
  (concatenate 'string
               "#+TODO: TODO | DONE" (string #\Newline)
               "# preserved Org comment" (format nil "~C~C" #\Return #\Newline)
               "#+VENDOR-FIELD: opaque" (string #\Newline)
               "* TODO 設計" (string #\Tab) "roadmap" (format nil "~C~C" #\Return #\Newline)
               "SCHEDULED: <2026-10-20>" (string #\Newline)
               ":PROPERTIES:" (format nil "~C~C" #\Return #\Newline)
               ":OWNER: 佐藤" (string #\Tab) "qa" (string #\Newline)
               ":TEAM: Org" (format nil "~C~C" #\Return #\Newline)
               ":END:" (string #\Newline)
               "| A | B |" (format nil "~C~C" #\Return #\Newline)
               "|---+---|" (string #\Newline)
               "| 1 | 2 |" (format nil "~C~C" #\Return #\Newline)
               "#+TBLFM: @2$2=2+1"))

(describe "aitools e2e org write commands"
  (define-org-write-case ("org todo and property edits preserve unrelated bytes" ws)
    (put ws "byte.org" +org-byte-fixture+)
    (let* ((todo (org-ok ws
                         (org-write-time-args
                          '("org" "todo" "set" "byte.org" "--match" "/TODO"
                            "--state" "DONE"))
                         "org.todo.set"))
           (todo-expected (org-replace-first +org-byte-fixture+ "TODO 設計" "DONE 設計")))
      (expect (jget todo "previous_state") :to-equal "TODO")
      (expect (jget todo "new_state") :to-equal "DONE")
      (expect (jget todo "changed") :to-be t)
      (expect-org-write-and-undo ws "byte.org" (octets +org-byte-fixture+) todo-expected todo))
    (put ws "byte.org" +org-byte-fixture+)
    (let* ((set (org-ok ws
                        (org-write-time-args
                         '("org" "property" "set" "byte.org" "--match" "/TODO"
                           "--name" "OWNER" "--value" "高橋"))
                        "org.property.set"))
           (expected (org-replace-first +org-byte-fixture+ (format nil "佐藤~Cqa" #\Tab) "高橋")))
      (expect (jget set "property") :to-equal "OWNER")
      (expect (jget set "changed") :to-be t)
      (expect-org-write-and-undo ws "byte.org" (octets +org-byte-fixture+) expected set))
    (put ws "byte.org" +org-byte-fixture+)
    (let* ((delete (org-ok ws
                           (org-write-time-args
                            '("org" "property" "delete" "byte.org" "--match" "/TODO"
                              "--name" "OWNER"))
                           "org.property.delete"))
           (owner-line (format nil ":OWNER: 佐藤~Cqa~%" #\Tab))
           (expected (org-replace-first +org-byte-fixture+ owner-line "")))
      (expect (jget delete "property") :to-equal "OWNER")
      (expect (jget delete "changed") :to-be t)
      (expect-org-write-and-undo ws "byte.org" (octets +org-byte-fixture+) expected delete)))

  (define-org-write-case ("org selector errors and delete no-ops preserve the file" ws)
    (put ws "no-match.org" (format nil "#+TODO: TODO | DONE~%* DONE Release~%"))
    (let ((before (file-octets ws "no-match.org")))
      (org-error ws (org-write-time-args
                     '("org" "todo" "set" "no-match.org" "--match" "/TODO"
                       "--state" "DONE"))
                 "org.todo.set" "selection.no-match")
      (expect (file-octets ws "no-match.org") :to-equalp before))
    (put ws "ambiguous.org" (format nil "#+TODO: TODO | DONE~%* TODO One~%* TODO Two~%"))
    (let ((before (file-octets ws "ambiguous.org")))
      (org-error ws (org-write-time-args
                     '("org" "todo" "set" "ambiguous.org" "--match" "/TODO"
                       "--state" "DONE"))
                 "org.todo.set" "selection.ambiguous")
      (expect (file-octets ws "ambiguous.org") :to-equalp before))
    (put ws "duplicate.org"
         (format nil "* TODO Item~%:PROPERTIES:~%:OWNER: A~%:OWNER: B~%:END:~%"))
    (let ((before (file-octets ws "duplicate.org")))
      (org-error ws (org-write-time-args
                     '("org" "property" "set" "duplicate.org" "--match" "/TODO"
                       "--name" "OWNER" "--value" "C"))
                 "org.property.set" "selection.ambiguous")
      (expect (file-octets ws "duplicate.org") :to-equalp before))
    (put ws "noop.org" (format nil "* TODO Item~%"))
    (let* ((before (file-octets ws "noop.org"))
           (property (org-ok ws (org-write-time-args
                                 '("org" "property" "delete" "noop.org" "--match" "/TODO"
                                   "--name" "OWNER"))
                              "org.property.delete"))
           (planning (org-ok ws (org-write-time-args
                                 '("org" "planning" "delete" "noop.org" "--match" "/TODO"
                                   "--kind" "scheduled"))
                              "org.planning.delete")))
      (expect (jget property "changed") :to-be nil)
      (expect (jget planning "changed") :to-be nil)
      (expect (file-octets ws "noop.org") :to-equalp before)))

  (define-org-write-case ("org planning set, update, and delete use the timestamp subset" ws)
    (put ws "planning.org" (format nil "* TODO Release~%"))
    (let* ((set (org-ok ws (org-write-time-args
                            '("org" "planning" "set" "planning.org" "--match" "/TODO"
                              "--kind" "scheduled" "--timestamp" "<2026-10-20>"))
                        "org.planning.set"))
           (expected (format nil "* TODO Release~%SCHEDULED: <2026-10-20>~%")))
      (expect (jget set "kind") :to-equal "scheduled")
      (expect (jget set "timestamp") :to-equal "<2026-10-20>")
      (expect (jget set "changed") :to-be t)
      (expect-org-write-and-undo ws "planning.org"
                                 (octets (format nil "* TODO Release~%")) expected set))
    (put ws "update.org" (format nil "* TODO Release~%SCHEDULED: <2026-10-20>~%"))
    (let ((updated (org-ok ws (org-write-time-args
                               '("org" "planning" "set" "update.org" "--match" "/TODO"
                                 "--kind" "scheduled" "--timestamp" "<2026-10-22>"))
                           "org.planning.set")))
      (expect (jget updated "changed") :to-be t)
      (expect-org-write-and-undo ws "update.org"
                                 (octets (format nil "* TODO Release~%SCHEDULED: <2026-10-20>~%"))
                                 (format nil "* TODO Release~%SCHEDULED: <2026-10-22>~%") updated))
    (put ws "planning.org" (format nil "* TODO Release~%SCHEDULED: <2026-10-20>~%"))
    (let ((deleted (org-ok ws (org-write-time-args
                               '("org" "planning" "delete" "planning.org" "--match" "/TODO"
                                 "--kind" "scheduled"))
                           "org.planning.delete")))
      (expect (jget deleted "changed") :to-be t)
      (expect-org-write-and-undo ws "planning.org"
                                 (octets (format nil "* TODO Release~%SCHEDULED: <2026-10-20>~%"))
                                 (format nil "* TODO Release~%") deleted)))

  (define-org-write-case ("org planning rejects timestamps outside the CLI contract" ws)
    (put ws "invalid-plan.org" (format nil "* TODO Release~%"))
    (let ((before (file-octets ws "invalid-plan.org")))
      (dolist (timestamp
               (list "<2026-10-20 Tue>"
                     "%%(diary-float t 4 2)"
                     "<2026-10-20>--<2026-10-22>"
                     "<2026-10-20 +1d +2w>"
                     "<2026-10-20 -1d --2d>"
                     (format nil "<2026-10-20>~%<2026-10-22>")
                     "<2026-02-30>"))
        (org-error ws (org-write-time-args
                       (list "org" "planning" "set" "invalid-plan.org" "--match" "/TODO"
                             "--kind" "scheduled" "--timestamp" timestamp))
                   "org.planning.set" "argument.invalid")
        (expect (file-octets ws "invalid-plan.org") :to-equalp before))
      (dolist (pair '(("scheduled" "[2026-10-20]") ("closed" "<2026-10-20>")))
        (org-error ws (org-write-time-args
                       (list "org" "planning" "set" "invalid-plan.org" "--match" "/TODO"
                             "--kind" (first pair) "--timestamp" (second pair)))
                   "org.planning.set" "argument.invalid")
        (expect (file-octets ws "invalid-plan.org") :to-equalp before))))

  (define-org-write-case ("org writes honor dry-run, tx staging, commit, and undo" ws)
    (put ws "write.org" (format nil "#+TODO: TODO | DONE~%* TODO Release~%"))
    (let* ((before (file-octets ws "write.org"))
           (expected (octets (format nil "#+TODO: TODO | DONE~%* DONE Release~%")))
           (arguments '("org" "todo" "set" "write.org" "--match" "/TODO" "--state" "DONE")))
      (let ((dry-run (org-ok ws (append (org-write-time-args arguments) '("--dry-run"))
                             "org.todo.set")))
        (expect (jget dry-run "dry_run") :to-be t)
        (expect (nth-value 1 (gethash "op_id" dry-run)) :to-be nil)
        (expect (file-octets ws "write.org") :to-equalp before))
      (let* ((tx (jget (run-ok ws '("tx" "begin")) "tx"))
             (dry-run (org-ok ws (append (org-write-time-args arguments)
                                         (list "--tx" tx "--dry-run"))
                              "org.todo.set")))
        (expect (jget dry-run "dry_run") :to-be t)
        (expect (nth-value 1 (gethash "tx_op" dry-run)) :to-be nil)
        (expect (file-octets ws "write.org") :to-equalp before)
        (let ((staged (org-ok ws (append (org-write-time-args arguments)
                                         (list "--tx" tx))
                              "org.todo.set")))
          (expect (jget staged "tx") :to-equal tx)
          (expect (jget staged "tx_op") :to-be 1)
          (expect (file-octets ws "write.org") :to-equalp before))
        (let* ((committed (run-ok ws (list "tx" "commit" tx)))
               (op-id (jget committed "op_id")))
          (expect (stringp op-id) :to-be t)
          (expect (file-octets ws "write.org") :to-equalp expected)
          (run-ok ws (list "undo" op-id))
          (expect (file-octets ws "write.org") :to-equalp before)))))

  (define-org-write-case ("org clock in and out write the documented intervals" ws)
    (put ws "clock-in.org" (format nil "* TODO Work~%"))
    (let ((clock-in
            (org-ok ws
                    (org-write-time-args
                     (list "org" "clock" "in" "clock-in.org" "--match" "/!"))
                    "org.clock.in")))
      (expect (jget clock-in "timestamp") :to-equal "[2026-10-19 Mon 09:00]")
      (expect (jget clock-in "changed") :to-be t)
      (expect-org-write-and-undo ws "clock-in.org"
                                 (octets (format nil "* TODO Work~%"))
                                 (format nil "* TODO Work~%:LOGBOOK:~%CLOCK: [2026-10-19 Mon 09:00]~%:END:~%")
                                 clock-in))
    (put ws "clock-out.org"
         (format nil "* TODO Work~%:LOGBOOK:~%CLOCK: [2026-10-19 Mon 09:00]~%:END:~%"))
    (let ((clock-out
            (org-ok ws
                    (org-write-time-args
                     (list "org" "clock" "out" "clock-out.org" "--match" "/!")
                     "2026-10-19T09:30:00")
                    "org.clock.out")))
      (expect (jget clock-out "interval")
              :to-equal "[2026-10-19 Mon 09:00]--[2026-10-19 Mon 09:30] => 0:30")
      (expect (jget clock-out "changed") :to-be t)
      (expect-org-write-and-undo ws "clock-out.org"
                                 (octets (format nil "* TODO Work~%:LOGBOOK:~%CLOCK: [2026-10-19 Mon 09:00]~%:END:~%"))
                                 (format nil "* TODO Work~%:LOGBOOK:~%CLOCK: [2026-10-19 Mon 09:00]--[2026-10-19 Mon 09:30] =>  0:30~%:END:~%")
                                 clock-out)))

  (define-org-write-case ("org clock report aggregates closed clocks and omits running clocks" ws)
    (put ws "clock-report.org"
         (format nil "* TODO Work~%:LOGBOOK:~%CLOCK: [2026-10-19 Mon 09:00]--[2026-10-19 Mon 09:30] =>  0:30~%CLOCK: [2026-10-19 Mon 10:00]--[2026-10-19 Mon 10:15] =>  0:15~%CLOCK: [2026-10-19 Mon 09:45]--~%:END:~%"))
    (let* ((before (file-octets ws "clock-report.org"))
           (report (org-ok ws
                           '("org" "clock" "report" "clock-report.org" "--match" "/!"
                             "--from" "2026-10-19T09:00:00" "--through" "2026-10-19T10:00:00"
                             "--at" "2026-10-19T10:00:00" "--tz" "UTC")
                           "org.clock.report"))
           (row (jget report "rows" 0)))
      (expect (jget report "total_minutes") :to-be 30)
      (expect (jget row "level") :to-be 1)
      (expect (jget row "title") :to-equal "Work")
      (expect (jget row "minutes") :to-be 30)
      (expect (file-octets ws "clock-report.org") :to-equalp before)
      (org-error ws '("org" "clock" "report" "clock-report.org" "--match" "/!"
                      "--from" "2026-10-19T10:00:00" "--through" "2026-10-19T09:00:00"
                      "--at" "2026-10-19T10:00:00" "--tz" "UTC")
                 "org.clock.report" "argument.invalid"))))
