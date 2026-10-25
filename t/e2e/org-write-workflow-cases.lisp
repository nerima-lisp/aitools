;;;; t/e2e/org-write-workflow-cases.lisp
(in-package #:aitools.e2e.test)

(describe "aitools e2e org write workflows"
  (define-org-write-case ("org tx dry-run plans against the staged overlay without changing it" ws)
    (let* ((original (format nil "* TODO Build :work:~%"))
           (property-only (format nil "* TODO Build :work:~%:PROPERTIES:~%:OWNER: 高橋~%:END:~%")))
      (put ws "tx-dry.org" original)
      (let* ((before (file-octets ws "tx-dry.org"))
             (tx (jget (run-ok ws (list "tx" "begin")) "tx"))
             (staged (org-ok ws (org-write-time-args
                                 (list "org" "property" "set" "tx-dry.org" "--match" "/TODO"
                                       "--name" "OWNER" "--value" "高橋" "--tx" tx))
                             "org.property.set"))
             (overlay-before (jget (run-ok ws (list "tx" "diff" tx)) "changes" 0 "diff"))
             (planned (org-ok ws
                              (append (org-write-time-args
                                       (list "org" "todo" "set" "tx-dry.org" "--match" "/TODO"
                                             "--state" "DONE" "--tx" tx))
                                      (list "--dry-run"))
                              "org.todo.set")))
        (expect (jget staged "tx") :to-equal tx)
        (expect (jget staged "tx_op") :to-be 1)
        (expect-org-fragments overlay-before (list "* TODO Build :work:" ":OWNER: 高橋"))
        (expect (file-octets ws "tx-dry.org") :to-equalp before)
        (expect (jget planned "dry_run") :to-be t)
        (dolist (key (list "tx" "tx_op" "op_id" "next_commands"))
          (expect (nth-value 1 (gethash key planned)) :to-be nil))
        (let* ((plan-diff (jget planned "changes" 0 "diff"))
               (overlay-after (jget (run-ok ws (list "tx" "diff" tx)) "changes" 0 "diff")))
          (expect-org-fragments plan-diff (list "+* DONE Build :work:" ":OWNER: 高橋"))
          (expect overlay-after :to-equal overlay-before)
          (expect (search "* DONE Build :work:" overlay-after) :to-be nil)
          (expect (file-octets ws "tx-dry.org") :to-equalp before))
        (run-ok ws (list "tx" "commit" tx))
        (expect (file-octets ws "tx-dry.org") :to-equalp (octets property-only)))))

  (define-org-write-case ("org todo note follows configured logging markers byte for byte" ws)
    (put ws "todo-note.org" (format nil "#+TODO: TODO(t!) | DONE(d@)~%* TODO Close~%"))
    (let* ((original (file-octets ws "todo-note.org"))
           (result (org-ok ws
                           (org-write-time-args
                            (list "org" "todo" "set" "todo-note.org" "--match" "/TODO"
                                  "--state" "DONE" "--note" "closing note"))
                           "org.todo.set"))
           (expected
             (format nil "#+TODO: TODO(t!) | DONE(d@)~%* DONE Close~%:LOGBOOK:~%- State \"DONE\"       from \"TODO\"       [2026-10-19 Mon 09:00] \\\\~%  closing note~%:END:~%")))
      (expect (jget result "previous_state") :to-equal "TODO")
      (expect (jget result "new_state") :to-equal "DONE")
      (expect (jget result "changed") :to-be t)
      (expect-org-write-and-undo ws "todo-note.org" original expected result)))

  (define-org-write-case ("org planning rejects duplicate timestamps on indented lines" ws)
    (put ws "duplicate-planning.org"
         (format nil "* TODO Release~%   SCHEDULED: <2026-10-20> SCHEDULED: <2026-10-22>~%"))
    (let ((before (file-octets ws "duplicate-planning.org")))
      (org-error ws
                 (org-write-time-args
                  '("org" "planning" "set" "duplicate-planning.org" "--match" "/TODO"
                    "--kind" "scheduled" "--timestamp" "<2026-10-23>"))
                 "org.planning.set" "selection.ambiguous")
      (expect (file-octets ws "duplicate-planning.org") :to-equalp before)
      (org-error ws
                 (org-write-time-args
                  '("org" "planning" "delete" "duplicate-planning.org" "--match" "/TODO"
                    "--kind" "scheduled"))
                 "org.planning.delete" "selection.ambiguous")
      (expect (file-octets ws "duplicate-planning.org") :to-equalp before))))
