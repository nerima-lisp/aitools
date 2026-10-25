(in-package #:cl-user)

(defpackage #:aitools.integration.org-write-dispatch-test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect))

(in-package #:aitools.integration.org-write-dispatch-test)

(defun %command-name (name)
  (substitute #\. #\Space name))

(defun %org-ok (root arguments name)
  (multiple-value-bind (code envelope stream)
      (aitools.test.support:run-aitools root arguments)
    (expect code :to-be 0)
    (expect stream :to-be :stdout)
    (expect (aitools.test.support:envelope-value envelope "command")
            :to-equal (%command-name name))
    (expect (aitools.test.support:envelope-value envelope "status")
            :to-equal "ok")
    envelope))

(defun %org-error (root arguments name error-code)
  (multiple-value-bind (code envelope stream)
      (aitools.test.support:run-aitools root arguments)
    (let ((expected-exit-code
            (aitools.protocol.domain:error-code-exit-code error-code)))
      (expect code :to-be expected-exit-code)
      (expect stream :to-be :stderr)
      (expect (aitools.test.support:envelope-value envelope "command")
              :to-equal (%command-name name))
      (expect (aitools.test.support:envelope-value envelope "status")
              :to-equal "error")
      (expect (aitools.test.support:envelope-value envelope "error" "code")
              :to-equal error-code)
      (expect (aitools.test.support:envelope-value envelope "error" "exit_code")
              :to-be expected-exit-code))
    envelope))

(defun %put-org (root path source)
  (aitools.test.support:write-text (concatenate 'string root "/" path) source))

(defun %org-text (root path)
  (aitools.test.support:read-text (concatenate 'string root "/" path)))

(defun %org-time-args (arguments &optional (at "2026-10-19T09:00:00"))
  (append arguments (list "--at" at "--tz" "UTC")))

(describe "Org write commands through the in-process dispatcher"
  (it "dispatches TODO, property, planning, clock, and table writes"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (%put-org root "todo.org"
                (format nil "#+TODO: TODO(t!) | DONE(d@)~%* TODO Task~%SCHEDULED: <2026-10-20>~%"))
      (let ((result
              (%org-ok root
                       (%org-time-args
                        '("org" "todo" "set" "todo.org" "--match" "/TODO"
                          "--state" "DONE" "--note" "closing note"))
                       "org todo set")))
        (expect (aitools.test.support:envelope-value result "previous_state")
                :to-equal "TODO")
        (expect (aitools.test.support:envelope-value result "new_state")
                :to-equal "DONE")
        (expect (search "SCHEDULED: <2026-10-20>" (%org-text root "todo.org"))
                :to-be-truthy)
        (expect (search "closing note" (%org-text root "todo.org"))
                :to-be-truthy))

      (%put-org root "property.org" (format nil "* TODO Task~%"))
      (%org-ok root
               (%org-time-args
                '("org" "property" "set" "property.org" "--match" "/TODO"
                  "--name" "OWNER" "--value" "A"))
               "org property set")
      (%org-ok root
               (%org-time-args
                '("org" "property" "delete" "property.org" "--match" "/TODO"
                  "--name" "OWNER"))
               "org property delete")

      (%put-org root "planning.org" (format nil "* TODO Release~%"))
      (%org-ok root
               (%org-time-args
                '("org" "planning" "set" "planning.org" "--match" "/TODO"
                  "--kind" "deadline" "--timestamp" "<2026-10-20>"))
               "org planning set")
      (%org-ok root
               (%org-time-args
                '("org" "planning" "delete" "planning.org" "--match" "/TODO"
                  "--kind" "deadline"))
               "org planning delete")

      (%put-org root "clock.org" (format nil "* TODO Work~%"))
      (%org-ok root
               (%org-time-args '("org" "clock" "in" "clock.org" "--match" "/TODO"))
               "org clock in")
      (%org-ok root
               (%org-time-args '("org" "clock" "out" "clock.org" "--match" "/TODO")
                               "2026-10-19T09:30:00")
               "org clock out")

      (%put-org root "align.org"
                (format nil "Before~%| key | value~%|--+-------~%| a | 1~%After~%"))
      (%org-ok root '("org" "table" "align" "align.org" "--table" "1")
               "org table align")
      (expect (search "| a   |     1 |" (%org-text root "align.org"))
              :to-be-truthy)
      (%put-org root "sort.org" (format nil "| Z | 2 |~%| A | 1 |~%"))
      (%org-ok root
               '("org" "table" "sort" "sort.org" "--table" "1" "--column" "1"
                 "--type" "lexical" "--order" "ascending")
               "org table sort")
      (%put-org root "formula.org"
                (format nil "| Input | Total |~%|-------+-------|~%| 2 | 0 |~%#+TBLFM: @2$2=$1+1~%"))
      (%org-ok root '("org" "table" "recalculate" "formula.org" "--table" "1")
               "org table recalculate")
      (expect (search "| 2 | 3 |" (%org-text root "formula.org")) :to-be-truthy))))

(describe "Org read commands through the in-process dispatcher"
  (it "dispatches agenda and clock report requests"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (%put-org root "agenda.org"
                (format nil "#+TODO: TODO | DONE~%* TODO Work~%SCHEDULED: <2026-10-19 Mon>~%"))
      (let ((result
              (%org-ok root
                       '("org" "agenda" "agenda.org" "--from" "2026-10-19"
                         "--through" "2026-10-19" "--future-repeats" "all"
                         "--at" "2026-10-19T09:00:00" "--tz" "UTC")
                       "org agenda")))
        (expect (aitools.test.support:envelope-value result "count") :to-be 1))
      (%put-org root "report.org"
                (format nil "* TODO Work~%:LOGBOOK:~%CLOCK: [2026-10-19 Mon 09:00]--[2026-10-19 Mon 09:30] =>  0:30~%:END:~%"))
      (let ((result
              (%org-ok root
                       '("org" "clock" "report" "report.org" "--match" "/TODO"
                         "--from" "2026-10-19T09:00:00" "--through" "2026-10-19T10:00:00"
                         "--at" "2026-10-19T10:00:00" "--tz" "UTC")
                       "org clock report")))
        (expect (aitools.test.support:envelope-value result "total_minutes") :to-be 30)))))

(describe "Org write command edge paths through the in-process dispatcher"
  (it "rejects invalid timestamps, selections, clock ranges, and table options"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (%put-org root "todo.org" (format nil "#+TODO: TODO | DONE~%* TODO One~%* TODO Two~%"))
      (progn
        (%org-error root
                    (%org-time-args '("org" "todo" "set" "todo.org" "--match" "/TODO"
                                      "--state" "DONE"))
                    "org todo set" "selection.ambiguous")
        (%put-org root "todo.org" (format nil "#+TODO: TODO | DONE~%* TODO One~%")))
      (%org-error root
                  (append '("org" "todo" "set" "todo.org" "--match" "/TODO"
                            "--state" "DONE")
                          '("--at" "2026-02-30T09:00:00" "--tz" "UTC"))
                  "org todo set" "argument.invalid")
      (%org-error root
                  (%org-time-args '("org" "planning" "set" "todo.org" "--match" "/TODO"
                                    "--kind" "scheduled" "--timestamp" "<2026-10-20 +1d +2w>"))
                  "org planning set" "argument.invalid")
      (%put-org root "clock.org" (format nil "* TODO Work~%"))
      (%org-error root
                  (%org-time-args '("org" "clock" "out" "clock.org" "--match" "/TODO")
                                  "2026-10-19T09:30:00")
                  "org clock out" "selection.no-match")
      (%put-org root "table.org" (format nil "| A | B |~%|---+---|~%| 1 | 2 |~%"))
      (%org-error root '("org" "table" "sort" "table.org" "--table" "1"
                         "--column" "1" "--type" "lexical" "--order" "sideways")
                  "org table sort" "argument.invalid")
      (%org-error root '("org" "table" "sort" "table.org" "--table" "1"
                         "--column" "0" "--type" "lexical" "--order" "ascending")
                  "org table sort" "argument.invalid")))
  (it "preserves no-op writes and round-trips an Org edit through a transaction"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (%put-org root "write.org"
                (format nil "#+TODO: TODO | DONE~%* TODO Release~%SCHEDULED: <2026-10-20>~%"))
      (let ((before (%org-text root "write.org")))
        (let ((property
                (%org-ok root
                         (%org-time-args
                          '("org" "property" "delete" "write.org" "--match" "/TODO"
                            "--name" "OWNER"))
                         "org property delete"))
              (planning
                (%org-ok root
                         (%org-time-args
                          '("org" "planning" "delete" "write.org" "--match" "/TODO"
                            "--kind" "deadline"))
                         "org planning delete")))
          (expect (aitools.test.support:envelope-value property "changed") :to-be json-kit:+json-false+)
          (expect (aitools.test.support:envelope-value planning "changed") :to-be json-kit:+json-false+)
          (expect (%org-text root "write.org") :to-equal before))
        (let* ((arguments
                 '("org" "todo" "set" "write.org" "--match" "/TODO" "--state" "DONE"))
               (dry-run
                 (%org-ok root (append (%org-time-args arguments) '("--dry-run"))
                          "org todo set")))
          (expect (aitools.test.support:envelope-value dry-run "dry_run") :to-be t)
          (expect (nth-value 1 (gethash "op_id" dry-run)) :to-be nil)
          (expect (%org-text root "write.org") :to-equal before)
          (multiple-value-bind (code envelope stream)
              (aitools.test.support:run-aitools root '("tx" "begin"))
            (expect code :to-be 0)
            (expect stream :to-be :stdout)
            (let* ((tx (aitools.test.support:envelope-value envelope "tx"))
                   (staged (%org-ok root
                                    (append (%org-time-args arguments) (list "--tx" tx))
                                    "org todo set")))
              (expect (aitools.test.support:envelope-value staged "tx_op") :to-be 1)
              (expect (%org-text root "write.org") :to-equal before)
              (multiple-value-bind (commit-code committed commit-stream)
                  (aitools.test.support:run-aitools root (list "tx" "commit" tx))
                (expect commit-code :to-be 0)
                (expect commit-stream :to-be :stdout)
                (let ((op-id (aitools.test.support:envelope-value committed "op_id")))
                  (expect (%org-text root "write.org")
                          :to-equal
                          (format nil "#+TODO: TODO | DONE~%* DONE Release~%SCHEDULED: <2026-10-20>~%"))
                  (multiple-value-bind (undo-code undo-envelope)
                      (aitools.test.support:run-aitools root (list "undo" op-id))
                    (expect undo-code :to-be 0)
                    (expect undo-envelope :to-match-envelope (list "undoes" op-id))
                    (expect (%org-text root "write.org") :to-equal before))))))))))
  (it "writes closed planning and rejects a one-sided table row bound"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (%put-org root "closed.org" (format nil "* TODO Release~%"))
      (let ((result (%org-ok root
                             (%org-time-args
                              '("org" "planning" "set" "closed.org" "--match" "/TODO"
                                "--kind" "closed" "--timestamp" "[2026-10-19 09:00]"))
                             "org planning set")))
        (expect (aitools.test.support:envelope-value result "kind") :to-equal "closed")
        (expect (search "CLOSED: [2026-10-19 09:00]" (%org-text root "closed.org")) :to-be-truthy))
      (%put-org root "table.org" (format nil "| A |~%|---|~%| x |~%"))
      (let ((before (%org-text root "table.org")))
        (%org-error root
                    '("org" "table" "align" "table.org" "--table" "1" "--start" "1")
                    "org table align" "argument.invalid")
        (expect (%org-text root "table.org") :to-equal before))))
)
