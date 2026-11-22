(in-package #:aitools.edit.test)

;;; Planning timestamps are stored the way Emacs writes them: the weekday follows the
;;; date. The expected strings below were produced by `org-schedule`, `org-deadline`,
;;; and `org-add-planning-info` of Org 9.8.10.

(defun %planning-canonical (kind timestamp)
  (multiple-value-bind (view failure)
      (aitools.org.infrastructure::validate-planning-timestamp kind timestamp)
    (expect failure :to-be nil)
    (aitools.org.infrastructure::canonical-planning-timestamp timestamp view)))

(defun %planning-weekday-edit (kind timestamp)
  "The (values updated-text timestamp-field failure) of setting TIMESTAMP on a bare heading."
  (let* ((engine (aitools.org.domain:org-document-engine-value
                  (%org-coverage-document (format nil "* TODO Release~%"))))
         (options (list :match "/TODO" :kind kind :timestamp timestamp
                        :at "2026-10-19T09:00:00" :tz "UTC" :dst-policy "compatible")))
    (multiple-value-bind (updated fields failure)
        (aitools.org.infrastructure::%org-planning-edit engine options nil)
      (values (and updated (org-kit:document-text updated))
              (cdr (assoc "timestamp" fields :test #'string=))
              failure))))

(describe "Org planning timestamp weekday"
  (it "names each weekday and a leap day"
    (loop for (day weekday) in '((19 "Mon") (20 "Tue") (21 "Wed") (22 "Thu")
                                 (23 "Fri") (24 "Sat") (25 "Sun"))
          do (expect (aitools.org.domain:org-weekday-abbreviation 2026 10 day)
                     :to-equal weekday))
    (expect (aitools.org.domain:org-weekday-abbreviation 2028 2 29) :to-equal "Tue"))
  (it "inserts the weekday after the date and keeps every other component"
    (dolist (case '(("scheduled" "<2026-10-20>" "<2026-10-20 Tue>")
                    ("scheduled" "<2026-10-20 09:00>" "<2026-10-20 Tue 09:00>")
                    ("scheduled" "<2026-10-21 +1w>" "<2026-10-21 Wed +1w>")
                    ("deadline" "<2026-10-22 -3d>" "<2026-10-22 Thu -3d>")
                    ("deadline" "<2026-10-19 09:00-10:00 +1w -2d>"
                     "<2026-10-19 Mon 09:00-10:00 +1w -2d>")
                    ("scheduled" "<2028-02-29>" "<2028-02-29 Tue>")
                    ("closed" "[2026-10-23 10:00]" "[2026-10-23 Fri 10:00]")
                    ("closed" "[2026-10-24]" "[2026-10-24 Sat]")))
      (expect (%planning-canonical (first case) (second case))
              :to-equal (third case))))
  (it "keeps rejecting a weekday token, right or wrong"
    (dolist (timestamp '("<2026-10-20 Tue>" "<2026-10-20 Fri>" "<2026-10-20 Tue 09:00>"))
      (let ((failure (nth-value 1 (aitools.org.infrastructure::validate-planning-timestamp
                                   "scheduled" timestamp))))
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)
        (expect (aitools.org.domain:org-failure-message failure)
                :to-equal "--timestamp contains an unsupported suffix"))))
  (it "writes and reports the canonical timestamp"
    (dolist (case '(("scheduled" "<2026-10-20>" "SCHEDULED: <2026-10-20 Tue>")
                    ("deadline" "<2026-10-20 09:00 +1w>" "DEADLINE: <2026-10-20 Tue 09:00 +1w>")
                    ("closed" "[2026-10-19 09:00]" "CLOSED: [2026-10-19 Mon 09:00]")))
      (multiple-value-bind (text reported failure)
          (%planning-weekday-edit (first case) (second case))
        (expect failure :to-be nil)
        (expect text :to-equal (format nil "* TODO Release~%~A~%" (third case)))
        (expect reported :to-equal (subseq (third case) (1+ (position #\Space (third case)))))))))
