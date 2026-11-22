(in-package #:aitools.edit.test)

(defun %archive-timestamp-at (epoch-second zone)
  (aitools.org.domain:format-org-archive-timestamp
   (cl-date-kit:instant-of-epoch-second epoch-second) zone))

(describe "Org archive timestamp formatting"
  (it "names every weekday and pads hours and minutes"
    ;; UTC instants: Monday 2026-10-19 through Sunday 2026-10-25.
    (dolist (case '((1792400700 "2026-10-19 Mon 09:05")
                    (1792540740 "2026-10-20 Tue 23:59")
                    (1792540800 "2026-10-21 Wed 00:00")
                    (1792672200 "2026-10-22 Thu 12:30")
                    (1792750200 "2026-10-23 Fri 10:10")
                    (1792867500 "2026-10-24 Sat 18:45")
                    (1792912080 "2026-10-25 Sun 07:08")))
      (expect (%archive-timestamp-at (first case) "UTC") :to-equal (second case))))
  (it "pads single-digit months and days and four-digit years"
    (expect (%archive-timestamp-at 1767323040 "UTC") :to-equal "2026-01-02 Fri 03:04")
    (expect (%archive-timestamp-at 1798761540 "UTC") :to-equal "2026-12-31 Thu 23:59")
    (expect (%archive-timestamp-at -30636273600 "UTC") :to-equal "0999-03-05 Tue 12:00"))
  (it "renders the local date, weekday, and time of the requested zone"
    ;; 2026-10-18T20:30Z is Sunday in UTC and Monday in Tokyo.
    (expect (%archive-timestamp-at 1792355400 "UTC") :to-equal "2026-10-18 Sun 20:30")
    (expect (%archive-timestamp-at 1792355400 "Asia/Tokyo") :to-equal "2026-10-19 Mon 05:30")
    ;; 2026-10-19T03:30Z is Monday in UTC and Sunday in New York (EDT).
    (expect (%archive-timestamp-at 1792380600 "UTC") :to-equal "2026-10-19 Mon 03:30")
    (expect (%archive-timestamp-at 1792380600 "America/New_York") :to-equal "2026-10-18 Sun 23:30")
    ;; A half-hour offset moves the minute field.
    (expect (%archive-timestamp-at 1792368000 "Asia/Kolkata") :to-equal "2026-10-19 Mon 05:30")
    ;; The same zone follows its daylight-saving rule: EST in January, EDT in July.
    (expect (%archive-timestamp-at 1768824000 "America/New_York") :to-equal "2026-01-19 Mon 07:00")
    (expect (%archive-timestamp-at 1784462400 "America/New_York") :to-equal "2026-07-19 Sun 08:00"))
  (it "signals a date-kit error for an unknown zone"
    (expect (handler-case (%archive-timestamp-at 0 "Not/AZone")
              (cl-date-kit:cl-date-kit-error () :signalled))
            :to-be :signalled)))
