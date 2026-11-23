(in-package #:aitools.org.domain)

(defun org-weekday-abbreviation (year month day)
  "The English three-letter weekday Org writes after a calendar date."
  (cdr (assoc (cl-date-kit:day-of-week (cl-date-kit:make-local-date year month day))
              '((:monday . "Mon") (:tuesday . "Tue")
                (:wednesday . "Wed") (:thursday . "Thu")
                (:friday . "Fri") (:saturday . "Sat") (:sunday . "Sun")))))

(defun format-org-archive-timestamp (instant zone)
  (let* ((local (cl-date-kit:local-date-time-of-instant
                 instant (cl-date-kit:find-time-zone zone)))
         (year (cl-date-kit:local-date-time-year local))
         (month (cl-date-kit:local-date-time-month local))
         (day (cl-date-kit:local-date-time-day local)))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~A ~2,'0D:~2,'0D"
            year month day
            (org-weekday-abbreviation year month day)
            (cl-date-kit:local-date-time-hour local)
            (cl-date-kit:local-date-time-minute local))))
