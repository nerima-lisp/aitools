(in-package #:aitools.org.domain)

(defun format-org-archive-timestamp (instant zone)
  (let* ((local (cl-date-kit:local-date-time-of-instant
                 instant (cl-date-kit:find-time-zone zone)))
         (year (cl-date-kit:local-date-time-year local))
         (month (cl-date-kit:local-date-time-month local))
         (day (cl-date-kit:local-date-time-day local))
         (weekday (cl-date-kit:day-of-week (cl-date-kit:make-local-date year month day))))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~A ~2,'0D:~2,'0D"
            year month day
            (cdr (assoc weekday '((:monday . "Mon") (:tuesday . "Tue")
                                  (:wednesday . "Wed") (:thursday . "Thu")
                                  (:friday . "Fri") (:saturday . "Sat") (:sunday . "Sun"))))
            (cl-date-kit:local-date-time-hour local)
            (cl-date-kit:local-date-time-minute local))))
