(in-package #:aitools.org.infrastructure)

(defun %iso-shape-p (text length separators)
  (let ((separator-positions (mapcar #'car separators)))
    (and (stringp text)
         (= (length text) length)
         (loop for (position . character) in separators
               always (char= (char text position) character))
         (loop for index below length
               always (or (member index separator-positions)
                          (char<= #\0 (char text index) #\9))))))

(defun %time-argument-failure (message)
  (%failure :argument message))

(defun %explicit-time-values (at tz dst-policy)
  (let ((policy (%dst-policy-keyword (or dst-policy "compatible"))))
    (unless (and (%iso-shape-p
                  at
                  19
                  '((4 . #\-) (7 . #\-) (10 . #\T)
                    (13 . #\:) (16 . #\:)))
                 (stringp tz)
                 (plusp (length tz))
                 policy
                 (string= at "00" :start1 17 :end1 19))
      (return-from %explicit-time-values
        (values nil nil nil
                (%time-argument-failure
                 "--at must be a local YYYY-MM-DDTHH:MM:00 date-time and --tz must name a zone"))))
    (handler-case
        (let* ((value (org-kit:make-org-time-value at))
               (zone (org-kit:resolve-time-zone tz)))
          (unless (and (eq (org-kit:org-time-value-kind value) :date-time)
                       (null (org-kit:org-time-value-offset-minutes value)))
            (return-from %explicit-time-values
              (values nil nil nil
                      (%time-argument-failure
                       "--at must be a zone-less local date-time"))))
          (let* ((context (org-kit:make-time-context
                           :reference-time value
                           :zone zone
                           :dst-policy policy))
                 (reference (cl-date-kit:local-date-time-of-instant
                             (org-kit:org-time-context-reference-instant context)
                             (cl-date-kit:find-time-zone tz))))
            (values value context reference nil)))
      (org-kit:org-time-error ()
        (values nil nil nil
                (%time-argument-failure "invalid --at or --tz value")))
      (cl-date-kit:cl-date-kit-error ()
        (values nil nil nil
                (%time-argument-failure "invalid --at or --tz value")))
      (org-kit:org-kit-error ()
        (values nil nil nil
                (%time-argument-failure "invalid --at or --tz value"))))))

(defun %date-value (text)
  (unless (%iso-shape-p text 10 '((4 . #\-) (7 . #\-)))
    (return-from %date-value
      (values nil (%time-argument-failure "dates must use YYYY-MM-DD"))))
  (handler-case
      (let ((value (org-kit:make-org-time-value text)))
        (if (eq (org-kit:org-time-value-kind value) :date)
            (values value nil)
            (values nil
                    (%time-argument-failure
                     "a date-only value is required"))))
    (org-kit:org-time-error ()
      (values nil (%time-argument-failure "invalid date value")))
    (org-kit:org-kit-error ()
      (values nil (%time-argument-failure "invalid date value")))))

(defun %time-range-values (from through)
  (multiple-value-bind (start start-failure)
      (%explicit-range-time from "--from")
    (when start-failure
      (return-from %time-range-values (values nil start-failure)))
    (multiple-value-bind (end end-failure)
        (%explicit-range-time through "--through")
      (when end-failure
        (return-from %time-range-values (values nil end-failure)))
      (handler-case
          (values (org-kit:make-time-range
                   start end :include-start-p t :include-end-p nil)
                  nil)
        (org-kit:org-time-error ()
          (values nil
                  (%time-argument-failure
                   "invalid clock report time range")))))))

(defun %explicit-range-time (text option)
  (unless (%iso-shape-p
           text 19
           '((4 . #\-) (7 . #\-) (10 . #\T)
             (13 . #\:) (16 . #\:)))
    (return-from %explicit-range-time
      (values nil
              (%time-argument-failure
               (format nil "~A must use YYYY-MM-DDTHH:MM:SS" option)))))
  (handler-case
      (let ((value (org-kit:make-org-time-value text)))
        (if (and (eq (org-kit:org-time-value-kind value) :date-time)
                 (null (org-kit:org-time-value-offset-minutes value))
                 (zerop (org-kit:org-time-value-second value)))
            (values value nil)
            (values nil
                    (%time-argument-failure
                     (format nil
                             "~A must be a local minute-aligned date-time"
                             option)))))
    (org-kit:org-time-error ()
      (values nil
              (%time-argument-failure
               (if (string= option "--from")
                   "invalid --from value"
                   "invalid --through value"))))
    (org-kit:org-kit-error ()
      (values nil
              (%time-argument-failure
               (if (string= option "--from")
                   "invalid --from value"
                   "invalid --through value"))))))
