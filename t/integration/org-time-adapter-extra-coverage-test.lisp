(in-package #:aitools.edit.test)

(defmacro %with-org-time-extra-value (function value &body body)
  `(cl-weave:with-mocked-functions
       (((symbol-function (quote ,function))
         (lambda (&rest arguments)
           (declare (ignore arguments))
           ,value)))
     ,@body))

(defmacro %with-org-time-extra-error (function condition &body body)
  `(cl-weave:with-mocked-functions
       (((symbol-function (quote ,function))
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (error ,condition))))
     ,@body))

(describe "Org time adapter remaining validation paths"
  (it "rejects non-string zones and non-local explicit time values"
    (multiple-value-bind (value context reference failure)
        (aitools.org.infrastructure::%explicit-time-values
         "2026-10-19T09:00:00" 42 "compatible")
      (expect value :to-be nil)
      (expect context :to-be nil)
      (expect reference :to-be nil)
      (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument))
    (let ((date-value (cl-date-kit:local-date-of 2026 10 19)))
      (%with-org-time-extra-value org-kit:make-org-time-value date-value
        (multiple-value-bind (value context reference failure)
            (aitools.org.infrastructure::%explicit-time-values
             "2026-10-19T09:00:00" "UTC" "compatible")
          (expect value :to-be nil)
          (expect context :to-be nil)
          (expect reference :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)
          (expect (aitools.org.domain:org-failure-message failure)
                  :to-equal "--at must be a zone-less local date-time"))))
    (let ((offset-value
            (org-kit:make-org-time-value "2026-10-19T09:00:00+01:00")))
      (%with-org-time-extra-value org-kit:make-org-time-value offset-value
        (multiple-value-bind (value context reference failure)
            (aitools.org.infrastructure::%explicit-time-values
             "2026-10-19T09:00:00" "UTC" "compatible")
          (expect value :to-be nil)
          (expect context :to-be nil)
          (expect reference :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)
          (expect (aitools.org.domain:org-failure-message failure)
                  :to-equal "--at must be a zone-less local date-time"))))
    (%with-org-time-extra-error
        org-kit:resolve-time-zone
        (make-condition 'cl-date-kit:cl-date-kit-error)
      (multiple-value-bind (value context reference failure)
          (aitools.org.infrastructure::%explicit-time-values
           "2026-10-19T09:00:00" "UTC" "compatible")
        (expect value :to-be nil)
        (expect context :to-be nil)
        (expect reference :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument))))
  (it "maps date and range parser alternatives"
    (let ((date-time (cl-date-kit:local-date-time-of 2026 10 19 0 0 0)))
      (%with-org-time-extra-value org-kit:make-org-time-value date-time
        (multiple-value-bind (date failure)
            (aitools.org.infrastructure::%date-value "2026-10-19")
          (expect date :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)
          (expect (aitools.org.domain:org-failure-message failure)
                  :to-equal "a date-only value is required"))))
    (%with-org-time-extra-error
        org-kit:make-org-time-value
        (make-condition 'org-kit:org-kit-error :message "date parse failed")
      (multiple-value-bind (date failure)
          (aitools.org.infrastructure::%date-value "2026-10-19")
        (expect date :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)
        (expect (aitools.org.domain:org-failure-message failure)
                :to-equal "invalid date value")))
    (let ((date-value (cl-date-kit:local-date-of 2026 10 19)))
      (%with-org-time-extra-value org-kit:make-org-time-value date-value
        (multiple-value-bind (time failure)
            (aitools.org.infrastructure::%explicit-range-time
             "2026-10-19T09:30:00" "--from")
          (expect time :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)
          (expect (aitools.org.domain:org-failure-message failure)
                  :to-equal "--from must be a local minute-aligned date-time"))))
    (let ((offset-value
            (org-kit:make-org-time-value "2026-10-19T09:30:00+01:00")))
      (%with-org-time-extra-value org-kit:make-org-time-value offset-value
        (multiple-value-bind (time failure)
            (aitools.org.infrastructure::%explicit-range-time
             "2026-10-19T09:30:00" "--through")
          (expect time :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)
          (expect (aitools.org.domain:org-failure-message failure)
                  :to-equal "--through must be a local minute-aligned date-time"))))
    (multiple-value-bind (time failure)
        (aitools.org.infrastructure::%explicit-range-time
         "2026-02-30T09:30:00" "--from")
      (expect time :to-be nil)
      (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)
      (expect (aitools.org.domain:org-failure-message failure)
              :to-equal "invalid --from value")))
  (it "maps generic range parser failures and query zone failures"
    (dolist (option (list "--from" "--through"))
      (%with-org-time-extra-error
          org-kit:make-org-time-value
          (make-condition 'org-kit:org-kit-error :message "range parser failed")
        (multiple-value-bind (time failure)
            (aitools.org.infrastructure::%explicit-range-time
             "2026-10-19T09:30:00" option)
          (expect time :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)
          (expect (aitools.org.domain:org-failure-message failure)
                  :to-equal (if (string= option "--from")
                                "invalid --from value"
                                "invalid --through value")))))
    (%with-org-time-extra-error
        org-kit:resolve-time-zone
        (make-condition 'cl-date-kit:cl-date-kit-error)
      (multiple-value-bind (reference failure)
          (aitools.org.infrastructure::%query-reference-time
           "2026-10-19T09:00:00" "UTC" nil)
        (expect reference :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :invalid-time)))))
