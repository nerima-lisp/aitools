(in-package #:aitools.edit.test)

(defmacro %with-org-time-adapter-condition (function condition &body body)
  `(cl-weave:with-mocked-functions
       (((symbol-function (quote ,function))
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (error ,condition))))
     ,@body))

(describe "Org time adapter coverage"
  (it "validates ISO shapes and maps explicit local times"
    (let ((separators '((4 . #\-) (7 . #\-) (10 . #\T)
                        (13 . #\:) (16 . #\:))))
      (expect
       (aitools.org.infrastructure::%iso-shape-p
        "2026-10-19T09:15:00" 19 separators)
       :to-be t)
      (expect
       (aitools.org.infrastructure::%iso-shape-p 7 19 separators)
       :to-be nil)
      (expect
       (aitools.org.infrastructure::%iso-shape-p
        "2026-1X-19T09:15:00" 19 separators)
       :to-be nil))
    (multiple-value-bind (value context reference failure)
        (aitools.org.infrastructure::%explicit-time-values
         "2026-10-19T09:15:00" "UTC" nil)
      (expect failure :to-be nil)
      (expect (org-kit:org-time-value-kind value) :to-be :date-time)
      (expect (org-kit:org-time-context-dst-policy context) :to-be :compatible)
      (expect (cl-date-kit:local-date-time-hour reference) :to-be 9)
      (expect (cl-date-kit:local-date-time-minute reference) :to-be 15))
    (dolist (at (list 7
                      "2026-1X-19T09:15:00"
                      "2026-10-19x09:15:00"
                      "2026-10-19T09:15:01"
                      "2026-10-19T09:15:00-04:00"
                      "2026-02-30T09:15:00"))
      (multiple-value-bind (value context reference failure)
          (aitools.org.infrastructure::%explicit-time-values at "UTC" "compatible")
        (expect value :to-be nil)
        (expect context :to-be nil)
        (expect reference :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)))
    (dolist (arguments
             (list (list "2026-10-19T09:15:00" "" nil)
                   (list "2026-10-19T09:15:00" "UTC" "unknown")
                   (list "2026-10-19T09:15:00" "Missing/Zone" "compatible")))
      (multiple-value-bind (value context reference failure)
          (apply #'aitools.org.infrastructure::%explicit-time-values arguments)
        (expect value :to-be nil)
        (expect context :to-be nil)
        (expect reference :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)))
    (%with-org-time-adapter-condition
        org-kit:make-time-context
        (make-condition 'org-kit:org-kit-error :message "context failure")
      (multiple-value-bind (value context reference failure)
          (aitools.org.infrastructure::%explicit-time-values
           "2026-10-19T09:15:00" "UTC" "compatible")
        (expect value :to-be nil)
        (expect context :to-be nil)
        (expect reference :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument))))
  (it "maps daylight-saving gaps and overlaps by policy"
    (multiple-value-bind (value context reference failure)
        (aitools.org.infrastructure::%explicit-time-values
         "2026-03-08T02:30:00" "America/New_York" "earlier")
      (declare (ignore value))
      (expect failure :to-be nil)
      (expect (org-kit:org-time-context-dst-policy context) :to-be :earlier)
      (expect (cl-date-kit:local-date-time-hour reference) :to-be 1)
      (expect (cl-date-kit:local-date-time-minute reference) :to-be 30))
    (multiple-value-bind (value context reference failure)
        (aitools.org.infrastructure::%explicit-time-values
         "2026-03-08T02:30:00" "America/New_York" nil)
      (declare (ignore value))
      (expect failure :to-be nil)
      (expect (org-kit:org-time-context-dst-policy context) :to-be :compatible)
      (expect (cl-date-kit:local-date-time-hour reference) :to-be 3)
      (expect (cl-date-kit:local-date-time-minute reference) :to-be 30))
    (multiple-value-bind (value context reference failure)
        (aitools.org.infrastructure::%explicit-time-values
         "2026-03-08T02:30:00" "America/New_York" "strict")
      (expect value :to-be nil)
      (expect context :to-be nil)
      (expect reference :to-be nil)
      (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument))
    (let ((earlier nil)
          (later nil))
      (multiple-value-bind (value context reference failure)
          (aitools.org.infrastructure::%explicit-time-values
           "2026-11-01T01:30:00" "America/New_York" "earlier")
        (declare (ignore value reference))
        (expect failure :to-be nil)
        (setf earlier (org-kit:org-time-context-reference-instant context)))
      (multiple-value-bind (value context reference failure)
          (aitools.org.infrastructure::%explicit-time-values
           "2026-11-01T01:30:00" "America/New_York" "later")
        (declare (ignore value reference))
        (expect failure :to-be nil)
        (setf later (org-kit:org-time-context-reference-instant context)))
      (expect (- (cl-date-kit:instant-epoch-second later)
                 (cl-date-kit:instant-epoch-second earlier))
              :to-be 3600)))
  (it "parses date-only and explicit half-open ranges"
    (multiple-value-bind (date failure)
        (aitools.org.infrastructure::%date-value "2026-10-19")
      (expect failure :to-be nil)
      (expect (list (org-kit:org-time-value-kind date) (org-kit:org-time-value-year date) (org-kit:org-time-value-day date)) :to-equal '(:date 2026 19)))
    (dolist (text (list "10/19/2026" "2026-02-30"
                        "2026-10-19T09:00:00"))
      (multiple-value-bind (date failure)
          (aitools.org.infrastructure::%date-value text)
        (expect date :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)))
    (multiple-value-bind (time failure)
        (aitools.org.infrastructure::%explicit-range-time
         "2026-10-19T09:30:00" "--from")
      (expect failure :to-be nil)
      (expect (list (org-kit:org-time-value-hour time) (org-kit:org-time-value-minute time) (org-kit:org-time-value-second time)) :to-equal '(9 30 0)))
    (dolist (text (list "2026-10-19T09:30"
                        "2026-10-19T09:30:01"
                        "2026-10-19T09:30:00+00:00"))
      (multiple-value-bind (time failure)
          (aitools.org.infrastructure::%explicit-range-time text "--from")
        (expect time :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)))
    (multiple-value-bind (time failure)
        (aitools.org.infrastructure::%explicit-range-time
         "2026-02-30T09:30:00" "--through")
      (expect time :to-be nil)
      (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)
      (expect (aitools.org.domain:org-failure-message failure)
              :to-equal "invalid --through value"))
    (multiple-value-bind (range failure)
        (aitools.org.infrastructure::%time-range-values
         "2026-10-19T09:00:00" "2026-10-19T10:00:00")
      (expect failure :to-be nil)
      (expect (org-kit:org-time-value-iso8601
               (org-kit:org-time-range-start range))
              :to-equal "2026-10-19T09:00:00")
      (expect (org-kit:org-time-value-iso8601
               (org-kit:org-time-range-end range))
              :to-equal "2026-10-19T10:00:00")
      (expect (list (org-kit:org-time-range-include-start-p range) (org-kit:org-time-range-include-end-p range)) :to-equal '(t nil)))
    (dolist (arguments
             (list (list "bad" "2026-10-19T10:00:00")
                   (list "2026-10-19T09:00:00" "bad")
                   (list "2026-10-19T10:00:00" "2026-10-19T09:00:00")))
      (multiple-value-bind (range failure)
          (apply #'aitools.org.infrastructure::%time-range-values arguments)
        (expect range :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument))))
  (it "validates query reference times and applies the compatible fallback"
    (multiple-value-bind (reference failure)
        (aitools.org.infrastructure::%query-reference-time
         "2026-10-19T09:15:00" "UTC" nil)
      (expect failure :to-be nil)
      (expect (cl-date-kit:local-date-time-hour reference) :to-be 9)
      (expect (cl-date-kit:local-date-time-minute reference) :to-be 15))
    (multiple-value-bind (reference failure)
        (aitools.org.infrastructure::%query-reference-time
         "2026-03-08T02:30:00" "America/New_York" nil)
      (expect failure :to-be nil)
      (expect (cl-date-kit:local-date-time-hour reference) :to-be 3))
    (dolist (arguments
             (list (list nil "UTC" nil)
                   (list "2026-10-19" "UTC" nil)
                   (list "2026-10-19T09:00:01" "UTC" nil)
                   (list "2026-10-19T09:00:00+00:00" "UTC" nil)
                   (list "2026-10-19T09:00:00" "" nil)
                   (list "2026-10-19T09:00:00" "UTC" "unknown")))
      (multiple-value-bind (reference failure)
          (apply #'aitools.org.infrastructure::%query-reference-time arguments)
        (expect reference :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)))
    (dolist (at (list "bad" "2026-02-30T09:00:00"))
      (multiple-value-bind (reference failure)
          (aitools.org.infrastructure::%query-reference-time at "UTC" nil)
        (expect reference :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :invalid-time)))
    (multiple-value-bind (reference failure)
        (aitools.org.infrastructure::%query-reference-time
         "2026-03-08T02:30:00" "America/New_York" "strict")
      (expect reference :to-be nil)
      (expect (aitools.org.domain:org-failure-kind failure) :to-be :invalid-time))
    (%with-org-time-adapter-condition
        org-kit:make-time-context
        (make-condition 'org-kit:org-kit-error :message "context failure")
      (multiple-value-bind (reference failure)
          (aitools.org.infrastructure::%query-reference-time
           "2026-10-19T09:00:00" "UTC" nil)
        (expect reference :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :internal))))
  (it "maps query results and query failures"
    (let ((document
            (%org-coverage-document
             (format nil "#+TODO: TODO | DONE~%* TODO Parent :root:~%** DONE Child :urgent:~%"))))
      (multiple-value-bind (matches failure)
          (aitools.org.infrastructure::query-org-document
           document "LEVEL=2" "2026-10-19T09:00:00" "UTC" nil)
        (expect failure :to-be nil)
        (expect (length matches) :to-be 1)
        (let* ((match (first matches))
               (path (aitools.org.domain:org-query-match-heading-path match)))
          (expect (aitools.org.domain:org-query-match-line match) :to-be 3)
          (expect (aitools.org.domain:org-query-match-level match) :to-be 2)
          (expect (aitools.org.domain:org-query-match-title match) :to-equal "Child")
          (expect (aitools.org.domain:org-query-match-todo match) :to-equal "DONE")
          (expect (aitools.org.domain:org-query-match-tags match)
                  :to-equal '("urgent"))
          (expect (mapcar #'aitools.org.domain:org-heading-path-entry-title path)
                  :to-equal '("Parent" "Child"))))
      (multiple-value-bind (matches failure)
          (aitools.org.infrastructure::query-org-document
           document "LEVEL=9" "2026-10-19T09:00:00" "UTC" nil)
        (expect matches :to-be nil)
        (expect failure :to-be nil))
      (%with-org-time-adapter-condition
          org-kit:query-match-at
          (make-condition 'org-kit:org-resource-limit-error
                          :message "query limit"
                          :limit-name :max-output-length :limit 1 :actual 2)
        (multiple-value-bind (matches failure)
            (aitools.org.infrastructure::query-org-document
             document "LEVEL=1" "2026-10-19T09:00:00" "UTC" nil)
          (expect matches :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :too-large)))
      (let ((match (format nil "LEVEL=1~%BAD")))
        (%with-org-time-adapter-condition
            org-kit:query-match-at
            (make-condition 'org-kit:org-query-error
                            :message "bad query" :query match :position 9)
          (multiple-value-bind (matches failure)
              (aitools.org.infrastructure::query-org-document
               document match "2026-10-19T09:00:00" "UTC" nil)
            (expect matches :to-be nil)
            (expect (aitools.org.domain:org-failure-kind failure)
                    :to-be :query-syntax)
            (expect (list (aitools.org.domain:org-failure-line failure) (aitools.org.domain:org-failure-column failure) (aitools.org.domain:org-failure-offset failure)) :to-equal '(2 2 9)))))
      (%with-org-time-adapter-condition
          org-kit:query-match-at
          (make-condition 'org-kit:org-kit-error :message "query failed")
        (multiple-value-bind (matches failure)
            (aitools.org.infrastructure::query-org-document
             document "LEVEL=1" "2026-10-19T09:00:00" "UTC" nil)
          (expect matches :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure) :to-be :internal))))))
