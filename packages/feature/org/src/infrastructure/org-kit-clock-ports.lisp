(in-package #:aitools.org.infrastructure)

(defun %org-clock-error-failure (condition)
  (case (org-kit:org-clock-error-reason condition)
    (:clock-not-running (%failure :selection-no-match "the selected heading has no running clock"))
    (:multiple-running-clocks (%failure :selection-ambiguous "the selected heading has multiple running clocks"))
    (:clock-already-running (%failure :argument "the selected heading already has a running clock"))
    (:invalid-range (%failure :argument "clock range is invalid"))
    (:unsupported-option (%failure :argument "clock options are invalid"))
    (otherwise (%failure :internal "Org clock operation failed"))))

(defun %org-clock-edit (engine options direction)
  (multiple-value-bind (heading at context failure) (%org-selected-heading engine options)
    (when failure
      (return-from %org-clock-edit
        (values nil nil failure)))
    (handler-case (multiple-value-bind (result value)
                      (if (eq direction :in)
                          (org-kit:clock-in engine heading :at at :time-context context)
                          (org-kit:clock-out engine heading :at at :time-context context))
                    (values (org-kit:edit-result-updated-document result)
                            (list
                             (cons
                              (if (eq direction :in)
                                  "timestamp"
                                  "interval")
                              value))
                            nil))
      (org-kit:org-clock-error (condition)
        (values nil nil (%org-clock-error-failure condition)))
      (org-kit:org-time-error ()
        (values nil nil (%failure :argument "clock time is invalid")))
      (org-kit:org-resource-limit-error ()
        (values nil nil (%failure :too-large "clock edit exceeded a resource limit")))
      (org-kit:org-kit-error ()
        (values nil nil (%failure :internal "Org clock edit failed"))))))

(defun %clock-report-range (from through)
  (multiple-value-bind (start start-failure) (%explicit-range-time from "--from")
    (when start-failure
      (return-from %clock-report-range
        (values nil start-failure)))
    (multiple-value-bind (end end-failure) (%explicit-range-time through "--through")
      (when end-failure
        (return-from %clock-report-range
          (values nil end-failure)))
      (handler-case (values (org-kit:make-time-range start end :include-start-p t :include-end-p nil) nil)
        (org-kit:org-time-error ()
          (values nil (%failure :argument "--from must be before --through")))))))

(defun %clock-report-row (row)
  (list :level (org-kit:clocktable-row-level row) :title (org-kit:clocktable-row-title row) :tags (org-kit:clocktable-row-tags row) :minutes (org-kit:clocktable-row-minutes row)))

(defun clock-report-org-document (document match from through at tz dst-policy)
  (multiple-value-bind (heading at-value context failure)
      (%org-selected-heading (aitools.org.domain:org-document-engine-value document) (list :match match :at at :tz tz :dst-policy dst-policy))
    (declare (ignore at-value))
    (when failure
      (return-from clock-report-org-document
        (values nil failure)))
    (multiple-value-bind (range range-failure) (%clock-report-range from through)
      (when range-failure
        (return-from clock-report-org-document
          (values nil range-failure)))
      (handler-case (let* ((engine (aitools.org.domain:org-document-engine-value document))
                           (result (org-kit:clocktable-data engine heading :range range :time-context context :match nil :max-depth 30)))
                      (values
                       (list :from
                             from
                             :through
                             through
                             :total-minutes
                             (org-kit:clocktable-result-total-minutes result)
                             :rows
                             (mapcar #'%clock-report-row (org-kit:clocktable-result-rows result)))
                       nil))
        (org-kit:org-clock-error (condition)
          (values nil (%org-clock-error-failure condition)))
        (org-kit:org-time-error ()
          (values nil (%failure :argument "clock report time range is invalid")))
        (org-kit:org-resource-limit-error ()
          (values nil (%failure :too-large "clock report exceeded a resource limit")))
        (org-kit:org-query-error (condition)
          (values nil (%query-syntax-failure match condition)))
        (org-kit:org-kit-error ()
          (values nil (%failure :internal "Org clock report failed")))))))
