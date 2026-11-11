(in-package #:aitools.org.infrastructure)

(defun %agenda-date-failure (message)
  (%failure :argument message))

(defun %agenda-repeat-mode (value)
  (cond
    ((null value) :all)
    ((and (stringp value) (string= value "all")) :all)
    ((and (stringp value) (string= value "next")) :next)
    ((and (stringp value) (string= value "none")) :none)))

(defun %agenda-entry-line (entry)
  (let ((source (org-kit:agenda-entry-source-node entry)))
    (loop for node = source then (org-kit:node-parent node)
          while node
          when (eq (org-kit:node-type node) :headline)
            return (nth-value 0 (org-kit:node-start-position node))
          finally
             (return (when source
                       (nth-value 0 (org-kit:node-start-position source)))))))

(defun %agenda-entry-kind-name (entry)
  (let ((kind (org-kit:agenda-entry-kind entry)))
    (typecase kind
      (symbol (string-downcase (symbol-name kind)))
      (string (string-downcase kind))
      (t "unknown"))))

(defun %agenda-entry-data (entry)
  (list :date (org-kit:org-time-value-iso8601
               (org-kit:agenda-entry-date entry))
        :kind (%agenda-entry-kind-name entry)
        :line (%agenda-entry-line entry)
        :title (or (org-kit:agenda-entry-headline entry) "")
        :todo (org-kit:agenda-entry-todo-state entry)
        :tags (org-kit:agenda-entry-effective-tags entry)))

(defun agenda-org-document (document from through at tz dst-policy
                                     future-repeats)
  (multiple-value-bind (start-date start-failure) (%date-value from)
    (when start-failure
      (return-from agenda-org-document
                   (values nil (%agenda-date-failure
                                "--from must be a valid YYYY-MM-DD date"))))
    (multiple-value-bind (end-date end-failure) (%date-value through)
      (when end-failure
        (return-from agenda-org-document
                     (values nil (%agenda-date-failure
                                  "--through must be a valid YYYY-MM-DD date"))))
      (multiple-value-bind (time-value time-context reference-time
                                       time-failure)
          (%explicit-time-values at tz dst-policy)
        (declare (ignore time-value reference-time))
        (when time-failure
          (return-from agenda-org-document
                       (values nil (%agenda-date-failure
                                    "--at, --tz, or --dst-policy is invalid for agenda"))))
        (let ((repeat-mode (%agenda-repeat-mode future-repeats)))
          (unless repeat-mode
            (return-from agenda-org-document
                         (values nil (%agenda-date-failure
                                      "--future-repeats must be all, next, or none"))))
          (handler-case
              (let* ((result
                      (org-kit:agenda
                       (list (aitools.org.domain:org-document-engine-value
                              document))
                       start-date end-date
                       :time-context time-context
                       :future-repeats repeat-mode))
                     (entries
                      (mapcar #'%agenda-entry-data
                              (org-kit:agenda-result-entries result))))
                (values (list :from (org-kit:org-time-value-iso8601 start-date)
                              :through (org-kit:org-time-value-iso8601 end-date)
                              :entries entries
                              :count (length entries))
                        nil))
            (org-kit:org-resource-limit-error (condition)
                                              (declare (ignore condition))
                                              (values nil (%failure :too-large
                                                                    "Org agenda exceeds a configured limit")))
            (org-kit:org-agenda-error (condition)
                                      (case (org-kit:org-agenda-error-reason condition)
                                        ((:invalid-range :invalid-option)
                                         (values nil (%agenda-date-failure
                                                      "agenda date range or option is invalid")))
                                        ((:habit-not-todo :habit-without-schedule
                                                          :habit-without-repeater :unsupported-entry)
                                         (values nil (%failure :syntax
                                                               "Org agenda content is invalid")))
                                        (otherwise
                                         (values nil (%failure :internal
                                                               "Org agenda operation failed")))))
            (org-kit:org-time-error (condition)
                                    (declare (ignore condition))
                                    (values nil (%agenda-date-failure
                                                 "agenda date or time is invalid")))
            (cl-date-kit:cl-date-kit-error (condition)
                                           (declare (ignore condition))
                                           (values nil (%agenda-date-failure
                                                        "agenda date or time is invalid")))
            (org-kit:org-kit-error (condition)
                                   (declare (ignore condition))
                                   (values nil (%failure :internal
                                                         "Org agenda operation failed"))))))))
  )