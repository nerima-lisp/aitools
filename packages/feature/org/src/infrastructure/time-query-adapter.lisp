(in-package #:aitools.org.infrastructure)

(defun %query-failure (kind message)
  (aitools.org.domain:make-org-failure
   :kind kind :message message))

(defun %query-syntax-failure (match condition)
  (let* ((offset (org-kit:org-query-error-position condition))
         (end (if (integerp offset)
                  (max 0 (min offset (length match)))
                  0))
         (line 1)
         (column 1))
    (loop for index below end
          for character = (char match index)
          do (if (char= character #\Newline)
                 (setf line (1+ line) column 1)
                 (incf column)))
    (aitools.org.domain:make-org-failure
     :kind :query-syntax :message (org-kit:org-kit-error-message condition)
     :line line :column column :offset offset)))

(defun %dst-policy-keyword (value)
  (if (stringp value)
      (cdr (assoc value '(("compatible" . :compatible)
                          ("earlier" . :earlier)
                          ("later" . :later)
                          ("strict" . :strict))
                    :test #'string=))
      (and (member value '(:compatible :earlier :later :strict)) value)))

(defun %query-reference-time (at tz dst-policy)
  (let ((policy (%dst-policy-keyword (or dst-policy "compatible"))))
    (unless (and (stringp at) (stringp tz) (plusp (length tz)) policy)
      (return-from %query-reference-time
        (values nil
                (%query-failure
                 :argument
                 "query requires valid --at, --tz, and --dst-policy"))))
    (handler-case
        (let* ((value (org-kit:make-org-time-value at))
               (zone (org-kit:resolve-time-zone tz)))
          (unless (and (eq (org-kit:org-time-value-kind value) :date-time)
                       (null (org-kit:org-time-value-offset-minutes value))
                       (zerop (org-kit:org-time-value-second value)))
            (return-from %query-reference-time
              (values nil
                      (%query-failure
                       :argument
                       "--at must be a local YYYY-MM-DDTHH:MM:00 date-time"))))
          (let* ((context
                   (org-kit:make-time-context
                    :reference-time value :zone zone :dst-policy policy))
                 (instant (org-kit:org-time-context-reference-instant context)))
            (values
             (cl-date-kit:local-date-time-of-instant
              instant (cl-date-kit:find-time-zone tz))
             nil)))
      (org-kit:org-time-error (condition)
        (values nil
                (%query-failure :invalid-time
                                (org-kit:org-kit-error-message condition))))
      (org-kit:org-kit-error (condition)
        (values nil
                (%query-failure :internal
                                (org-kit:org-kit-error-message condition))))
      (cl-date-kit:cl-date-kit-error (condition)
        (values nil
                (%query-failure :invalid-time (princ-to-string condition)))))))

(defun %heading-path (node)
  (mapcar (lambda (segment)
            (aitools.org.domain:make-org-heading-path-entry
             :title (getf segment :title)
             :index (getf segment :index)))
          (org-kit:node-heading-path node)))

(defun %query-match-entry (node)
  (multiple-value-bind (line column) (org-kit:node-start-position node)
    (declare (ignore column))
    (aitools.org.domain:make-org-query-match
     :line (or line 1)
     :level (org-kit:node-property node :level 1)
     :title (org-kit:node-property node :raw-value "")
     :todo (org-kit:node-property node :todo-keyword)
     :tags (or (org-kit:node-property node :tags) '())
     :heading-path (%heading-path node))))

(defun query-org-document (document match at tz dst-policy)
  (multiple-value-bind (reference failure) (%query-reference-time at tz dst-policy)
    (when failure (return-from query-org-document (values nil failure)))
    (handler-case
        (let* ((engine (aitools.org.domain:org-document-engine-value document))
               (nodes (org-kit:query-match-at engine match reference)))
          (values (mapcar (lambda (node) (%query-match-entry node)) nodes) nil))
      (org-kit:org-resource-limit-error (condition)
        (values nil (%query-failure :too-large
                                    (org-kit:org-kit-error-message condition))))
      (org-kit:org-query-error (condition)
        (values nil (%query-syntax-failure match condition)))
      (org-kit:org-kit-error (condition)
        (values nil (%query-failure :internal
                                    (org-kit:org-kit-error-message condition)))))))
