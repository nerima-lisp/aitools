(in-package #:aitools.org.infrastructure)

(defun %timestamp-content-tokens (timestamp)
  (let ((position 1)
        (end (1- (length timestamp)))
        (tokens nil))
    (loop while (< position end)
          do (loop while (and (< position end) (find (char timestamp position) '(#\Space #\Tab)))
                   do (incf position)) (when (< position end)
                                         (let ((token-end
                                                (or
                                                 (position-if
                                                  (lambda (character)
                                                    (find character '(#\Space #\Tab)))
                                                  timestamp
                                                  :start
                                                  position
                                                  :end
                                                  end)
                                                 end)))
                                           (push (subseq timestamp position token-end) tokens)
                                           (setf position token-end))))
    (nreverse tokens)))

(defun %timestamp-component-count (view)
  (let ((start (org-kit:org-timestamp-view-start view)))
    (+ 1
       (if (eq (org-kit:org-time-value-kind start) :date-time)
           1
           0)
       (if (org-kit:org-timestamp-view-repeater-amount view)
           1
           0)
       (if (org-kit:org-timestamp-view-warning-amount view)
           1
           0))))

(defun %timestamp-ast-token-sequence-p (timestamp view)
  (= (length (%timestamp-content-tokens timestamp)) (%timestamp-component-count view)))

(defun %planning-timestamp-failure (message)
  (%failure :argument message))

(defun %planning-timestamp-node (timestamp)
  (let ((nodes nil)
        (source (format nil "* Probe~%SCHEDULED: ~A~%" timestamp)))
    (handler-case (let* ((engine (org-kit:parse-org (sb-ext:string-to-octets source :external-format :utf-8)))
                         (root (org-kit:document-root engine)))
                    (org-kit:map-nodes
                     (lambda (node)
                       (when (eq (org-kit:node-type node) :timestamp)
                         (push node nodes)))
                     root)
                    (if (= (length nodes) 1)
                        (values (car nodes) nil)
                        (values nil (%planning-timestamp-failure "--timestamp must contain one Org timestamp"))))
      (org-kit:org-kit-error (condition)
        (declare (ignore condition))
        (values nil (%planning-timestamp-failure "--timestamp is not a valid timestamp for this command"))))))

(defun %same-calendar-date-p (left right)
  (and (= (org-kit:org-time-value-year left) (org-kit:org-time-value-year right))
       (= (org-kit:org-time-value-month left) (org-kit:org-time-value-month right))
       (= (org-kit:org-time-value-day left) (org-kit:org-time-value-day right))))

(defun validate-planning-timestamp (kind timestamp)
  (let* ((expected
          (if (and (stringp kind) (string= kind "closed"))
              :inactive
              :active))
         (opening (and (stringp timestamp) (plusp (length timestamp)) (char timestamp 0)))
         (closing
          (and opening
               (case opening
                 (#\< #\>)
                 (#\[ #\])))))
    (unless
        (and (member kind '("scheduled" "deadline" "closed") :test #'equal)
             (stringp timestamp)
             (> (length timestamp) 2)
             (not (position #\Newline timestamp))
             (not (position #\Return timestamp))
             closing
             (char= (char timestamp (1- (length timestamp))) closing))
      (return-from validate-planning-timestamp
        (values nil (%planning-timestamp-failure "--timestamp must be one single-line Org timestamp"))))
    (multiple-value-bind (node failure) (%planning-timestamp-node timestamp)
      (when failure
        (return-from validate-planning-timestamp
          (values nil failure)))
      (handler-case (let* ((view (org-kit:timestamp-view node))
                           (timestamp-kind (org-kit:org-timestamp-view-kind view))
                           (start (org-kit:org-timestamp-view-start view))
                           (end (org-kit:org-timestamp-view-end view)))
                      (unless
                          (and
                           (if (eq expected :active)
                               (member timestamp-kind '(:active :active-range))
                               (member timestamp-kind '(:inactive :inactive-range)))
                           (or (null end) (and (eq (org-kit:org-time-value-kind start) :date-time) (eq (org-kit:org-time-value-kind end) :date-time) (%same-calendar-date-p start end))))
                        (return-from validate-planning-timestamp
                          (values nil (%planning-timestamp-failure "--timestamp kind or range does not match --kind"))))
                      (unless (%timestamp-ast-token-sequence-p timestamp view)
                        (return-from validate-planning-timestamp
                          (values nil (%planning-timestamp-failure "--timestamp contains an unsupported suffix"))))
                      (values view nil))
        (org-kit:org-time-error (condition)
          (declare (ignore condition))
          (values nil (%planning-timestamp-failure "--timestamp is not a valid timestamp for this command")))))))
