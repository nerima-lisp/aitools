(in-package #:aitools.process.application)

;;; ------------------------------------------------------------- bg logs

(defun bg-logs/k (ports id &key (tail 100) from grep (strip-ansi t) on-ok on-partial on-error)
  "Lines of ID's log. Without FROM, the last TAIL lines; with FROM
(a byte offset), up to TAIL lines starting there. `next_offset` is where the
following `--from` read continues without skipping or repeating a line."
  (aitools.kernel.domain:flow-call
      (%call-with-line-pattern grep "bg.logs")
    (:on-pattern (pattern)
      (%reporting-port-errors (on-error "bg.logs")
        (%call-with-bg-directory
         ports "bg.logs" on-error
         (lambda (directory)
           (%call-with-bg-record
            ports directory id on-error
            (lambda (record)
              (let* ((log-path (%bg-path directory (aitools.process.domain:bg-log-file-name id)))
                     (size (or (funcall (process-ports-file-size ports) log-path) 0)))
                (if (and from (> from size))
                    (funcall on-error
                             "argument.invalid"
                             (format nil "--from ~D is past the end of the log (~D bytes)" from size)
                             :repairs
                             (list (repair "read-from-end"
                                           "Read from the current end of the log."
                                           (aitools.protocol.domain:command-line
                                            "aitools" "bg" "logs" id "--from"
                                            (princ-to-string size)))))
                    (let* ((start (if from from (max 0 (- size +log-read-limit+))))
                           (end (if from (min size (+ from +log-read-limit+)) size))
                           (running (nth-value 0 (%bg-state ports directory record)))
                           (slice (aitools.process.domain:slice-log
                                   (funcall (process-ports-read-file-octets ports) log-path start end)
                                   start
                                   :count tail :from-p (and from t) :final-p (and (not running) (= end size))
                                   :strip-p strip-ansi :pattern pattern
                                   :omitted-before-p (and (null from) (plusp start))
                                   :more-after-p (< end size)))
                           (next-offset (aitools.process.domain:log-slice-next-offset slice))
                           (fields (list (cons "id" id)
                                         (cons "running" (aitools.protocol.domain:json-boolean running))
                                         (cons "lines" (aitools.process.domain:log-slice-lines slice))
                                         (cons "truncated"
                                               (aitools.protocol.domain:json-boolean
                                                (aitools.process.domain:log-slice-truncated slice)))
                                         (cons "next_offset" next-offset)
                                         (cons "redactions"
                                               (aitools.process.domain:log-slice-redactions slice))
                                         (cons "next_commands"
                                               (list (aitools.protocol.domain:command-line
                                                      "aitools" "bg" "logs" id "--from"
                                                      (princ-to-string next-offset)))))))
                      (if (aitools.process.domain:log-slice-truncated slice)
                          (funcall on-partial fields)
                          (funcall on-ok fields)))))))))))
    (:on-error (code message &key repairs)
      (funcall on-error code message :repairs repairs))))

;;; ----------------------------------------------------------- bg status

(defun %status-item (ports directory record)
  (multiple-value-bind (running exit-code signal) (%bg-state ports directory record)
    (aitools.process.domain:bg-status-item record running exit-code signal)))

(defun bg-status/k (ports id &key on-ok on-partial on-error)
  "Every bg process started in this workspace, or only ID."
  (%reporting-port-errors (on-error "bg.status")
    (%call-with-bg-directory
     ports "bg.status" on-error
     (lambda (directory)
       (flet ((report (records observed-total omitted)
                (funcall (if omitted on-partial on-ok)
                         (list (cons "items" (mapcar (lambda (record) (%status-item ports directory record))
                                                    records))
                               (cons "total" (length records))
                               (cons "observed_total" observed-total)
                               (cons "omitted" omitted)))))
         (if id
             (%call-with-bg-record ports directory id on-error
                                   (lambda (record) (report (list record) 1 nil)))
             (let ((ids (%recorded-bg-ids ports directory))
                   (records '())
                   (omitted '()))
               (dolist (listed-id ids)
                 (let* ((text (funcall (process-ports-read-file-text ports)
                                       (%bg-path directory (aitools.process.domain:bg-record-file-name listed-id))))
                        (reason (if text
                                    (handler-case
                                        (progn
                                          (push (aitools.process.domain:parse-bg-record text listed-id) records)
                                          nil)
                                      (aitools.process.domain:invalid-bg-record () "invalid-record"))
                                    "record-unavailable")))
                   (when reason
                     (push (aitools.protocol.domain:json-object "id" listed-id "reason" reason)
                           omitted))))
               (report (nreverse records) (length ids) (nreverse omitted)))))))))
