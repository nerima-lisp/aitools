(in-package #:aitools.process.application)

;;; ------------------------------------------------------------- bg stop

(defun %wait-for-exit (ports alive-p budget-ms)
  (let ((start (funcall (process-ports-monotonic-ms ports))))
    (loop
      (unless (funcall alive-p)
        (return t))
      (let ((elapsed (- (funcall (process-ports-monotonic-ms ports)) start)))
        (when (>= elapsed budget-ms)
          (return nil))
        (funcall (process-ports-sleep-ms ports) (min +stop-poll-ms+ (- budget-ms elapsed)))))))

(defun %wait-for-group-exit (ports pid budget-ms)
  (%wait-for-exit ports (lambda () (funcall (process-ports-group-alive-p ports) pid)) budget-ms))

(defun %terminate-with-grace (ports alive-p send grace-ms)
  "Return the last signal sent. SEND handles its own safety checks."
  (funcall send +sigterm+)
  (if (%wait-for-exit ports alive-p grace-ms)
      +sigterm+
      (progn (funcall send +sigkill+) +sigkill+)))

(defun %send-stop-signal (ports directory record signal)
  "Record SIGNAL, then send it. Recording comes first because the
supervisor dies with its group and can never report the signal itself."
  (let ((updated (aitools.process.domain:copy-bg-record-with-stop-signal record signal)))
    (funcall (process-ports-replace-file ports)
             (%bg-path directory (aitools.process.domain:bg-record-file-name
                                  (aitools.process.domain:bg-record-id record)))
             (aitools.process.domain:serialize-bg-record updated))
    (funcall (process-ports-signal-group ports) (aitools.process.domain:bg-record-pid record) signal)
    updated))

(defun %stop-fields (ports directory record stopped)
  (multiple-value-bind (running exit-code signal) (%bg-state ports directory record)
    (declare (ignore running))
    (list (cons "id" (aitools.process.domain:bg-record-id record))
          (cons "stopped" (aitools.protocol.domain:json-boolean stopped))
          (cons "exit_code" (aitools.process.domain:json-or-null exit-code))
          (cons "signal" (aitools.process.domain:json-or-null signal)))))

(defun bg-stop/k (ports id &key (grace "5s") on-ok on-partial on-error)
  "SIGTERM to ID's process group, then SIGKILL if it is still there
after GRACE. `stopped` is false when the process had already ended."
  (declare (ignore on-partial))
  (aitools.kernel.domain:flow-call
      (%call-with-duration-ms grace "--grace" "bg.stop")
    (:on-value (grace-ms)
      (%reporting-port-errors (on-error "bg.stop")
        (%call-with-bg-directory
         ports "bg.stop" on-error
         (lambda (directory)
           (%call-with-bg-record
            ports directory id on-error
            (lambda (record)
              (let ((pid (aitools.process.domain:bg-record-pid record)))
                (if (not (nth-value 0 (%bg-state ports directory record)))
                    (funcall on-ok (%stop-fields ports directory record nil))
                    (let ((record record))
                      (%terminate-with-grace
                       ports
                       (lambda () (funcall (process-ports-group-alive-p ports) pid))
                       (lambda (signal)
                         (setf record (%send-stop-signal ports directory record signal)))
                       grace-ms)
                      (if (%wait-for-group-exit ports pid +kill-wait-ms+)
                          (funcall on-ok (%stop-fields ports directory record t))
                          (funcall on-error
                                   "environment.io"
                                   (format nil
                                           "bg process ~A (pid ~D) did not exit after SIGKILL"
                                           id pid)
                                   :repairs
                                   (list (repair "check-status"
                                                 "Check the process again."
                                                 (aitools.protocol.domain:command-line
                                                  "aitools" "bg" "status" id))))))))))))))
    (:on-error (code message &key repairs)
      (funcall on-error code message :repairs repairs))))
