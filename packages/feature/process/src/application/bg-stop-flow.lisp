(in-package #:aitools.process.application)

;;; ------------------------------------------------------------- bg stop

(defun %wait-for-exit (boundaries alive-p budget-ms)
  (let ((start (process-boundary-call boundaries :process-monotonic-ms)))
    (loop
      (unless (funcall alive-p)
        (return t))
      (let ((elapsed (- (process-boundary-call boundaries :process-monotonic-ms) start)))
        (when (>= elapsed budget-ms)
          (return nil))
        (process-boundary-call boundaries :process-sleep-ms (min +stop-poll-ms+ (- budget-ms elapsed)))))))

(defun %wait-for-group-exit (boundaries pid budget-ms)
  (%wait-for-exit boundaries (lambda () (process-boundary-call boundaries :process-group-alive-p pid)) budget-ms))

(defun %terminate-with-grace (boundaries alive-p send grace-ms)
  "Return the last signal sent. SEND handles its own safety checks."
  (funcall send +sigterm+)
  (if (%wait-for-exit boundaries alive-p grace-ms)
      +sigterm+
      (progn (funcall send +sigkill+) +sigkill+)))

(defun %send-stop-signal (boundaries directory record signal)
  "Record SIGNAL, then send it. Recording comes first because the
supervisor dies with its group and can never report the signal itself."
  (let ((updated (aitools.process.domain:copy-bg-record-with-stop-signal record signal)))
    (process-boundary-call boundaries :process-replace-file (%bg-path directory (aitools.process.domain:bg-record-file-name
                                  (aitools.process.domain:bg-record-id record)))
             (aitools.process.domain:serialize-bg-record updated))
    (process-boundary-call boundaries :process-signal-group (aitools.process.domain:bg-record-pid record) signal)
    updated))

(defun %stop-fields (boundaries directory record stopped)
  (multiple-value-bind (running exit-code signal) (%bg-state boundaries directory record)
    (declare (ignore running))
    (list (cons "id" (aitools.process.domain:bg-record-id record))
          (cons "stopped" (aitools.protocol.domain:json-boolean stopped))
          (cons "exit_code" (aitools.process.domain:json-or-null exit-code))
          (cons "signal" (aitools.process.domain:json-or-null signal)))))

(aitools.kernel.domain:define-flow
    (bg-stop/k boundaries id &key (grace "5s") on-ok on-partial on-error)
  "SIGTERM to ID's process group, then SIGKILL if it is still there
after GRACE. `stopped` is false when the process had already ended."
  (declare (ignore on-partial))
  (aitools.kernel.domain:flow-call
   (%call-with-duration-ms grace "--grace" "bg.stop")
   (:on-value (grace-ms)
     (%reporting-boundary-errors "bg.stop"
       (aitools.kernel.domain:flow-call
        (%call-with-bg-directory boundaries "bg.stop")
        (:on-directory (directory)
          (aitools.kernel.domain:flow-call
           (%call-with-bg-record boundaries directory id)
           (:on-record (record)
             (let ((pid (aitools.process.domain:bg-record-pid record)))
               (if (not (nth-value 0 (%bg-state boundaries directory record)))
                   (aitools.kernel.domain:flow-continue
                    :on-ok (%stop-fields boundaries directory record nil))
                   (let ((record record))
                     (%terminate-with-grace
                      boundaries
                      (lambda () (process-boundary-call boundaries :process-group-alive-p pid))
                      (lambda (signal)
                        (setf record (%send-stop-signal boundaries directory record signal)))
                      grace-ms)
                     (if (%wait-for-group-exit boundaries pid +kill-wait-ms+)
                         (aitools.kernel.domain:flow-continue
                          :on-ok (%stop-fields boundaries directory record t))
                         (aitools.kernel.domain:flow-continue
                          :on-error
                          "environment.io"
                          (format nil
                                  "bg process ~A (pid ~D) did not exit after SIGKILL"
                                  id pid)
                          :repairs
                          (list (repair "check-status"
                                        "Check the process again."
                                        (aitools.protocol.domain:command-line
                                         "aitools" "bg" "status" id)))))))))
           (:on-error (code message &key repairs candidates)
             (aitools.kernel.domain:flow-continue :on-error code message
               :repairs repairs
               :candidates candidates))))
        (:on-error (code message &key repairs)
          (aitools.kernel.domain:flow-continue :on-error code message
            :repairs repairs)))))
    (:on-error (code message &key repairs)
      (aitools.kernel.domain:flow-continue :on-error code message
        :repairs repairs))))
