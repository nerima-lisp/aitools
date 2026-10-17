;;;; packages/feature/process/src/application/wait-flow.lisp
;;;;
;;;; `wait`. Unlike `run`, a timeout here is a failure
;;;; (`environment.timeout`, exit 1): the condition holding is the whole
;;;; point of the command.
(in-package #:aitools.process.application)

(defparameter +wait-poll-ms+ 100)

(defstruct (wait-request (:constructor make-wait-request
                             (&key file pattern port bg exit duration (timeout "60s")))
                         (:copier nil))
  (file nil :type (or null string) :read-only t)
  (pattern nil :type (or null string) :read-only t)
  (port nil :type (or null (integer 1 65535)) :read-only t)
  (bg nil :type (or null string) :read-only t)
  (exit nil :type boolean :read-only t)
  (duration nil :type (or null string) :read-only t)
  (timeout "60s" :type string :read-only t))

(defun wait-until/k (ports timeout-ms probe &key (poll-ms +wait-poll-ms+) wake-at-ms on-matched on-timeout)
  "Call PROBE with the elapsed milliseconds every POLL-MS until it returns a
non-NIL result alist, then call ON-MATCHED with (RESULT ELAPSED-MS); call
ON-TIMEOUT with ELAPSED-MS once TIMEOUT-MS passes first. PROBE runs once
more at the deadline, so a condition that becomes true exactly then counts.
WAKE-AT-MS, when given, is an elapsed time the loop never sleeps past, for
a condition known to turn true at that moment."
  (let ((start (funcall (process-ports-monotonic-ms ports))))
    (loop
      (let* ((elapsed (- (funcall (process-ports-monotonic-ms ports)) start))
             (result (funcall probe elapsed)))
        (cond (result (return (funcall on-matched result elapsed)))
              ((>= elapsed timeout-ms) (return (funcall on-timeout elapsed)))
              (t (funcall (process-ports-sleep-ms ports)
                        (min poll-ms (- timeout-ms elapsed)
                             (if (and wake-at-ms (> wake-at-ms elapsed)) (- wake-at-ms elapsed) poll-ms)))))))))

(defun %matched-line-result (line redactions)
  (and line (list (cons "line" line) (cons "redactions" redactions))))

(defun %log-tail-text (ports directory id)
  (let* ((path (%bg-path directory (aitools.process.domain:bg-log-file-name id)))
         (size (or (funcall (process-ports-file-size ports) path) 0))
         (start (max 0 (- size +log-read-limit+))))
    (aitools.process.domain:decode-output-octets
     (funcall (process-ports-read-file-octets ports) path start size))))

(defun %wait-probe (ports condition pattern directory record)
  "A PROBE for WAIT-UNTIL/K checking CONDITION."
  (ecase (aitools.process.domain:wait-condition-kind condition)
    (:file-pattern
     (lambda (elapsed)
       (declare (ignore elapsed))
       (let ((text (funcall (process-ports-read-file-text ports)
                            (aitools.process.domain:wait-condition-path condition))))
         (and text (multiple-value-call #'%matched-line-result
                     (aitools.process.domain:first-matching-line text pattern nil))))))
    (:port
     (lambda (elapsed)
       (declare (ignore elapsed))
       (let ((port (aitools.process.domain:wait-condition-port condition)))
         (and (funcall (process-ports-tcp-connectable-p ports) port)
              (list (cons "port" port))))))
    (:bg-pattern
     (lambda (elapsed)
       (declare (ignore elapsed))
       (multiple-value-call #'%matched-line-result
         (aitools.process.domain:first-matching-line
          (%log-tail-text ports directory (aitools.process.domain:bg-record-id record)) pattern t))))
    (:bg-exit
     (lambda (elapsed)
       (declare (ignore elapsed))
       (multiple-value-bind (running exit-code signal) (%bg-state ports directory record)
         (and (not running)
              (list (cons "exit_code" (aitools.process.domain:json-or-null exit-code))
                    (cons "signal" (aitools.process.domain:json-or-null signal)))))))
    (:duration
     (let ((duration-ms (aitools.process.domain:wait-condition-duration-ms condition)))
       (lambda (elapsed)
         (and (>= elapsed duration-ms) (list (cons "duration_ms" duration-ms))))))))

(defun %wait-for-condition (ports condition pattern timeout-ms directory record on-ok on-error)
  (let ((duration-ms (aitools.process.domain:wait-condition-duration-ms condition)))
    (wait-until/k
     ports timeout-ms (%wait-probe ports condition pattern directory record)
     :wake-at-ms duration-ms
     :on-matched (lambda (result elapsed)
                   (funcall on-ok (append (list (cons "matched" t) (cons "elapsed_ms" elapsed)) result)))
     :on-timeout (lambda (elapsed)
                   (let ((arguments (aitools.process.domain:wait-condition-arguments condition))
                         (bg-id (aitools.process.domain:wait-condition-bg-id condition)))
                     (funcall on-error "environment.timeout"
                              (format nil "the wait condition did not hold within ~Dms" elapsed)
                              :repairs (append
                                        (list (repair "wait-longer" "Wait again with a longer timeout."
                                                       (aitools.protocol.domain:command-line
                                                        "aitools" "wait" arguments "--timeout"
                                                        (format nil "~Dms" (* 2 timeout-ms)))))
                                        (when bg-id
                                          (list (repair "read-log" "Read the bg process's latest output."
                                                         (aitools.protocol.domain:command-line
                                                          "aitools" "bg" "logs" bg-id)))))))))))

(defun %wait-file-path (ports file)
  "--file as the user typed it, made absolute against the working directory
(the shared rule of AITOOLS.WORKSPACE.APPLICATION:USER-PATH-ABSOLUTE)."
  (let ((host (process-ports-workspace-host ports)))
    (if (and file host)
        (aitools.workspace.application:user-path-absolute host file)
        file)))

(defun wait-command/k (ports request &key on-ok on-partial on-error)
  "Block until REQUEST's single condition holds or its timeout
passes. A missing file or a refused connection is not an error; they are
what `wait` waits out."
  (declare (ignore on-partial))
  (let ((duration-text (wait-request-duration request)))
    (flet ((with-condition (duration-ms)
             (multiple-value-bind (condition message)
                 (aitools.process.domain:make-wait-condition
                  :file (%wait-file-path ports (wait-request-file request)) :pattern (wait-request-pattern request)
                  :port (wait-request-port request) :bg (wait-request-bg request)
                  :exit (wait-request-exit request) :duration-text duration-text :duration-ms duration-ms)
               (if (null condition)
                   (funcall on-error "argument.invalid" message
                            :repairs (list (%schema-repair "wait")))
                   (%call-with-duration-ms
                    (wait-request-timeout request) "--timeout" "wait" on-error
                    (lambda (timeout-ms)
                      (%call-with-line-pattern
                       (aitools.process.domain:wait-condition-pattern condition) "wait" on-error
                       (lambda (pattern)
                         (%reporting-port-errors (on-error "wait")
                           (if (aitools.process.domain:wait-condition-bg-id condition)
                               (%call-with-bg-directory
                                ports "wait" on-error
                                (lambda (directory)
                                  (%call-with-bg-record
                                   ports directory (aitools.process.domain:wait-condition-bg-id condition)
                                   on-error
                                   (lambda (record)
                                     (%wait-for-condition ports condition pattern timeout-ms directory record
                                                          on-ok on-error)))))
                               (%wait-for-condition ports condition pattern timeout-ms nil nil
                                                    on-ok on-error)))))))))))
      (if duration-text
          (%call-with-duration-ms duration-text "--duration" "wait" on-error #'with-condition)
          (with-condition nil)))))
