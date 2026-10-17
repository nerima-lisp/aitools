(in-package #:aitools.process.application)

(defun %signal-error (on-error code message)
  (funcall on-error code message :repairs (list (%schema-repair "signal"))))

(defun %signal-alive-p (ports target)
  (let ((current (funcall (process-ports-process-info ports)
                          (aitools.process.domain:process-identity-pid target))))
    (and current
         (let ((expected-command-line
                 (aitools.process.domain:process-identity-command-line target))
               (current-command-line
                 (aitools.process.domain:process-identity-command-line current)))
           (and (stringp expected-command-line)
                (stringp current-command-line)
                (aitools.process.domain:same-process-p target current))))))

(defun %deliver-process-signal (ports target number)
  (unless (funcall (process-ports-signal-process ports) target number)
    (error 'process-target-changed
           :message (format nil "process ~D disappeared before signalling"
                            (aitools.process.domain:process-identity-pid target)))))

(defun %signal-targets (ports pid pattern)
  (if pid
      (let ((info (funcall (process-ports-process-info ports) pid)))
        (and info (list info)))
      (let ((compiled (aitools.process.domain:compile-line-pattern pattern)))
        (loop for candidate in (funcall (process-ports-list-pids ports))
              for info = (funcall (process-ports-process-info ports) candidate)
              when (and info
                        (let ((command-line
                                (aitools.process.domain:process-identity-command-line info)))
                          (unless (stringp command-line)
                            (error 'process-target-changed
                                   :message (format nil "cannot read argv for process ~D during pattern selection"
                                                    candidate)))
                          (and (funcall (process-ports-safe-target-p ports) info)
                               (aitools.process.domain:line-pattern-matches-p compiled command-line))))
                collect info))))

(defun signal-command/k (ports &key pid pattern expect-command expect-start expect-count
                                 (signal "TERM") grace on-ok on-partial on-error)
  (declare (type function on-ok on-partial on-error))
  (cond
    ((not (if pid (null pattern) (and pattern (plusp (length pattern)))))
     (%signal-error on-error "argument.invalid" "specify exactly one of --pid or --pattern"))
    ((and pid (< pid 2))
     (%signal-error on-error "refusal.target-changed" "PID below 2 is unsafe"))
    ((and pid (not (or (and expect-command (plusp (length expect-command))) expect-start)))
     (%signal-error on-error "argument.invalid" "--pid needs --expect-command or --expect-start"))
    ((and pattern (null expect-count))
     (%signal-error on-error "argument.invalid" "--pattern needs --expect-count"))
    ((and pattern (or expect-command expect-start))
     (%signal-error on-error "argument.invalid" "PID identity guards require --pid"))
    ((and pid expect-count)
     (%signal-error on-error "argument.invalid" "--expect-count requires --pattern"))
    ((null (aitools.process.domain:signal-number signal))
     (%signal-error on-error "argument.invalid" "unknown signal name"))
    ((and grace (not (string-equal signal "TERM")))
     (%signal-error on-error "argument.invalid" "--grace requires TERM"))
    (t
     (flet ((run (grace-ms)
              (%reporting-port-errors (on-error "signal")
                (let ((targets (%signal-targets ports pid pattern)))
                  (cond
                    ((and pattern (/= (length targets) expect-count))
                     (%signal-error on-error "selection.count-mismatch"
                                    (format nil "expected ~D processes, found ~D" expect-count (length targets))))
                    ((and pid (null targets))
                     (%signal-error on-error "refusal.target-changed" "target process unavailable"))
                    ((and pid (not (and (or (null expect-command)
                                            (let ((command-line
                                                    (aitools.process.domain:process-identity-command-line
                                                     (first targets))))
                                              (and (stringp command-line)
                                                   (search expect-command command-line))))
                                        (or (null expect-start)
                                            (string= expect-start
                                                     (princ-to-string
                                                      (aitools.process.domain:process-identity-start
                                                       (first targets))))))))
                     (%signal-error on-error "refusal.target-changed" "PID identity guard does not match"))
                    (t
                     ;; Preflight the entire set before the first send. A later
                     ;; PID reuse may still race a send; the signal port checks
                     ;; each identity again immediately before kill(2), but
                     ;; only a process handle could close that race completely.
                     (dolist (target targets)
                       (unless (funcall (process-ports-safe-target-p ports) target)
                         (return-from signal-command/k
                           (%signal-error on-error "refusal.target-changed"
                                          "target changed before signalling"))))
                     (let ((sent '())
                           (number (aitools.process.domain:signal-number signal)))
                       (handler-case
                           (progn
                             (dolist (target targets)
                               (let ((last-signal
                                       (if grace-ms
                                           (%terminate-with-grace
                                            ports (lambda () (%signal-alive-p ports target))
                                            (lambda (value) (%deliver-process-signal ports target value))
                                            grace-ms)
                                           (progn (%deliver-process-signal ports target number)
                                                  number))))
                                 (push (aitools.protocol.domain:json-object
                                        "pid" (aitools.process.domain:process-identity-pid target)
                                        "signal" last-signal) sent)))
                             (funcall on-ok (list (cons "items" (nreverse sent))
                                                  (cons "total" (length targets)))))
                         (process-target-changed (condition)
                           (if sent
                               (funcall on-partial
                                        (list (cons "items" (nreverse sent))
                                              (cons "total" (length targets))))
                               (error condition)))
                         (process-port-error (condition)
                           (if sent
                               (funcall on-partial
                                        (list (cons "items" (nreverse sent))
                                              (cons "total" (length targets))))
                               (error condition)))))))))))
       (if grace
           (%call-with-duration-ms grace "--grace" "signal" on-error #'run)
           (run nil))))))
