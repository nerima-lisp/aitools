(in-package #:aitools.process.application)

(aitools.kernel.domain:define-flow
    (%signal-error code message &key on-error)
  (aitools.kernel.domain:flow-continue
   :on-error code message :repairs (list (%schema-repair "signal"))))

(defun %signal-alive-p (boundaries target)
  (let ((current (process-boundary-call boundaries :process-process-info (aitools.process.domain:process-identity-pid target))))
    (and current
         (let ((expected-command-line
                 (aitools.process.domain:process-identity-command-line target))
               (current-command-line
                 (aitools.process.domain:process-identity-command-line current)))
           (and (stringp expected-command-line)
                (stringp current-command-line)
                (aitools.process.domain:same-process-p target current))))))

(defun %deliver-process-signal (boundaries target number)
  (unless (process-boundary-call boundaries :process-signal-process target number)
    (error 'process-target-changed
           :message (format nil "process ~D disappeared before signalling"
                            (aitools.process.domain:process-identity-pid target)))))

(defun %signal-target-signal (boundaries target number grace-ms)
  (if grace-ms
      (%terminate-with-grace
       boundaries
       (lambda () (%signal-alive-p boundaries target))
       (lambda (value) (%deliver-process-signal boundaries target value))
       grace-ms)
      (progn
        (%deliver-process-signal boundaries target number)
        number)))

(defun %signal-targets (boundaries pid pattern)
  (if pid
      (let ((info (process-boundary-call boundaries :process-process-info pid)))
        (and info (list info)))
      (let ((compiled (aitools.process.domain:compile-line-pattern pattern)))
        (loop for candidate in (process-boundary-call boundaries :process-list-pids)
              for info = (process-boundary-call boundaries :process-process-info candidate)
              when (and info
                        (let ((command-line
                                (aitools.process.domain:process-identity-command-line info)))
                          (unless (stringp command-line)
                            (error 'process-target-changed
                                   :message (format nil "cannot read argv for process ~D during pattern selection"
                                                    candidate)))
                          (and (process-boundary-call boundaries :process-safe-target-p info)
                               (aitools.process.domain:line-pattern-matches-p compiled command-line))))
                collect info))))
(defun %signal-target-result
    (boundaries pid pattern expect-command expect-start expect-count signal grace-ms)
  (let ((targets (%signal-targets boundaries pid pattern)))
    (cond
      ((and pattern (/= (length targets) expect-count))
       (list :error
             "selection.count-mismatch"
             (format nil "expected ~D processes, found ~D"
                     expect-count
                     (length targets))))
      ((and pid (null targets))
       (list :error
             "refusal.target-changed"
             "target process unavailable"))
      ((and pid
            (not
             (and
              (or (null expect-command)
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
       (list :error
             "refusal.target-changed"
             "PID identity guard does not match"))
      (t
       (if (some (lambda (target)
                   (not (process-boundary-call boundaries :process-safe-target-p target)))
                 targets)
           (list :error
                 "refusal.target-changed"
                 "target changed before signalling")
           (let ((sent '())
                 (number (aitools.process.domain:signal-number signal)))
             (handler-case
                 (progn
                   (dolist (target targets)
                     (let ((last-signal
                             (%signal-target-signal
                              boundaries target number grace-ms)))
                       (push
                        (aitools.protocol.domain:json-object
                         "pid"
                         (aitools.process.domain:process-identity-pid target)
                         "signal"
                         last-signal)
                        sent)))
                   (list :ok
                         (list (cons "items" (nreverse sent))
                               (cons "total" (length targets)))))
               (process-target-changed (condition)
                 (if sent
                     (list :partial
                           (list (cons "items" (nreverse sent))
                                 (cons "total" (length targets))))
                     (error condition)))
               (process-port-error (condition)
                 (if sent
                     (list :partial
                           (list (cons "items" (nreverse sent))
                                 (cons "total" (length targets))))
                     (error condition))))))))))

(defun %signal-selector-error (pid pattern)
  (cond
    ((not (if pid (null pattern) (and pattern (plusp (length pattern)))))
     (list "argument.invalid" "specify exactly one of --pid or --pattern"))
    ((and pid (< pid 2))
     (list "refusal.target-changed" "PID below 2 is unsafe"))))

(defun %signal-identity-error (pid pattern expect-command expect-start expect-count)
  (cond
    ((and pid (not (or (and expect-command (plusp (length expect-command))) expect-start)))
     (list "argument.invalid" "--pid needs --expect-command or --expect-start"))
    ((and pattern (null expect-count))
     (list "argument.invalid" "--pattern needs --expect-count"))
    ((and pattern (or expect-command expect-start))
     (list "argument.invalid" "PID identity guards require --pid"))
    ((and pid expect-count)
     (list "argument.invalid" "--expect-count requires --pattern"))))

(defun %signal-option-error (signal grace)
  (cond
    ((null (aitools.process.domain:signal-number signal))
     (list "argument.invalid" "unknown signal name"))
    ((and grace (not (string-equal signal "TERM")))
     (list "argument.invalid" "--grace requires TERM"))))

(defun %signal-request-error (pid pattern expect-command expect-start expect-count signal grace)
  (or (%signal-selector-error pid pattern)
      (%signal-identity-error pid pattern expect-command expect-start expect-count)
      (%signal-option-error signal grace)))

(aitools.kernel.domain:define-flow
    (signal-command/k boundaries &key pid pattern expect-command expect-start expect-count
                             (signal "TERM") grace on-ok on-partial on-error)
  (declare (type function on-ok on-partial on-error))
  (labels ((report-error (code message)
             (aitools.kernel.domain:flow-call
              (%signal-error code message)
              (:on-error (error-code error-message &key repairs)
                (aitools.kernel.domain:flow-continue
                 :on-error error-code error-message
                 :repairs repairs))))
           (run (grace-ms)
             (%reporting-boundary-errors "signal"
               (let ((result
                       (%signal-target-result
                        boundaries pid pattern expect-command expect-start expect-count signal
                        grace-ms)))
                 (ecase (first result)
                   (:error
                    (report-error (second result) (third result)))
                   (:ok
                    (aitools.kernel.domain:flow-continue
                     :on-ok (second result)))
                   (:partial
                    (aitools.kernel.domain:flow-continue
                     :on-partial (second result))))))))
    (let ((input-error
            (%signal-request-error
             pid pattern expect-command expect-start expect-count signal grace)))
      (cond
        (input-error
         (report-error (first input-error) (second input-error)))
        (t
         (if grace
             (aitools.kernel.domain:flow-call
              (%call-with-duration-ms grace "--grace" "signal")
              (:on-value (grace-ms)
                (run grace-ms))
              (:on-error (code message &key repairs)
                (aitools.kernel.domain:flow-continue
                 :on-error code message :repairs repairs)))
             (run nil)))))))
