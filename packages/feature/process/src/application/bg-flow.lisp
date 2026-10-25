;;;;
;;;; Background process record helpers and `bg start`. Processes are tracked
;;;; through records in this workspace's `bg/` directory, so `bg stop` can
;;;; only reach processes started by aitools.
(in-package #:aitools.process.application)

(defparameter +log-read-limit+ (* 16 1024 1024)
  "Most log bytes one `bg logs` call or `wait --bg --pattern` poll reads.")

(defparameter +id-reservation-attempts+ 64
  "How many successive IDs `bg start` tries when concurrent starts in the
same workspace keep claiming the one it picked.")

(defparameter +stop-poll-ms+ 50)

(defparameter +kill-wait-ms+ 2000
  "How long `bg stop` waits for the group to vanish after SIGKILL.")

(defconstant +sigterm+ 15)
(defconstant +sigkill+ 9)

(defun %bg-path (directory file-name)
  (merge-pathnames file-name directory))

(defun %status-repair ()
  (repair "list-bg" "List the bg processes started in this workspace." "aitools bg status"))

(aitools.kernel.domain:define-flow
    (%call-with-bg-directory ports command-name &key on-error on-directory)
  (let ((directory (funcall (process-ports-bg-directory ports))))
    (if directory
        (aitools.kernel.domain:flow-continue :on-directory directory)
        (aitools.kernel.domain:flow-continue
         :on-error "environment.unavailable"
         "no aitools state directory is available for this workspace, so bg processes cannot be tracked"
         :repairs (list (%schema-repair command-name))))))

(defun %recorded-bg-ids (ports directory)
  "IDs with a record in DIRECTORY, in start order."
  (sort (remove nil (mapcar #'aitools.process.domain:bg-record-id-from-file-name
                            (funcall (process-ports-list-directory ports) directory)))
        #'< :key #'aitools.process.domain:bg-id-number))

(aitools.kernel.domain:define-flow
    (%call-with-bg-record ports directory id &key on-error on-record)
  "Call ON-RECORD with the record for ID. If no record exists, report
`input.not-found` with the existing IDs as candidates through ON-ERROR.
Report an invalid record as `environment.io` through ON-ERROR. Check ID
against the ID grammar before using it in a path."
  (let ((text (and (aitools.process.domain:bg-id-p id)
                   (funcall (process-ports-read-file-text ports)
                            (%bg-path directory (aitools.process.domain:bg-record-file-name id))))))
    (if (null text)
        (aitools.kernel.domain:flow-continue
         :on-error
         "input.not-found"
         (format nil "no bg process ~S was started by aitools in this workspace" id)
         :candidates (%recorded-bg-ids ports directory)
         :repairs (list (%status-repair)))
        (handler-case (aitools.process.domain:parse-bg-record text id)
          (aitools.process.domain:invalid-bg-record (condition)
            (aitools.kernel.domain:flow-continue
             :on-error
             "environment.io"
             (aitools.process.domain:invalid-bg-record-message condition)
             :repairs (list (%status-repair))))
          (:no-error (record)
            (aitools.kernel.domain:flow-continue :on-record record))))))

(defun %read-exit-status (ports directory id)
  "(VALUES FOUND EXIT-CODE SIGNAL) from ID's exit file."
  (let ((text (funcall (process-ports-read-file-text ports)
                       (%bg-path directory (aitools.process.domain:bg-exit-file-name id)))))
    (if (null text)
        (values nil nil nil)
        (multiple-value-bind (exit-code signal) (aitools.process.domain:parse-exit-status text)
          (values (or exit-code signal) exit-code signal)))))

(defun %bg-state (ports directory record)
  "(VALUES RUNNING EXIT-CODE SIGNAL) for RECORD. A surviving process group is
reported as running even when the supervisor has already written its exit
file, because a target may leave descendants in that group. Once the group is
gone, the exit file is authoritative; it is read again after a negative probe
because the supervisor may have written it in between. A group that is gone
without an exit file was killed by a signal together with its supervisor, so
the signal `bg stop` recorded is the best available answer (NIL when something
else killed it)."
  (let ((id (aitools.process.domain:bg-record-id record)))
    (if (funcall (process-ports-group-alive-p ports) (aitools.process.domain:bg-record-pid record))
        (values t nil nil)
        (multiple-value-bind (found exit-code signal) (%read-exit-status ports directory id)
          (cond (found (values nil exit-code signal))
                (t (multiple-value-bind (found exit-code signal) (%read-exit-status ports directory id)
                     (if found
                         (values nil exit-code signal)
                         (values nil nil (aitools.process.domain:bg-record-stop-signal record))))))))))

;;; ------------------------------------------------------------ bg start

(defun %reserve-bg-id (ports directory)
  "Claim a fresh ID by creating its log file exclusively; return (VALUES ID
LOG-PATH), or NIL when every attempt lost a race."
  (loop repeat +id-reservation-attempts+
        do (let* ((ids (remove nil (mapcar #'aitools.process.domain:bg-file-id
                                           (funcall (process-ports-list-directory ports) directory))))
                  (id (aitools.process.domain:next-bg-id ids))
                  (log-path (%bg-path directory (aitools.process.domain:bg-log-file-name id))))
             (when (funcall (process-ports-create-file-exclusive ports) log-path "")
               (return (values id log-path))))))

(defun %bg-started-fields (record log-path)
  (let ((id (aitools.process.domain:bg-record-id record)))
    (list (cons "id" id)
          (cons "name" (aitools.process.domain:json-or-null (aitools.process.domain:bg-record-name record)))
          (cons "pid" (aitools.process.domain:bg-record-pid record))
          (cons "log" (namestring log-path))
          (cons "next_commands" (list (aitools.protocol.domain:command-line "aitools" "bg" "logs" id)
                                      (aitools.protocol.domain:command-line "aitools" "bg" "stop" id))))))

(aitools.kernel.domain:define-flow
    (%record-started-process ports directory id name argv log-path pid &key on-ok on-error)
  ;; The record's argv is display-only (bg status shows it, nothing re-runs it),
  ;; so persist a redacted copy: a secret passed on the command line must not
  ;; live on disk in bg/<id>.json.
  (let ((record (aitools.process.domain:make-bg-record
                 :id id :name name
                 :argv (aitools.protocol.domain:redact-secret-sequence argv)
                 :pid pid
                 :started (aitools.process.domain:format-utc-timestamp
                           (funcall (process-ports-universal-time ports))))))
    (handler-case
        (funcall (process-ports-replace-file ports)
                 (%bg-path directory (aitools.process.domain:bg-record-file-name id))
                 (aitools.process.domain:serialize-bg-record record))
      (process-port-error (condition)
        ;; Untracked, the process could never be stopped through aitools again.
        (funcall (process-ports-signal-group ports) pid +sigkill+)
        (aitools.kernel.domain:flow-continue
         :on-error
         "environment.io"
         (format nil "~A; the process was killed because it could not be recorded"
                 (process-port-error-message condition))
         :repairs (list (%status-repair))))
      (:no-error (&rest values)
        (declare (ignore values))
        (aitools.kernel.domain:flow-continue
         :on-ok (%bg-started-fields record log-path))))))

(aitools.kernel.domain:define-flow
    (bg-start/k ports argv name &key on-ok on-partial on-error)
  "Start ARGV detached, logging to `bg/<id>.log`."
  (declare (ignore on-partial))
  (cond
    ((null argv)
     (aitools.kernel.domain:flow-continue
      :on-error
      "argument.invalid"
      "bg start needs the program and its arguments after --"
      :repairs (list (repair "start-program" "Pass the program after --."
                             "aitools bg start -- sleep 60"))))
    ((and name (not (aitools.process.domain:bg-name-valid-p name)))
     (aitools.kernel.domain:flow-continue
      :on-error
      "argument.invalid"
      "--name must be 1 to 64 characters without control characters"
      :repairs (list (%schema-repair "bg.start"))))
    (t
     (%reporting-port-errors "bg.start"
       (aitools.kernel.domain:flow-call
        (%call-with-bg-directory ports "bg.start")
        (:on-directory (directory)
          (multiple-value-bind (id log-path) (%reserve-bg-id ports directory)
            (if (null id)
                (aitools.kernel.domain:flow-continue
                 :on-error
                 "environment.busy"
                 "could not claim a bg ID; other starts kept taking it"
                 :repairs
                 (list (repair "retry" "Start the process again."
                               (aitools.protocol.domain:command-line
                                "aitools" "bg" "start" "--" argv))))
                (multiple-value-bind (status value program)
                    (funcall (process-ports-launch-detached ports)
                             argv
                             log-path
                             (%bg-path directory (aitools.process.domain:bg-exit-file-name id)))
                  (ecase status
                    (:started
                     (aitools.kernel.domain:flow-call
                      (%record-started-process
                       ports directory id name argv log-path value)
                      (:on-ok (fields)
                        (aitools.kernel.domain:flow-continue :on-ok fields))
                      (:on-error (code message &key repairs)
                        (aitools.kernel.domain:flow-continue
                         :on-error code message :repairs repairs))))
                    (:unavailable
                     (funcall (process-ports-remove-file ports) log-path)
                     (aitools.kernel.domain:flow-call
                      (%unavailable-program-error value program)
                      (:on-error (code text &key repairs)
                        (aitools.kernel.domain:flow-continue
                         :on-error code text :repairs repairs)))))))))
        (:on-error (code message &key repairs)
          (aitools.kernel.domain:flow-continue
           :on-error code message :repairs repairs)))))))
