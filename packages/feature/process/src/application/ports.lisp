;;;; packages/feature/process/src/application/ports.lisp
;;;;
;;;; The side-effect boundary of the process context. Each slot is a
;;;; function; the infrastructure layer supplies the production ones and unit
;;;; tests supply fakes. Ports signal PROCESS-PORT-ERROR (and nothing else)
;;;; for an I/O failure, which the flows report as `environment.io`.
(in-package #:aitools.process.application)

(define-condition process-port-error (error)
  ((message :initarg :message :reader process-port-error-message))
  (:report (lambda (condition stream)
             (write-string (process-port-error-message condition) stream))))

(define-condition process-target-changed (process-port-error) ())

(defun %missing-port (&rest arguments)
  (declare (ignore arguments))
  (error "process port not supplied"))

(defstruct (process-ports (:constructor make-process-ports
                              (&key (run-program #'%missing-port)
                                    (bg-directory #'%missing-port)
                                    (launch-detached #'%missing-port)
                                    (list-directory #'%missing-port)
                                    (read-file-text #'%missing-port)
                                    (read-file-octets #'%missing-port)
                                    (file-size #'%missing-port)
                                    (create-file-exclusive #'%missing-port)
                                    (replace-file #'%missing-port)
                                    (remove-file #'%missing-port)
                                    (group-alive-p #'%missing-port)
                                    (signal-group #'%missing-port)
                                    (list-pids #'%missing-port)
                                    (process-info #'%missing-port)
                                    (safe-target-p #'%missing-port)
                                    (signal-process #'%missing-port)
                                    (tcp-connectable-p #'%missing-port)
                                    (universal-time #'%missing-port)
                                    (monotonic-ms #'%missing-port)
                                    (sleep-ms #'%missing-port)
                                    workspace-host
                                    (temporary-directory (constantly nil))))
                          (:copier nil))
  ;; (ARGV TIMEOUT-MS &key STDOUT-PATH ON-EXITED ON-TIMED-OUT ON-UNAVAILABLE
  ;; ON-EXISTS): run ARGV without a shell, stdin at /dev/null. ON-EXITED /
  ;; ON-TIMED-OUT receive a PROCESS-OUTCOME; ON-UNAVAILABLE receives (MESSAGE
  ;; PROGRAM) when ARGV could not be started at all. With STDOUT-PATH, stdout
  ;; goes to that newly created file instead of being captured; ON-EXISTS is
  ;; called, and nothing runs, when the file already exists.
  (run-program nil :type function :read-only t)
  ;; () -> the workspace's `bg/` directory pathname (existing), or NIL when
  ;; no state directory is available to this process.
  (bg-directory nil :type function :read-only t)
  ;; (ARGV LOG-PATH EXIT-PATH &key ON-STARTED ON-UNAVAILABLE): start ARGV
  ;; detached in its own session with stdout+stderr appended to LOG-PATH; its
  ;; exit status is later written to EXIT-PATH. ON-STARTED receives the PID
  ;; (also the process-group and session ID); ON-UNAVAILABLE (MESSAGE PROGRAM).
  (launch-detached nil :type function :read-only t)
  ;; (DIRECTORY) -> list of file-name strings (no directory part).
  (list-directory nil :type function :read-only t)
  ;; (PATH) -> file contents decoded as UTF-8 with replacement, or NIL.
  (read-file-text nil :type function :read-only t)
  ;; (PATH START END) -> octet vector of bytes [START,END).
  (read-file-octets nil :type function :read-only t)
  ;; (PATH) -> byte size, or NIL when PATH does not exist.
  (file-size nil :type function :read-only t)
  ;; (PATH TEXT) -> T, or NIL when PATH already exists (nothing written).
  (create-file-exclusive nil :type function :read-only t)
  ;; (PATH TEXT): atomically replace PATH's contents.
  (replace-file nil :type function :read-only t)
  ;; (PATH): remove PATH if it exists.
  (remove-file nil :type function :read-only t)
  ;; (PID) -> true while PID still leads the session and process group it
  ;; was started as.
  (group-alive-p nil :type function :read-only t)
  ;; (PID SIGNAL) -> T when SIGNAL was delivered to PID's process group,
  ;; NIL when the group no longer exists.
  (signal-group nil :type function :read-only t)
  ;; () -> PID list; (PID) -> PROCESS-IDENTITY or NIL; (IDENTITY SIGNAL)
  ;; -> T on delivery, NIL when gone. The signal port independently checks
  ;; identity, uid, ancestry, and process group immediately before kill(2).
  (list-pids nil :type function :read-only t)
  (process-info nil :type function :read-only t)
  (safe-target-p nil :type function :read-only t)
  (signal-process nil :type function :read-only t)
  ;; (PORT) -> true when a TCP connection to PORT on the loopback succeeds.
  (tcp-connectable-p nil :type function :read-only t)
  ;; () -> the current universal time.
  (universal-time nil :type function :read-only t)
  ;; () -> a monotonic clock reading in milliseconds.
  (monotonic-ms nil :type function :read-only t)
  ;; (MILLISECONDS): block for MILLISECONDS.
  (sleep-ms nil :type function :read-only t)
  ;; An AITOOLS.WORKSPACE.APPLICATION:WORKSPACE-HOST for workspace boundary checks of
  ;; `run --stdout-to`, or NIL when none was wired in.
  (workspace-host nil :read-only t)
  ;; () -> the real path of the workspace's mktemp area (a string), or NIL.
  (temporary-directory nil :type function :read-only t))
