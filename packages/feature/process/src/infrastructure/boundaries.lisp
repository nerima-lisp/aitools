;;;;
;;;; Production capability entries are supplied to composition for registration.
;;;; Construction does no I/O: the state directory is resolved, and
;;;; `bg/` created, only when a bg command first asks for it.
(in-package #:aitools.process.infrastructure)

(defun %bg-directory-function (state-directory-function)
  (if (null state-directory-function)
      (constantly nil)
      (lambda ()
        (let ((directory (merge-pathnames "bg/" (uiop:ensure-directory-pathname
                                                 (funcall state-directory-function)))))
          (%process-boundary-io ("creating ~A" directory)
            (ensure-directories-exist directory))
          directory))))

(defun %temporary-directory-function (state-directory-function)
  (if (null state-directory-function)
      (constantly nil)
      (lambda ()
        (string-right-trim "/" (uiop:native-namestring
                                (merge-pathnames "tmp/" (uiop:ensure-directory-pathname
                                                         (funcall state-directory-function))))))))

(defun make-production-process-capabilities (&key state-directory-function workspace-host with-span &allow-other-keys)
  "Return production process, signal, time, and file capability entries."
  (labels ((span-outcome (values)
             (case (first values)
               (:timed-out :timeout)
               (:unavailable :unavailable)
               (:exists :error)
               (otherwise :ok)))
           (instrument (name operation)
             (lambda (&rest arguments)
               (if with-span
                   (funcall with-span :child_process name operation arguments
                            :outcome-function (function span-outcome))
                   (apply operation arguments)))))
    (list (cons :process-run-program
                (instrument "process.run" (function %run-program)))
          (cons :process-bg-directory
                (%bg-directory-function state-directory-function))
          (cons :process-launch-detached
                (instrument "process.launch-detached" (function %launch-detached)))
          (cons :process-list-directory (function %list-directory))
          (cons :process-read-file-text (function %read-file-text))
          (cons :process-read-file-octets (function %read-octets))
          (cons :process-file-size (function %file-size))
          (cons :process-create-file-exclusive (function %create-file-exclusive))
          (cons :process-replace-file (function %replace-file))
          (cons :process-remove-file (function %remove-file))
          (cons :process-group-alive-p (function %group-alive-p))
          (cons :process-signal-group (function %signal-group))
          (cons :process-list-pids (function %host-list-pids))
          (cons :process-process-info (function %safe-process-info))
          (cons :process-safe-target-p (function %safe-target-p))
          (cons :process-signal-process (function %signal-process))
          #+linux (cons :process-pidfd-open (function %linux-pidfd-open))
          #+linux (cons :process-pidfd-send-signal (function %linux-pidfd-send-signal))
          (cons :process-tcp-connectable-p (function %tcp-connectable-p))
          (cons :process-universal-time (function get-universal-time))
          (cons :process-monotonic-ms (function %monotonic-ms))
          (cons :process-sleep-ms (function %sleep-ms))
          (cons :process-workspace-host workspace-host)
          (cons :process-temporary-directory
                (%temporary-directory-function state-directory-function)))))

