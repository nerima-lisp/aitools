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
                            :outcome-function #'span-outcome)
                   (apply operation arguments)))))
    (list (cons :process-run-program
                (instrument "process.run" #'%run-program))
          (cons :process-bg-directory
                (%bg-directory-function state-directory-function))
          (cons :process-launch-detached
                (instrument "process.launch-detached" #'%launch-detached))
          (cons :process-list-directory #'%list-directory)
          (cons :process-read-file-text #'%read-file-text)
          (cons :process-read-file-octets #'%read-octets)
          (cons :process-file-size #'%file-size)
          (cons :process-create-file-exclusive #'%create-file-exclusive)
          (cons :process-replace-file #'%replace-file)
          (cons :process-remove-file #'%remove-file)
          (cons :process-group-alive-p #'%group-alive-p)
          (cons :process-signal-group #'%signal-group)
          (cons :process-list-pids #'%host-list-pids)
          (cons :process-process-info #'%safe-process-info)
          (cons :process-safe-target-p #'%safe-target-p)
          (cons :process-signal-process #'%signal-process)
          (cons :process-tcp-connectable-p #'%tcp-connectable-p)
          (cons :process-universal-time #'get-universal-time)
          (cons :process-monotonic-ms #'%monotonic-ms)
          (cons :process-sleep-ms #'%sleep-ms)
          (cons :process-workspace-host workspace-host)
          (cons :process-temporary-directory
                (%temporary-directory-function state-directory-function)))))
