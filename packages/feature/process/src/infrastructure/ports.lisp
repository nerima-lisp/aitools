;;;; packages/feature/process/src/infrastructure/ports.lisp
;;;;
;;;; The composition root calls MAKE-PRODUCTION-PROCESS-PORTS once per aitools
;;;; run. Construction does no I/O: the state directory is resolved, and
;;;; `bg/` created, only when a bg command first asks for it.
(in-package #:aitools.process.infrastructure)

(defun %bg-directory-function (state-directory-function)
  (if (null state-directory-function)
      (constantly nil)
      (lambda ()
        (let ((directory (merge-pathnames "bg/" (uiop:ensure-directory-pathname
                                                 (funcall state-directory-function)))))
          (%port-io ("creating ~A" directory)
            (ensure-directories-exist directory))
          directory))))

(defun %temporary-directory-function (state-directory-function)
  (if (null state-directory-function)
      (constantly nil)
      (lambda ()
        (string-right-trim "/" (uiop:native-namestring
                                (merge-pathnames "tmp/" (uiop:ensure-directory-pathname
                                                         (funcall state-directory-function))))))))

(defun make-production-process-ports (&key state-directory-function workspace-host &allow-other-keys)
  "PROCESS-PORTS backed by real processes, files, and sockets.
STATE-DIRECTORY-FUNCTION returns this workspace's state directory;
without it the bg commands report `environment.unavailable`. WORKSPACE-HOST
is the workspace context's production host, used for `run --stdout-to`'s
workspace boundary check; without it that option reports `environment.unavailable`."
  (aitools.process.application:make-process-ports
   :workspace-host workspace-host
   :temporary-directory (%temporary-directory-function state-directory-function)
   :run-program #'%run-program
   :bg-directory (%bg-directory-function state-directory-function)
   :launch-detached #'%launch-detached
   :list-directory #'%list-directory
   :read-file-text #'%read-file-text
   :read-file-octets #'%read-octets
   :file-size #'%file-size
   :create-file-exclusive #'%create-file-exclusive
   :replace-file #'%replace-file
   :remove-file #'%remove-file
   :group-alive-p #'%group-alive-p
   :signal-group #'%signal-group
   :list-pids #'%host-list-pids
   :process-info #'%safe-process-info
   :safe-target-p #'%safe-target-p
   :signal-process #'%signal-process
   :tcp-connectable-p #'%tcp-connectable-p
   :universal-time #'get-universal-time
   :monotonic-ms #'%monotonic-ms
   :sleep-ms #'%sleep-ms))
