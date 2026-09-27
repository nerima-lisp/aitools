;;;; packages/feature/env/src/application/ports.lisp
;;;;
;;;; The side-effect boundary of the env context. The layer rules let only
;;;; infrastructure name cl-boundary-kit, so the port here is a record of
;;;; plain functions; infrastructure builds one from cl-boundary-kit
;;;; boundaries (clock, environment, filesystem, host-info) plus process-kit
;;;; and sb-posix adapters, and unit tests build one from cl-boundary-kit
;;;; fakes through the same constructor.
(in-package #:aitools.env.application)

(defstruct (env-ports (:constructor make-env-ports
                          (&key now-milliseconds environment-variable environment-variables
                             read-file list-directory read-link executable-p run-program
                             file-system-space system-identity user-id host-name user-name
                             workspace-root))
                      (:copier nil))
  "NOW-MILLISECONDS () -> Unix epoch milliseconds.
ENVIRONMENT-VARIABLE (name) -> string or NIL.
ENVIRONMENT-VARIABLES () -> alist (NAME . VALUE) sorted by name.
READ-FILE (path external-format) -> string, or NIL when PATH does not exist
  or vanished (a /proc entry of an exited process).
LIST-DIRECTORY (path) -> entry names, or NIL when unreadable.
READ-LINK (path) -> symlink target string, or NIL.
EXECUTABLE-P (path) -> true for an executable regular file.
RUN-PROGRAM (program arguments timeout-seconds) -> (VALUES STATUS EXIT-CODE
  STDOUT STDERR), STATUS one of :EXITED, :TIMEOUT, :NOT-STARTED. Never reads
  stdin.
FILE-SYSTEM-SPACE (path) -> (VALUES TOTAL-BYTES AVAILABLE-BYTES) or NIL.
SYSTEM-IDENTITY () -> (VALUES SYSNAME RELEASE MACHINE), as uname(2) reports them.
USER-ID () -> integer. HOST-NAME (), USER-NAME () -> string.
WORKSPACE-ROOT () -> the workspace root directory."
  (now-milliseconds nil :type function :read-only t)
  (environment-variable nil :type function :read-only t)
  (environment-variables nil :type function :read-only t)
  (read-file nil :type function :read-only t)
  (list-directory nil :type function :read-only t)
  (read-link nil :type function :read-only t)
  (executable-p nil :type function :read-only t)
  (run-program nil :type function :read-only t)
  (file-system-space nil :type function :read-only t)
  (system-identity nil :type function :read-only t)
  (user-id nil :type function :read-only t)
  (host-name nil :type function :read-only t)
  (user-name nil :type function :read-only t)
  (workspace-root nil :type function :read-only t))

(defun %now (ports) (funcall (env-ports-now-milliseconds ports)))
(defun %getenv (ports name) (funcall (env-ports-environment-variable ports) name))
(defun %read-text (ports path) (funcall (env-ports-read-file ports) path :utf-8))
(defun %read-octets (ports path)
  (let ((text (funcall (env-ports-read-file ports) path :latin-1)))
    (and text (aitools.env.domain:octets-from-latin-1 text))))
(defun %run (ports program arguments timeout-seconds)
  (funcall (env-ports-run-program ports) program arguments timeout-seconds))
