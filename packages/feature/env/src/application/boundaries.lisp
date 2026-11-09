(in-package #:aitools.env.application)

(defmacro define-env-operation (name key parameters)
  `(defun ,name (boundaries ,@parameters)
     (funcall
      (cl-boundary-kit:boundary-context-require boundaries ,key)
      ,@parameters)))

(defun %now (boundaries)
  (cl-boundary-kit:clock-now
   (cl-boundary-kit:boundary-context-require boundaries :env-clock)))

(defun %getenv (boundaries name)
  (cl-boundary-kit:environment-get
   (cl-boundary-kit:boundary-context-require boundaries :env-environment)
   name))

(defun %environment-variables (boundaries)
  (cl-boundary-kit:environment-list
   (cl-boundary-kit:boundary-context-require boundaries :env-environment)))

(defun %read-file (boundaries path external-format)
  (let ((filesystem
          (cl-boundary-kit:boundary-context-require boundaries :env-filesystem)))
    (when (cl-boundary-kit:filesystem-path-exists-p filesystem path)
      (handler-case
          (cl-boundary-kit:filesystem-read-file
           filesystem path :external-format external-format)
        (file-error () nil)))))

(defun %read-text (boundaries path)
  (%read-file boundaries path :utf-8))

(defun %read-octets (boundaries path)
  (let ((text (%read-file boundaries path :latin-1)))
    (and text (aitools.env.domain:octets-from-latin-1 text))))

(defun %host-name (boundaries)
  (cl-boundary-kit:host-info-hostname
   (cl-boundary-kit:boundary-context-require boundaries :env-host-info)))

(defun %user-name (boundaries)
  (cl-boundary-kit:host-info-username
   (cl-boundary-kit:boundary-context-require boundaries :env-host-info)))

(define-env-operation %sleep-seconds :env-sleep-seconds (seconds))

#+darwin
(define-env-operation %cpu-counters :env-cpu-counters ())

#+darwin
(define-env-operation %sysctl-mib :env-sysctl-mib (name))

(define-env-operation %list-directory :env-list-directory (path))

(define-env-operation %read-link :env-read-link (path))

(define-env-operation %executable-p :env-executable-p (path))

(define-env-operation %run :env-run-program (program arguments timeout-seconds))

(define-env-operation %file-system-space :env-file-system-space (path))

(define-env-operation %system-identity :env-system-identity ())

(define-env-operation %user-id :env-user-id ())

(define-env-operation %workspace-root :env-workspace-root ())
