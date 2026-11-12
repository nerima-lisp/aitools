(require :asdf)

(let* ((fixture-directory
         (make-pathname :name nil :type nil :defaults *load-truename*))
       (source-root (truename (merge-pathnames "../../../" fixture-directory)))
       (fasl-directory (uiop:getenv "AITOOLS_E2E_SYS_CLI_FASL")))
  (unless (and fasl-directory (plusp (length fasl-directory)))
    (error "AITOOLS_E2E_SYS_CLI_FASL is required."))
  (asdf:initialize-output-translations
   (list :output-translations
         (list t
               (list (uiop:ensure-directory-pathname fasl-directory)
                     :**/ :*.*.*))
         :ignore-inherited-configuration))
  (asdf:initialize-source-registry
   (list :source-registry (list :directory source-root) :inherit-configuration))
  (asdf:load-system "aitools/cli"))

(defun sys-source-e2e-filesystem (boundaries mode)
  (let* ((filesystem
           (cl-boundary-kit:boundary-context-require boundaries :env-filesystem))
         (target
           (if (member mode '("uptime-missing" "uptime-corrupt") :test #'string=)
               "/proc/uptime"
               "/proc/loadavg"))
         (missing
           (member mode '("uptime-missing" "load-average-missing") :test #'string=))
         (corrupt
           (member mode '("uptime-corrupt" "load-average-corrupt") :test #'string=)))
    (cl-boundary-kit:boundary-context-with
     boundaries
     :env-filesystem
     (cl-boundary-kit:make-filesystem
      :path-exists-p-fn
      (lambda (path)
        (and (not (and missing (string= path target)))
             (cl-boundary-kit:filesystem-path-exists-p filesystem path)))
      :read-file-fn
      (lambda (path &key external-format)
        (if (string= path target)
            (and corrupt
                 (if (string= target "/proc/uptime")
                     "invalid uptime"
                     "invalid load average"))
            (cl-boundary-kit:filesystem-read-file
             filesystem path :external-format external-format)))))))

(defun sys-source-e2e-sysctl (boundaries mode)
  (let* ((sysctl
           (cl-boundary-kit:boundary-context-require boundaries :env-sysctl-mib))
         (target
           (if (member mode '("uptime-missing" "uptime-corrupt") :test #'string=)
               :kern-boottime
               :vm-loadavg))
         (missing
           (member mode '("uptime-missing" "load-average-missing") :test #'string=)))
    (cl-boundary-kit:boundary-context-with
     boundaries
     :env-sysctl-mib
     (lambda (name)
       (if (eq name target)
           (if missing
               nil
               (if (eq target :kern-boottime)
                   "invalid uptime"
                   '("invalid" "invalid" "invalid")))
           (funcall sysctl name))))))

(defun sys-source-e2e-dependencies (&rest arguments)
  (let* ((boundaries
           (apply #'aitools.env.infrastructure:make-production-env-boundaries arguments))
         (mode (uiop:getenv "AITOOLS_E2E_SYS_SOURCE_MODE")))
    (unless (and mode
                 (member mode '("uptime-missing" "uptime-corrupt"
                                "load-average-missing" "load-average-corrupt")
                         :test #'string=))
      (error "AITOOLS_E2E_SYS_SOURCE_MODE is missing or invalid."))
    #+linux (setf boundaries (sys-source-e2e-filesystem boundaries mode))
    #+darwin (setf boundaries (sys-source-e2e-sysctl boundaries mode))
    #-(or linux darwin) (error "The sys source fixture requires Linux or Darwin." )
    boundaries))

(defun sys-source-e2e-image-entry-point ()
  (let ((registration
          (assoc :env aitools/cli::*context-command-registrations*)))
    (unless registration
      (error "The env context registration is unavailable."))
    (setf (third registration) (function sys-source-e2e-dependencies)))
  (setf aitools/cli::*application*
        (multiple-value-list (aitools/cli::build-app)))
  (aitools/cli::image-entry-point))

(defmethod asdf:output-files :around
    ((operation asdf:program-op)
     (system (eql (asdf:find-system "aitools/cli"))))
  (declare (ignore operation system))
  (let ((target (uiop:getenv "AITOOLS_E2E_SYS_CLI_OUTPUT")))
    (unless (and target (plusp (length target)))
      (error "AITOOLS_E2E_SYS_CLI_OUTPUT is required."))
    (values (list (pathname target)) t)))

(progn
  (setf (asdf/system:component-entry-point (asdf:find-system "aitools/cli"))
        "CL-USER::SYS-SOURCE-E2E-IMAGE-ENTRY-POINT")
  (asdf:operate 'asdf:program-op "aitools/cli"))
