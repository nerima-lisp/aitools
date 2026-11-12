;;;;
;;;; `sys info`, `sys env`, `sys tools`, `sys procs`, `sys ports`
;;;; The OS decides the source: Linux reads
;;;; /proc, Darwin runs sysctl, vm_stat, ps, and lsof by absolute path, so a
;;;; PATH entry cannot substitute them. Every source is read-only; nothing
;;;; here signals or otherwise touches another process.
(in-package #:aitools.env.application)

(defparameter *darwin-programs*
  '(:sysctl "/usr/sbin/sysctl" :vm-stat "/usr/bin/vm_stat" :ps "/bin/ps" :lsof "/usr/sbin/lsof"
    :netstat "/usr/sbin/netstat"))

(aitools.kernel.domain:define-flow
    (sys-network/k boundaries &key (limit 100) on-ok on-partial on-error)
  (if (not (and (integerp limit) (<= 1 limit 1000)))
      (aitools.kernel.domain:flow-continue
       :on-error
       "argument.invalid"
       "Network limit must be between 1 and 1000.")
      (let* ((os (%os-kind (%system-identity boundaries)))
             (source (case os
                       #+linux (:linux "/proc/net/dev")
                       #+darwin (:darwin "netstat -ibn"))))
        (if (null source)
            (aitools.kernel.domain:flow-continue
             :on-error
             "environment.unavailable"
             "Network counters are unsupported on this OS.")
            (let ((text (case os
                          #+linux (:linux (%read-text boundaries source))
                          #+darwin (:darwin (%run-output boundaries (%darwin-program :netstat) '("-ibn"))))))
              (multiple-value-bind (items valid)
                  (if text
                      (aitools.env.domain:parse-network-counters text os)
                      (values nil nil))
                (if (not valid)
                    (aitools.kernel.domain:flow-continue
                     :on-error
                     "environment.unavailable"
                     (concatenate 'string
                                  "Network counters unavailable: missing source, permission denied, "
                                  "failed command, or malformed data."))
                    (let* ((truncated (> (length items) limit))
                           (fields
                             (list
                              (cons "source" source)
                              (cons "basis" "cumulative_interface_counters")
                              (cons "units"
                                    (%object
                                     (cons "bytes" "bytes")
                                     (cons "packets" "packets")
                                     (cons "errors" "events")))
                              (cons "items"
                                    (mapcar
                                     (lambda (row) (apply #'%object row))
                                     (subseq items 0 (min limit (length items)))))
                              (cons "total" (length items))
                              (cons "limit" limit)
                              (cons "truncated"
                                    (aitools.protocol.domain:json-boolean truncated)))))
                      (if truncated
                          (aitools.kernel.domain:flow-continue :on-partial fields)
                          (aitools.kernel.domain:flow-continue :on-ok fields))))))))))

(aitools.kernel.domain:define-flow
    (sys-disk/k boundaries &key path on-ok on-error)
  (let* ((path (or path (%workspace-root boundaries)))
         (os (%os-kind (%system-identity boundaries))))
    (if (null os)
        (aitools.kernel.domain:flow-continue
         :on-error
         "environment.unavailable"
         "Filesystem capacity is unsupported on this OS.")
        (multiple-value-bind (total available)
            (%file-system-space boundaries path)
          (if (not (and (integerp total)
                        (integerp available)
                        (<= 0 available total)))
              (aitools.kernel.domain:flow-continue
               :on-error
               "environment.unavailable"
               (concatenate 'string
                            "Filesystem capacity unavailable: missing path, permission denied, "
                            "or failed statfs."))
              (aitools.kernel.domain:flow-continue
               :on-ok
               (list (cons "path" path)
                     (cons "source" "statfs")
                     (cons "units" "bytes")
                     (cons "scope" "filesystem_containing_path")
                     (cons "total_bytes" total)
                     (cons "available_bytes" available)
                     (cons "available_basis" "unprivileged_available_blocks")
                     (cons "truncated"
                           (aitools.protocol.domain:json-boolean nil)))))))))

(defparameter *source-timeout-seconds* 60
  "Upper bound for one OS source command. Only a hang guard: `ps -axww` was
observed taking 6-18 s on a heavily loaded Darwin host, so a tight bound
would turn load into environment.unavailable.")
(defun %snapshot-user-name (boundaries)
  (if *snapshot-deadline*
      (multiple-value-bind (status exit-code stdout)
          (%run boundaries "/usr/bin/id" '("-un") *source-timeout-seconds*)
        (when (and (eq status :exited) (eql exit-code 0))
          (let ((name (string-trim '(#\Space #\Tab #\Return #\Newline)
                                   (or stdout ""))))
            (and (plusp (length name)) name))))
      (%user-name boundaries)))

(defun %darwin-program (key &optional boundaries)
  (or (and (eq key :ps) boundaries (%getenv boundaries "AITOOLS_DARWIN_PS"))
      (getf *darwin-programs* key)))

(defun %null-if-nil (value)
  (if (null value) (aitools.env.domain:json-null) value))

(defun %os-kind (sysname)
  (cond ((string-equal sysname "Linux") :linux)
        ((string-equal sysname "Darwin") :darwin)))

(defun %run-output (boundaries program arguments
                    &optional (timeout-seconds *source-timeout-seconds*))
  "STDOUT of PROGRAM when it ran and exited 0, else NIL."
  (multiple-value-bind (status exit-code stdout)
      (%run boundaries program arguments timeout-seconds)
    (and (eq status :exited) (eql exit-code 0) stdout)))

(aitools.kernel.domain:define-flow
    (%unavailable what tool &key on-error)
  (aitools.kernel.domain:flow-continue
   :on-error
   "environment.unavailable"
   what
   :repairs
   (list (repair "check-tool"
                 (format nil "Check whether ~A is installed." tool)
                 (format nil "aitools sys tools ~A" tool)))))

;;; ---------------------------------------------------------------- sys info

(defun %cpus-and-memory (boundaries os &key strict
                                             (timeout-seconds *source-timeout-seconds*))
  "(VALUES CPUS MEMORY-TOTAL MEMORY-AVAILABLE), each NIL when unknown."
  (case os
    (:linux
     (let ((cpuinfo (%read-text boundaries "/proc/cpuinfo"))
           (meminfo (%read-text boundaries "/proc/meminfo")))
       (multiple-value-bind (total available)
           (if meminfo
               (aitools.env.domain:parse-meminfo meminfo :strict strict)
               (values nil nil))
         (values (and cpuinfo
                      (let ((count (aitools.env.domain:count-cpuinfo-processors cpuinfo)))
                        (and (plusp count) count)))
                 total available))))
    (:darwin
     (let* ((sysctl (%run-output boundaries (%darwin-program :sysctl) '("-n" "hw.ncpu" "hw.memsize") timeout-seconds))
            (lines (and sysctl (aitools.env.domain:parse-sysctl-values sysctl)))
            (vm-stat (%run-output boundaries (%darwin-program :vm-stat) '() timeout-seconds)))
       (flet ((integer-at (index)
                (let ((text (nth index lines)))
                  (and text (aitools.env.domain:ascii-digits-p text) (parse-integer text)))))
         (values (integer-at 0) (integer-at 1)
                 (and vm-stat (aitools.env.domain:parse-vm-stat vm-stat :strict strict))))))
    (t (values nil nil nil))))
(defun %uptime-seconds (boundaries os)
  #+linux
  (when (eq os :linux)
    (aitools.env.domain:parse-proc-uptime
     (%read-text boundaries "/proc/uptime")))
  #+darwin
  (when (eq os :darwin)
    (let ((boot-seconds (%sysctl-mib boundaries :kern-boottime)))
      (when (realp boot-seconds)
        (let ((uptime (- (/ (%now boundaries) 1000d0) boot-seconds)))
          (when (>= uptime 0)
            uptime)))))
  #-(or linux darwin)
  nil)

(aitools.kernel.domain:define-flow
    (sys-info/k boundaries &key on-ok on-error)
  (multiple-value-bind (sysname release machine)
      (%system-identity boundaries)
    (let* ((os (%os-kind sysname))
           (uptime-seconds (%uptime-seconds boundaries os)))
      (if (not (realp uptime-seconds))
          (aitools.kernel.domain:flow-continue
           :on-error
           "environment.unavailable"
           "System uptime source is unavailable or malformed.")
          (multiple-value-bind (cpus memory-total memory-available)
              (%cpus-and-memory boundaries os)
            (multiple-value-bind (disk-total disk-available)
                (%file-system-space boundaries (%workspace-root boundaries))
              (aitools.kernel.domain:flow-continue
               :on-ok
               (list
                (cons "os" (string-downcase sysname))
                (cons "os_version" release)
                (cons "arch" machine)
                (cons "cpus" (%null-if-nil cpus))
                (cons "user" (%snapshot-user-name boundaries))
                (cons "uid" (%user-id boundaries))
                (cons "hostname" (%host-name boundaries))
                (cons "uptime_seconds" uptime-seconds)
                (cons "shell" (%null-if-nil (%getenv boundaries "SHELL")))
                (cons "memory"
                      (%object (cons "total" (%null-if-nil memory-total))
                               (cons "available" (%null-if-nil memory-available))))
                (cons "disk"
                      (%object (cons "total" (%null-if-nil disk-total))
                               (cons "available" (%null-if-nil disk-available))))))))))))

;;; ----------------------------------------------------------------- sys env

(defparameter *redacted-secret* "[REDACTED_SECRET]")

(aitools.kernel.domain:define-flow
    (sys-env/k boundaries &key prefix on-ok on-error)
  "PREFIX filters by case-sensitive name prefix. Secret values are replaced whole;
the envelope writer still masks matching patterns in other values."
  (declare (ignore on-error))
  (let ((items nil)
        (redactions 0))
    (dolist (entry
             (sort (copy-list (%environment-variables boundaries))
                   #'string< :key #'car))
      (destructuring-bind (name . value) entry
        (when (or (null prefix)
                  (and (<= (length prefix) (length name))
                       (string= prefix name :end2 (length prefix))))
          (let ((secret-p (aitools.env.domain:secret-environment-name-p name)))
            (when secret-p (incf redactions))
            (push (%object
                   (cons "name" name)
                   (cons "value" (if secret-p *redacted-secret* value)))
                  items)))))
    (aitools.kernel.domain:flow-continue
     :on-ok
     (let ((items (nreverse items)))
       (list (cons "items" items)
             (cons "total" (length items))
             (cons "redactions" redactions))))))

;;; --------------------------------------------------------------- sys tools

