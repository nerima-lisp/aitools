;;;; packages/feature/env/src/application/sys-flows.lisp
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
    (sys-network/k ports &key (limit 100) on-ok on-partial on-error)
  (if (not (and (integerp limit) (<= 1 limit 1000)))
      (aitools.kernel.domain:flow-continue
       :on-error
       "argument.invalid"
       "Network limit must be between 1 and 1000.")
      (let* ((os (%os-kind (funcall (env-ports-system-identity ports))))
             (source (case os
                       (:linux "/proc/net/dev")
                       (:darwin "netstat -ibn"))))
        (if (null source)
            (aitools.kernel.domain:flow-continue
             :on-error
             "environment.unavailable"
             "Network counters are unsupported on this OS.")
            (let ((text (case os
                          (:linux (%read-text ports source))
                          (:darwin
                           (%run-output ports (%darwin-program :netstat) '("-ibn"))))))
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
    (sys-disk/k ports &key path on-ok on-error)
  (let* ((path (or path (funcall (env-ports-workspace-root ports))))
         (os (%os-kind (funcall (env-ports-system-identity ports)))))
    (if (null os)
        (aitools.kernel.domain:flow-continue
         :on-error
         "environment.unavailable"
         "Filesystem capacity is unsupported on this OS.")
        (multiple-value-bind (total available)
            (funcall (env-ports-file-system-space ports) path)
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

(defun %darwin-program (key &optional ports)
  (or (and (eq key :ps) ports (%getenv ports "AITOOLS_DARWIN_PS"))
      (getf *darwin-programs* key)))

(defun %null-if-nil (value)
  (if (null value) (aitools.env.domain:json-null) value))

(defun %os-kind (sysname)
  (cond ((string-equal sysname "Linux") :linux)
        ((string-equal sysname "Darwin") :darwin)))

(defun %run-output (ports program arguments)
  "STDOUT of PROGRAM when it ran and exited 0, else NIL."
  (multiple-value-bind (status exit-code stdout) (%run ports program arguments *source-timeout-seconds*)
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

(defun %cpus-and-memory (ports os &key strict)
  "(VALUES CPUS MEMORY-TOTAL MEMORY-AVAILABLE), each NIL when unknown."
  (case os
    (:linux
     (let ((cpuinfo (%read-text ports "/proc/cpuinfo"))
           (meminfo (%read-text ports "/proc/meminfo")))
       (multiple-value-bind (total available)
         (if meminfo
             (aitools.env.domain:parse-meminfo meminfo :strict strict)
             (values nil nil))
         (values (and cpuinfo
                      (let ((count (aitools.env.domain:count-cpuinfo-processors cpuinfo)))
                        (and (plusp count) count)))
                 total available))))
    (:darwin
     (let* ((sysctl (%run-output ports (%darwin-program :sysctl) '("-n" "hw.ncpu" "hw.memsize")))
            (lines (and sysctl (aitools.env.domain:parse-sysctl-values sysctl)))
            (vm-stat (%run-output ports (%darwin-program :vm-stat) '())))
       (flet ((integer-at (index)
                (let ((text (nth index lines)))
                  (and text (aitools.env.domain:ascii-digits-p text) (parse-integer text)))))
         (values (integer-at 0) (integer-at 1)
                 (and vm-stat (aitools.env.domain:parse-vm-stat vm-stat :strict strict))))))
    (t (values nil nil nil))))

(aitools.kernel.domain:define-flow
    (sys-info/k ports &key on-ok on-error)
  (declare (ignore on-error))
  (multiple-value-bind (sysname release machine)
      (funcall (env-ports-system-identity ports))
    (multiple-value-bind (cpus memory-total memory-available)
        (%cpus-and-memory ports (%os-kind sysname))
      (multiple-value-bind (disk-total disk-available)
          (funcall (env-ports-file-system-space ports)
                   (funcall (env-ports-workspace-root ports)))
        (aitools.kernel.domain:flow-continue
         :on-ok
         (list
          (cons "os" (string-downcase sysname))
          (cons "os_version" release)
          (cons "arch" machine)
          (cons "cpus" (%null-if-nil cpus))
          (cons "user" (funcall (env-ports-user-name ports)))
          (cons "uid" (funcall (env-ports-user-id ports)))
          (cons "hostname" (funcall (env-ports-host-name ports)))
          (cons "shell" (%null-if-nil (%getenv ports "SHELL")))
          (cons "memory"
                (%object (cons "total" (%null-if-nil memory-total))
                         (cons "available" (%null-if-nil memory-available))))
          (cons "disk"
                (%object (cons "total" (%null-if-nil disk-total))
                         (cons "available" (%null-if-nil disk-available))))))))))

;;; ----------------------------------------------------------------- sys env

(defparameter *redacted-secret* "[REDACTED_SECRET]")

(aitools.kernel.domain:define-flow
    (sys-env/k ports &key prefix on-ok on-error)
  "PREFIX filters by case-sensitive name prefix. Secret values are replaced whole;
the envelope writer still masks matching patterns in other values."
  (declare (ignore on-error))
  (let ((items nil)
        (redactions 0))
    (dolist (entry
             (sort (copy-list (funcall (env-ports-environment-variables ports)))
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

