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

(defun sys-network/k (ports &key (limit 100) on-ok on-partial on-error)
  (unless (and (integerp limit) (<= 1 limit 1000))
    (return-from sys-network/k (funcall on-error "argument.invalid" "Network limit must be between 1 and 1000.")))
  (let* ((os (%os-kind (funcall (env-ports-system-identity ports))))
         (source (case os (:linux "/proc/net/dev") (:darwin "netstat -ibn"))))
    (unless source
      (return-from sys-network/k (funcall on-error "environment.unavailable" "Network counters are unsupported on this OS.")))
    (let ((text (case os
                  (:linux (%read-text ports source))
                  (:darwin (%run-output ports (%darwin-program :netstat) '("-ibn"))))))
      (multiple-value-bind (items valid)
          (if text (aitools.env.domain:parse-network-counters text os) (values nil nil))
        (unless valid
          (return-from sys-network/k
            (funcall on-error "environment.unavailable"
                     "Network counters unavailable: missing source, permission denied, failed command, or malformed data.")))
        (let* ((truncated (> (length items) limit))
               (fields (list (cons "source" source) (cons "basis" "cumulative_interface_counters")
                             (cons "units" (%object (cons "bytes" "bytes") (cons "packets" "packets") (cons "errors" "events")))
                             (cons "items" (mapcar (lambda (row) (apply #'%object row)) (subseq items 0 (min limit (length items)))))
                             (cons "total" (length items)) (cons "limit" limit)
                             (cons "truncated" (aitools.protocol.domain:json-boolean truncated)))))
          (funcall (if truncated on-partial on-ok) fields))))))

(defun sys-disk/k (ports &key path on-ok on-error)
  (let* ((path (or path (funcall (env-ports-workspace-root ports))))
         (os (%os-kind (funcall (env-ports-system-identity ports)))))
    (unless os
      (return-from sys-disk/k (funcall on-error "environment.unavailable" "Filesystem capacity is unsupported on this OS.")))
    (multiple-value-bind (total available) (funcall (env-ports-file-system-space ports) path)
      (unless (and (integerp total) (integerp available) (<= 0 available total))
        (return-from sys-disk/k
          (funcall on-error "environment.unavailable" "Filesystem capacity unavailable: missing path, permission denied, or failed statfs.")))
      (funcall on-ok (list (cons "path" path) (cons "source" "statfs") (cons "units" "bytes")
                           (cons "scope" "filesystem_containing_path")
                           (cons "total_bytes" total) (cons "available_bytes" available)
                           (cons "available_basis" "unprivileged_available_blocks")
                           (cons "truncated" (aitools.protocol.domain:json-boolean nil)))))))

(defparameter *source-timeout-seconds* 60
  "Upper bound for one OS source command. Only a hang guard: `ps -axww` was
observed taking 6-18 s on a heavily loaded Darwin host, so a tight bound
would turn load into environment.unavailable.")

(defun %darwin-program (key) (getf *darwin-programs* key))

(defun %null-if-nil (value)
  (if (null value) (aitools.env.domain:json-null) value))

(defun %os-kind (sysname)
  (cond ((string-equal sysname "Linux") :linux)
        ((string-equal sysname "Darwin") :darwin)))

(defun %run-output (ports program arguments)
  "STDOUT of PROGRAM when it ran and exited 0, else NIL."
  (multiple-value-bind (status exit-code stdout) (%run ports program arguments *source-timeout-seconds*)
    (and (eq status :exited) (eql exit-code 0) stdout)))

(defun %unavailable (on-error what tool)
  (funcall on-error "environment.unavailable" what
           :repairs (list (%repair "check-tool" (format nil "Check whether ~A is installed." tool)
                                   (format nil "aitools sys tools ~A" tool)))))

;;; ---------------------------------------------------------------- sys info

(defun %cpus-and-memory (ports os &key strict)
  "(VALUES CPUS MEMORY-TOTAL MEMORY-AVAILABLE), each NIL when unknown."
  (case os
    (:linux
     (let ((cpuinfo (%read-text ports "/proc/cpuinfo"))
           (meminfo (%read-text ports "/proc/meminfo")))
       (multiple-value-bind (total available) (if meminfo (aitools.env.domain:parse-meminfo meminfo :strict strict) (values nil nil))
         (values (and cpuinfo (let ((count (aitools.env.domain:count-cpuinfo-processors cpuinfo)))
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

(defun sys-info/k (ports &key on-ok on-error)
  (declare (ignore on-error))
  (multiple-value-bind (sysname release machine) (funcall (env-ports-system-identity ports))
    (multiple-value-bind (cpus memory-total memory-available) (%cpus-and-memory ports (%os-kind sysname))
      (multiple-value-bind (disk-total disk-available)
          (funcall (env-ports-file-system-space ports) (funcall (env-ports-workspace-root ports)))
        (funcall on-ok
                 (list (cons "os" (string-downcase sysname))
                       (cons "os_version" release)
                       (cons "arch" machine)
                       (cons "cpus" (%null-if-nil cpus))
                       (cons "user" (funcall (env-ports-user-name ports)))
                       (cons "uid" (funcall (env-ports-user-id ports)))
                       (cons "hostname" (funcall (env-ports-host-name ports)))
                       (cons "shell" (%null-if-nil (%getenv ports "SHELL")))
                       (cons "memory" (%object (cons "total" (%null-if-nil memory-total))
                                               (cons "available" (%null-if-nil memory-available))))
                       (cons "disk" (%object (cons "total" (%null-if-nil disk-total))
                                             (cons "available" (%null-if-nil disk-available))))))))))

;;; ----------------------------------------------------------------- sys env

(defparameter *redacted-secret* "[REDACTED_SECRET]")

(defun sys-env/k (ports &key prefix on-ok on-error)
  "PREFIX filters by case-sensitive name prefix. A secret-named variable's
value is replaced whole, independent of its shape; the envelope
writer's pattern masking still applies to every other value."
  (declare (ignore on-error))
  (let ((items nil) (redactions 0))
    (dolist (entry (sort (copy-list (funcall (env-ports-environment-variables ports))) #'string< :key #'car))
      (destructuring-bind (name . value) entry
        (when (or (null prefix)
                  (and (<= (length prefix) (length name)) (string= prefix name :end2 (length prefix))))
          (let ((secret-p (aitools.env.domain:secret-environment-name-p name)))
            (when secret-p (incf redactions))
            (push (%object (cons "name" name) (cons "value" (if secret-p *redacted-secret* value))) items)))))
    (let ((items (nreverse items)))
      (funcall on-ok (list (cons "items" items)
                           (cons "total" (length items))
                           (cons "redactions" redactions))))))

;;; --------------------------------------------------------------- sys tools

(defun find-executable (ports name)
  "The first PATH directory entry NAME that is an executable file, or NIL."
  (let ((path (%getenv ports "PATH")))
    (when path
      (dolist (directory (aitools.env.domain:split-search-path path))
        (let ((candidate (aitools.env.domain:join-directory directory name)))
          (when (funcall (env-ports-executable-p ports) candidate)
            (return candidate)))))))

(defun %tool-version (ports path arguments timeout-seconds)
  "The first nonblank line of PATH's version output (stdout, else stderr:
some tools print their version there), or NIL when it did not finish in
TIMEOUT-SECONDS or printed nothing."
  (multiple-value-bind (status exit-code stdout stderr) (%run ports path arguments timeout-seconds)
    (declare (ignore exit-code))
    (when (eq status :exited)
      (or (aitools.env.domain:first-output-line (or stdout ""))
          (aitools.env.domain:first-output-line (or stderr ""))))))

(defun sys-tools/k (ports &key names (timeout "5s") on-ok on-error)
  "NAMES NIL means the default list (aitools.data:*env-default-tools*).
TIMEOUT is a duration string bounding each version probe."
  (let ((bad (find-if-not #'aitools.env.domain:valid-tool-name-p names))
        (timeout-milliseconds
          (handler-case (aitools.kernel.domain:duration-milliseconds (aitools.kernel.domain:parse-duration timeout))
            (aitools.kernel.domain:invalid-duration-error () nil))))
    (cond
      (bad
       (return-from sys-tools/k
         (funcall on-error "argument.invalid" (format nil "~S is not a bare command name" bad)
                  :repairs (list (%repair "use-name" "Pass command names without a directory."
                                          "aitools sys tools git")))))
      ((null timeout-milliseconds)
       (return-from sys-tools/k
         (funcall on-error "argument.invalid" (format nil "--timeout: not a duration: ~S" timeout)
                  :repairs (list (%repair "fix-duration" "Durations are <number>ms|s|m|h|d."
                                          "aitools sys tools --timeout 5s"))))))
    (%probe-tools ports names timeout-milliseconds on-ok)))

(defun %probe-tools (ports names timeout-milliseconds on-ok)
  (let ((probes (if names
                    (mapcar (lambda (name)
                              (cons name (or (cdr (assoc name aitools.data:*env-default-tools* :test #'string=))
                                             aitools.data:*env-version-arguments-default*)))
                            names)
                    aitools.data:*env-default-tools*))
        (items nil))
    (dolist (probe probes)
      (destructuring-bind (name . arguments) probe
        (let ((path (find-executable ports name)))
          (push (%object (cons "name" name)
                         (cons "path" (%null-if-nil path))
                         (cons "version" (%null-if-nil (and path (%tool-version ports path arguments
                                                                                (/ timeout-milliseconds 1000))))))
                items))))
    (funcall on-ok (list (cons "items" (nreverse items)) (cons "total" (length probes))))))

;;; --------------------------------------------------------------- sys procs

(defun %linux-processes (ports)
  "Records (:PID :PPID :USER :COMMAND :STARTED-MS) from /proc, or NIL when
/proc is unreadable. starttime is in clock ticks since boot; Linux fixes the
userspace tick (USER_HZ) at 100."
  (let* ((entries (funcall (env-ports-list-directory ports) "/proc"))
         (stat (%read-text ports "/proc/stat"))
         (boot-seconds (and stat (aitools.env.domain:parse-proc-boot-time stat)))
         (passwd (let ((text (%read-text ports "/etc/passwd")))
                   (and text (aitools.env.domain:parse-passwd text))))
         (records nil))
    (dolist (entry entries records)
      (when (aitools.env.domain:ascii-digits-p entry)
        (let ((stat-text (%read-text ports (format nil "/proc/~A/stat" entry))))
          (when stat-text
            (multiple-value-bind (pid comm ppid start-ticks) (aitools.env.domain:parse-proc-stat-process stat-text)
              (when pid
                (let* ((status (%read-text ports (format nil "/proc/~A/status" entry)))
                       (uid (and status (aitools.env.domain:parse-proc-status-uid status)))
                       (cmdline (or (%read-text ports (format nil "/proc/~A/cmdline" entry)) "")))
                  (push (list :pid pid :ppid ppid
                              :user (or (cdr (assoc uid passwd)) (and uid (princ-to-string uid)))
                              :command (aitools.env.domain:proc-cmdline-command cmdline comm)
                              :started-ms (and boot-seconds start-ticks
                                               (+ (* boot-seconds 1000) (* start-ticks 10))))
                        records))))))))))

(defun %darwin-processes (ports)
  "Records from ps, or :UNAVAILABLE. `started` derives from ps's elapsed
time against the clock port, to the second."
  (let ((output (%run-output ports (%darwin-program :ps) '("-axww" "-o" "pid=,ppid=,user=,etime=,command="))))
    (if (null output)
        :unavailable
        (let ((now-second (* 1000 (floor (%now ports) 1000))))
          (mapcar (lambda (record)
                    (list :pid (getf record :pid) :ppid (getf record :ppid) :user (getf record :user)
                          :command (getf record :command)
                          :started-ms (- now-second (* 1000 (getf record :elapsed-seconds)))))
                  (aitools.env.domain:parse-ps-output output))))))

(defun %process-item (record)
  (let ((started (getf record :started-ms)))
    (%object (cons "pid" (getf record :pid))
             (cons "ppid" (%null-if-nil (getf record :ppid)))
             (cons "user" (%null-if-nil (getf record :user)))
             (cons "command" (getf record :command))
             (cons "started" (if started
                                 (aitools.env.domain:format-iso8601 started 0 :utc-designator t)
                                 (aitools.env.domain:json-null))))))

(defun sys-procs/k (ports &key pattern (limit 50) on-ok on-partial on-error)
  "Processes whose command contains PATTERN (case-insensitive), by pid. Over
LIMIT items the result is partial."
  (let* ((os (%os-kind (nth-value 0 (funcall (env-ports-system-identity ports)))))
         (records (case os
                    (:linux (or (%linux-processes ports) :unavailable))
                    (:darwin (%darwin-processes ports))
                    (t :unavailable))))
    (if (eq records :unavailable)
        (%unavailable on-error "no process source (/proc on Linux, ps on Darwin) is available" "ps")
        (let* ((matching (sort (remove-if-not (lambda (record)
                                                (aitools.env.domain:process-matches-pattern-p
                                                 (getf record :command) pattern))
                                              records)
                               #'< :key (lambda (record) (getf record :pid))))
               (total (length matching))
               (shown (subseq matching 0 (min limit total)))
               (fields (list (cons "items" (mapcar #'%process-item shown)) (cons "total" total))))
          (if (> total limit)
              (funcall on-partial
                       (append fields
                               (list (cons "truncated" t)
                                     (cons "next_commands"
                                           (list (format nil "aitools sys procs~@[ ~A~] --limit ~D"
                                                         (and pattern (aitools.protocol.domain:shell-quote pattern)) total))))))
              (funcall on-ok fields))))))

;;; ------------------------------------------------------------- sys metrics

(defun %metrics-cpu (ports os)
  (case os
    (:linux
     (let ((before (aitools.env.domain:parse-cpu-counters (%read-text ports "/proc/stat"))))
       (when before
         (funcall (env-ports-sleep-seconds ports) 1)
         (aitools.env.domain:cpu-busy-percent
          before (aitools.env.domain:parse-cpu-counters (%read-text ports "/proc/stat"))))))
    (:darwin
     (let ((before (funcall (env-ports-cpu-counters ports))))
       (when before
         (funcall (env-ports-sleep-seconds ports) 1)
         (aitools.env.domain:cpu-busy-percent before (funcall (env-ports-cpu-counters ports))))))))

(defun sys-metrics/k (ports &key (limit 20) on-ok on-partial on-error)
  (let* ((os (%os-kind (nth-value 0 (funcall (env-ports-system-identity ports)))))
         (cpu (%metrics-cpu ports os)))
    (multiple-value-bind (cpus total available) (%cpus-and-memory ports os :strict t)
      (let ((records (and os (aitools.env.domain:parse-process-metrics
                              (%run-output ports "/bin/ps" '("-ax" "-o" "pid=,pcpu=,rss=,comm="))))))
        (if (not (and cpu cpus (plusp cpus) total (plusp total)
                      available (<= 0 available total) records))
            (funcall on-error "environment.unavailable"
                     "CPU, memory, or process metrics source is unavailable or malformed")
            (let* ((sorted (stable-sort records
                                        (lambda (a b)
                                          (if (= (getf a :cpu-percent) (getf b :cpu-percent))
                                              (< (getf a :pid) (getf b :pid))
                                              (> (getf a :cpu-percent) (getf b :cpu-percent))))))
                   (count (length sorted))
                   (fields
                     (list (cons "sample_interval_ms" 1000)
                           (cons "cpu" (%object (cons "busy_percent" cpu)
                                                (cons "logical_cpus" cpus)
                                                (cons "basis" "interval_all_cpus")))
                           (cons "memory" (%object (cons "total_bytes" total)
                                                   (cons "available_bytes" available)
                                                   (cons "available_basis" (if (eq os :linux)
                                                                               "mem_available_or_free_buffers_cached"
                                                                               "free_inactive_speculative_pages"))))
                           (cons "process_cpu_basis" (if (eq os :linux) "lifetime_average" "decaying_average"))
                           (cons "items" (loop for record in (subseq sorted 0 (min limit count))
                                               collect (%object (cons "pid" (getf record :pid))
                                                                (cons "command" (getf record :command))
                                                                (cons "cpu_percent" (getf record :cpu-percent))
                                                                (cons "rss_bytes" (getf record :rss-bytes)))))
                           (cons "total" count))))
              (if (> count limit)
                  (funcall on-partial (append fields (list (cons "truncated" t))))
                  (funcall on-ok fields))))))))

;;; --------------------------------------------------------------- sys ports

(defun %linux-socket-owners (ports)
  "Hash table inode -> (PID . COMMAND) from /proc/<pid>/fd links. Other
users' fd directories are unreadable without privileges; their sockets keep
pid and command null."
  (let ((owners (make-hash-table)))
    (dolist (entry (funcall (env-ports-list-directory ports) "/proc") owners)
      (when (aitools.env.domain:ascii-digits-p entry)
        (let ((fd-directory (format nil "/proc/~A/fd" entry))
              (command nil))
          (dolist (fd (funcall (env-ports-list-directory ports) fd-directory))
            (let ((inode (aitools.env.domain:parse-socket-inode
                          (or (funcall (env-ports-read-link ports) (format nil "~A/~A" fd-directory fd)) ""))))
              (when (and inode (not (gethash inode owners)))
                (unless command
                  (setf command (string-trim '(#\Newline #\Space)
                                             (or (%read-text ports (format nil "/proc/~A/comm" entry)) ""))))
                (setf (gethash inode owners) (cons (parse-integer entry) command))))))))))

(defun %linux-listeners (ports)
  (let ((tcp (%read-text ports "/proc/net/tcp"))
        (tcp6 (%read-text ports "/proc/net/tcp6")))
    (if (and (null tcp) (null tcp6))
        :unavailable
        (let ((owners (%linux-socket-owners ports)))
          (values (mapcar (lambda (record)
                    (let ((owner (gethash (getf record :inode) owners)))
                      (list :port (getf record :port) :address (getf record :address)
                            :pid (car owner) :command (cdr owner))))
                  (append (and tcp (aitools.env.domain:parse-proc-net-tcp tcp :ipv4))
                          (and tcp6 (aitools.env.domain:parse-proc-net-tcp tcp6 :ipv6))))
                  (%object (cons "ipv4" (if tcp "ok" "source_unavailable"))
                           (cons "ipv6" (if tcp6 "ok" "source_unavailable")))
                  (append (unless tcp (list "/proc/net/tcp"))
                          (unless tcp6 (list "/proc/net/tcp6"))))))))

(defun %darwin-listeners (ports)
  "lsof exits 1 when nothing matches, so 1 with empty output is an empty
list rather than a failure."
  (multiple-value-bind (status exit-code stdout)
      (%run ports (%darwin-program :lsof) '("-nP" "-iTCP" "-sTCP:LISTEN" "-Fpcnt") *source-timeout-seconds*)
    (cond ((not (eq status :exited)) :unavailable)
          ((eql exit-code 0) (aitools.env.domain:parse-lsof-listen stdout))
          ((and (eql exit-code 1) (zerop (length (string-trim '(#\Newline #\Space) (or stdout ""))))) nil)
          (t :unavailable))))

(defun sys-snapshot/k (ports &key path (process-limit 20) (network-limit 100)
                                  (port-limit 100) on-ok on-partial on-error)
  (unless (every (lambda (n) (and (integerp n) (<= 1 n 1000)))
                 (list process-limit network-limit port-limit))
    (return-from sys-snapshot/k
      (funcall on-error "argument.invalid" "Snapshot limits must be between 1 and 1000.")))
  (let ((started (%now ports)) (sections nil) (usable 0) (incomplete nil))
    (labels ((observe (name flow &optional partial-p)
               (let ((begin (%now ports)))
                 (flet ((finish (status data reason)
                          (unless (string= status "unavailable") (incf usable))
                          (unless (string= status "ok") (setf incomplete t))
                          (push (cons name (%object (cons "status" status)
                                                    (cons "data" (if data (apply #'%object data) (aitools.protocol.domain:json-null)))
                                                    (cons "reason" (%null-if-nil reason))
                                                    (cons "started_at_epoch_ms" begin)
                                                    (cons "finished_at_epoch_ms" (%now ports)))) sections)))
                   (handler-case (apply flow
                          :on-ok (lambda (data)
                                   (let ((reason (cond
                                                   ((string= name "info") (%snapshot-info-missing data))
                                                   ((string= name "ports") (%snapshot-limit-ports data port-limit)))))
                                     (finish (if reason "partial" "ok") data reason)))
                          :on-error (lambda (code message &rest details)
                                      (declare (ignore details))
                                      (finish "unavailable" nil (%object (cons "code" code) (cons "message" message))))
                          (when partial-p
                            (list :on-partial (lambda (data)
                                                (finish "partial" data
                                                        (if (string= name "ports")
                                                            (%snapshot-limit-ports data port-limit)
                                                            "output_limit"))))))
                     (file-error () (finish "unavailable" nil "source_file_unreadable")))))))
      (observe "info" (lambda (&rest callbacks) (apply #'sys-info/k ports callbacks)))
      (observe "metrics" (lambda (&rest callbacks) (apply #'sys-metrics/k ports :limit process-limit callbacks)) t)
      (observe "network" (lambda (&rest callbacks) (apply #'sys-network/k ports :limit network-limit callbacks)) t)
      (observe "disk" (lambda (&rest callbacks) (apply #'sys-disk/k ports :path path callbacks)))
      (observe "ports" (lambda (&rest callbacks) (apply #'sys-ports/k ports callbacks)) t))
    (let ((data (list (cons "scope" "host") (cons "atomic" (aitools.protocol.domain:json-boolean nil))
                      (cons "started_at_epoch_ms" started) (cons "finished_at_epoch_ms" (%now ports))
                      (cons "source_timeout_seconds" *source-timeout-seconds*)
                      (cons "limits" (%object (cons "processes" process-limit) (cons "interfaces" network-limit) (cons "ports" port-limit)))
                      (cons "collection_bounds" "Output limits only; no overall deadline or memory bound. OS calls and socket-owner scans are not time bounded; external commands capture complete output.")
                      (cons "sections" (apply #'%object (nreverse sections))))))
      (if (zerop usable)
          (funcall on-error "environment.unavailable" "All snapshot sections are unavailable." :diagnostics (list (apply #'%object data)))
          (funcall (if incomplete on-partial on-ok) data)))))

(defun %snapshot-info-missing (data)
  (let ((missing nil))
    (dolist (key '("os" "os_version" "arch" "cpus" "user" "uid" "hostname"))
      (let ((value (cdr (assoc key data :test #'string=))))
        (when (or (null value) (eq value (aitools.protocol.domain:json-null))) (push key missing))))
    (dolist (key '("memory" "disk"))
      (dolist (entry (aitools.protocol.domain:json-object-members (cdr (assoc key data :test #'string=))))
        (when (eq (cdr entry) (aitools.protocol.domain:json-null))
          (push (format nil "~A.~A" key (car entry)) missing))))
    (when missing (%object (cons "code" "missing_observations") (cons "fields" (nreverse missing))))))

(defun %snapshot-limit-ports (data limit)
  (let* ((items (cdr (assoc "items" data :test #'string=)))
         (truncated (> (length items) limit))
         (unavailable (cdr (assoc "source_unavailable" data :test #'string=)))
         (unknown (some (lambda (item)
                          (eq (cdr (assoc "pid" (aitools.protocol.domain:json-object-members item) :test #'string=))
                              (aitools.protocol.domain:json-null))) items)))
    (setf (cdr (assoc "items" data :test #'string=)) (subseq items 0 (min limit (length items))))
    (nconc data (list (cons "limit" limit) (cons "truncated" (aitools.protocol.domain:json-boolean truncated))
                      (cons "scope" "tcp_listen") (cons "owner_scan_bounded" (aitools.protocol.domain:json-boolean nil))))
    (when (or truncated unknown unavailable)
      (%object (cons "output_truncated" (aitools.protocol.domain:json-boolean truncated))
               (cons "source_unavailable" unavailable)
               (cons "owner_unknown" (aitools.protocol.domain:json-boolean unknown))))))

(defun sys-ports/k (ports &key on-ok on-partial on-error)
  "TCP sockets in LISTEN state."
  (let ((os (%os-kind (nth-value 0 (funcall (env-ports-system-identity ports))))))
    (multiple-value-bind (records coverage unavailable)
        (case os
                    (:linux (%linux-listeners ports))
                    (:darwin (%darwin-listeners ports))
                    (t :unavailable))
    (if (eq records :unavailable)
        (%unavailable on-error "no listening-socket source (/proc/net/tcp on Linux, lsof on Darwin) is available" "lsof")
        (let ((items (mapcar (lambda (record)
                               (%object (cons "port" (getf record :port))
                                        (cons "address" (getf record :address))
                                        (cons "protocol" "tcp")
                                        (cons "pid" (%null-if-nil (getf record :pid)))
                                        (cons "command" (%null-if-nil (getf record :command)))))
                             (aitools.env.domain:sort-and-deduplicate-ports records))))
          (funcall (if unavailable on-partial on-ok)
                   (append (list (cons "items" items) (cons "total" (length items)))
                           (when coverage (list (cons "coverage" coverage)))
                           (when unavailable (list (cons "source_unavailable" unavailable))))))))))
