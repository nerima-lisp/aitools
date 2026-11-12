(in-package #:aitools.env.application)

(defun %linux-processes (boundaries)
  "Records (:PID :PPID :USER :COMMAND :STARTED-MS) from /proc, or NIL when
/proc is unreadable. starttime is in clock ticks since boot; Linux fixes the
userspace tick (USER_HZ) at 100."
  (let* ((entries (%list-directory boundaries "/proc"))
         (stat (%read-text boundaries "/proc/stat"))
         (boot-seconds (and stat (aitools.env.domain:parse-proc-boot-time stat)))
         (passwd (let ((text (%read-text boundaries "/etc/passwd")))
                   (and text (aitools.env.domain:parse-passwd text))))
         (records nil))
    (dolist (entry entries records)
      (when (aitools.env.domain:ascii-digits-p entry)
        (let ((stat-text (%read-text boundaries (format nil "/proc/~A/stat" entry))))
          (when stat-text
            (multiple-value-bind (pid comm ppid start-ticks) (aitools.env.domain:parse-proc-stat-process stat-text)
              (when pid
                (let* ((status (%read-text boundaries (format nil "/proc/~A/status" entry)))
                       (uid (and status (aitools.env.domain:parse-proc-status-uid status)))
                       (cmdline (or (%read-text boundaries (format nil "/proc/~A/cmdline" entry)) "")))
                  (push (list :pid pid :ppid ppid
                              :user (or (cdr (assoc uid passwd)) (and uid (princ-to-string uid)))
                              :command (aitools.env.domain:proc-cmdline-command cmdline comm)
                              :started-ms (and boot-seconds start-ticks
                                               (+ (* boot-seconds 1000) (* start-ticks 10))))
                        records))))))))))

(defun %darwin-processes (boundaries)
  "Records from ps, or :UNAVAILABLE. `started` derives from elapsed time
  against the clock boundary, to the second."
  (let ((output (%run-output boundaries
                             (%darwin-program :ps boundaries)
                             (quote ("-axww" "-o" "pid=,ppid=,user=,etime=,command=")))))
    (if (null output)
        :unavailable
        (let ((now-second (* 1000 (floor (%now boundaries) 1000))))
          (mapcar (lambda (record)
                    (list :pid (getf record :pid)
                          :ppid (getf record :ppid)
                          :user (getf record :user)
                          :command (getf record :command)
                          :started-ms (- now-second
                                         (* 1000 (getf record :elapsed-seconds)))))
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

(aitools.kernel.domain:define-flow
    (sys-procs/k boundaries &key pattern (limit 50) on-ok on-partial on-error)
  "Processes whose command contains PATTERN (case-insensitive), by pid. Over LIMIT items the result is partial."
  (let* ((os (%os-kind (nth-value 0 (%system-identity boundaries))))
         (records (case os
                    (:linux (or (%linux-processes boundaries) :unavailable))
                    (:darwin (%darwin-processes boundaries))
                    (t :unavailable))))
    (if (eq records :unavailable)
        (aitools.kernel.domain:flow-call
            (%unavailable
             "no process source (/proc on Linux, ps on Darwin) is available"
             "ps")
          (:on-error (code message &key repairs)
            (aitools.kernel.domain:flow-continue
             :on-error code message :repairs repairs)))
        (let* ((matching
                 (sort
                  (remove-if-not
                   (lambda (record)
                     (aitools.env.domain:process-matches-pattern-p
                      (getf record :command) pattern))
                   records)
                  #'< :key (lambda (record) (getf record :pid))))
               (total (length matching))
               (shown (subseq matching 0 (min limit total)))
               (fields
                 (list (cons "items" (mapcar #'%process-item shown))
                       (cons "total" total))))
          (if (> total limit)
              (aitools.kernel.domain:flow-continue
               :on-partial
               (append
                fields
                (list
                 (cons "truncated" t)
                 (cons "next_commands"
                       (list
                        (format nil "aitools sys procs~@[ ~A~] --limit ~D"
                                (and pattern
                                     (aitools.protocol.domain:shell-quote pattern))
                                total))))))
              (aitools.kernel.domain:flow-continue :on-ok fields))))))

#+linux
(defun %metrics-cpu (boundaries os)
  (when (eq os :linux)
    (let ((before
            (aitools.env.domain:parse-cpu-counters
             (%read-text boundaries "/proc/stat"))))
      (when before
        (%sleep-seconds boundaries 1)
        (aitools.env.domain:cpu-busy-percent
         before
         (aitools.env.domain:parse-cpu-counters
          (%read-text boundaries "/proc/stat"))
         :iowait-may-decrease t)))))
#-linux
(defun %metrics-cpu (boundaries os)
  #+darwin
  (when (eq os :darwin)
    (let ((before (%cpu-counters boundaries)))
      (when before
        (%sleep-seconds boundaries 1)
        (aitools.env.domain:cpu-busy-percent
         before (%cpu-counters boundaries)))))
  #-(or linux darwin)
  (declare (ignore boundaries os))
  #-(or linux darwin)
  nil)
(defun %load-average (boundaries os)
  #+linux
  (when (eq os :linux)
    (aitools.env.domain:parse-proc-load-average
     (%read-text boundaries "/proc/loadavg")))
  #+darwin
  (when (eq os :darwin)
    (let ((values (%sysctl-mib boundaries :vm-loadavg)))
      (when (and (listp values)
                 (= (length values) 3)
                 (every (lambda (value) (and (realp value) (>= value 0))) values))
        (mapcar (lambda (value) (float value 1d0)) values))))
  #-(or linux darwin)
  nil)

(aitools.kernel.domain:define-flow
 (sys-metrics/k boundaries &key (limit 20) (timeout "60s") on-ok on-partial on-error)
 (let* ((timeout-seconds (%snapshot-timeout-seconds timeout)))
   (if (null timeout-seconds)
       (aitools.kernel.domain:flow-continue
        :on-error
        "argument.invalid"
        (format nil "--timeout: not a positive finite duration: ~S" timeout)
        :repairs
        (list (repair "fix-duration"
                      "Durations are positive <number>ms|s|m|h|d."
                      "aitools sys metrics --timeout 60s")))
     (let* ((os (%os-kind (nth-value 0 (%system-identity boundaries))))
            (cpu (%metrics-cpu boundaries os))
            (load-average (%load-average boundaries os)))
       (multiple-value-bind (cpus total available)
           (%cpus-and-memory boundaries os :strict t :timeout-seconds timeout-seconds)
         (let* ((program
                 (progn
                   #+linux
                   (and (eq os :linux) (find-executable boundaries "ps"))
                   #+darwin
                   (and (eq os :darwin) (%darwin-program :ps boundaries))
                   #-(or linux darwin)
                   nil))
                (records
                 (and program
                      (aitools.env.domain:parse-process-metrics
                       (%run-output boundaries
                                    program
                                    '("-ax" "-o" "pid=,pcpu=,rss=,comm=")
                                    timeout-seconds)))))
           (if (not (and cpu load-average cpus (plusp cpus) total (plusp total)
                         available (<= 0 available total) records))
               (aitools.kernel.domain:flow-continue
                :on-error
                "environment.unavailable"
                "CPU, load average, memory, or process metrics source is unavailable or malformed")
             (let* ((sorted
                     (stable-sort
                      records
                      (lambda (a b)
                        (if (= (getf a :cpu-percent) (getf b :cpu-percent))
                            (< (getf a :pid) (getf b :pid))
                          (> (getf a :cpu-percent) (getf b :cpu-percent))))))
                    (count (length sorted))
                    (fields
                     (list
                      (cons "sample_interval_ms" 1000)
                      (cons "cpu"
                            (%object (cons "busy_percent" cpu)
                                     (cons "logical_cpus" cpus)
                                     (cons "basis" "interval_all_cpus")))
                      (cons "load_average"
                            (%object (cons "one_minute" (first load-average))
                                     (cons "five_minutes" (second load-average))
                                     (cons "fifteen_minutes" (third load-average))))
                      (cons "memory"
                            (%object (cons "total_bytes" total)
                                     (cons "available_bytes" available)
                                     (cons "available_basis"
                                           (progn
                                             #+linux "mem_available_or_free_buffers_cached"
                                             #+darwin "free_inactive_speculative_pages"
                                             #-(or linux darwin) "unknown"))))
                      (cons "process_cpu_basis"
                            (progn
                              #+linux "lifetime_average"
                              #+darwin "decaying_average"
                              #-(or linux darwin) "unknown"))
                      (cons "items"
                            (loop for record in (subseq sorted 0 (min limit count))
                                  collect
                                  (%object
                                   (cons "pid" (getf record :pid))
                                   (cons "command" (getf record :command))
                                   (cons "cpu_percent" (getf record :cpu-percent))
                                   (cons "rss_bytes" (getf record :rss-bytes)))))
                      (cons "total" count))))
               (if (> count limit)
                   (aitools.kernel.domain:flow-continue
                    :on-partial
                    (append
                     fields
                     (list
                      (cons "truncated" t)
                      (cons "next_commands"
                            (list
                             (format nil "aitools sys metrics --limit ~D --timeout ~A"
                                     count timeout)))))
                    (aitools.kernel.domain:flow-continue :on-ok fields))))))))))
 )
