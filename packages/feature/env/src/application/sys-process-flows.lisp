(in-package #:aitools.env.application)

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
  (let ((output (%run-output ports (%darwin-program :ps ports) '("-axww" "-o" "pid=,ppid=,user=,etime=,command="))))
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

(aitools.kernel.domain:define-flow
    (sys-procs/k ports &key pattern (limit 50) on-ok on-partial on-error)
  "Processes whose command contains PATTERN (case-insensitive), by pid. Over LIMIT items the result is partial."
  (let* ((os (%os-kind (nth-value 0 (funcall (env-ports-system-identity ports)))))
         (records (case os
                    (:linux (or (%linux-processes ports) :unavailable))
                    (:darwin (%darwin-processes ports))
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

;;; ------------------------------------------------------------- sys metrics

(defun %metrics-cpu (ports os)
  (case os
    (:linux
     (let ((before (aitools.env.domain:parse-cpu-counters (%read-text ports "/proc/stat"))))
       (when before
         (funcall (env-ports-sleep-seconds ports) 1)
         (aitools.env.domain:cpu-busy-percent
          before (aitools.env.domain:parse-cpu-counters (%read-text ports "/proc/stat"))
          :iowait-may-decrease t))))
    (:darwin
     (let ((before (funcall (env-ports-cpu-counters ports))))
       (when before
         (funcall (env-ports-sleep-seconds ports) 1)
         (aitools.env.domain:cpu-busy-percent before (funcall (env-ports-cpu-counters ports))))))))

(aitools.kernel.domain:define-flow
    (sys-metrics/k ports &key (limit 20) on-ok on-partial on-error)
  (let* ((os (%os-kind (nth-value 0 (funcall (env-ports-system-identity ports)))))
         (cpu (%metrics-cpu ports os)))
    (multiple-value-bind (cpus total available)
        (%cpus-and-memory ports os :strict t)
      (let* ((program (case os
                        (:linux (find-executable ports "ps"))
                        (:darwin (%darwin-program :ps))))
             (records
               (and program
                    (aitools.env.domain:parse-process-metrics
                     (%run-output ports program '("-ax" "-o" "pid=,pcpu=,rss=,comm="))))))
        (if (not (and cpu cpus (plusp cpus) total (plusp total)
                      available (<= 0 available total) records))
            (aitools.kernel.domain:flow-continue
             :on-error
             "environment.unavailable"
             "CPU, memory, or process metrics source is unavailable or malformed")
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
                      (cons "memory"
                            (%object (cons "total_bytes" total)
                                     (cons "available_bytes" available)
                                     (cons "available_basis"
                                           (if (eq os :linux)
                                               "mem_available_or_free_buffers_cached"
                                               "free_inactive_speculative_pages"))))
                      (cons "process_cpu_basis"
                            (if (eq os :linux)
                                "lifetime_average"
                                "decaying_average"))
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
                   (append fields (list (cons "truncated" t))))
                  (aitools.kernel.domain:flow-continue :on-ok fields))))))))
