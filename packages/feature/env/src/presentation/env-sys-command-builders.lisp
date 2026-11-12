(in-package #:aitools.env.presentation)

(defun %sys-info-command (boundaries)
  (make-command
   :name "info"
   :description "Describe the host: OS, architecture, CPUs, user, uptime, memory, and disk."
   :handler
   (lambda (invocation)
     (declare (ignore invocation))
     (%run-flow #'aitools.env.application:sys-info/k '(:on-ok :on-error) boundaries))))

(defun %sys-env-command (boundaries)
  (make-command
   :name "env"
   :description "List environment variables with secrets masked."
   :positionals
   (list (make-positional :name "prefix" :key :prefix :required-p nil))
   :handler
   (lambda (invocation)
     (%run-flow #'aitools.env.application:sys-env/k
                 '(:on-ok :on-error)
                 boundaries
                 :prefix
                 (positional-value invocation :prefix)))))

(defun %sys-tools-command (boundaries)
  (make-command
   :name "tools"
   :description "Locate commands on PATH and report their versions."
   :positionals
   (list (make-positional :name "names" :key :names :rest-p t :required-p nil))
   :options
   (list
    (make-option
     :name "timeout" :kind :value :value-name "DURATION" :default "5s"
     :description "Per-command time limit for the version probe."))
   :handler
   (lambda (invocation)
     (%run-flow #'aitools.env.application:sys-tools/k '(:on-ok :on-error) boundaries
             :names (positional-value invocation :names)
             :timeout (option-value invocation :timeout)))))

(defun %sys-procs-command (boundaries)
  (make-command
   :name "procs"
   :description "List processes (read-only)."
   :positionals
   (list (make-positional :name "pattern" :key :pattern :required-p nil))
   :options
   (list
    (make-option
     :name "limit" :kind :value :type :integer :min 1 :default 50
     :description "Maximum items."))
   :handler
   (lambda (invocation)
     (%run-flow #'aitools.env.application:sys-procs/k '(:on-ok :on-partial :on-error) boundaries
             :pattern (positional-value invocation :pattern)
             :limit (option-value invocation :limit)))))

(defun %sys-metrics-command (boundaries)
  (make-command
   :name "metrics"
   :description "Sample CPU, memory, load average, and process metrics once."
   :options
   (list
    (make-option
     :name "limit" :kind :value :type :integer :min 1 :default 20
     :description "Maximum processes, ordered by CPU usage.")
    (make-option
     :name "timeout" :kind :value :value-name "DURATION" :default "60s"
     :description "Per-subprocess source time limit."))
   :handler
   (lambda (invocation)
     (%run-flow #'aitools.env.application:sys-metrics/k
                 '(:on-ok :on-partial :on-error)
                 boundaries
                 :limit
                 (option-value invocation :limit)
                 :timeout
                 (option-value invocation :timeout)))))

(defun %sys-network-command (boundaries)
  (make-command
   :name "network"
   :description "Read cumulative network interface counters."
   :options
   (list
    (make-option
     :name "limit" :kind :value :type :integer :min 1 :max 1000 :default 100))
   :handler
   (lambda (invocation)
     (%run-flow #'aitools.env.application:sys-network/k
                 '(:on-ok :on-partial :on-error)
                 boundaries
                 :limit
                 (option-value invocation :limit)))))

(defun %sys-disk-command (boundaries)
  (make-command
   :name "disk"
   :description "Read capacity of the filesystem containing PATH."
   :positionals
   (list (make-positional :name "path" :key :path :required-p nil))
   :handler
   (lambda (invocation)
     (%run-flow #'aitools.env.application:sys-disk/k
                 '(:on-ok :on-error)
                 boundaries
                 :path
                 (positional-value invocation :path)))))

(defun %sys-snapshot-command (boundaries)
  (make-command
   :name "snapshot"
   :description "Observe host sections sequentially (non-atomic)."
   :positionals
   (list (make-positional :name "path" :key :path :required-p nil))
   :options
   (append
    (loop
      for (name default) in
        '(("process-limit" 20) ("network-limit" 100) ("port-limit" 100))
      collect
        (make-option
         :name name :kind :value :type :integer :min 1 :max 1000 :default default))
    (list
     (make-option
      :name "timeout" :kind :value :value-name "DURATION" :default "600s"
      :description "Overall deadline for every section, including socket-owner scans.")))
   :handler
   (lambda (invocation)
     (%run-flow #'aitools.env.application:sys-snapshot/k
                '(:on-ok :on-partial :on-error) boundaries
                :path (positional-value invocation :path)
                :timeout (option-value invocation :timeout)
                :process-limit (option-value invocation :process-limit)
                :network-limit (option-value invocation :network-limit)
                :port-limit (option-value invocation :port-limit)))))

(defun %sys-ports-command (boundaries)
  (make-command
   :name "ports"
   :description "List listening TCP sockets."
   :handler
   (lambda (invocation)
     (declare (ignore invocation))
     (%run-flow #'aitools.env.application:sys-ports/k '(:on-ok :on-partial :on-error) boundaries))))
