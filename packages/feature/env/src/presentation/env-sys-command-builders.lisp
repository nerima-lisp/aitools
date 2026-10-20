(in-package #:aitools.env.presentation)

;;; ---------------------------------------------------------------------- sys

(defun %sys-info-command (ports)
  (make-command
   :name "info"
   :description "Describe the host: OS, CPU, memory, disk."
   :handler
   (lambda (invocation)
     (declare (ignore invocation))
     (%run-flow #'aitools.env.application:sys-info/k '(:on-ok :on-error) ports))))

(defun %sys-env-command (ports)
  (make-command
   :name "env"
   :description "List environment variables with secrets masked."
   :positionals
   (list (make-positional :name "prefix" :key :prefix :required-p nil))
   :handler
   (lambda (invocation)
     (%run-flow #'aitools.env.application:sys-env/k
                 '(:on-ok :on-error)
                 ports
                 :prefix
                 (positional-value invocation :prefix)))))

(defun %sys-tools-command (ports)
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
     (%run-flow #'aitools.env.application:sys-tools/k '(:on-ok :on-error) ports
             :names (positional-value invocation :names)
             :timeout (option-value invocation :timeout)))))

(defun %sys-procs-command (ports)
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
     (%run-flow #'aitools.env.application:sys-procs/k '(:on-ok :on-partial :on-error) ports
             :pattern (positional-value invocation :pattern)
             :limit (option-value invocation :limit)))))

(defun %sys-metrics-command (ports)
  (make-command
   :name "metrics"
   :description "Sample CPU, memory, and process metrics once."
   :options
   (list
    (make-option
     :name "limit" :kind :value :type :integer :min 1 :default 20
     :description "Maximum processes, ordered by CPU usage."))
   :handler
   (lambda (invocation)
     (%run-flow #'aitools.env.application:sys-metrics/k
                 '(:on-ok :on-partial :on-error)
                 ports
                 :limit
                 (option-value invocation :limit)))))

(defun %sys-network-command (ports)
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
                 ports
                 :limit
                 (option-value invocation :limit)))))

(defun %sys-disk-command (ports)
  (make-command
   :name "disk"
   :description "Read capacity of the filesystem containing PATH."
   :positionals
   (list (make-positional :name "path" :key :path :required-p nil))
   :handler
   (lambda (invocation)
     (%run-flow #'aitools.env.application:sys-disk/k
                 '(:on-ok :on-error)
                 ports
                 :path
                 (positional-value invocation :path)))))

(defun %sys-snapshot-command (ports)
  (make-command
   :name "snapshot"
   :description "Observe host sections sequentially (non-atomic)."
   :positionals
   (list (make-positional :name "path" :key :path :required-p nil))
   :options
   (loop
     for (name default) in
       '(("process-limit" 20) ("network-limit" 100) ("port-limit" 100))
     collect
       (make-option
        :name name :kind :value :type :integer :min 1 :max 1000 :default default))
   :handler
   (lambda (invocation)
     (%run-flow #'aitools.env.application:sys-snapshot/k '(:on-ok :on-partial :on-error) ports
             :path (positional-value invocation :path)
             :process-limit (option-value invocation :process-limit)
             :network-limit (option-value invocation :network-limit)
             :port-limit (option-value invocation :port-limit)))))

(defun %sys-ports-command (ports)
  (make-command
   :name "ports"
   :description "List listening TCP sockets."
   :handler
   (lambda (invocation)
     (declare (ignore invocation))
     (%run-flow #'aitools.env.application:sys-ports/k '(:on-ok :on-partial :on-error) ports))))
