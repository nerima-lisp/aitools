;;;; packages/feature/env/src/presentation/env-commands.lisp
;;;;
;;;; cl-cli definitions and schemas for the `sys` and `time` groups
;;;; (docs/src/reference/commands.md). Handlers turn the parsed invocation into a flow call;
;;;; the flow's continuation decides the envelope. The `schema` text
;;;; (summaries, descriptions, args, output fields, error codes) is data:
;;;; AITOOLS.DATA:*ENV-COMMAND-SCHEMAS*.
(in-package #:aitools.env.presentation)

(defun %call-flow (flow)
  "Run FLOW, a function of the three command-result continuations, and
return the COMMAND-RESULT."
  (aitools.protocol.application:call-with-command-result/k
   (lambda (&key on-ok on-partial on-error)
     (funcall flow on-ok on-partial on-error))))

(defun %schema-data (full-name)
  (or (find full-name aitools.data:*env-command-schemas*
            :key (lambda (entry) (getf entry :name)) :test #'string=)
      (error "no env schema data for ~S" full-name)))

(defun %register (registry group name cli-command)
  (let* ((full-name (format nil "~A.~A" group name))
         (data (%schema-data full-name)))
    (aitools.protocol.application:register-command
     registry
     :name full-name
     :group group
     :cli-command cli-command
     :schema (aitools.protocol.domain:make-command-schema
              full-name (getf data :summary)
              :description (getf data :description) :args (getf data :args)
              :output-fields (getf data :output-fields)
              :error-codes (getf data :error-codes)))))

(defun %tz-option ()
  (make-option :name "tz" :kind :value :value-name "IANA-NAME"
               :description "IANA time zone name, e.g. Asia/Tokyo (default: $TZ, then /etc/localtime)."))

;;; ---------------------------------------------------------------------- sys

(defun %register-sys-info (registry ports)
  (%register
   registry "sys" "info"
   (make-command :name "info" :description "Describe the host: OS, CPU, memory, disk."
                 :handler (lambda (invocation)
                            (declare (ignore invocation))
                            (%call-flow (lambda (on-ok on-partial on-error)
                                          (declare (ignore on-partial))
                                          (aitools.env.application:sys-info/k ports :on-ok on-ok :on-error on-error)))))))

(defun %register-sys-env (registry ports)
  (%register
   registry "sys" "env"
   (make-command :name "env" :description "List environment variables with secrets masked."
                 :positionals (list (make-positional :name "prefix" :key :prefix :required-p nil))
                 :handler (lambda (invocation)
                            (%call-flow (lambda (on-ok on-partial on-error)
                                          (declare (ignore on-partial))
                                          (aitools.env.application:sys-env/k
                                           ports :prefix (positional-value invocation :prefix)
                                                 :on-ok on-ok :on-error on-error)))))))

(defun %register-sys-tools (registry ports)
  (%register
   registry "sys" "tools"
   (make-command :name "tools" :description "Locate commands on PATH and report their versions."
                 :positionals (list (make-positional :name "names" :key :names :rest-p t :required-p nil))
                 :options (list (make-option :name "timeout" :kind :value :value-name "DURATION" :default "5s"
                                             :description "Per-command time limit for the version probe."))
                 :handler (lambda (invocation)
                            (%call-flow (lambda (on-ok on-partial on-error)
                                          (declare (ignore on-partial))
                                          (aitools.env.application:sys-tools/k
                                           ports :names (positional-value invocation :names)
                                                 :timeout (option-value invocation :timeout)
                                                 :on-ok on-ok :on-error on-error)))))))

(defun %register-sys-procs (registry ports)
  (%register
   registry "sys" "procs"
   (make-command :name "procs" :description "List processes (read-only)."
                 :positionals (list (make-positional :name "pattern" :key :pattern :required-p nil))
                 :options (list (make-option :name "limit" :kind :value :type :integer :min 1 :default 50
                                             :description "Maximum items."))
                 :handler (lambda (invocation)
                            (%call-flow (lambda (on-ok on-partial on-error)
                                          (aitools.env.application:sys-procs/k
                                           ports :pattern (positional-value invocation :pattern)
                                                 :limit (option-value invocation :limit)
                                                 :on-ok on-ok :on-partial on-partial :on-error on-error)))))))

(defun %register-sys-ports (registry ports)
  (%register
   registry "sys" "ports"
   (make-command :name "ports" :description "List listening TCP sockets."
                 :handler (lambda (invocation)
                            (declare (ignore invocation))
                            (%call-flow (lambda (on-ok on-partial on-error)
                                          (declare (ignore on-partial))
                                          (aitools.env.application:sys-ports/k ports :on-ok on-ok :on-error on-error)))))))

;;; --------------------------------------------------------------------- time

(defun %register-time-now (registry ports)
  (%register
   registry "time" "now"
   (make-command :name "now" :description "Show the current time."
                 :options (list (%tz-option))
                 :handler (lambda (invocation)
                            (%call-flow (lambda (on-ok on-partial on-error)
                                          (declare (ignore on-partial))
                                          (aitools.env.application:time-now/k
                                           ports :tz (option-value invocation :tz)
                                                 :on-ok on-ok :on-error on-error)))))))

(defun %register-time-convert (registry ports)
  (%register
   registry "time" "convert"
   (make-command :name "convert" :description "Convert a time between formats and zones."
                 :positionals (list (make-positional :name "value" :key :value :required-p t))
                 :options (list (make-option :name "to" :kind :value :default "iso8601"
                                             :choices '("iso8601" "epoch_ms" "epoch_s")
                                             :description "Output format.")
                                (make-option :name "add" :kind :value :multiple-p t :value-name "DURATION"
                                             :description "Add a duration (repeatable).")
                                (make-option :name "sub" :kind :value :multiple-p t :value-name "DURATION"
                                             :description "Subtract a duration (repeatable).")
                                (%tz-option))
                 :handler (lambda (invocation)
                            (%call-flow (lambda (on-ok on-partial on-error)
                                          (declare (ignore on-partial))
                                          (aitools.env.application:time-convert/k
                                           ports (positional-value invocation :value)
                                           :to (option-value invocation :to)
                                           :add (option-value invocation :add)
                                           :sub (option-value invocation :sub)
                                           :tz (option-value invocation :tz)
                                           :on-ok on-ok :on-error on-error)))))))

(defun %register-time-diff (registry ports)
  (%register
   registry "time" "diff"
   (make-command :name "diff" :description "Difference between two times."
                 :positionals (list (make-positional :name "a" :key :a :required-p t)
                                    (make-positional :name "b" :key :b :required-p t))
                 :handler (lambda (invocation)
                            (%call-flow (lambda (on-ok on-partial on-error)
                                          (declare (ignore on-partial))
                                          (aitools.env.application:time-diff/k
                                           ports (positional-value invocation :a) (positional-value invocation :b)
                                           :on-ok on-ok :on-error on-error)))))))

(defun register-env-commands (registry ports)
  "Register the `sys` and `time` commands on REGISTRY, running their flows
against PORTS (an AITOOLS.ENV.APPLICATION:ENV-PORTS)."
  (%register-sys-info registry ports)
  (%register-sys-env registry ports)
  (%register-sys-tools registry ports)
  (%register-sys-procs registry ports)
  (%register-sys-ports registry ports)
  (%register-time-now registry ports)
  (%register-time-convert registry ports)
  (%register-time-diff registry ports)
  registry)
