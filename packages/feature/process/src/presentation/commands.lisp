;;;; packages/feature/process/src/presentation/commands.lisp
;;;;
;;;; cl-cli commands for `run`, `wait`, and the `bg` group. Each handler turns
;;;; the parsed invocation into one AITOOLS.PROCESS.APPLICATION flow call and
;;;; returns the COMMAND-RESULT that call produced.
(in-package #:aitools.process.presentation)

(defun %schema (name)
  (let ((entry (find name aitools.data:*process-command-schemas*
                     :key (lambda (entry) (getf entry :name)) :test #'string=)))
    (aitools.protocol.domain:make-command-schema
     name (getf entry :summary)
     :description (getf entry :description)
     :args (getf entry :args)
     :output-fields (getf entry :output-fields)
     :error-codes (getf entry :error-codes))))

(defun %summary (name)
  (aitools.protocol.domain:command-schema-summary (%schema name)))

(defun %run-flow (flow &rest arguments)
  "Call FLOW with ARGUMENTS followed by the three command-result
continuations, returning the resulting COMMAND-RESULT."
  (aitools.protocol.application:call-with-command-result/k
   (lambda (&rest continuations)
     (apply flow (append arguments continuations)))))

(defun %argv-positional ()
  (make-positional :key :argv :name "argv" :rest-p t :required-p nil))

(defun %id-positional (required-p)
  (make-positional :key :id :name "id" :required-p required-p))

(defun %strip-ansi-option ()
  (make-option :name "strip-ansi" :kind :boolean :default t
               :description "Remove ANSI escapes and collapse \\r redraws; --no-strip-ansi keeps them."))

(defun %grep-option ()
  (make-option :name "grep" :kind :value :value-name "RE" :description "Line pattern."))

(defun %run-command (ports)
  (make-command
   :name "run" :description (%summary "run")
   :positionals (list (%argv-positional))
   :options (list (make-option :name "timeout" :kind :value :value-name "DURATION" :default "120s"
                               :description "Kill the process group after this long.")
                  (make-option :name "head" :kind :value :type :integer :min 0 :default 50
                               :description "Leading lines kept per stream.")
                  (make-option :name "tail" :kind :value :type :integer :min 0 :default 150
                               :description "Trailing lines kept per stream.")
                  (%grep-option)
                  (make-option :name "grep-limit" :kind :value :type :integer :min 0 :default 50
                               :description "Matches reported per stream.")
                  (%strip-ansi-option)
                  (make-option :name "stdout-to" :kind :value :value-name "PATH"
                               :description "Write stdout to this new file instead of returning it."))
   :handler (lambda (invocation)
              (%run-flow #'aitools.process.application:run-command/k ports
                         (aitools.process.application:make-run-request
                          :argv (positional-value invocation :argv)
                          :timeout (option-value invocation :timeout)
                          :head (option-value invocation :head)
                          :tail (option-value invocation :tail)
                          :grep (option-value invocation :grep)
                          :grep-limit (option-value invocation :grep-limit)
                          :strip-ansi (and (option-value invocation :strip-ansi) t)
                          :stdout-to (option-value invocation :stdout-to)
                          :root (option-value invocation :root))))))

(defun %wait-command (ports)
  (make-command
   :name "wait" :description (%summary "wait")
   :options (list (make-option :name "file" :kind :value :value-name "PATH" :description "File to watch.")
                  (make-option :name "pattern" :kind :value :value-name "RE" :description "Line pattern.")
                  (make-option :name "port" :kind :value :type :integer :min 1 :max 65535
                               :description "TCP port on the loopback.")
                  (make-option :name "bg" :kind :value :value-name "ID" :description "bg ID.")
                  (make-option :name "exit" :kind :flag :description "With --bg: wait for it to end.")
                  (make-option :name "duration" :kind :value :value-name "DURATION" :description "Wait this long.")
                  (make-option :name "timeout" :kind :value :value-name "DURATION" :default "60s"
                               :description "Give up after this long."))
   :handler (lambda (invocation)
              (%run-flow #'aitools.process.application:wait-command/k ports
                         (aitools.process.application:make-wait-request
                          :file (option-value invocation :file)
                          :pattern (option-value invocation :pattern)
                          :port (option-value invocation :port)
                          :bg (option-value invocation :bg)
                          :exit (and (option-value invocation :exit) t)
                          :duration (option-value invocation :duration)
                          :timeout (option-value invocation :timeout))))))

(defun %bg-start-command (ports)
  (make-command
   :name "start" :description (%summary "bg.start")
   :positionals (list (%argv-positional))
   :options (list (make-option :name "name" :kind :value :value-name "LABEL" :description "Label for bg status."))
   :handler (lambda (invocation)
              (%run-flow #'aitools.process.application:bg-start/k ports
                         (positional-value invocation :argv)
                         (option-value invocation :name)))))

(defun %bg-logs-command (ports)
  (make-command
   :name "logs" :description (%summary "bg.logs")
   :positionals (list (%id-positional t))
   :options (list (make-option :name "tail" :kind :value :type :integer :min 1 :default 100
                               :description "Lines returned.")
                  (make-option :name "from" :kind :value :type :integer :min 0
                               :description "Byte offset to read from.")
                  (%grep-option)
                  (%strip-ansi-option))
   :handler (lambda (invocation)
              (%run-flow #'aitools.process.application:bg-logs/k ports
                         (positional-value invocation :id)
                         :tail (option-value invocation :tail)
                         :from (option-value invocation :from)
                         :grep (option-value invocation :grep)
                         :strip-ansi (and (option-value invocation :strip-ansi) t)))))

(defun %bg-status-command (ports)
  (make-command
   :name "status" :description (%summary "bg.status")
   :positionals (list (%id-positional nil))
   :handler (lambda (invocation)
              (%run-flow #'aitools.process.application:bg-status/k ports
                         (positional-value invocation :id)))))

(defun %bg-stop-command (ports)
  (make-command
   :name "stop" :description (%summary "bg.stop")
   :positionals (list (%id-positional t))
   :options (list (make-option :name "grace" :kind :value :value-name "DURATION" :default "5s"
                               :description "Time between SIGTERM and SIGKILL."))
   :handler (lambda (invocation)
              (%run-flow #'aitools.process.application:bg-stop/k ports
                         (positional-value invocation :id)
                         :grace (option-value invocation :grace)))))

(defun register-process-commands (registry ports)
  "Register `run`, `wait`, and `bg start|logs|status|stop` on REGISTRY, each
handler running against PORTS (an AITOOLS.PROCESS.APPLICATION:PROCESS-PORTS)."
  (flet ((add (name group cli-command)
           (aitools.protocol.application:register-command
            registry :name name :group group :cli-command cli-command :schema (%schema name))))
    (add "run" nil (%run-command ports))
    (add "wait" nil (%wait-command ports))
    (add "bg.start" "bg" (%bg-start-command ports))
    (add "bg.logs" "bg" (%bg-logs-command ports))
    (add "bg.status" "bg" (%bg-status-command ports))
    (add "bg.stop" "bg" (%bg-stop-command ports)))
  registry)
