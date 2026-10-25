;;;;
;;;; cl-cli definitions and schemas for the `util` group (docs/src/reference/commands/util.md).
;;;; Each handler turns the parsed invocation into a flow call; the flow's
;;;; continuation decides the envelope, never this layer. The `schema` text
;;;; (summaries, args, output fields, error codes) is data:
;;;; AITOOLS.DATA:*UTIL-COMMAND-SCHEMAS*.
(in-package #:aitools.util.presentation)

(defun %input-options ()
  (list (make-option :name "content" :kind :value :value-name "TEXT" :description "Input text.")
        (make-option :name "content-file" :kind :value :value-name "PATH" :description "Input bytes from a file.")
        (make-option :name "stdin" :kind :flag :description "Read UTF-8 text from standard input.")))

(defun %input-request (invocation)
  (aitools.util.application:make-input-request
   :content (option-value invocation :content)
   :content-file (option-value invocation :content-file)
   :stdin (and (option-value invocation :stdin) t)))

(defmacro %with-command-result ((on-ok on-error) &body body)
  "Run BODY with ON-OK and ON-ERROR bound to the command-result
continuations and return the resulting COMMAND-RESULT."
  (let ((on-partial (gensym "ON-PARTIAL")))
    `(aitools.protocol.application:call-with-command-result/k
      (lambda (&key ((:on-ok ,on-ok)) ((:on-partial ,on-partial)) ((:on-error ,on-error)))
        (declare (ignore ,on-partial))
        ,@body))))

(defun %schema-data (name)
  (or (find name aitools.data:*util-command-schemas*
            :key (lambda (entry) (getf entry :name)) :test #'string=)
      (error "no util schema data for ~S" name)))

(defun %schema (name)
  (let ((full-name (format nil "util.~A" name))
        (data (%schema-data name)))
    (aitools.protocol.domain:make-command-schema
     full-name (getf data :summary)
     :args (getf data :args)
     :output-fields (getf data :output-fields)
     :error-codes (getf data :error-codes))))

(defun %scheme-positional ()
  (make-positional :name "scheme" :key :scheme :required-p t
                   :choices aitools.util.application:+util-codec-schemes+))

(defun %encode-command (ports)
  (make-command
   :name "encode"
   :description "Encode bytes as base64, URL percent-encoding, or hex."
   :positionals (list (%scheme-positional))
   :options (%input-options)
   :handler (lambda (invocation)
              (%with-command-result (on-ok on-error)
                (aitools.util.application:util-encode-flow
                 ports (positional-value invocation :scheme) (%input-request invocation)
                 :on-ok on-ok :on-error on-error)))))

(defun %decode-command (ports)
  (make-command
   :name "decode"
   :description "Decode base64, URL percent-encoding, or hex."
   :positionals (list (%scheme-positional))
   :options (append
             (%input-options)
             (list
              (make-option :name "to" :kind :value :value-name "PATH"
                           :description "Write the decoded bytes to this new file.")
              (make-option :name "dry-run" :kind :flag
                           :description "With --to: validate and show the change; write nothing.")
              (make-option :name "tx" :kind :value :value-name "TX"
                           :description "With --to: stage the write in this tx.")))
   :handler (lambda (invocation)
              (%with-command-result (on-ok on-error)
                (aitools.util.application:util-decode-flow
                 ports (positional-value invocation :scheme) (%input-request invocation)
                 :to (option-value invocation :to)
                 :root (option-value invocation :root)
                 :lock-timeout (option-value invocation :lock-timeout)
                 :dry-run (and (option-value invocation :dry-run) t)
                 :tx (option-value invocation :tx)
                 :display-argv (rest (cl-cli:invocation-raw-argv invocation))
                 :on-ok on-ok :on-error on-error)))))

(defun %redact-command (ports)
  (make-command
   :name "redact"
   :description "Mask known secret formats in text."
   :options (%input-options)
   :handler (lambda (invocation)
              (%with-command-result (on-ok on-error)
                (aitools.util.application:util-redact-flow
                 ports (%input-request invocation) :on-ok on-ok :on-error on-error)))))

(defun %tokens-command (ports)
  (make-command
   :name "tokens"
   :description "Count characters, bytes, lines, words, and approximate tokens."
   :options (%input-options)
   :handler (lambda (invocation)
              (%with-command-result (on-ok on-error)
                (aitools.util.application:util-tokens-flow
                 ports (%input-request invocation) :on-ok on-ok :on-error on-error)))))

(defun %calc-command (ports)
  (make-command
   :name "calc"
   :description "Evaluate an arithmetic expression exactly."
   :positionals
   (list (make-positional :name "expression" :key :expression :required-p nil))
   :options
   (list
    (make-option :name "stdin" :kind :flag
                 :description "Read the expression from standard input.")
    (make-option :name "decimals" :kind :value :type :integer :value-name "N"
                 :default aitools.util.application:+util-default-decimals+
                 :description "Fractional digits in result."))
   :handler (lambda (invocation)
              (%with-command-result (on-ok on-error)
                (aitools.util.application:util-calc-flow
                 ports (positional-value invocation :expression)
                 (and (option-value invocation :stdin) t)
                 (option-value invocation :decimals)
                 :on-ok on-ok :on-error on-error)))))

(defun %uuid-command (ports)
  (make-command
   :name "uuid"
   :description "Generate UUIDs (v4 random, v7 time-ordered)."
   :options
   (list
    (make-option :name "kind" :kind :value :value-name "KIND"
                 :choices aitools.util.application:+util-uuid-kinds+
                 :default "v4"
                 :description "UUID version.")
    (make-option :name "count" :kind :value :type :integer :value-name "N"
                 :default 1
                 :description "Number of UUIDs."))
   :handler (lambda (invocation)
              (%with-command-result (on-ok on-error)
                (aitools.util.application:util-uuid-flow
                 ports (option-value invocation :kind) (option-value invocation :count)
                 :on-ok on-ok :on-error on-error)))))

(defun %random-command (ports)
  (make-command
   :name "random"
   :description "Generate random strings from the OS cryptographic random source."
   :options
   (list
    (make-option :name "length" :kind :value :type :integer :value-name "N"
                 :default 32
                 :description "Characters per value.")
    (make-option :name "alphabet" :kind :value :value-name "NAME"
                 :choices aitools.util.application:+util-random-alphabets+
                 :default (first aitools.util.application:+util-random-alphabets+)
                 :description "Character set.")
    (make-option :name "count" :kind :value :type :integer :value-name "N"
                 :default 1
                 :description "Number of values."))
   :handler (lambda (invocation)
              (%with-command-result (on-ok on-error)
                (aitools.util.application:util-random-flow
                 ports (option-value invocation :length)
                 (option-value invocation :alphabet)
                 (option-value invocation :count)
                 :on-ok on-ok :on-error on-error)))))

(defun register-util-commands (registry ports)
  "Register every `util` command on REGISTRY, each running its flow against
PORTS (an AITOOLS.UTIL.APPLICATION:UTIL-PORTS). Returns REGISTRY."
  (aitools.protocol.application:register-command-declarations
   registry
   (list
    (aitools.protocol.application:define-command
     "util.encode"
     (:group "util"
      :command-builder #'%encode-command
      :schema (%schema "encode")))
    (aitools.protocol.application:define-command
     "util.decode"
     (:group "util"
      :command-builder #'%decode-command
      :schema (%schema "decode")))
    (aitools.protocol.application:define-command
     "util.redact"
     (:group "util"
      :command-builder #'%redact-command
      :schema (%schema "redact")))
    (aitools.protocol.application:define-command
     "util.tokens"
     (:group "util"
      :command-builder #'%tokens-command
      :schema (%schema "tokens")))
    (aitools.protocol.application:define-command
     "util.calc"
     (:group "util"
      :command-builder #'%calc-command
      :schema (%schema "calc")))
    (aitools.protocol.application:define-command
     "util.uuid"
     (:group "util"
      :command-builder #'%uuid-command
      :schema (%schema "uuid")))
    (aitools.protocol.application:define-command
     "util.random"
     (:group "util"
      :command-builder #'%random-command
      :schema (%schema "random"))))
   ports))
