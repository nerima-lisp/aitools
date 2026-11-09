;;;
;;; cl-cli builders for net.get, net.head, and net.resolve. Flow results are
;;; translated by the command-result continuations supplied by the protocol.
(in-package #:aitools.net.presentation)

(defun %schema (name)
  (let ((entry (find name aitools.data:*net-command-schemas*
                     :key (lambda (entry) (getf entry :name))
                     :test #'string=)))
    (unless entry
      (error "no net schema data for ~S" name))
    (aitools.protocol.domain:make-command-schema
     name (getf entry :summary)
     :description (getf entry :description)
     :args (getf entry :args)
     :output-fields (getf entry :output-fields)
     :error-codes (getf entry :error-codes))))

(defun %summary (name)
  (aitools.protocol.domain:command-schema-summary (%schema name)))

(defun %value-option (name value-name description &rest keys)
  (apply #'make-option :name name :kind :value :value-name value-name
         :description description keys))

(defun %required-positional (key name)
  (make-positional :key key :name name :required-p t))

(defun %run-flow (flow continuation-keys &rest arguments)
  (aitools.protocol.application:call-with-command-result/k
   (lambda (&rest continuations)
     (apply flow
            (append arguments
                    (loop for key in continuation-keys
                          append (list key (getf continuations key))))))))

(defun %get-command (ports)
  (make-command
   :name "get"
   :description (%summary "net.get")
   :positionals (list (%required-positional :url "URL"))
   :options (list (%value-option "timeout" "DURATION" "Total request deadline."
                                 :default "30s")
                  (%value-option "max-lines" "COUNT" "Maximum response body lines returned."
                                 :type :integer :min 1 :default 80)
                  (%value-option "range" "START:END" "1-based response body line range, START:END or START:.")
                  (%value-option "line-offset" "COUNT"
                                 "0-based Unicode character offset on the first selected line."
                                 :type :integer :min 0))
   :handler (lambda (invocation)
              (%run-flow #'aitools.net.application:get/k
                         '(:on-ok :on-partial :on-error)
                         ports
                         (positional-value invocation :url)
                         :timeout (option-value invocation :timeout)
                         :max-lines (option-value invocation :max-lines)
                         :range (option-value invocation :range)
                         :line-offset (option-value invocation :line-offset)))))

(defun %head-command (ports)
  (make-command
   :name "head"
   :description (%summary "net.head")
   :positionals (list (%required-positional :url "URL"))
   :options (list (%value-option "timeout" "DURATION" "Total request deadline."
                                 :default "30s"))
   :handler (lambda (invocation)
              (%run-flow #'aitools.net.application:head/k
                         '(:on-ok :on-error)
                         ports
                         (positional-value invocation :url)
                         :timeout (option-value invocation :timeout)))))

(defun %resolve-command (ports)
  (make-command
   :name "resolve"
   :description (%summary "net.resolve")
   :positionals (list (%required-positional :host "HOST"))
   :options (list (%value-option "timeout" "DURATION" "Total resolver deadline."
                                 :default "30s"))
   :handler (lambda (invocation)
              (%run-flow #'aitools.net.application:resolve/k
                         '(:on-ok :on-error)
                         ports
                         (positional-value invocation :host)
                         :timeout (option-value invocation :timeout)))))

(defun register-net-commands (registry ports)
  (aitools.protocol.application:register-command-declarations
   registry
   (list
    (aitools.protocol.application:define-command
     "net.get"
     (:group "net"
      :command-builder #'%get-command
      :schema (%schema "net.get")))
    (aitools.protocol.application:define-command
     "net.head"
     (:group "net"
      :command-builder #'%head-command
      :schema (%schema "net.head")))
    (aitools.protocol.application:define-command
     "net.resolve"
     (:group "net"
      :command-builder #'%resolve-command
      :schema (%schema "net.resolve"))))
   ports)
  registry)
