(in-package #:aitools.org.presentation)

(defun %schema (name)
  (let ((entry (find name aitools.data:*org-command-schemas*
                     :key (lambda (row) (getf row :name))
                     :test #'string=)))
    (aitools.protocol.domain:make-command-schema
     name (getf entry :summary)
     :description (getf entry :description)
     :args (getf entry :args)
     :output-fields (getf entry :output-fields)
     :error-codes (getf entry :error-codes))))

(defun %summary (name)
  (aitools.protocol.domain:command-schema-summary (%schema name)))
(defun %backend (name)
  (cdr (assoc name
              (quote (("html" . :html) ("gfm" . :gfm) ("ascii" . :ascii)))
              :test (function string=))))

(defun %run-flow (flow &rest arguments)
  (aitools.protocol.application:call-with-command-result/k
   (lambda (&rest continuations)
     (apply flow (append arguments continuations)))))

(defun %path-positional ()
  (make-positional :key :path :name "path" :required-p t))

(defun %value-option (name value-name description &rest options)
  (apply #'make-option :name name :kind :value
         :value-name value-name :description description options))

(defun %outline-command (ports)
  (make-command
   :name "outline" :description (%summary "org.outline")
   :positionals (list (%path-positional))
   :options (list (%value-option "limit" "N" "Maximum entries returned."
                                 :type :integer :min 0))
   :handler (lambda (invocation)
              (%run-flow #'aitools.org.application:org-outline/k ports
                         :path (positional-value invocation :path)
                         :limit (option-value invocation :limit)
                         :root (option-value invocation :root)))))

(defun %query-command (ports)
  (make-command
   :name "query" :description (%summary "org.query")
   :positionals (list (%path-positional))
   :options (list (%value-option "match" "MATCH" "An Org match expression.")
                  (%value-option "at" "LOCAL-DATETIME" "The local reference time.")
                  (%value-option "tz" "ZONE" "IANA time zone name.")
                  (%value-option "dst-policy" "POLICY" "DST gap and overlap policy."
                                 :choices '("compatible" "earlier" "later" "strict")
                                 :default "compatible"))
   :handler (lambda (invocation)
              (%run-flow #'aitools.org.application:org-query/k ports
                         :path (positional-value invocation :path)
                         :match (option-value invocation :match)
                         :at (option-value invocation :at)
                         :tz (option-value invocation :tz)
                         :dst-policy (option-value invocation :dst-policy)
                         :root (option-value invocation :root)))))

(defun %export-command (ports)
  (make-command
   :name "export" :description (%summary "org.export")
   :positionals (list (%path-positional))
   :options (list (%value-option "backend" "html|gfm|ascii" "Export backend."
                                 :choices '("html" "gfm" "ascii")))
   :handler (lambda (invocation)
              (%run-flow #'aitools.org.application:org-export/k ports
                         :path (positional-value invocation :path)
                         :backend (%backend (option-value invocation :backend))
                         :root (option-value invocation :root)))))

(defun register-org-commands (registry ports)
  (aitools.protocol.application:register-command-declarations
   registry
   (list
    (aitools.protocol.application:define-command
     "org.outline"
     (:group "org" :command-builder #'%outline-command
      :schema (%schema "org.outline")))
    (aitools.protocol.application:define-command
     "org.query"
     (:group "org" :command-builder #'%query-command
      :schema (%schema "org.query")))
    (aitools.protocol.application:define-command
     "org.export"
     (:group "org" :command-builder #'%export-command
      :schema (%schema "org.export"))))
   ports)
  registry)
