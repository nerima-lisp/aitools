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

(defun %paths-positional ()
  (make-positional :key :paths :name "path" :rest-p t :required-p t :min-count 1))

(defun %value-option (name value-name description &rest options)
  (apply #'make-option :name name :kind :value
         :value-name value-name :description description options))

(defun %outline-command (boundaries)
  (make-command
   :name "outline" :description (%summary "org.outline")
   :positionals (list (%path-positional))
   :options (list (%value-option "limit" "N" "Maximum entries returned."
                                 :type :integer :min 0))
   :handler (lambda (invocation)
              (%run-flow #'aitools.org.application:org-outline/k boundaries
                         :path (positional-value invocation :path)
                         :limit (option-value invocation :limit)
                         :root (option-value invocation :root)))))

(defun %query-command (boundaries)
  (make-command
   :name "query" :description (%summary "org.query")
   :positionals (list (%paths-positional))
   :options (list (%value-option "match" "MATCH" "An Org match expression.")
                  (%value-option "at" "LOCAL-DATETIME" "The local reference time.")
                  (%value-option "tz" "ZONE" "IANA time zone name.")
                  (%value-option "dst-policy" "POLICY" "DST gap and overlap policy."
                                 :choices '("compatible" "earlier" "later" "strict")
                                 :default "compatible"))
   :handler (lambda (invocation)
              (%run-flow #'aitools.org.application:org-query/k boundaries
                         :paths (positional-value invocation :paths)
                         :match (option-value invocation :match)
                         :at (option-value invocation :at)
                         :tz (option-value invocation :tz)
                         :dst-policy (option-value invocation :dst-policy)
                         :root (option-value invocation :root)))))

(defun %export-command (boundaries)
  (make-command
   :name "export" :description (%summary "org.export")
   :positionals (list (%path-positional))
   :options (list (%value-option "backend" "html|gfm|ascii" "Export backend."
                                 :choices '("html" "gfm" "ascii")))
   :handler (lambda (invocation)
              (%run-flow #'aitools.org.application:org-export/k boundaries
                         :path (positional-value invocation :path)
                         :backend (%backend (option-value invocation :backend))
                         :root (option-value invocation :root)))))

(defun %archive-command (boundaries)
  (make-command
   :name "archive" :description (%summary "org.archive")
   :positionals (list (%path-positional))
   :options (append (%org-selector-time-options)
                    (list (%value-option "to" "PATH" "Destination Org file." :required-p t)
                          (%value-option "target-heading" "TITLE" "Destination heading title." :required-p t))
                    (%org-write-options))
   :handler (lambda (invocation)
              (%org-run-selector-write
               #'aitools.org.application:org-archive/k boundaries invocation
               (list :to (option-value invocation :to)
                     :target-heading (option-value invocation :target-heading))))))
