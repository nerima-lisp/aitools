(in-package #:aitools.org.presentation)

(defun %org-time-options ()
  (list (%value-option "at" "LOCAL-DATETIME" "Minute-aligned local date-time." :required-p t)
        (%value-option "tz" "ZONE" "IANA time zone name." :required-p t)
        (%value-option "dst-policy" "POLICY" "DST gap and overlap policy."
                       :choices '("compatible" "earlier" "later" "strict")
                       :default "compatible")))

(defun %org-selector-time-options ()
  (append (list (%value-option "match" "MATCH" "An Org match expression."
                               :required-p t))
          (%org-time-options)))

(defun %org-write-options ()
  (list (make-option :name "dry-run" :kind :flag
                     :description "Validate the write and show its diff without changing files.")
        (%value-option "tx" "TX" "Stage the write in this transaction.")))

(defun %org-time-arguments (invocation)
  (list :at (option-value invocation :at)
        :tz (option-value invocation :tz)
        :dst-policy (option-value invocation :dst-policy)))

(defun %org-write-arguments (invocation)
  (list :root (option-value invocation :root)
        :lock-timeout (option-value invocation :lock-timeout)
        :tx (option-value invocation :tx)
        :dry-run (and (option-value invocation :dry-run) t)
        :expect-count nil
        :expect-hash nil
        :display-argv (rest (cl-cli:invocation-raw-argv invocation))))

(defun %org-read-arguments (invocation)
  (list :root (option-value invocation :root)))

(defun %org-run-write (flow ports invocation arguments)
  (apply #'%run-flow flow ports
         (append (list :path (positional-value invocation :path))
                 arguments
                 (%org-write-arguments invocation))))

(defun %org-run-selector-write (flow ports invocation arguments)
  (apply #'%run-flow flow ports
         (append (list :path (positional-value invocation :path)
                       :match (option-value invocation :match))
                 arguments
                 (%org-time-arguments invocation)
                 (%org-write-arguments invocation))))

(defun %org-run-selector-read (flow ports invocation arguments)
  (apply #'%run-flow flow ports
         (append (list :path (positional-value invocation :path)
                       :match (option-value invocation :match))
                 arguments
                 (%org-time-arguments invocation)
                 (%org-read-arguments invocation))))
