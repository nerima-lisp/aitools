(in-package #:aitools.md.presentation)

(defun %schema (name)
  (let ((entry
         (find name
               aitools.data:*md-command-schemas*
               :key
               (lambda (row)
                 (getf row :name))
               :test
               #'string=)))
    (aitools.protocol.domain:make-command-schema name
                                                 (getf entry :summary)
                                                 :description
                                                 (getf entry :description)
                                                 :args
                                                 (getf entry :args)
                                                 :output-fields
                                                 (getf entry :output-fields)
                                                 :error-codes
                                                 (getf entry :error-codes))))

(defun %summary (name)
  (aitools.protocol.domain:command-schema-summary (%schema name)))

(defun %run-flow (flow &rest arguments)
  (aitools.protocol.application:call-with-command-result/k
   (lambda (&rest continuations)
     (apply flow (append arguments continuations)))))

(defun %path-positional ()
  (make-positional :key :path :name "path" :required-p t))

(defun %section-positional ()
  (make-positional :key :section :name "section" :type :integer :min 1 :required-p t))

(defun %flag-option (name description &rest options)
  (apply #'make-option :name name :kind :flag :description description options))

(defun %value-option (name value-name description &rest options)
  (apply #'make-option :name name :kind :value :value-name value-name :description description options))

(defun %headings-command (boundaries)
  (make-command :name
                "headings"
                :description
                (%summary "md.headings")
                :positionals
                (list (%path-positional))
                :handler
                (lambda (invocation)
                  (%run-flow #'aitools.md.application:md-headings/k
                             boundaries
                             (positional-value invocation :path)
                             :root
                             (option-value invocation :root)))))

(defun %read-command (boundaries)
  (make-command :name
                "read"
                :description
                (%summary "md.read")
                :positionals
                (list (%path-positional) (%section-positional))
                :handler
                (lambda (invocation)
                  (%run-flow #'aitools.md.application:md-read/k
                             boundaries
                             (positional-value invocation :path)
                             (positional-value invocation :section)
                             :root
                             (option-value invocation :root)))))

(defun %replace-command (boundaries)
  (make-command :name
                "replace"
                :description
                (%summary "md.replace")
                :positionals
                (list (%path-positional) (%section-positional))
                :options
                (list (%value-option "expect-hash" "HASH" "Required SHA-256 of the current target." :required-p t)
                      (%flag-option "stdin" "Read the complete replacement section from standard input." :required-p t)
                      (%flag-option "dry-run" "Validate the replacement without writing.")
                      (%value-option "tx" "TX" "Stage the write in this transaction."))
                :handler
                (lambda (invocation)
                  (%run-flow #'aitools.md.application:md-replace/k
                             boundaries
                             (positional-value invocation :path)
                             (positional-value invocation :section)
                             :expect-hash
                             (option-value invocation :expect-hash)
                             :stdin
                             (and (option-value invocation :stdin) t)
                             :dry-run
                             (and (option-value invocation :dry-run) t)
                             :tx
                             (option-value invocation :tx)
                             :root
                             (option-value invocation :root)
                             :lock-timeout
                             (option-value invocation :lock-timeout)
                             :display-argv
                             (rest (cl-cli:invocation-raw-argv invocation))))))
