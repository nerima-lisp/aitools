;;;; packages/feature/vcs/src/presentation/commands.lisp
;;;;
;;;; The `git` group's cl-cli commands. Each handler turns the parsed
;;;; invocation into one AITOOLS.VCS.APPLICATION flow call and returns the
;;;; COMMAND-RESULT that call produced.
(in-package #:aitools.vcs.presentation)

(defun %schema (name)
  (let ((entry (find name aitools.data:*vcs-command-schemas*
                     :key (lambda (entry) (getf entry :name)) :test #'string=)))
    (aitools.protocol.domain:make-command-schema
     name (getf entry :summary)
     :args (getf entry :args)
     :output-fields (getf entry :output-fields)
     :error-codes (getf entry :error-codes))))

(defun %run-flow (flow &rest arguments)
  "Call FLOW with ARGUMENTS followed by the three command-result
continuations, returning the resulting COMMAND-RESULT."
  (aitools.protocol.application:call-with-command-result/k
   (lambda (&rest continuations)
     (apply flow (append arguments continuations)))))

(defun %selector-option (option)
  "OPTION, a plist from AITOOLS.DATA:*SELECTOR-OPTIONS*, as a cl-cli option."
  (let ((name (getf option :name))
        (description (getf option :description))
        (value-name (getf option :value-name)))
    (ecase (getf option :kind)
      (:flag (make-option :name name :kind :flag :description description))
      (:value (apply #'make-option :name name :kind :value :description description
                     (when value-name (list :value-name value-name))))
      (:pair (apply #'make-option :name name :kind :value :value-count 2 :description description
                    (when value-name (list :value-name value-name)))))))

(defun %selector-options ()
  "The selectors `git blame` and `git show` accept (all but
--old), built from the shared AITOOLS.DATA:*SELECTOR-OPTIONS* table."
  (mapcar #'%selector-option aitools.data:*selector-options*))

(defun %selector-arguments (invocation)
  (loop for key in '(:range :symbol :kind :between :exclusive :match :invert)
        for value = (option-value invocation key)
        when value append (list key value)))

(defun %max-lines-option (default)
  (make-option :name "max-lines" :kind :value :type :integer :min 1 :default default
               :description "Maximum lines returned."))

(defun %status-command (ports)
  (make-command
   :name "status" :description (aitools.protocol.domain:command-schema-summary (%schema "git.status"))
   :handler (lambda (invocation)
              (%run-flow #'aitools.vcs.application:git-status/k ports :root (option-value invocation :root)))))

(defun %log-command (ports)
  (make-command
   :name "log" :description (aitools.protocol.domain:command-schema-summary (%schema "git.log"))
   :positionals (list (make-positional :key :path :name "path" :required-p nil))
   :options (list (make-option :name "limit" :kind :value :type :integer :min 1 :default 20
                               :description "Maximum number of commits."))
   :handler (lambda (invocation)
              (%run-flow #'aitools.vcs.application:git-log/k ports :root (option-value invocation :root)
                         :path (positional-value invocation :path)
                         :limit (option-value invocation :limit)))))

(defun %diff-command (ports)
  (make-command
   :name "diff" :description (aitools.protocol.domain:command-schema-summary (%schema "git.diff"))
   :positionals (list (make-positional :key :path :name "path" :required-p nil))
   :options (list (make-option :name "staged" :kind :flag :description "Compare the index with HEAD.")
                  (make-option :name "ref" :kind :value :value-name "REV" :description "Revision or A..B range.")
                  (make-option :name "output" :kind :value :choices '("hunks" "stat") :default "hunks"
                               :description "hunks or stat.")
                  (%max-lines-option 400))
   :handler (lambda (invocation)
              (%run-flow #'aitools.vcs.application:git-diff/k ports :root (option-value invocation :root)
                         :path (positional-value invocation :path)
                         :staged (option-value invocation :staged)
                         :ref (option-value invocation :ref)
                         :output (if (string= (option-value invocation :output) "stat") :stat :hunks)
                         :max-lines (option-value invocation :max-lines)))))

(defun %blame-command (ports)
  (make-command
   :name "blame" :description (aitools.protocol.domain:command-schema-summary (%schema "git.blame"))
   :positionals (list (make-positional :key :path :name "path" :required-p t))
   :options (%selector-options)
   :handler (lambda (invocation)
              (apply #'%run-flow #'aitools.vcs.application:git-blame/k ports
                     (positional-value invocation :path)
                     :root (option-value invocation :root)
                     (%selector-arguments invocation)))))

(defun %show-command (ports)
  (make-command
   :name "show" :description (aitools.protocol.domain:command-schema-summary (%schema "git.show"))
   :positionals (list (make-positional :key :object :name "object" :required-p t))
   :options (append (%selector-options) (list (%max-lines-option 80)))
   :handler (lambda (invocation)
              (apply #'%run-flow #'aitools.vcs.application:git-show/k ports
                     (positional-value invocation :object)
                     :root (option-value invocation :root)
                     :max-lines (option-value invocation :max-lines)
                     (%selector-arguments invocation)))))

(defun register-vcs-commands (registry ports)
  "Register `git status|log|diff|blame|show` on REGISTRY. PORTS is the
AITOOLS.VCS.APPLICATION:GIT-PORT the composition root built."
  (loop for (name builder) in (list (list "status" #'%status-command)
                                    (list "log" #'%log-command)
                                    (list "diff" #'%diff-command)
                                    (list "blame" #'%blame-command)
                                    (list "show" #'%show-command))
        for full-name = (concatenate 'string "git." name)
        do (aitools.protocol.application:register-command
            registry :name full-name :group "git"
                     :cli-command (funcall builder ports)
                     :schema (%schema full-name)))
  registry)
