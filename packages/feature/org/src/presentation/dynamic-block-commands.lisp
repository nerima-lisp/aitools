(in-package #:aitools.org.presentation)

(defun %dynamic-block-update-command (boundaries)
  (make-command
   :name "update" :description (%summary "org.dynamic-block.update")
   :positionals (list (%path-positional))
   :options (append
             (list (%value-option "block" "N" "One-based dynamic block number."
                                  :type :integer :min 1 :required-p t)
                   (%value-option "agenda-glob" "GLOB" "Workspace-relative agenda file glob; repeatable."
                                  :multiple-p t))
             (%org-time-options)
             (%org-write-options))
   :handler (lambda (invocation)
              (%org-run-write #'aitools.org.application:org-dynamic-block-update/k
                              boundaries invocation
                              (append (list :block (option-value invocation :block)
                                            :agenda-glob (option-value invocation :agenda-glob))
                                      (%org-time-arguments invocation))))))

(defun %org-dynamic-block-declarations ()
  (list (aitools.protocol.application:define-command
         "org.dynamic-block.update"
         (:group "org.dynamic-block" :command-builder #'%dynamic-block-update-command
          :schema (%schema "org.dynamic-block.update")))))
