(in-package #:aitools.org.presentation)

(defun register-org-commands (registry ports)
  (let ((declarations
         (append
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
          (%org-todo-property-declarations)
          (%org-planning-clock-declarations)
          (%org-agenda-report-declarations)
          (%org-table-declarations))))
    (aitools.protocol.application:register-command-declarations
     registry declarations ports))
  registry)
