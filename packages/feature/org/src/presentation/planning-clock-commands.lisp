(in-package #:aitools.org.presentation)

(defun %planning-set-command (boundaries)
  (make-command
   :name "set" :description (%summary "org.planning.set")
   :positionals (list (%path-positional))
   :options (append
             (%org-selector-time-options)
             (list (%value-option "kind" "KIND" "Planning kind."
                                  :choices '("scheduled" "deadline" "closed")
                                  :required-p t)
                   (%value-option "timestamp" "TIMESTAMP" "One supported Org timestamp."
                                  :required-p t))
             (%org-write-options))
   :handler (lambda (invocation)
              (%org-run-selector-write
               #'aitools.org.application:org-planning-set/k boundaries invocation
               (list :kind (option-value invocation :kind)
                     :timestamp (option-value invocation :timestamp))))))

(defun %planning-delete-command (boundaries)
  (make-command
   :name "delete" :description (%summary "org.planning.delete")
   :positionals (list (%path-positional))
   :options (append
             (%org-selector-time-options)
             (list (%value-option "kind" "KIND" "Planning kind."
                                  :choices '("scheduled" "deadline" "closed")
                                  :required-p t))
             (%org-write-options))
   :handler (lambda (invocation)
              (%org-run-selector-write
               #'aitools.org.application:org-planning-delete/k boundaries invocation
               (list :kind (option-value invocation :kind))))))

(defun %clock-in-command (boundaries)
  (make-command
   :name "in" :description (%summary "org.clock.in")
   :positionals (list (%path-positional))
   :options (append (%org-selector-time-options) (%org-write-options))
   :handler (lambda (invocation)
              (%org-run-selector-write
               #'aitools.org.application:org-clock-in/k boundaries invocation nil))))

(defun %clock-out-command (boundaries)
  (make-command
   :name "out" :description (%summary "org.clock.out")
   :positionals (list (%path-positional))
   :options (append (%org-selector-time-options) (%org-write-options))
   :handler (lambda (invocation)
              (%org-run-selector-write
               #'aitools.org.application:org-clock-out/k boundaries invocation nil))))

(defun %org-planning-clock-declarations ()
  (list
   (aitools.protocol.application:define-command
    "org.planning.set"
    (:group "org.planning" :command-builder #'%planning-set-command
     :schema (%schema "org.planning.set")))
   (aitools.protocol.application:define-command
    "org.planning.delete"
    (:group "org.planning" :command-builder #'%planning-delete-command
     :schema (%schema "org.planning.delete")))
   (aitools.protocol.application:define-command
    "org.clock.in"
    (:group "org.clock" :command-builder #'%clock-in-command
     :schema (%schema "org.clock.in")))
   (aitools.protocol.application:define-command
    "org.clock.out"
    (:group "org.clock" :command-builder #'%clock-out-command
     :schema (%schema "org.clock.out")))))
