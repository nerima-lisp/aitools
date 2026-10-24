(in-package #:aitools.org.presentation)

(defun %todo-set-command (ports)
  (make-command
   :name "set" :description (%summary "org.todo.set")
   :positionals (list (%path-positional))
   :options (append (%org-selector-time-options)
                    (list (%value-option "state" "KEYWORD" "A TODO keyword defined by the document."
                                         :required-p t)
                          (%value-option "note" "TEXT" "Optional TODO logging note."))
                    (%org-write-options))
   :handler (lambda (invocation)
              (%org-run-selector-write
               #'aitools.org.application:org-todo-set/k ports invocation
               (list :state (option-value invocation :state)
                     :note (option-value invocation :note))))))

(defun %property-set-command (ports)
  (make-command
   :name "set" :description (%summary "org.property.set")
   :positionals (list (%path-positional))
   :options (append (%org-selector-time-options)
                    (list (%value-option "name" "NAME" "Property name." :required-p t)
                          (%value-option "value" "VALUE" "Single-line property value."
                                         :required-p t))
                    (%org-write-options))
   :handler (lambda (invocation)
              (%org-run-selector-write
               #'aitools.org.application:org-property-set/k ports invocation
               (list :name (option-value invocation :name)
                     :value (option-value invocation :value))))))

(defun %property-delete-command (ports)
  (make-command
   :name "delete" :description (%summary "org.property.delete")
   :positionals (list (%path-positional))
   :options (append (%org-selector-time-options)
                    (list (%value-option "name" "NAME" "Property name."
                                         :required-p t))
                    (%org-write-options))
   :handler (lambda (invocation)
              (%org-run-selector-write
               #'aitools.org.application:org-property-delete/k ports invocation
               (list :name (option-value invocation :name))))))

(defun %org-todo-property-declarations ()
  (list
   (aitools.protocol.application:define-command
    "org.todo.set"
    (:group "org.todo" :command-builder #'%todo-set-command
     :schema (%schema "org.todo.set")))
   (aitools.protocol.application:define-command
    "org.property.set"
    (:group "org.property" :command-builder #'%property-set-command
     :schema (%schema "org.property.set")))
   (aitools.protocol.application:define-command
    "org.property.delete"
    (:group "org.property" :command-builder #'%property-delete-command
     :schema (%schema "org.property.delete")))))
