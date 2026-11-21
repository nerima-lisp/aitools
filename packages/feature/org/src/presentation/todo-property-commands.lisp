(in-package #:aitools.org.presentation)

(defun %todo-set-command (boundaries)
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
               #'aitools.org.application:org-todo-set/k boundaries invocation
               (list :state (option-value invocation :state)
                     :note (option-value invocation :note))))))

(defun %property-set-command (boundaries)
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
               #'aitools.org.application:org-property-set/k boundaries invocation
               (list :name (option-value invocation :name)
                     :value (option-value invocation :value))))))

(defun %property-delete-command (boundaries)
  (make-command
   :name "delete" :description (%summary "org.property.delete")
   :positionals (list (%path-positional))
   :options (append (%org-selector-time-options)
                    (list (%value-option "name" "NAME" "Property name."
                                         :required-p t))
                    (%org-write-options))
   :handler (lambda (invocation)
              (%org-run-selector-write
               #'aitools.org.application:org-property-delete/k boundaries invocation
               (list :name (option-value invocation :name))))))
(defun %heading-edit-command (boundaries command name operation)
  (let ((option (case operation
                  (:priority-set (%value-option "priority" "PRIORITY" "Priority value." :required-p t))
                  ((:tags-add :tags-remove :tags-replace)
                   (%value-option "tags" "TAGS" "Comma-separated local tags." :required-p t))
                  (:heading-rename (%value-option "title" "TEXT" "New heading text." :required-p t))
                  (:checkbox-toggle (%value-option "item" "N" "One-based checkbox number."
                                                  :type :integer :min 1 :required-p t)))))
    (make-command
     :name name :description (%summary command)
     :positionals (list (%path-positional))
     :options (append (%org-selector-time-options) (when option (list option)) (%org-write-options))
     :handler (lambda (invocation)
                (%org-run-selector-write
                 #'aitools.org.application:org-heading-edit/k boundaries invocation
                 (append (list :command command :operation operation)
                         (case operation
                           (:priority-set (list :priority (option-value invocation :priority)))
                           ((:tags-add :tags-remove :tags-replace)
                            (list :tags (option-value invocation :tags)))
                           (:heading-rename (list :title (option-value invocation :title)))
                           (:checkbox-toggle (list :item (option-value invocation :item))))))))))

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
     :schema (%schema "org.property.delete")))
   (aitools.protocol.application:define-command
    "org.priority.set"
    (:group "org.priority"
     :command-builder (lambda (boundaries)
                        (%heading-edit-command boundaries "org.priority.set" "set" :priority-set))
     :schema (%schema "org.priority.set")))
   (aitools.protocol.application:define-command
    "org.priority.delete"
    (:group "org.priority"
     :command-builder (lambda (boundaries)
                        (%heading-edit-command boundaries "org.priority.delete" "delete" :priority-delete))
     :schema (%schema "org.priority.delete")))
   (aitools.protocol.application:define-command
    "org.tags.add"
    (:group "org.tags"
     :command-builder (lambda (boundaries)
                        (%heading-edit-command boundaries "org.tags.add" "add" :tags-add))
     :schema (%schema "org.tags.add")))
   (aitools.protocol.application:define-command
    "org.tags.remove"
    (:group "org.tags"
     :command-builder (lambda (boundaries)
                        (%heading-edit-command boundaries "org.tags.remove" "remove" :tags-remove))
     :schema (%schema "org.tags.remove")))
   (aitools.protocol.application:define-command
    "org.tags.replace"
    (:group "org.tags"
     :command-builder (lambda (boundaries)
                        (%heading-edit-command boundaries "org.tags.replace" "replace" :tags-replace))
     :schema (%schema "org.tags.replace")))
   (aitools.protocol.application:define-command
    "org.heading.rename"
    (:group "org.heading"
     :command-builder (lambda (boundaries)
                        (%heading-edit-command boundaries "org.heading.rename" "rename" :heading-rename))
     :schema (%schema "org.heading.rename")))
   (aitools.protocol.application:define-command
    "org.checkbox.toggle"
    (:group "org.checkbox"
     :command-builder (lambda (boundaries)
                        (%heading-edit-command boundaries "org.checkbox.toggle" "toggle" :checkbox-toggle))
     :schema (%schema "org.checkbox.toggle")))))
