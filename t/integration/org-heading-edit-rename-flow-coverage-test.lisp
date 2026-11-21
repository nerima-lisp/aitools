(in-package #:aitools.edit.test)
(describe "Org heading rename adapter"
  (it "keeps the statistics cookie when the new title has none"
    (multiple-value-bind (updated fields failure)
        (aitools.org.infrastructure::%org-title-edit
         (%heading-edit-engine) (%heading-edit-options :title "New"))
      (expect failure :to-be nil)
      (expect fields :to-be nil)
      (expect (%heading-edit-text updated)
              :to-equal (format nil "#+TODO: TODO | DONE~%* TODO [#B] New [1/2] :a:b:~%- [ ] x~%- [X] y~%"))))
  (it "uses the cookie of the new title instead of the old one"
    (expect (%heading-edit-text
             (nth-value 0 (aitools.org.infrastructure::%org-title-edit
                           (%heading-edit-engine)
                           (%heading-edit-options :title "New [50%]"))))
            :to-equal (format nil "#+TODO: TODO | DONE~%* TODO [#B] New [50%] :a:b:~%- [ ] x~%- [X] y~%")))
  (it "renames a heading that has no cookie"
    (expect (%heading-edit-text
             (nth-value 0 (aitools.org.infrastructure::%org-title-edit
                           (%heading-edit-engine
                            (format nil "#+TODO: TODO | DONE~%* TODO Plain *bold*~%"))
                           (%heading-edit-options :title "Other"))))
            :to-equal (format nil "#+TODO: TODO | DONE~%* TODO Other~%")))
  (it "returns selection failures before editing"
    (expect (%heading-edit-failure-kind
             (multiple-value-list
              (aitools.org.infrastructure::%org-title-edit
               (%heading-edit-engine)
               (list :match "/ABSENT" :title "New" :at "2026-10-19T09:00:00"
                     :tz "UTC" :dst-policy "compatible"))))
            :to-be :selection-no-match))
  (it "maps edit, resource, and generic Org kit errors to failures"
    (let ((engine (%heading-edit-engine)))
      (expect (%heading-edit-failure-kind
               (multiple-value-list
                (aitools.org.infrastructure::%org-title-edit
                 engine (%heading-edit-options :title (format nil "two~%lines")))))
              :to-be :argument)
      (expect (%heading-edit-failure-kind
               (multiple-value-list
                (aitools.org.infrastructure::%org-title-edit
                 engine (%heading-edit-options :title "Title :tag:"))))
              :to-be :argument)
      (expect (%heading-edit-failure-kind
               (multiple-value-list
                (aitools.org.infrastructure::%org-title-edit
                 (%heading-edit-limited-engine)
                 (%heading-edit-options :title "A considerably longer title"))))
              :to-be :too-large)
      (%with-heading-edit-error
          org-kit:replace-heading-title
          (make-condition 'org-kit:org-kit-error :message "title failed")
        (expect (%heading-edit-failure-kind
                 (multiple-value-list
                  (aitools.org.infrastructure::%org-title-edit
                   engine (%heading-edit-options :title "New"))))
                :to-be :internal)))))

(describe "Org heading edit dispatch"
  (it "routes priority, tags, rename, and checkbox operations to their adapters"
    (let ((engine (%heading-edit-engine)))
      (flet ((edited (operation &rest options)
               (multiple-value-bind (updated fields failure)
                   (aitools.org.infrastructure::edit-org-document
                    engine operation (apply #'%heading-edit-options options))
                 (expect failure :to-be nil)
                 (values (%heading-edit-text updated) fields))))
        (expect (edited :priority-set :priority "A")
                :to-equal (format nil "#+TODO: TODO | DONE~%* TODO [#A] Task [1/2] :a:b:~%- [ ] x~%- [X] y~%"))
        (expect (edited :priority-delete)
                :to-equal (format nil "#+TODO: TODO | DONE~%* TODO Task [1/2] :a:b:~%- [ ] x~%- [X] y~%"))
        (expect (edited :tags-add :tags "c")
                :to-equal (format nil "#+TODO: TODO | DONE~%* TODO [#B] Task [1/2] :a:b:c:~%- [ ] x~%- [X] y~%"))
        (expect (edited :tags-remove :tags "a")
                :to-equal (format nil "#+TODO: TODO | DONE~%* TODO [#B] Task [1/2] :b:~%- [ ] x~%- [X] y~%"))
        (expect (edited :tags-replace :tags "z")
                :to-equal (format nil "#+TODO: TODO | DONE~%* TODO [#B] Task [1/2] :z:~%- [ ] x~%- [X] y~%"))
        (expect (edited :heading-rename :title "Renamed")
                :to-equal (format nil "#+TODO: TODO | DONE~%* TODO [#B] Renamed [1/2] :a:b:~%- [ ] x~%- [X] y~%"))
        (multiple-value-bind (text fields) (edited :checkbox-toggle :item 1)
          (expect text
                  :to-equal (format nil "#+TODO: TODO | DONE~%* TODO [#B] Task [2/2] :a:b:~%- [X] x~%- [X] y~%"))
          (expect fields :to-equal (list (cons "item" 1))))))))

(describe "Org heading edit flow"
  (it "writes each heading edit through the shared write flow"
    (call-with-edit-workspace
     (lambda ()
       (dolist (case (list
                      (list (list :command "org.priority.set" :operation :priority-set :priority "A")
                            (format nil "#+TODO: TODO | DONE~%* TODO [#A] Task [1/2] :a:b:~%- [ ] x~%- [X] y~%"))
                      (list (list :command "org.priority.delete" :operation :priority-delete)
                            (format nil "#+TODO: TODO | DONE~%* TODO Task [1/2] :a:b:~%- [ ] x~%- [X] y~%"))
                      (list (list :command "org.tags.add" :operation :tags-add :tags "c")
                            (format nil "#+TODO: TODO | DONE~%* TODO [#B] Task [1/2] :a:b:c:~%- [ ] x~%- [X] y~%"))
                      (list (list :command "org.heading.rename" :operation :heading-rename :title "New")
                            (format nil "#+TODO: TODO | DONE~%* TODO [#B] New [1/2] :a:b:~%- [ ] x~%- [X] y~%"))
                      (list (list :command "org.checkbox.toggle" :operation :checkbox-toggle :item 1)
                            (format nil "#+TODO: TODO | DONE~%* TODO [#B] Task [2/2] :a:b:~%- [X] x~%- [X] y~%"))))
         (put "edit.org" *heading-edit-source*)
         (let ((fields (%heading-edit-flow-fields
                        (append (list :match "/TODO") (first case)))))
           (expect (field fields "changed") :to-be t)
           (expect (text "edit.org") :to-equal (second case)))))))
  (it "previews a heading edit without changing the file"
    (call-with-edit-workspace
     (lambda ()
       (put "edit.org" *heading-edit-source*)
       (let ((fields (%heading-edit-flow-fields
                      (list :match "/TODO" :command "org.priority.set"
                            :operation :priority-set :priority "A" :dry-run t))))
         (expect (field fields "dry_run") :to-be t)
         (expect (field fields "changed") :to-be t)
         (expect (text "edit.org") :to-equal *heading-edit-source*)))))
  (it "reports selection and argument failures and keeps the file"
    (call-with-edit-workspace
     (lambda ()
       (put "edit.org" *heading-edit-source*)
       (expect (%heading-edit-flow-error
                (list :match "/DONE" :command "org.tags.add"
                      :operation :tags-add :tags "c"))
               :to-equal "selection.no-match")
       (expect (%heading-edit-flow-error
                (list :match "/TODO" :command "org.priority.set"
                      :operation :priority-set :priority "Z"))
               :to-equal "argument.invalid")
       (expect (text "edit.org") :to-equal *heading-edit-source*)))))

(defun %heading-edit-invocation (arguments options)
  "A parsed invocation of an org heading edit command on edit.org."
  (cl-cli::%make-invocation
   :app nil :command nil :command-path nil :action :dispatch :argv0 "aitools"
   :raw-argv (append (list "org") arguments (list "edit.org"))
   :global-options nil
   :command-options (append options
                            (list :match "/TODO" :heading-path nil
                                  :at "2026-10-19T09:00:00" :tz "UTC"
                                  :dst-policy "compatible" :root *root*
                                  :lock-timeout nil :tx nil :dry-run nil))
   :positionals (list :path "edit.org")
   :option-sources nil :stdout nil :stderr nil))

(defun %heading-edit-command (command name operation)
  (aitools.org.presentation::%heading-edit-command
   (make-org-write-flow-boundaries) command name operation))

(defun %heading-edit-option-summary (command)
  "(key type min required-p) of each option the command declares beyond selection, time, and write options."
  (loop for option in (cl-cli:command-options command)
        for key = (cl-cli::option-key option)
        unless (member key '(:match :heading-path :at :tz :dst-policy :dry-run :tx))
          collect (list key (cl-cli::option-value-type option)
                        (cl-cli::option-value-min option)
                        (cl-cli::option-required-p option))))

(describe "Org heading edit commands"
  (it "declares the value option of each operation"
    (flet ((summary (command name operation)
             (%heading-edit-option-summary
              (%heading-edit-command command name operation))))
      (expect (summary "org.priority.set" "set" :priority-set)
              :to-equal '((:priority nil nil t)))
      (expect (summary "org.priority.delete" "delete" :priority-delete)
              :to-equal nil)
      (dolist (case (list (list "org.tags.add" "add" :tags-add)
                          (list "org.tags.remove" "remove" :tags-remove)
                          (list "org.tags.replace" "replace" :tags-replace)))
        (expect (summary (first case) (second case) (third case))
                :to-equal '((:tags nil nil t))))
      (expect (summary "org.heading.rename" "rename" :heading-rename)
              :to-equal '((:title nil nil t)))
      (expect (summary "org.checkbox.toggle" "toggle" :checkbox-toggle)
              :to-equal '((:item :integer 1 t))))
    (let ((command (%heading-edit-command "org.heading.rename" "rename" :heading-rename)))
      (expect (cl-cli:command-name command) :to-equal "rename")
      (expect (length (cl-cli:command-positionals command)) :to-be 1)))
  (it "runs each operation through the handler and writes the file"
    (call-with-edit-workspace
     (lambda ()
       (dolist (case (list
                      (list "org.priority.set" "set" :priority-set (list :priority "A")
                            (format nil "#+TODO: TODO | DONE~%* TODO [#A] Task [1/2] :a:b:~%- [ ] x~%- [X] y~%"))
                      (list "org.priority.delete" "delete" :priority-delete nil
                            (format nil "#+TODO: TODO | DONE~%* TODO Task [1/2] :a:b:~%- [ ] x~%- [X] y~%"))
                      (list "org.tags.add" "add" :tags-add (list :tags "c")
                            (format nil "#+TODO: TODO | DONE~%* TODO [#B] Task [1/2] :a:b:c:~%- [ ] x~%- [X] y~%"))
                      (list "org.tags.remove" "remove" :tags-remove (list :tags "a")
                            (format nil "#+TODO: TODO | DONE~%* TODO [#B] Task [1/2] :b:~%- [ ] x~%- [X] y~%"))
                      (list "org.tags.replace" "replace" :tags-replace (list :tags "z")
                            (format nil "#+TODO: TODO | DONE~%* TODO [#B] Task [1/2] :z:~%- [ ] x~%- [X] y~%"))
                      (list "org.heading.rename" "rename" :heading-rename (list :title "New")
                            (format nil "#+TODO: TODO | DONE~%* TODO [#B] New [1/2] :a:b:~%- [ ] x~%- [X] y~%"))
                      (list "org.checkbox.toggle" "toggle" :checkbox-toggle (list :item 1)
                            (format nil "#+TODO: TODO | DONE~%* TODO [#B] Task [2/2] :a:b:~%- [X] x~%- [X] y~%"))))
         (destructuring-bind (command name operation options expected) case
           (put "edit.org" *heading-edit-source*)
           (let* ((handler (cl-cli:command-handler
                            (%heading-edit-command command name operation)))
                  (result (funcall handler
                                   (%heading-edit-invocation (list name) options))))
             (expect (aitools.protocol.application:command-result-kind result) :to-be :ok)
             (expect (field (aitools.protocol.application:command-result-fields result)
                            "changed")
                     :to-be t)
             (expect (text "edit.org") :to-equal expected)))))))
  (it "forwards a dry run and selection failures from the handler"
    (call-with-edit-workspace
     (lambda ()
       (put "edit.org" *heading-edit-source*)
       (let* ((handler (cl-cli:command-handler
                        (%heading-edit-command "org.priority.set" "set" :priority-set)))
              (preview (funcall handler
                                (%heading-edit-invocation
                                 (list "set" "--dry-run")
                                 (list :priority "A" :dry-run t))))
              (failed (funcall handler
                               (%heading-edit-invocation
                                (list "set")
                                (list :priority "A" :match "/DONE")))))
         (expect (aitools.protocol.application:command-result-kind preview) :to-be :ok)
         (expect (field (aitools.protocol.application:command-result-fields preview)
                        "dry_run")
                 :to-be t)
         (expect (aitools.protocol.application:command-result-kind failed) :to-be :error)
         (expect (getf (aitools.protocol.application:command-result-fields failed) :code)
                 :to-equal "selection.no-match")
         (expect (text "edit.org") :to-equal *heading-edit-source*))))))
