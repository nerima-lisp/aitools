(in-package #:aitools.edit.test)
(describe "Org property and planning adapter edits"
  (it "updates, removes, and validates properties"
    (let* ((document
             (%org-coverage-document
              (format nil "#+TODO: TODO | DONE~%* TODO Task~%:PROPERTIES:~%:OWNER: Ada~%:END:~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (options (list :match "/TODO" :at "2026-10-19T09:00:00"
                          :tz "UTC" :dst-policy "compatible")))
      (expect
       (nth-value 2
        (aitools.org.infrastructure::%org-property-edit
         engine (append options (list :name "STATUS" :value "ready")) nil))
       :to-be nil)
      (expect
       (nth-value 2
        (aitools.org.infrastructure::%org-property-edit
         engine (append options (list :name "OWNER")) t))
       :to-be nil)
      (expect
       (nth-value 0
        (aitools.org.infrastructure::%org-property-edit
         engine (append options (list :name "MISSING")) t))
       :to-be engine)
      (expect
       (aitools.org.domain:org-failure-kind
        (nth-value 2
         (aitools.org.infrastructure::%org-property-edit
          engine (append options (list :name "" :value "ready")) nil)))
       :to-be :argument))
    (let ((aitools.org.infrastructure::*org-kit-limits*
            (org-kit:make-limits :max-output-length 1)))
      (let* ((document (%org-coverage-document
                        (format nil "#+TODO: TODO | DONE~%* TODO Task~%")))
             (engine (aitools.org.domain:org-document-engine-value document))
             (options (list :match "/TODO" :name "OWNER" :value "Ada"
                            :at "2026-10-19T09:00:00" :tz "UTC"
                            :dst-policy "compatible")))
        (expect
         (aitools.org.domain:org-failure-kind
          (nth-value 2
           (aitools.org.infrastructure::%org-property-edit
            engine options nil)))
         :to-be :too-large))))
  (it "applies every planning kind and maps edit failures"
    (let* ((document
             (%org-coverage-document
              (format nil "#+TODO: TODO | DONE~%* TODO Release~%   Description~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (options (list :match "/TODO" :at "2026-10-19T09:00:00"
                          :tz "UTC" :dst-policy "compatible")))
      (dolist (case (list (list "scheduled" "<2026-10-20>")
                          (list "deadline" "<2026-10-21>")
                          (list "closed" "[2026-10-22]")))
        (expect
         (nth-value 2
          (aitools.org.infrastructure::%org-planning-edit
           engine (append options (list :kind (first case)
                                        :timestamp (second case))) nil))
         :to-be nil))
      (expect
       (aitools.org.domain:org-failure-kind
        (nth-value 2
         (aitools.org.infrastructure::%org-planning-edit
          engine (append options (list :kind "invalid")) nil)))
       :to-be :argument)
      (expect
       (nth-value 0
        (aitools.org.infrastructure::%org-planning-edit
         engine (append options (list :kind "deadline")) t))
       :to-be engine))
    (let* ((document
             (%org-coverage-document
              (format nil "#+TODO: TODO | DONE~%* TODO Release~%   SCHEDULED: <2026-10-20> SCHEDULED: <2026-10-22>~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (options (list :match "/TODO" :kind "scheduled"
                          :timestamp "<2026-10-21>"
                          :at "2026-10-19T09:00:00" :tz "UTC"
                          :dst-policy "compatible")))
      (expect
       (aitools.org.domain:org-failure-kind
        (nth-value 2
         (aitools.org.infrastructure::%org-planning-edit engine options nil)))
       :to-be :selection-ambiguous))
    (let ((aitools.org.infrastructure::*org-kit-limits*
            (org-kit:make-limits :max-output-length 1)))
      (let* ((document (%org-coverage-document
                        (format nil "#+TODO: TODO | DONE~%* TODO Release~%")))
             (engine (aitools.org.domain:org-document-engine-value document))
             (options (list :match "/TODO" :kind "scheduled"
                            :timestamp "<2026-10-21>"
                            :at "2026-10-19T09:00:00" :tz "UTC"
                            :dst-policy "compatible")))
        (expect
         (aitools.org.domain:org-failure-kind
          (nth-value 2
           (aitools.org.infrastructure::%org-planning-edit
            engine options nil)))
         :to-be :too-large)))))
(describe "Org planning adapter selection and TODO transitions"
  (it "validates selected headings and maps query failures"
    (let* ((document
             (%org-coverage-document
              (format nil "#+TODO: TODO | DONE~%* TODO Task~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (options
             (list :match "/TODO" :at "2026-10-19T09:00:00"
                   :tz "UTC" :dst-policy "compatible")))
      (multiple-value-bind (heading at context failure)
          (aitools.org.infrastructure::%org-selected-heading engine options)
        (expect failure :to-be nil)
        (expect (org-kit:node-property heading :raw-value) :to-equal "Task")
        (expect at :to-be-truthy)
        (expect context :to-be-truthy))
      (dolist (match (list nil "" 7))
        (multiple-value-bind (heading at context failure)
            (aitools.org.infrastructure::%org-selected-heading
             engine (append options (list :match match)))
          (declare (ignore heading at context))
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :argument)))
      (multiple-value-bind (heading at context failure)
          (aitools.org.infrastructure::%org-selected-heading
           engine (append options (list :at "bad")))
        (declare (ignore heading at context))
        (expect (aitools.org.domain:org-failure-kind failure)
                :to-be :argument))
      (multiple-value-bind (heading at context failure)
          (aitools.org.infrastructure::%org-selected-heading
           engine (append options (list :tz "No/Such_Zone")))
        (declare (ignore heading at context))
        (expect (aitools.org.domain:org-failure-kind failure)
                :to-be :argument))
      (multiple-value-bind (heading at context failure)
          (aitools.org.infrastructure::%org-selected-heading
           engine (append options (list :match "/DONE")))
        (declare (ignore heading at context))
        (expect (aitools.org.domain:org-failure-kind failure)
                :to-be :selection-no-match))
      (multiple-value-bind (heading at context failure)
          (aitools.org.infrastructure::%org-selected-heading
           engine (append options (list :match "{")))
        (declare (ignore heading at context))
        (expect (aitools.org.domain:org-failure-kind failure)
                :to-be :query-syntax))))
  (it "reports ambiguous and resource-limited selections"
    (let* ((document
             (%org-coverage-document
              (format nil "#+TODO: TODO | DONE~%* TODO First~%* TODO Second~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (options
             (list :match "/TODO" :at "2026-10-19T09:00:00"
                   :tz "UTC" :dst-policy "compatible")))
      (multiple-value-bind (heading at context failure)
          (aitools.org.infrastructure::%org-selected-heading engine options)
        (declare (ignore heading at context))
        (expect (aitools.org.domain:org-failure-kind failure)
                :to-be :selection-ambiguous)))
    (let ((aitools.org.infrastructure::*org-kit-limits*
            (org-kit:make-limits :max-input-length 3)))
      (let* ((document (%org-coverage-document "* A"))
             (engine (aitools.org.domain:org-document-engine-value document)))
        (multiple-value-bind (heading at context failure)
            (aitools.org.infrastructure::%org-selected-heading
             engine
             (list :match "/TODO" :at "2026-10-19T09:00:00"
                   :tz "UTC" :dst-policy "compatible"))
          (declare (ignore heading at context))
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :too-large)))))
  (it "applies TODO transitions and maps edit failures"
    (let* ((document
             (%org-coverage-document
              (format nil
                      "#+TODO: TODO | DONE~%* TODO Task~%SCHEDULED: <2026-10-20>~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (options
             (list :match "/TODO" :at "2026-10-19T09:00:00"
                   :tz "UTC" :dst-policy "compatible")))
      (multiple-value-bind (updated fields failure)
          (aitools.org.infrastructure::%org-todo-edit
           engine (append options (list :state "DONE")))
        (expect failure :to-be nil)
        (expect (cdr (assoc "previous_state" fields :test (function string=)))
                :to-equal "TODO")
        (expect (cdr (assoc "new_state" fields :test (function string=)))
                :to-equal "DONE")
        (expect (search "* DONE Task" (org-kit:document-text updated))
                :to-be-truthy)
        (expect (search "SCHEDULED: <2026-10-20>"
                        (org-kit:document-text updated))
                :to-be-truthy))
      (multiple-value-bind (updated fields failure)
          (aitools.org.infrastructure::%org-todo-edit
           engine (append options (list :state "UNKNOWN")))
        (expect updated :to-be nil)
        (expect fields :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure)
                :to-be :argument))
      (multiple-value-bind (updated fields failure)
          (aitools.org.infrastructure::%org-todo-edit
           engine (append options (list :match "/ABSENT" :state "DONE")))
        (expect updated :to-be nil)
        (expect fields :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure)
                :to-be :selection-no-match)))
    (let* ((document
             (%org-coverage-document
              (format nil "#+TODO: TODO WAIT | DONE~%* TODO Parent~%** WAIT Child~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (options
             (list :match "/TODO" :state "DONE"
                   :at "2026-10-19T09:00:00"
                   :tz "UTC" :dst-policy "compatible")))
      (multiple-value-bind (updated fields failure)
          (aitools.org.infrastructure::%org-todo-edit engine options)
        (expect updated :to-be nil)
        (expect fields :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure)
                :to-be :syntax)))
    (let ((aitools.org.infrastructure::*org-kit-limits*
            (org-kit:make-limits :max-output-length 1)))
      (let* ((document
               (%org-coverage-document
                (format nil "#+TODO: TODO | DONE~%* TODO Task~%")))
             (engine (aitools.org.domain:org-document-engine-value document))
             (options
               (list :match "/TODO" :state "DONE"
                     :at "2026-10-19T09:00:00"
                     :tz "UTC" :dst-policy "compatible")))
        (multiple-value-bind (updated fields failure)
            (aitools.org.infrastructure::%org-todo-edit engine options)
          (expect updated :to-be nil)
          (expect fields :to-be nil)
          (expect (aitools.org.domain:org-failure-kind failure)
                  :to-be :too-large))))))
