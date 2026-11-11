(in-package #:aitools.edit.test)
(describe "Org planning adapter selection and TODO transitions"
          (it "maps query syntax and adapter failures"
              (let* ((document
                      (%org-coverage-document
                       (format nil "#+TAGS: { First : Second } { Second : First }~%* Entry :x:~%")))
                     (engine (aitools.org.domain:org-document-engine-value document))
                     (options (list :match "First" :at "2026-10-19T09:00:00"
                                    :tz "UTC" :dst-policy "compatible")))
                (multiple-value-bind (heading at context failure)
                    (aitools.org.infrastructure::%org-selected-heading engine options)
                  (declare (ignore heading at context))
                  (expect (aitools.org.domain:org-failure-kind failure)
                          :to-be :query-syntax)
                  (expect (aitools.org.domain:org-failure-line failure) :to-be 1)
                  (expect (aitools.org.domain:org-failure-column failure) :to-be 1)
                  (expect (aitools.org.domain:org-failure-offset failure) :to-be nil))
                (cl-weave:with-mocked-functions
                 (((symbol-function (quote org-kit:query-match-at))
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error
                      (make-condition (quote org-kit:org-kit-error)
                                      :message "selection failed")))))
                 (multiple-value-bind (heading at context failure)
                     (aitools.org.infrastructure::%org-selected-heading engine options)
                   (declare (ignore heading at context))
                   (expect (aitools.org.domain:org-failure-kind failure)
                           :to-be :internal)))))
          (it "validates heading matches, explicit time, ambiguity, and limits"
              (let* ((document
                      (%org-coverage-document
                       (format nil "#+TODO: TODO | DONE~%* TODO Task~%")))
                     (engine (aitools.org.domain:org-document-engine-value document))
                     (options (list :match "/TODO" :at "2026-10-19T09:00:00"
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
                       engine (list :match match :at "2026-10-19T09:00:00"
                                    :tz "UTC" :dst-policy "compatible"))
                    (declare (ignore heading at context))
                    (expect (aitools.org.domain:org-failure-kind failure)
                            :to-be :argument)))
                (dolist (invalid-options
                         (list (list :match "/TODO" :at "bad" :tz "UTC"
                                     :dst-policy "compatible")
                               (list :match "/TODO" :at "2026-10-19T09:00:00"
                                     :tz "No/Such_Zone" :dst-policy "compatible")
                               (list :match "/TODO" :at "2026-10-19T09:00:00"
                                     :tz "UTC" :dst-policy "unknown")))
                  (multiple-value-bind (heading at context failure)
                      (aitools.org.infrastructure::%org-selected-heading
                       engine invalid-options)
                    (declare (ignore heading at context))
                    (expect (aitools.org.domain:org-failure-kind failure)
                            :to-be :argument)))
                (multiple-value-bind (heading at context failure)
                    (aitools.org.infrastructure::%org-selected-heading
                     engine (list :match "/TODO" :tz "UTC"
                                  :dst-policy "compatible"))
                  (declare (ignore heading at context))
                  (expect (aitools.org.domain:org-failure-kind failure)
                          :to-be :argument))
                (multiple-value-bind (heading at context failure)
                    (aitools.org.infrastructure::%org-selected-heading
                     engine (list :match "/DONE" :at "2026-10-19T09:00:00"
                                  :tz "UTC" :dst-policy "compatible"))
                  (declare (ignore heading at context))
                  (expect (aitools.org.domain:org-failure-kind failure)
                          :to-be :selection-no-match)))
              (let* ((document
                      (%org-coverage-document
                       (format nil "#+TODO: TODO | DONE~%* TODO First~%* TODO Second~%")))
                     (engine (aitools.org.domain:org-document-engine-value document)))
                (multiple-value-bind (heading at context failure)
                    (aitools.org.infrastructure::%org-selected-heading
                     engine (list :match "/TODO" :at "2026-10-19T09:00:00"
                                  :tz "UTC" :dst-policy "compatible"))
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
          (it "uses fresh documents for successful, invalid, and unmatched TODO edits"
              (let ((source (format nil "#+TODO: TODO(t!) | DONE(d@)~%* TODO Task~%SCHEDULED: <2026-10-20>~%"))
                    (options (list :match "/TODO" :at "2026-10-19T09:00:00"
                                   :tz "UTC" :dst-policy "compatible")))
                (labels ((fresh-engine ()
                                       (aitools.org.domain:org-document-engine-value
                                        (%org-coverage-document source))))
                  (let ((engine (fresh-engine)))
                    (multiple-value-bind (updated fields failure)
                        (aitools.org.infrastructure::%org-todo-edit
                         engine (append options (list :state "DONE" :note "closing note")))
                      (expect failure :to-be nil)
                      (expect (cdr (assoc "previous_state" fields :test (function string=)))
                              :to-equal "TODO")
                      (expect (cdr (assoc "new_state" fields :test (function string=)))
                              :to-equal "DONE")
                      (expect (search "* DONE Task" (org-kit:document-text updated))
                              :to-be-truthy)
                      (expect (search "SCHEDULED: <2026-10-20>"
                                      (org-kit:document-text updated))
                              :to-be-truthy)
                      (expect (search "closing note" (org-kit:document-text updated))
                              :to-be-truthy)))
                  (let ((engine (fresh-engine)))
                    (multiple-value-bind (updated fields failure)
                        (aitools.org.infrastructure::%org-todo-edit
                         engine (append options (list :state "UNKNOWN")))
                      (expect updated :to-be nil)
                      (expect fields :to-be nil)
                      (expect (aitools.org.domain:org-failure-kind failure)
                              :to-be :argument)))
                  (let ((engine (fresh-engine)))
                    (multiple-value-bind (updated fields failure)
                        (aitools.org.infrastructure::%org-todo-edit
                         engine (list :match "/ABSENT" :state "DONE"
                                      :at "2026-10-19T09:00:00"
                                      :tz "UTC" :dst-policy "compatible"))
                      (expect updated :to-be nil)
                      (expect fields :to-be nil)
                      (expect (aitools.org.domain:org-failure-kind failure)
                              :to-be :selection-no-match))))))
          (it "maps blocked and resource-limited TODO edits"
              (let* ((document
                      (%org-coverage-document
                       (format nil "#+TODO: TODO WAIT | DONE~%* TODO Parent~%** WAIT Child~%")))
                     (engine (aitools.org.domain:org-document-engine-value document))
                     (options (list :match "/TODO" :state "DONE"
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
                       (options (list :match "/TODO" :state "DONE"
                                      :at "2026-10-19T09:00:00"
                                      :tz "UTC" :dst-policy "compatible")))
                  (multiple-value-bind (updated fields failure)
                      (aitools.org.infrastructure::%org-todo-edit engine options)
                    (expect updated :to-be nil)
                    (expect fields :to-be nil)
                    (expect (aitools.org.domain:org-failure-kind failure)
                            :to-be :too-large))))))
(describe "Org TODO adapter boundary failures"
  (it "maps transition time and generic failures"
    (let* ((document
             (%org-coverage-document
              (format nil "#+TODO: TODO | DONE~%* TODO Task~%")))
           (engine (aitools.org.domain:org-document-engine-value document))
           (options (list :match "/TODO" :state "DONE"
                          :at "2026-10-19T09:00:00"
                          :tz "UTC" :dst-policy "compatible"))
           (condition-to-signal nil))
      (cl-weave:with-mocked-functions
          (((symbol-function 'org-kit:transition-heading-todo)
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (error condition-to-signal))))
        (dolist (case
                  (list (list (make-condition 'org-kit:org-time-error
                                              :message "transition time")
                              :argument)
                        (list (make-condition 'org-kit:org-kit-error
                                              :message "transition failed")
                              :internal)))
          (setf condition-to-signal (first case))
          (multiple-value-bind (updated fields failure)
              (aitools.org.infrastructure::%org-todo-edit engine options)
            (expect updated :to-be nil)
            (expect fields :to-be nil)
            (expect (aitools.org.domain:org-failure-kind failure)
                    :to-be (second case))))))))


