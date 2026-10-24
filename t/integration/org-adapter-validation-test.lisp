(in-package #:aitools.edit.test)
(describe "Org adapter validation and error translation"
  (it "maps kit table, edit, and clock failure reasons"
    (labels ((table-failure (reason operation)
               (aitools.org.infrastructure::%table-error-failure
                (make-condition 'org-kit:org-table-error
                                :message "table failure"
                                :reason reason)
                operation))
             (table-kind (reason operation)
               (aitools.org.domain:org-failure-kind
                (table-failure reason operation)))
             (edit-kind (reason)
               (aitools.org.domain:org-failure-kind
                (aitools.org.infrastructure::%table-edit-error-failure
                 (make-condition 'org-kit:org-edit-error
                                 :message "edit failure"
                                 :reason reason))))
             (clock-kind (reason)
               (aitools.org.domain:org-failure-kind
                (aitools.org.infrastructure::%org-clock-error-failure
                 (make-condition 'org-kit:org-clock-error
                                 :message "clock failure"
                                 :reason reason))))
             (planning-kind (reason)
               (aitools.org.domain:org-failure-kind
                (aitools.org.infrastructure::%org-edit-condition-failure
                 (make-condition 'org-kit:org-edit-error
                                 :message "planning failure"
                                 :reason reason)))))
      (expect (table-kind :unsupported-table-type :table-align) :to-be :unsupported)
      (dolist (reason '(:unsupported-mode :unknown-function))
        (expect (table-kind reason :table-recalculate) :to-be :unsupported))
      (dolist (reason '(:invalid-number :invalid-value :invalid-index
                        :invalid-range :ambiguous-range))
        (expect (table-kind reason :table-sort) :to-be :argument)
        (expect (table-kind reason :table-recalculate) :to-be :syntax))
      (dolist (reason '(:division-by-zero :non-finite-result
                        :invalid-function-result :empty-range
                        :unknown-column-name :ambiguous-column-name
                        :invalid-reference :duplicate-target :unknown-property
                        :unknown-constant :unknown-remote-table :cyclic-formula
                        :cyclic-remote-reference))
        (expect (table-kind reason :table-recalculate) :to-be :syntax))
      (expect (table-kind :unexpected :table-recalculate) :to-be :internal)
      (dolist (reason '(:invalid-value :target-not-found :wrong-node-type
                        :ambiguous-target))
        (expect (edit-kind reason) :to-be :argument))
      (expect (edit-kind :uneditable-structure) :to-be :unsupported)
      (expect (edit-kind :unexpected) :to-be :internal)
      (expect (clock-kind :clock-not-running) :to-be :selection-no-match)
      (expect (clock-kind :multiple-running-clocks) :to-be :selection-ambiguous)
      (dolist (reason '(:clock-already-running :invalid-range :unsupported-option))
        (expect (clock-kind reason) :to-be :argument))
      (expect (clock-kind :unexpected) :to-be :internal)
      (dolist (reason '(:ambiguous-target :target-not-found :invalid-value :blocked))
        (expect (planning-kind reason)
                :to-be (case reason
                         (:ambiguous-target :selection-ambiguous)
                         (:target-not-found :not-found)
                         (:invalid-value :argument)
                         (:blocked :syntax))))
      (expect (planning-kind :unexpected) :to-be :internal)))
  (it "validates Org dates, local instants, and clock report ranges"
    (dolist (value '("bad" "2026-10-19T09:00:00" "2026-02-30"))
      (multiple-value-bind (date failure)
          (aitools.org.infrastructure::%date-value value)
        (expect date :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)))
    (dolist (input '("bad" "2026-10-19T09:00:30"
                     "2026-02-30T09:00:00" "2026-10-19T09:00:00+00:00"))
      (multiple-value-bind (time failure)
          (aitools.org.infrastructure::%explicit-range-time input "--from")
        (expect time :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)))
    (dolist (arguments '((nil "UTC" "compatible")
                         ("bad" "UTC" "compatible")
                         ("2026-02-30T09:00:00" "UTC" "compatible")
                         ("2026-10-19T09:00:00" "No/Such_Zone" "compatible")
                         ("2026-10-19T09:00:00" "UTC" "unknown")))
      (multiple-value-bind (value context reference failure)
          (apply #'aitools.org.infrastructure::%explicit-time-values arguments)
        (declare (ignore context reference))
        (expect value :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)))
    (multiple-value-bind (range failure)
        (aitools.org.infrastructure::%time-range-values
         "2026-10-19T10:00:00" "2026-10-19T09:00:00")
      (expect range :to-be nil)
      (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument))
    (dolist (arguments '(("bad" "2026-10-19T09:00:00")
                         ("2026-10-19T09:00:00" "bad")))
      (multiple-value-bind (range failure)
          (apply #'aitools.org.infrastructure::%time-range-values arguments)
        (expect range :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument))))
  (it "validates table sort options and maps every public failure code"
    (multiple-value-bind (column type order start end failure)
        (aitools.org.infrastructure::%table-sort-options
         '(:column 2 :type :numeric :order :descending :start 1 :end 3))
      (expect (list column type order start end) :to-equal
              '(2 :numeric :descending 1 3))
      (expect failure :to-be nil))
    (dolist (options '((:column 0)
                       (:column 1 :type "decimal")
                       (:column 1 :order :sideways)
                       (:column 1 :start 1)
                       (:column 1 :start 0 :end 2)))
      (multiple-value-bind (column type order start end failure)
          (aitools.org.infrastructure::%table-sort-options options)
        (declare (ignore column type order start end))
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument)))
    (dolist (entry '((:invalid-utf8 "input.not-utf8")
                     (:syntax "input.syntax-error")
                     (:query-syntax "input.syntax-error")
                     (:unsupported "input.unsupported-format")
                     (:internal-link "input.unsupported-format")
                     (:too-large "refusal.too-large")
                     (:argument "argument.invalid")
                     (:invalid-time "argument.invalid")
                     (:selection-no-match "selection.no-match")
                     (:selection-ambiguous "selection.ambiguous")
                     (:not-found "input.not-found")
                     (:io "environment.io")
                     (:unexpected "internal.unexpected")))
      (expect (aitools.org.application::%failure-code
               (aitools.org.domain:make-org-failure
                :kind (first entry) :message "failure"))
              :to-equal (second entry)))
    (expect (length
             (aitools.org.application::%failure-diagnostics
              (aitools.org.domain:make-org-failure
               :kind :syntax :message "source failure" :line 2 :column 3)))
            :to-be 1)
    (expect (length
             (aitools.org.application::%failure-diagnostics
              (aitools.org.domain:make-org-failure
               :kind :syntax :message "source failure" :offset 4)))
            :to-be 1)
    (expect (aitools.org.application::%failure-diagnostics
             (aitools.org.domain:make-org-failure
              :kind :syntax :message "source failure"))
            :to-be nil)))
