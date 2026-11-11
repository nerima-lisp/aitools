(in-package #:aitools.edit.test)
(describe "Org adapter flow edge cases"
  (it "returns agenda validation, habit, and resource failures"
    (call-with-edit-workspace (lambda () (let ((boundaries (make-org-write-flow-boundaries)))
        (put "agenda.org" (format nil "#+TODO: TODO | DONE~%* TODO Work~%SCHEDULED: <2026-10-19 Mon>~%"))
        (expect (org-flow-error-code #'aitools.org.application:org-agenda/k boundaries
                                     :path "agenda.org" :root *root*
                                     :from "2026-10-20" :through "2026-10-19"
                                     :at "2026-10-19T09:00:00" :tz "UTC")
                :to-equal "argument.invalid")
        (put "habit.org"
             (format nil "#+TODO: TODO | DONE~%* TODO Habit~%SCHEDULED: <2026-10-19 Mon>~%:PROPERTIES:~%:STYLE: habit~%:END:~%"))
        (expect (org-flow-error-code #'aitools.org.application:org-agenda/k boundaries
                                     :path "habit.org" :root *root*
                                     :from "2026-10-19" :through "2026-10-19"
                                     :at "2026-10-19T09:00:00" :tz "UTC")
                :to-equal "input.syntax-error")
        (let ((aitools.org.infrastructure::*org-kit-limits*
                (org-kit:make-limits :max-agenda-entries 0)))
          (let ((limited-boundaries (make-org-write-flow-boundaries)))
            (expect (org-flow-error-code #'aitools.org.application:org-agenda/k
                                         limited-boundaries :path "agenda.org" :root *root*
                                         :from "2026-10-19" :through "2026-10-19"
                                         :at "2026-10-19T09:00:00" :tz "UTC")
                    :to-equal "refusal.too-large")))))))
  (it "reports clock precondition and report-range errors"
    (call-with-edit-workspace (lambda () (let ((boundaries (make-org-write-flow-boundaries)))
        (put "report.org" (format nil "#+TODO: TODO | DONE~%* TODO Work~%SCHEDULED: <2026-10-19 Mon>~%"))
        (expect (org-flow-error-code #'aitools.org.application:org-clock-report/k boundaries
                                     :path "report.org" :root *root* :match ""
                                     :from "2026-10-19T09:00:00"
                                     :through "2026-10-19T10:00:00"
                                     :at "2026-10-19T09:00:00" :tz "UTC")
                :to-equal "argument.invalid")
        (expect (org-flow-error-code #'aitools.org.application:org-clock-report/k boundaries
                                     :path "report.org" :root *root* :match "/TODO"
                                     :from "2026-10-19T10:00:00"
                                     :through "2026-10-19T09:00:00"
                                     :at "2026-10-19T09:00:00" :tz "UTC")
                :to-equal "argument.invalid")))))
  (it "serializes agenda entries without a TODO state as JSON null"
    (call-with-edit-workspace (lambda () (let ((boundaries (make-org-write-flow-boundaries)))
        (put "agenda-no-todo.org"
             (format nil "* Reminder~%SCHEDULED: <2026-10-19 Mon>~%"))
        (let* ((fields (org-flow-ok #'aitools.org.application:org-agenda/k boundaries
                                    :path "agenda-no-todo.org" :root *root*
                                    :from "2026-10-19" :through "2026-10-19"
                                    :at "2026-10-19T09:00:00" :tz "UTC"))
               (entry (first (coerce (field fields "entries") 'list)))
               (serialized (json-field entry "todo")))
          (expect (field fields "count") :to-be 1)
          (expect serialized :to-be json-kit:+json-null+))))))
  (it "reports when the application context omits agenda or clock report capabilities"
    (call-with-edit-workspace (lambda () (let* ((production (make-org-write-flow-boundaries))
             (boundaries (cl-boundary-kit:make-boundary-context
                     :org-workspace-host
                     (cl-boundary-kit:boundary-context-require production :org-workspace-host)
                     :org-text-source
                     (cl-boundary-kit:boundary-context-require production :org-text-source)
                     :org-max-input-bytes
                     (cl-boundary-kit:boundary-context-require production :org-max-input-bytes)
                     :org-parse
                     (cl-boundary-kit:boundary-context-require production :org-parse)
                     :org-outline
                     (cl-boundary-kit:boundary-context-require production :org-outline)
                     :org-query
                     (cl-boundary-kit:boundary-context-require production :org-query)
                     :org-export
                     (cl-boundary-kit:boundary-context-require production :org-export)
                     :org-open-store
                     (cl-boundary-kit:boundary-context-require production :org-open-store)
                     :org-write
                     (cl-boundary-kit:boundary-context-require production :org-write)
                     :org-edit
                     (cl-boundary-kit:boundary-context-require production :org-edit)
                     :org-agenda nil
                     :org-clock-report nil)))
        (put "agenda.org" (format nil "* Task~%"))
        (expect (org-flow-error-code #'aitools.org.application:org-agenda/k boundaries
                                     :path "agenda.org" :root *root*
                                     :from "2026-10-19" :through "2026-10-19"
                                     :at "2026-10-19T09:00:00" :tz "UTC")
                :to-equal "internal.unexpected")
        (expect (org-flow-error-code #'aitools.org.application:org-clock-report/k boundaries
                                     :path "agenda.org" :root *root* :match "/!"
                                     :from "2026-10-19T09:00:00"
                                     :through "2026-10-19T10:00:00"
                                     :at "2026-10-19T09:00:00" :tz "UTC")
                :to-equal "internal.unexpected")))))
  (it "validates active and inactive planning timestamp forms and ranges"
    (dolist (case '(("scheduled" "<2026-10-19>" t)
                    ("scheduled" "<2026-10-19 09:00-10:00>" t)
                    ("closed" "[2026-10-19 09:00-10:00]" t)
                    ("scheduled" "<2026-10-19 Mon>" nil)
                    ("scheduled" "[2026-10-19 Mon>" nil)
                    ("closed" "<2026-10-19 Mon>" nil)
                    ("invalid" "<2026-10-19 Mon>" nil)
                    ("scheduled" "<2026-10-19 Mon 10:00-09:00>" nil)
                    ("scheduled" "<2026-10-19 Mon +1d +2w>" nil)
                    ("scheduled" "<2026-10-19 Mon>\n<2026-10-20 Tue>" nil)
                    ("scheduled" "<2026-10-19 Mon><2026-10-20 Tue>" nil)
                    ("scheduled" "<2026-02-30>" nil)))
      (multiple-value-bind (view failure)
          (aitools.org.infrastructure::validate-planning-timestamp
           (first case) (second case))
        (if (third case)
            (progn (expect view :to-be-truthy) (expect failure :to-be nil))
            (progn (expect view :to-be nil)
                   (expect (aitools.org.domain:org-failure-kind failure)
                           :to-be :argument))))))
)

(describe "Org adapter uncovered validation paths"
  (it "rejects invalid agenda end dates, local context, and repeat modes"
    (call-with-edit-workspace (lambda () (let ((boundaries (make-org-write-flow-boundaries)))
        (put "agenda-options.org" (format nil "* TODO Work~%"))
        (expect
         (org-flow-error-code (function aitools.org.application:org-agenda/k) boundaries
                              :path "agenda-options.org" :root *root*
                              :from "2026-10-19" :through "2026-02-30"
                              :at "2026-10-19T09:00:00" :tz "UTC")
         :to-equal "argument.invalid")
        (expect
         (org-flow-error-code (function aitools.org.application:org-agenda/k) boundaries
                              :path "agenda-options.org" :root *root*
                              :from "2026-10-19" :through "2026-10-19"
                              :at "2026-10-19T09:00:00+00:00" :tz "UTC")
         :to-equal "argument.invalid")
        (expect
         (org-flow-error-code (function aitools.org.application:org-agenda/k) boundaries
                              :path "agenda-options.org" :root *root*
                              :from "2026-10-19" :through "2026-10-19"
                              :future-repeats "sometimes"
                              :at "2026-10-19T09:00:00" :tz "UTC")
         :to-equal "argument.invalid")))))
  (it "rejects invalid clock report endpoints and clocking out without a clock"
    (call-with-edit-workspace (lambda () (let ((boundaries (make-org-write-flow-boundaries)))
        (put "clock-options.org" (format nil "* TODO Work~%"))
        (expect
         (org-flow-error-code (function aitools.org.application:org-clock-report/k) boundaries
                              :path "clock-options.org" :root *root* :match "/TODO"
                              :from "bad" :through "2026-10-19T10:00:00"
                              :at "2026-10-19T09:00:00" :tz "UTC")
         :to-equal "argument.invalid")
        (expect
         (org-flow-error-code (function aitools.org.application:org-clock-report/k) boundaries
                              :path "clock-options.org" :root *root* :match "/TODO"
                              :from "2026-10-19T09:00:00" :through "bad"
                              :at "2026-10-19T09:00:00" :tz "UTC")
         :to-equal "argument.invalid")
        (expect
         (org-flow-error-code (function aitools.org.application:org-clock-out/k) boundaries
                              :path "clock-options.org" :root *root* :match "/TODO"
                              :at "2026-10-19T10:00:00" :tz "UTC")
         :to-equal "selection.no-match")))))
  (it "rejects invalid Org table selections and numeric values"
    (call-with-edit-workspace (lambda () (let ((boundaries (make-org-write-flow-boundaries)))
        (put "table-options.org"
             (format nil "| Name | Count |~%|------+-------|~%| apple | 1 |~%| pear | nope |~%"))
        (expect
         (org-flow-error-code (function aitools.org.application:org-table-align/k) boundaries
                              :path "table-options.org" :table 0 :root *root*)
         :to-equal "argument.invalid")
        (expect
         (org-flow-error-code (function aitools.org.application:org-table-align/k) boundaries
                              :path "table-options.org" :table 2 :root *root*)
         :to-equal "argument.invalid")
        (expect
         (org-flow-error-code (function aitools.org.application:org-table-sort/k) boundaries
                              :path "table-options.org" :table 1 :column 2
                              :type "numeric" :order "ascending" :start 1 :end 2
                              :root *root*)
         :to-equal "argument.invalid")))))
  (it "rejects invalid planning kinds, ambiguous headings, and duplicate markers"
    (call-with-edit-workspace (lambda () (let ((boundaries (make-org-write-flow-boundaries)))
        (put "planning-options.org"
             (format nil "#+TODO: TODO | DONE~%* TODO First~%* TODO Second~%"))
        (expect
         (org-flow-error-code (function aitools.org.application:org-planning-set/k) boundaries
                              :path "planning-options.org" :match "/TODO" :kind "scheduled"
                              :timestamp "<2026-10-20>" :at "2026-10-19T09:00:00"
                              :tz "UTC" :root *root*)
         :to-equal "selection.ambiguous")
        (put "duplicate-planning.org"
             (format nil "* TODO Release~%   SCHEDULED: <2026-10-20> SCHEDULED: <2026-10-22>~%"))
        (expect
         (org-flow-error-code (function aitools.org.application:org-planning-set/k) boundaries
                              :path "duplicate-planning.org" :match "/TODO"
                              :kind "invalid" :timestamp "<2026-10-21>"
                              :at "2026-10-19T09:00:00" :tz "UTC" :root *root*)
         :to-equal "argument.invalid")
        (expect
         (org-flow-error-code (function aitools.org.application:org-planning-set/k) boundaries
                              :path "duplicate-planning.org" :match "/TODO"
                              :kind "scheduled" :timestamp "<2026-10-21>"
                              :at "2026-10-19T09:00:00" :tz "UTC" :root *root*)
         :to-equal "selection.ambiguous")))))
  (it "preserves table bytes when the serialized output exceeds the kit limit"
    (call-with-edit-workspace (lambda () (put "limited-table.org" (format nil "| A | 1 |~%"))
      (let ((before (text "limited-table.org")))
        (let ((aitools.org.infrastructure::*org-kit-limits*
                (org-kit:make-limits :max-output-length 1)))
          (let ((boundaries (make-org-write-flow-boundaries)))
            (expect
             (org-flow-error-code (function aitools.org.application:org-table-align/k) boundaries
                                  :path "limited-table.org" :table 1 :root *root*)
             :to-equal "refusal.too-large")
            (expect (text "limited-table.org") :to-equal before))))))))
