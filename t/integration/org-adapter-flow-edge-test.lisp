(in-package #:aitools.edit.test)
(describe "Org adapter flow edge cases"
  (it "returns agenda validation, habit, and resource failures"
    (with-edit-workspace ()
      (let ((ports (make-org-write-flow-ports)))
        (put "agenda.org" (format nil "#+TODO: TODO | DONE~%* TODO Work~%SCHEDULED: <2026-10-19 Mon>~%"))
        (expect (org-flow-error-code #'aitools.org.application:org-agenda/k ports
                                     :path "agenda.org" :root *root*
                                     :from "2026-10-20" :through "2026-10-19"
                                     :at "2026-10-19T09:00:00" :tz "UTC")
                :to-equal "argument.invalid")
        (put "habit.org"
             (format nil "#+TODO: TODO | DONE~%* TODO Habit~%SCHEDULED: <2026-10-19 Mon>~%:PROPERTIES:~%:STYLE: habit~%:END:~%"))
        (expect (org-flow-error-code #'aitools.org.application:org-agenda/k ports
                                     :path "habit.org" :root *root*
                                     :from "2026-10-19" :through "2026-10-19"
                                     :at "2026-10-19T09:00:00" :tz "UTC")
                :to-equal "input.syntax-error")
        (let ((aitools.org.infrastructure::*org-kit-limits*
                (org-kit:make-limits :max-agenda-entries 0)))
          (let ((limited-ports (make-org-write-flow-ports)))
            (expect (org-flow-error-code #'aitools.org.application:org-agenda/k
                                         limited-ports :path "agenda.org" :root *root*
                                         :from "2026-10-19" :through "2026-10-19"
                                         :at "2026-10-19T09:00:00" :tz "UTC")
                    :to-equal "refusal.too-large"))))))
  (it "reports clock precondition and report-range errors"
    (with-edit-workspace ()
      (let ((ports (make-org-write-flow-ports)))
        (put "report.org" (format nil "#+TODO: TODO | DONE~%* TODO Work~%SCHEDULED: <2026-10-19 Mon>~%"))
        (expect (org-flow-error-code #'aitools.org.application:org-clock-report/k ports
                                     :path "report.org" :root *root* :match ""
                                     :from "2026-10-19T09:00:00"
                                     :through "2026-10-19T10:00:00"
                                     :at "2026-10-19T09:00:00" :tz "UTC")
                :to-equal "argument.invalid")
        (expect (org-flow-error-code #'aitools.org.application:org-clock-report/k ports
                                     :path "report.org" :root *root* :match "/TODO"
                                     :from "2026-10-19T10:00:00"
                                     :through "2026-10-19T09:00:00"
                                     :at "2026-10-19T09:00:00" :tz "UTC")
                :to-equal "argument.invalid"))))
  (it "serializes agenda entries without a TODO state as JSON null"
    (with-edit-workspace ()
      (let ((ports (make-org-write-flow-ports)))
        (put "agenda-no-todo.org"
             (format nil "* Reminder~%SCHEDULED: <2026-10-19 Mon>~%"))
        (let* ((fields (org-flow-ok #'aitools.org.application:org-agenda/k ports
                                    :path "agenda-no-todo.org" :root *root*
                                    :from "2026-10-19" :through "2026-10-19"
                                    :at "2026-10-19T09:00:00" :tz "UTC"))
               (entry (first (coerce (field fields "entries") 'list)))
               (serialized (json-field entry "todo")))
          (expect (field fields "count") :to-be 1)
          (expect serialized :to-be json-kit:+json-null+)))))
  (it "reports when the application context omits agenda or clock report ports"
    (with-edit-workspace ()
      (let* ((production (make-org-write-flow-ports))
             (ports (aitools.org.application:make-org-ports
                     :workspace-host
                     (aitools.org.application:org-ports-workspace-host production)
                     :text-source
                     (aitools.org.application:org-ports-text-source production)
                     :max-input-bytes
                     (aitools.org.application:org-ports-max-input-bytes production)
                     :parse-org
                     (aitools.org.application:org-ports-parse-org production))))
        (put "agenda.org" (format nil "* Task~%"))
        (expect (org-flow-error-code #'aitools.org.application:org-agenda/k ports
                                     :path "agenda.org" :root *root*
                                     :from "2026-10-19" :through "2026-10-19"
                                     :at "2026-10-19T09:00:00" :tz "UTC")
                :to-equal "internal.unexpected")
        (expect (org-flow-error-code #'aitools.org.application:org-clock-report/k ports
                                     :path "agenda.org" :root *root* :match "/!"
                                     :from "2026-10-19T09:00:00"
                                     :through "2026-10-19T10:00:00"
                                     :at "2026-10-19T09:00:00" :tz "UTC")
                :to-equal "internal.unexpected"))))
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
