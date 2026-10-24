(in-package #:aitools.edit.test)

(defun make-org-write-flow-ports ()
  (aitools.org.infrastructure:make-production-org-ports
   :workspace-host (aitools.workspace.infrastructure:make-host-workspace-host)
   :text-source (aitools.text.infrastructure:make-host-text-source)
   :open-store #'open-store))

(defun run-org-flow (flow ports &rest arguments)
  (let ((result
          (aitools.protocol.application:call-with-command-result/k
           (lambda (&rest continuations)
             (apply flow (append (list ports) arguments continuations))))))
    (values (aitools.protocol.application:command-result-kind result)
            (aitools.protocol.application:command-result-fields result))))

(defun org-flow-ok (flow ports &rest arguments)
  (multiple-value-bind (kind fields)
      (apply #'run-org-flow flow ports arguments)
    (expect kind :to-be :ok)
    fields))

(defun org-flow-error-code (flow ports &rest arguments)
  (multiple-value-bind (kind fields)
      (apply #'run-org-flow flow ports arguments)
    (expect kind :to-be :error)
    (getf fields :code)))

(defun org-write-time-arguments (&optional (at "2026-10-19T09:00:00"))
  (list :at at :tz "UTC" :dst-policy "compatible"
        :root *root*))

(describe "Org write flows through the shared edit application"
  (it "runs TODO, property, planning, clock, and table writes in process"
    (call-with-edit-workspace (lambda () (let ((ports (make-org-write-flow-ports)))
        (put "todo.org" (format nil "#+TODO: TODO | DONE~%* TODO Task~%"))
        (let ((fields
                (apply #'org-flow-ok #'aitools.org.application:org-todo-set/k ports
                       :path "todo.org" :match "/TODO" :state "DONE"
                       (org-write-time-arguments))))
          (expect (field fields "previous_state") :to-equal "TODO")
          (expect (field fields "new_state") :to-equal "DONE")
          (expect (field fields "changed") :to-be t)
          (expect (search "* DONE Task" (text "todo.org")) :to-be-truthy))

        (put "property.org" (format nil "* TODO Task~%"))
        (let ((fields
                (apply #'org-flow-ok #'aitools.org.application:org-property-set/k ports
                       :path "property.org" :match "/TODO" :name "OWNER" :value "A"
                       (org-write-time-arguments))))
          (expect (field fields "property") :to-equal "OWNER")
          (expect (field fields "changed") :to-be t)
          (expect (search ":OWNER: A" (text "property.org")) :to-be-truthy))
        (let ((fields
                (apply #'org-flow-ok #'aitools.org.application:org-property-delete/k ports
                       :path "property.org" :match "/TODO" :name "OWNER"
                       (org-write-time-arguments))))
          (expect (field fields "property") :to-equal "OWNER")
          (expect (field fields "changed") :to-be t)
          (expect (search ":OWNER:" (text "property.org")) :to-be nil))

        (put "planning.org" (format nil "* TODO Release~%"))
        (let ((fields
                (apply #'org-flow-ok #'aitools.org.application:org-planning-set/k ports
                       :path "planning.org" :match "/TODO" :kind "scheduled"
                       :timestamp "<2026-10-20>" (org-write-time-arguments))))
          (expect (field fields "kind") :to-equal "scheduled")
          (expect (field fields "timestamp") :to-equal "<2026-10-20>")
          (expect (field fields "changed") :to-be t)
          (expect (search "SCHEDULED: <2026-10-20>" (text "planning.org"))
                  :to-be-truthy))
        (let ((fields
                (apply #'org-flow-ok #'aitools.org.application:org-planning-delete/k ports
                       :path "planning.org" :match "/TODO" :kind "scheduled"
                       (org-write-time-arguments))))
          (expect (field fields "changed") :to-be t)
          (expect (search "SCHEDULED:" (text "planning.org")) :to-be nil))

        (put "clock-in.org" (format nil "* TODO Work~%"))
        (let ((fields
                (apply #'org-flow-ok #'aitools.org.application:org-clock-in/k ports
                       :path "clock-in.org" :match "/!"
                       (org-write-time-arguments))))
          (expect (field fields "timestamp") :to-equal "[2026-10-19 Mon 09:00]")
          (expect (field fields "changed") :to-be t)
          (expect (search ":LOGBOOK:" (text "clock-in.org")) :to-be-truthy))
        (put "clock-out.org"
             (format nil "* TODO Work~%:LOGBOOK:~%CLOCK: [2026-10-19 Mon 09:00]~%:END:~%"))
        (let ((fields
                (apply #'org-flow-ok #'aitools.org.application:org-clock-out/k ports
                       :path "clock-out.org" :match "/!"
                       (org-write-time-arguments "2026-10-19T09:30:00"))))
          (expect (field fields "interval")
                  :to-equal "[2026-10-19 Mon 09:00]--[2026-10-19 Mon 09:30] => 0:30")
          (expect (field fields "changed") :to-be t)
          (expect (search "--[2026-10-19 Mon 09:30]" (text "clock-out.org"))
                  :to-be-truthy))

        (put "align.org"
             (format nil "Before~%  | key | value~%|--+-------~%| a | 1~%After~%"))
        (let ((fields
                (org-flow-ok #'aitools.org.application:org-table-align/k ports
                             :path "align.org" :table 1 :root *root*)))
          (expect (field fields "table") :to-be 1)
          (expect (field fields "changed") :to-be t)
          (expect (search "Before" (text "align.org")) :to-be-truthy)
          (expect (search "After" (text "align.org")) :to-be-truthy))
        (put "sort.org" (format nil "| Z | 2 |~%| A | 1 |~%"))
        (let ((fields
                (org-flow-ok #'aitools.org.application:org-table-sort/k ports
                             :path "sort.org" :table 1 :column 1 :type "lexical"
                             :order "ascending" :start 1 :end 2 :root *root*)))
          (expect (field fields "changed") :to-be t)
          (expect (search "| A | 1 |" (text "sort.org")) :to-be-truthy))
        (put "formula.org"
             (format nil "| Input | Total |~%|-------+-------|~%| 2 | 0 |~%#+TBLFM: @2$2=$1+1~%"))
        (let ((fields
                (org-flow-ok #'aitools.org.application:org-table-recalculate/k ports
                             :path "formula.org" :table 1 :root *root*)))
          (expect (field fields "changed") :to-be t)
          (expect (search "| 2 | 3 |" (text "formula.org")) :to-be-truthy))))))

  (it "rejects unknown formula functions as unsupported syntax"
    (call-with-edit-workspace (lambda () (let ((ports (make-org-write-flow-ports)))
        (put "unknown-function.org"
             (format nil "| Input | Result |~%|-------+--------|~%| 2 | 0 |~%#+TBLFM: @2$2=unknown(1)~%"))
        (expect
         (org-flow-error-code #'aitools.org.application:org-table-recalculate/k ports
                              :path "unknown-function.org" :table 1 :root *root*)
         :to-equal "input.unsupported-format"))))))

(describe "Org read flows through the infrastructure adapters"
  (it "returns agenda and clock report data in the command result"
    (call-with-edit-workspace (lambda () (let ((ports (make-org-write-flow-ports)))
        (put "agenda.org"
             (format nil "#+TODO: TODO | DONE~%* TODO Work :work:~%SCHEDULED: <2026-10-19 Mon>~%"))
        (let* ((fields
                 (org-flow-ok #'aitools.org.application:org-agenda/k ports
                              :path "agenda.org" :from "2026-10-19"
                              :through "2026-10-19" :future-repeats "all"
                              :at "2026-10-19T09:00:00" :tz "UTC" :root *root*))
               (entries (coerce (field fields "entries") 'list))
               (entry (first entries)))
          (expect (field fields "count") :to-be 1)
          (expect (json-field entry "title") :to-equal "Work")
          (expect (json-field entry "kind") :to-equal "scheduled")
          (expect (json-field entry "line") :to-be 2))
        (put "clock-report.org"
             (format nil "* TODO Work~%:LOGBOOK:~%CLOCK: [2026-10-19 Mon 09:00]--[2026-10-19 Mon 09:30] =>  0:30~%:END:~%"))
        (let ((fields
                (org-flow-ok #'aitools.org.application:org-clock-report/k ports
                             :path "clock-report.org" :match "/!"
                             :from "2026-10-19T09:00:00"
                             :through "2026-10-19T10:00:00"
                             :at "2026-10-19T10:00:00" :tz "UTC" :root *root*)))
          (expect (field fields "total_minutes") :to-be 30)
          (expect (length (field fields "rows")) :to-be 1)))))))

(describe "Org adapter argument and selection failures"
  (it "rejects invalid planning, clock, table, agenda, and report requests"
    (call-with-edit-workspace (lambda () (let ((ports (make-org-write-flow-ports)))
        (put "invalid.org" (format nil "#+TODO: TODO | DONE~%* TODO Task~%"))
        (expect
         (org-flow-error-code #'aitools.org.application:org-planning-set/k ports
                              :path "invalid.org" :match "/TODO" :kind "scheduled"
                              :timestamp "<2026-10-20 +1d +2w>"
                              :at "2026-10-19T09:00:00" :tz "UTC" :root *root*)
         :to-equal "argument.invalid")
        (expect
         (org-flow-error-code #'aitools.org.application:org-todo-set/k ports
                              :path "invalid.org" :match "/MISSING" :state "DONE"
                              :at "2026-10-19T09:00:00" :tz "UTC" :root *root*)
         :to-equal "selection.no-match")
        (put "running.org" (format nil "* TODO Work~%"))
        (expect
         (org-flow-error-code #'aitools.org.application:org-clock-out/k ports
                              :path "running.org" :match "/!"
                              :at "2026-10-19T10:00:00" :tz "UTC" :root *root*)
         :to-equal "selection.no-match")
        (put "table.org" (format nil "| A | B |~%|---+---|~%| 1 | 2 |~%"))
        (expect
         (org-flow-error-code #'aitools.org.application:org-table-sort/k ports
                              :path "table.org" :table 1 :column 1 :type "numeric"
                              :order "sideways" :root *root*)
         :to-equal "argument.invalid")
        (put "agenda.org" (format nil "* TODO Work~%"))
        (expect
         (org-flow-error-code #'aitools.org.application:org-agenda/k ports
                              :path "agenda.org" :from "2026-02-30"
                              :through "2026-03-01" :future-repeats "all"
                              :at "2026-03-01T09:00:00" :tz "UTC" :root *root*)
         :to-equal "argument.invalid")
        (put "clock-report.org" (format nil "* TODO Work~%"))
        (expect
         (org-flow-error-code #'aitools.org.application:org-clock-report/k ports
                              :path "clock-report.org" :match "/!"
                              :from "2026-10-19T10:00:00"
                              :through "2026-10-19T09:00:00"
                              :at "2026-10-19T10:00:00" :tz "UTC" :root *root*)
         :to-equal "argument.invalid"))))))

(describe "Org table error mapping"
  (it "maps unknown-function errors to unsupported syntax"
    (let ((failure
            (aitools.org.infrastructure::%table-error-failure
             (make-condition 'org-kit:org-table-error
                             :message "Unknown formula function"
                             :reason :unknown-function)
             :table-recalculate)))
      (expect (aitools.org.domain:org-failure-kind failure)
              :to-be :unsupported))))
