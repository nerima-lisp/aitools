(in-package #:aitools.org.presentation)

(defun %agenda-command (ports)
  (make-command
   :name "agenda" :description (%summary "org.agenda")
   :positionals (list (%path-positional))
   :options (append
             (list (%value-option "from" "DATE" "Inclusive start date in YYYY-MM-DD form."
                                  :required-p t)
                   (%value-option "through" "DATE" "Inclusive end date in YYYY-MM-DD form."
                                  :required-p t)
                   (%value-option "future-repeats" "MODE" "How to include future repeating entries."
                                  :choices '("all" "next" "none")
                                  :default "all"))
             (%org-time-options))
   :handler (lambda (invocation)
              (apply #'%run-flow #'aitools.org.application:org-agenda/k ports
                     (append
                      (list :path (positional-value invocation :path)
                            :from (option-value invocation :from)
                            :through (option-value invocation :through)
                            :future-repeats (option-value invocation :future-repeats))
                      (%org-time-arguments invocation)
                      (%org-read-arguments invocation))))))

(defun %clock-report-command (ports)
  (make-command
   :name "report" :description (%summary "org.clock.report")
   :positionals (list (%path-positional))
   :options (append
             (%org-selector-time-options)
             (list (%value-option "from" "LOCAL-DATETIME" "Inclusive start of the half-open clock range."
                                  :required-p t)
                   (%value-option "through" "LOCAL-DATETIME" "Exclusive end of the half-open clock range."
                                  :required-p t)))
   :handler (lambda (invocation)
              (%org-run-selector-read
               #'aitools.org.application:org-clock-report/k ports invocation
               (list :from (option-value invocation :from)
                     :through (option-value invocation :through))))))

(defun %org-agenda-report-declarations ()
  (list
   (aitools.protocol.application:define-command
    "org.agenda"
    (:group "org" :command-builder #'%agenda-command
     :schema (%schema "org.agenda")))
   (aitools.protocol.application:define-command
    "org.clock.report"
    (:group "org.clock" :command-builder #'%clock-report-command
     :schema (%schema "org.clock.report")))))
