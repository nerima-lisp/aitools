(in-package #:aitools.env.test)

(describe "aitools.env integration for uptime and load averages"
  (it "reads host uptime from production boundaries"
    (multiple-value-bind (kind fields)
        (run-flow (function sys-info/k) (host-boundaries))
      (expect kind :to-be :ok)
      (expect (and (realp (field fields "uptime_seconds"))
                   (>= (field fields "uptime_seconds") 0))
              :to-be t)))
  (it "reads host load averages from production boundaries"
    (multiple-value-bind (kind fields)
        (run-flow (function sys-metrics/k) (host-boundaries) :limit 10000)
      (expect (member kind (list :ok :partial)) :to-be-truthy)
      (let ((averages (field fields "load_average")))
        (dolist (name (list "one_minute" "five_minutes" "fifteen_minutes"))
          (let ((value (object-field averages name)))
            (expect (and (realp value) (>= value 0)) :to-be t)))))))
