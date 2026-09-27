;;;; t/unit/env/time-flows-test.lisp
(in-package #:aitools.env.test)

(defun convert (value &rest keys &key (ports (make-fake-ports)) &allow-other-keys)
  (let ((keys (loop for (key val) on keys by #'cddr unless (eq key :ports) append (list key val))))
    (apply #'run-flow #'time-convert/k ports value keys)))

(defun convert-result (value &rest keys)
  (multiple-value-bind (kind fields) (apply #'convert value keys)
    (expect kind :to-be :ok)
    (field fields "result")))

(describe "aitools.env.application time now"
  (it "reports one instant in every field"
    (multiple-value-bind (kind fields) (run-flow #'time-now/k (make-fake-ports) :tz "Asia/Tokyo")
      (expect kind :to-be :ok)
      (expect (field fields "epoch_ms") :to-be *fake-epoch-milliseconds*)
      (expect (field fields "iso8601") :to-equal "2026-03-08T15:30:00.250+09:00")
      (expect (field fields "utc") :to-equal "2026-03-08T06:30:00.250Z")
      (expect (field fields "timezone") :to-equal "Asia/Tokyo")
      (expect (field fields "utc_offset") :to-equal "+09:00")
      (expect (field fields "abbreviation") :to-equal "JST")
      ;; Each string field reads back as the same instant.
      (dolist (name '("iso8601" "utc"))
        (expect (time-input-milliseconds (parse-time-input (field fields name)))
                :to-be *fake-epoch-milliseconds*))))

  (it "uses $TZ when --tz is absent, then the /etc/localtime link"
    (expect (field (nth-value 1 (run-flow #'time-now/k (make-fake-ports :environment '(("TZ" . "America/New_York")))))
                   "iso8601")
            :to-equal "2026-03-08T01:30:00.250-05:00")
    (expect (field (nth-value 1 (run-flow #'time-now/k
                                         (make-fake-ports :environment nil
                                                          :links '(("/etc/localtime" . "/var/db/timezone/zoneinfo/Asia/Tokyo")))))
                   "timezone")
            :to-equal "Asia/Tokyo")
    (expect (field (nth-value 1 (run-flow #'time-now/k (make-fake-ports :environment nil))) "timezone")
            :to-equal "UTC"))

  (it "rejects an unknown zone with argument.invalid and a repair"
    (multiple-value-bind (kind error) (run-flow #'time-now/k (make-fake-ports) :tz "Nowhere/City")
      (expect kind :to-be :error)
      (expect (first error) :to-equal "argument.invalid")
      (expect (getf (first (getf (cddr error) :repairs)) :command) :to-equal "aitools time now --tz UTC")))

  (it "reports environment.unavailable when no zoneinfo directory exists"
    (multiple-value-bind (kind error) (run-flow #'time-now/k (make-fake-ports :directories nil) :tz "Asia/Tokyo")
      (expect kind :to-be :error)
      (expect (first error) :to-equal "environment.unavailable")))

  (it "serves UTC without any zoneinfo database"
    (multiple-value-bind (kind fields) (run-flow #'time-now/k (make-fake-ports :files nil :directories nil) :tz "UTC")
      (expect kind :to-be :ok)
      (expect (field fields "iso8601") :to-equal "2026-03-08T06:30:00.250Z"))))

(describe "aitools.env.application time convert"
  (it "round-trips between ISO 8601, epoch_ms, and epoch_s"
    (let* ((epoch-ms (convert-result "2026-03-08T15:30:00+09:00" :to "epoch_ms"))
           (iso (convert-result (princ-to-string epoch-ms) :to "iso8601" :tz "Asia/Tokyo"))
           (epoch-s (convert-result iso :to "epoch_s")))
      (expect epoch-ms :to-be 1772951400000)
      (expect iso :to-equal "2026-03-08T15:30:00+09:00")
      (expect epoch-s :to-be 1772951400)
      (expect (convert-result (princ-to-string epoch-s) :to "epoch_ms") :to-be epoch-ms)))

  (it "adds across the New York spring-forward boundary"
    (expect (convert-result "2026-03-08T01:30:00" :tz "America/New_York" :add '("1h"))
            :to-equal "2026-03-08T03:30:00-04:00"))

  (it "subtracts across the New York fall-back boundary"
    (expect (convert-result "2026-11-01T01:30:00-05:00" :tz "America/New_York" :sub '("1h"))
            :to-equal "2026-11-01T01:30:00-04:00"))

  (it "sums repeated --add and --sub"
    (expect (convert-result "2026-01-01T00:00:00Z" :tz "UTC" :add '("1d" "2h") :sub '("30m"))
            :to-equal "2026-01-02T01:30:00Z"))

  (it "reads an offset-less input in the --tz zone"
    (expect (convert-result "2026-03-08T15:30:00" :tz "Asia/Tokyo" :to "epoch_ms") :to-be 1772951400000))

  (it "converts `now` from the clock port"
    (multiple-value-bind (kind fields) (convert "now" :to "epoch_ms" :add '("1d"))
      (expect kind :to-be :ok)
      (expect (field fields "input_format") :to-equal "now")
      (expect (field fields "result") :to-be (+ *fake-epoch-milliseconds* 86400000))))

  (it "reports the detected input format"
    (expect (field (nth-value 1 (convert "1700000000123" :to "iso8601")) "input_format") :to-equal "epoch_ms"))

  (it-each (("garbage") ("2026-02-30T00:00:00") ("20260308X"))
      "rejects ~S with input.syntax-error"
      (value)
    (multiple-value-bind (kind error) (convert value)
      (expect kind :to-be :error)
      (expect (first error) :to-equal "input.syntax-error")
      (expect (getf (first (getf (cddr error) :repairs)) :command) :not :to-be nil)))

  (it "rejects an invalid duration with argument.invalid"
    (multiple-value-bind (kind error) (convert "now" :add '("-1h"))
      (expect kind :to-be :error)
      (expect (first error) :to-equal "argument.invalid")))

  (it "rejects a result outside years 0000-9999"
    (multiple-value-bind (kind error) (convert "9999-12-31T23:00:00Z" :add '("2h"))
      (expect kind :to-be :error)
      (expect (first error) :to-equal "input.syntax-error"))))

(describe "aitools.env.application time diff"
  (it "returns B minus A with a human form"
    (multiple-value-bind (kind fields) (run-flow #'time-diff/k (make-fake-ports)
                                                 "2026-03-08T00:00:00Z" "2026-03-08T01:23:00Z")
      (expect kind :to-be :ok)
      (expect (field fields "diff_ms") :to-be 4980000)
      (expect (field fields "human") :to-equal "1h23m")))

  (it "is negative when B is earlier, and mixes formats"
    (multiple-value-bind (kind fields) (run-flow #'time-diff/k (make-fake-ports) "1772951400" "2026-03-08T06:00:00Z")
      (expect kind :to-be :ok)
      (expect (field fields "diff_ms") :to-be -1800000)
      (expect (field fields "human") :to-equal "-30m")))

  (it "spans a DST change in wall-clock inputs"
    (multiple-value-bind (kind fields)
        (run-flow #'time-diff/k (make-fake-ports :environment '(("TZ" . "America/New_York")))
                  "2026-03-08T00:00:00" "2026-03-08T04:00:00")
      (expect kind :to-be :ok)
      (expect (field fields "human") :to-equal "3h")))

  (it "rejects an unreadable value with input.syntax-error"
    (expect (first (nth-value 1 (run-flow #'time-diff/k (make-fake-ports) "now" "later")))
            :to-equal "input.syntax-error")))

(describe "aitools.env.application time zones and targets at their edges"
  (it "rejects an output format the flow does not know, pointing at an example"
    (multiple-value-bind (kind error) (convert "now" :to "rfc2822")
      (expect kind :to-be :error)
      (expect (first error) :to-equal "argument.invalid")
      (expect (second error) :to-equal "--to must be one of iso8601, epoch_ms, epoch_s")
      (expect (getf (first (getf (cddr error) :repairs)) :command)
              :to-equal "aitools time convert 2026-03-08T01:30:00Z --to rfc2822")))

  (it-each (("../../etc/passwd") ("Asia//Tokyo") ("Asia/Tokyo,x"))
      "refuses the zone name ~S without reading any file"
      (name)
    (let ((ports (make-fake-ports :files (list (cons (concatenate 'string "/usr/share/zoneinfo/" name)
                                                     (latin-1-from-hex *tokyo-tzif-hex*))))))
      (multiple-value-bind (kind error) (run-flow #'time-now/k ports :tz name)
        (expect kind :to-be :error)
        (expect (first error) :to-equal "argument.invalid"))))

  (it "treats a zoneinfo file that is not TZif as an unknown zone, and keeps looking in later directories"
    (let ((bad (make-fake-ports :files (list (cons "/usr/share/zoneinfo/Asia/Tokyo" "not a tzif file")))))
      (expect (first (nth-value 1 (run-flow #'time-now/k bad :tz "Asia/Tokyo"))) :to-equal "argument.invalid"))
    (let ((later (make-fake-ports :files (list (cons "/usr/share/zoneinfo/Asia/Tokyo" "not a tzif file")
                                               (cons "/etc/zoneinfo/Asia/Tokyo" (latin-1-from-hex *tokyo-tzif-hex*)))
                                  :directories '("/usr/share/zoneinfo" "/etc/zoneinfo"))))
      (multiple-value-bind (kind fields) (run-flow #'time-now/k later :tz "Asia/Tokyo")
        (expect kind :to-be :ok)
        (expect (field fields "utc_offset") :to-equal "+09:00")))))
