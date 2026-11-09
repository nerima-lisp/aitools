(in-package #:aitools.e2e.test)

;;;; Correspondence table row 44: time conversion and comparison.

(define-row-case (44 "fixed-offset ISO timestamp converts to UTC" :foreign ()) (ws)
  (expect (run-ok ws '("time" "convert" "2026-03-08T15:30:00+09:00" "--tz" "UTC"))
          :to-match-envelope
          "input_format" "iso8601"
          "result" "2026-03-08T06:30:00Z"
          "timezone" "UTC"))

(define-row-case (44 "the difference of two date +%s values is time diff" :foreign ("date")) (ws)
  (expect (format nil "~D~%" (jget (run-ok ws '("time" "diff" "2026-01-01T00:00:00Z" "2026-01-02T01:30:00Z")) "diff_ms"))
          :to-equal (oracle ws '("date")
                            "e() { date -u -d \"$1\" +%s 2>/dev/null || date -u -j -f %Y-%m-%dT%H:%M:%SZ \"$1\" +%s; }; echo $(( ($(e 2026-01-02T01:30:00Z) - $(e 2026-01-01T00:00:00Z)) * 1000 ))"
                            (format nil "91800000~%"))))

(define-row-case (44 "TZ=Asia/Tokyo date is time now --tz Asia/Tokyo" :foreign ("date")) (ws)
  (let ((envelope (run-ok ws '("time" "now" "--tz" "Asia/Tokyo"))))
    (expect (format nil "~A ~A~%" (remove #\: (jget envelope "utc_offset")) (jget envelope "abbreviation"))
            :to-equal (oracle ws '("date") "TZ=Asia/Tokyo date '+%z %Z'" (format nil "+0900 JST~%")))))

(define-row-case (44 "date -d '+1 day' is time convert --add 1d" :foreign ("date")) (ws)
  (expect (format nil "~A~%" (jget (run-ok ws '("time" "convert" "2026-01-31T00:00:00Z" "--add" "1d" "--tz" "UTC")) "result"))
          :to-equal (oracle ws '("date")
                            "date -u -d '2026-01-31 00:00:00 UTC + 1 day' +%Y-%m-%dT%H:%M:%SZ 2>/dev/null || date -u -j -v+1d -f %Y-%m-%dT%H:%M:%SZ 2026-01-31T00:00:00Z +%Y-%m-%dT%H:%M:%SZ"
                            (format nil "2026-02-01T00:00:00Z~%"))))

(define-row-case (44 "date +%s brackets time now's epoch" :foreign ("date")) (ws)
  (let* ((before (parse-integer (oracle ws '("date") "date +%s" nil)))
         (epoch (floor (jget (run-ok ws '("time" "now")) "epoch_ms") 1000))
         (after (parse-integer (oracle ws '("date") "date +%s" nil))))
    (expect (<= before epoch after) :to-be t)))
