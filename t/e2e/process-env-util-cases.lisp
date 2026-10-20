(in-package #:aitools.e2e.test)

;;; Correspondence table rows 42-45: time and utility commands. The shared
;;; TRIMMED helper is in t/e2e/process-env-cases.lisp.

(defun uuid-shape (text)
  "TEXT with each hex digit shown as x, except the version nibble and the
RFC 4122 variant (v when it is 8, 9, a, or b)."
  (let ((lower (string-downcase (trimmed text))))
    (map 'string (lambda (char index)
                   (cond ((char= char #\-) #\-)
                         ((= index 14) char)
                         ((and (= index 19) (find char "89ab")) #\v)
                         ((digit-char-p char 16) #\x)
                         (t #\?)))
         lower (loop for i below (length lower) collect i))))

(define-row-case (42 "date +%s brackets time now's epoch" :foreign ("date")) (ws)
  (let* ((before (parse-integer (oracle ws '("date") "date +%s" nil)))
         (epoch (floor (jget (run-ok ws '("time" "now")) "epoch_ms") 1000))
         (after (parse-integer (oracle ws '("date") "date +%s" nil))))
    (expect (<= before epoch after) :to-be t)))

(define-row-case (42 "date -d '+1 day' is time convert --add 1d" :foreign ("date")) (ws)
  (expect (format nil "~A~%" (jget (run-ok ws '("time" "convert" "2026-01-31T00:00:00Z" "--add" "1d" "--tz" "UTC")) "result"))
          :to-equal (oracle ws '("date")
                            "date -u -d '2026-01-31 00:00:00 UTC + 1 day' +%Y-%m-%dT%H:%M:%SZ 2>/dev/null || date -u -j -v+1d -f %Y-%m-%dT%H:%M:%SZ 2026-01-31T00:00:00Z +%Y-%m-%dT%H:%M:%SZ"
                            (format nil "2026-02-01T00:00:00Z~%"))))

(define-row-case (42 "TZ=Asia/Tokyo date is time now --tz Asia/Tokyo" :foreign ("date")) (ws)
  (let ((envelope (run-ok ws '("time" "now" "--tz" "Asia/Tokyo"))))
    (expect (format nil "~A ~A~%" (remove #\: (jget envelope "utc_offset")) (jget envelope "abbreviation"))
            :to-equal (oracle ws '("date") "TZ=Asia/Tokyo date '+%z %Z'" (format nil "+0900 JST~%")))))

(define-row-case (42 "the difference of two date +%s values is time diff" :foreign ("date")) (ws)
  (expect (format nil "~D~%" (jget (run-ok ws '("time" "diff" "2026-01-01T00:00:00Z" "2026-01-02T01:30:00Z")) "diff_ms"))
          :to-equal (oracle ws '("date")
                            "e() { date -u -d \"$1\" +%s 2>/dev/null || date -u -j -f %Y-%m-%dT%H:%M:%SZ \"$1\" +%s; }; echo $(( ($(e 2026-01-02T01:30:00Z) - $(e 2026-01-01T00:00:00Z)) * 1000 ))"
                            (format nil "91800000~%"))))

(define-row-case (42 "fixed-offset ISO timestamp converts to UTC" :foreign ()) (ws)
  (expect (run-ok ws '("time" "convert" "2026-03-08T15:30:00+09:00" "--tz" "UTC"))
          :to-match-envelope
          "input_format" "iso8601"
          "result" "2026-03-08T06:30:00Z"
          "timezone" "UTC"))

(define-row-case (43 "base64 is util encode base64" :foreign ("base64")) (ws)
  (put ws "b.bin" (coerce #(0 1 2 250 255 104 105) '(vector (unsigned-byte 8))))
  (expect (format nil "~A~%" (jget (run-ok ws '("util" "encode" "base64" "--content-file" "b.bin")) "output"))
          :to-equal (oracle ws '("base64") "base64 < b.bin" (format nil "AAEC+v9oaQ==~%"))))

(define-row-case (43 "base64 -d > file is util decode --to" :foreign ("base64")) (ws)
  (let ((expected (oracle ws '("base64") "printf 'AAEC+v9oaQ==' | base64 -d > o.bin && cat o.bin"
                          (coerce #(0 1 2 250 255 104 105) '(vector (unsigned-byte 8))) :octets t)))
    (run-ok ws '("util" "decode" "base64" "--content" "AAEC+v9oaQ==" "--to" "a.bin"))
    (expect (file-octets ws "a.bin") :to-equalp expected)))

(define-row-case (43 "xxd -r -p is util decode hex" :foreign ("xxd")) (ws)
  (expect (jget (run-ok ws '("util" "decode" "hex" "--content" "68692074686572650a")) "output")
          :to-equal (oracle ws '("xxd") "echo 68692074686572650a | xxd -r -p" (format nil "hi there~%"))))

(define-row-case (43 "URL encoding as jq @uri does is util encode url" :foreign ("jq")) (ws)
  (let ((value (format nil "a b&c/~C?x=1" (code-char #xE9))))
    (expect (format nil "~A~%" (jget (run-ok ws (list "util" "encode" "url" "--content" value)) "output"))
            :to-equal (oracle ws '("jq") "jq -rn --arg v \"$E2E_VALUE\" '$v | @uri'" (format nil "a%20b%26c%2F%C3%A9%3Fx%3D1~%")
                              :env `(("E2E_VALUE" . ,value))))))

(define-row-case (44 "bc is util calc" :foreign ("bc")) (ws)
  (expect (format nil "~A~%" (jget (run-ok ws '("util" "calc" "2**100 - 3*(4+5)")) "result"))
          :to-equal (oracle ws '("bc") "echo '2^100 - 3*(4+5)' | BC_LINE_LENGTH=0 bc"
                            (format nil "1267650600228229401496703205349~%"))))

(define-row-case (44 "expr is util calc" :foreign ("expr")) (ws)
  (expect (format nil "~A~%" (jget (run-ok ws '("util" "calc" "7*6-5")) "result"))
          :to-equal (oracle ws '("expr") "expr 7 \\* 6 - 5" (format nil "37~%"))))

(define-row-case (44 "$((...)) is util calc" :foreign ()) (ws)
  (expect (format nil "~A~%" (jget (run-ok ws '("util" "calc" "(17 + 3) % 7")) "result"))
          :to-equal (oracle ws '() "echo $(( (17 + 3) % 7 ))" (format nil "6~%"))))

(define-row-case (44 "python3 -c 'print(10/4)' is util calc" :foreign ("python3")) (ws)
  (expect (format nil "~A~%" (jget (run-ok ws '("util" "calc" "10/4")) "result"))
          :to-equal (oracle ws '("python3") "python3 -c 'print(10/4)'" (format nil "2.5~%"))))

(define-row-case (45 "uuidgen's format is util uuid's" :foreign ("uuidgen")) (ws)
  (expect (uuid-shape (jget (run-ok ws '("util" "uuid")) "values" 0))
          :to-equal (uuid-shape (oracle ws '("uuidgen") "uuidgen" nil))))

(define-row-case (45 "openssl rand -hex 16's format is util random --length 32 --alphabet hex" :foreign ("openssl")) (ws)
  (flet ((shape (text) (map 'string (lambda (char) (if (find char "0123456789abcdef") #\x #\?)) (trimmed text))))
    (expect (shape (jget (run-ok ws '("util" "random" "--length" "32" "--alphabet" "hex")) "values" 0))
            :to-equal (shape (oracle ws '("openssl") "openssl rand -hex 16" nil)))))
