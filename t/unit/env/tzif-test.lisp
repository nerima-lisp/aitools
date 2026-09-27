;;;; t/unit/env/tzif-test.lisp
(in-package #:aitools.env.test)

(defparameter *new-york* (parse-tzif "America/New_York" (octets-from-hex *new-york-tzif-hex*)))
(defparameter *tokyo* (parse-tzif "Asia/Tokyo" (octets-from-hex *tokyo-tzif-hex*)))

(defun utc-ms (year month day hour minute second)
  (civil-to-epoch-milliseconds year month day hour minute second 0))

(defun offset-at (zone instant)
  (multiple-value-list (zone-offset-at zone instant)))

(describe "aitools.env.domain TZif reader"
  (it "reads the version-2 footer of both fixtures"
    (expect (posix-tz-daylight-name (zone-footer *new-york*)) :to-equal "EDT")
    (expect (posix-tz-standard-name (zone-footer *tokyo*)) :to-equal "JST")
    (expect (posix-tz-daylight-name (zone-footer *tokyo*)) :to-be nil))

  (it "switches New York to EDT at 2026-03-08 02:00 EST (07:00Z)"
    (expect (offset-at *new-york* (- (utc-ms 2026 3 8 7 0 0) 1000)) :to-equal '(-18000 nil "EST"))
    (expect (offset-at *new-york* (utc-ms 2026 3 8 7 0 0)) :to-equal '(-14400 t "EDT")))

  (it "switches New York back to EST at 2026-11-01 02:00 EDT (06:00Z)"
    (expect (offset-at *new-york* (- (utc-ms 2026 11 1 6 0 0) 1000)) :to-equal '(-14400 t "EDT"))
    (expect (offset-at *new-york* (utc-ms 2026 11 1 6 0 0)) :to-equal '(-18000 nil "EST")))

  (it "answers past the last table transition from the footer rule"
    (let ((transitions (zone-transitions *new-york*)))
      (expect (< (aref transitions (1- (length transitions))) (floor (utc-ms 2040 1 1 0 0 0) 1000))
              :to-be-truthy))
    ;; Second Sunday of March 2040 is the 11th; first Sunday of November is the 4th.
    (expect (offset-at *new-york* (- (utc-ms 2040 3 11 7 0 0) 1000)) :to-equal '(-18000 nil "EST"))
    (expect (offset-at *new-york* (utc-ms 2040 3 11 7 0 0)) :to-equal '(-14400 t "EDT"))
    (expect (offset-at *new-york* (utc-ms 2040 11 4 6 0 0)) :to-equal '(-18000 nil "EST")))

  (it "keeps Tokyo at JST +09:00 today and shows its 1949 daylight time"
    (expect (offset-at *tokyo* (utc-ms 2026 3 8 0 0 0)) :to-equal '(32400 nil "JST"))
    (expect (offset-at *tokyo* (utc-ms 1949 7 1 0 0 0)) :to-equal '(36000 t "JDT")))

  (it "uses the first time type before the first transition"
    (expect (first (offset-at *new-york* (utc-ms 1800 1 1 0 0 0))) :to-be -17762))

  (it "resolves a wall time in the spring-forward gap with the pre-gap offset"
    (expect (zone-local-to-epoch-milliseconds *new-york* (utc-ms 2026 3 8 2 30 0))
            :to-be (utc-ms 2026 3 8 7 30 0)))

  (it "resolves a repeated wall time on the fall-back day to the earlier instant"
    (expect (zone-local-to-epoch-milliseconds *new-york* (utc-ms 2026 11 1 1 30 0))
            :to-be (utc-ms 2026 11 1 5 30 0)))

  (it "resolves an ordinary wall time exactly"
    (expect (zone-local-to-epoch-milliseconds *tokyo* (utc-ms 2026 3 8 15 30 0))
            :to-be (utc-ms 2026 3 8 6 30 0)))

  (it "rejects data that is not TZif or is truncated"
    (signals tzif-format-error (parse-tzif "x" (octets-from-hex "00010203")))
    (let ((octets (octets-from-hex *tokyo-tzif-hex*)))
      (signals tzif-format-error (parse-tzif "x" (subseq octets 0 60))))))

(describe "aitools.env.domain POSIX TZ rules"
  (it "reads a fixed offset east of Greenwich"
    (expect (multiple-value-list (posix-tz-offset-at (parse-posix-tz "JST-9") 0)) :to-equal '(32400 nil "JST")))

  (it "handles southern-hemisphere rules that wrap the new year"
    (let ((sydney (parse-posix-tz "AEST-10AEDT,M10.1.0,M4.1.0/3")))
      (expect (nth-value 0 (posix-tz-offset-at sydney (floor (utc-ms 2026 1 15 0 0 0) 1000))) :to-be 39600)
      (expect (nth-value 0 (posix-tz-offset-at sydney (floor (utc-ms 2026 7 1 0 0 0) 1000))) :to-be 36000)))

  (it "reads quoted names and Julian-day rules"
    (let ((zone (parse-posix-tz "<+03>-3<+04>,J60/0,J300/0")))
      (expect (posix-tz-standard-name zone) :to-equal "+03")
      ;; J60 is always March 1, ignoring Feb 29.
      (expect (nth-value 0 (posix-tz-offset-at zone (floor (utc-ms 2028 2 29 12 0 0) 1000))) :to-be 10800)
      (expect (nth-value 0 (posix-tz-offset-at zone (floor (utc-ms 2028 3 1 12 0 0) 1000))) :to-be 14400)
      ;; In a common year J60 is March 1 as well, and J59 would be February 28.
      (expect (nth-value 0 (posix-tz-offset-at zone (floor (utc-ms 2027 2 28 12 0 0) 1000))) :to-be 10800)
      (expect (nth-value 0 (posix-tz-offset-at zone (floor (utc-ms 2027 3 1 12 0 0) 1000))) :to-be 14400)))

  (it "rejects malformed strings"
    (signals posix-tz-syntax-error (parse-posix-tz "E5"))
    (signals posix-tz-syntax-error (parse-posix-tz "EST5EDT,M13.1.0,M11.1.0"))))

(defun %u32 (value)
  (loop for shift from 24 downto 0 by 8 collect (ldb (byte 8 shift) value)))

(defun %signed-bytes (value size)
  (loop for shift from (* 8 (1- size)) downto 0 by 8 collect (ldb (byte 8 shift) value)))

(defun %tzif-header (version-byte counts)
  (append (map 'list #'char-code "TZif") (list version-byte) (make-list 15 :initial-element 0)
          (mapcan #'%u32 counts)))

(defun tzif-octets (&key (version 2) transitions indices types (chars "") footer timecnt typecnt charcnt)
  "A TZif file with one data block built from TRANSITIONS (epoch seconds),
INDICES, TYPES ((offset isdst abbreviation-index) ...) and CHARS. Version 1
puts it in the 32-bit block; version 2 leaves the 32-bit block empty and
puts it in the 64-bit block, followed by FOOTER's characters when given.
TIMECNT, TYPECNT and CHARCNT override the counts the header declares."
  (let* ((time-size (if (= version 1) 4 8))
         (counts (list 0 0 0 (or timecnt (length transitions)) (or typecnt (length types))
                       (or charcnt (length chars))))
         (block (append (mapcan (lambda (time) (%signed-bytes time time-size)) transitions)
                        (copy-list indices)
                        (mapcan (lambda (type)
                                  (destructuring-bind (offset isdst index) type
                                    (append (%signed-bytes offset 4) (list isdst index))))
                                types)
                        (map 'list #'char-code chars)))
         (bytes (if (= version 1)
                    (append (%tzif-header 0 counts) block)
                    (append (%tzif-header (char-code #\2) '(0 0 0 0 0 0))
                            (%tzif-header (char-code #\2) counts)
                            block
                            (and footer (map 'list #'char-code footer))))))
    (coerce bytes '(simple-array (unsigned-byte 8) (*)))))

(defun tzif-failure-reason (octets)
  (handler-case (progn (parse-tzif "x" octets) :parsed)
    (tzif-format-error (condition) (tzif-format-error-reason condition))))

(defparameter *nul* (string (code-char 0)))

(defun lines (&rest parts)
  "PARTS joined, each :NL standing for a newline."
  (format nil "~{~A~}" (substitute (string #\Newline) :nl parts)))

(describe "aitools.env.domain TZif reader on crafted files"
  (it "reads a version-1 file from its 32-bit block, with no footer"
    (let ((zone (parse-tzif "v1" (tzif-octets :version 1 :transitions '(0) :indices '(1)
                                              :types '((-18000 0 0) (3600 1 4))
                                              :chars (concatenate 'string "EST" *nul* "XDT" *nul*)))))
      (expect (zone-footer zone) :to-be nil)
      (expect (offset-at zone -1000) :to-equal '(-18000 nil "EST"))
      (expect (offset-at zone 0) :to-equal '(3600 t "XDT"))
      ;; Past the last transition with no footer the last transition's type stays in effect.
      (expect (offset-at zone (utc-ms 2100 1 1 0 0 0)) :to-equal '(3600 t "XDT"))))

  (it "answers the first type everywhere for a file with no transitions and no footer"
    (let ((zone (parse-tzif "fixed" (tzif-octets :types '((19800 0 0)) :chars (concatenate 'string "IST" *nul*)))))
      (expect (zone-footer zone) :to-be nil)
      (expect (offset-at zone (utc-ms 2026 1 1 0 0 0)) :to-equal '(19800 nil "IST"))))

  (it "treats an empty footer as no footer, and reads an abbreviation running to the end of the characters"
    (let ((zone (parse-tzif "empty-footer" (tzif-octets :types '((0 0 0)) :chars "GMT" :footer (lines :nl :nl)))))
      (expect (zone-footer zone) :to-be nil)
      (expect (offset-at zone 0) :to-equal '(0 nil "GMT"))))

  (it-each (("no local time types" (:types ()))
            ("truncated" (:types ((0 0 0)) :chars "UTC" :timecnt 100000))
            ("transitions not ascending" (:transitions (100 50) :indices (0 0) :types ((0 0 0)) :chars "UTC"))
            ("transitions not ascending" (:transitions (100 100) :indices (0 0) :types ((0 0 0)) :chars "UTC"))
            ("type index out of range" (:transitions (100) :indices (1) :types ((0 0 0)) :chars "UTC"))
            ("abbreviation index out of range" (:types ((0 0 3)) :chars "UTC"))
            ("unterminated footer" (:types ((0 0 0)) :chars "UTC" :footer "
EST5"))
            ("invalid footer TZ string" (:types ((0 0 0)) :chars "UTC" :footer "
E5
")))
      "fails as ~S for the crafted file ~S"
      (reason arguments)
    (expect (tzif-failure-reason (apply #'tzif-octets arguments)) :to-equal reason))

  (it-each ((0) (1) (2) (3))
      "fails as missing magic when byte ~D of the magic is wrong"
      (index)
    (let ((octets (tzif-octets :types '((0 0 0)) :chars "UTC")))
      (setf (aref octets index) (char-code #\X))
      (expect (tzif-failure-reason octets) :to-equal "missing TZif magic")))

  (it "fails with the missing magic when a version-2 file ends before its second header"
    (let ((octets (tzif-octets :types '((0 0 0)) :chars "UTC")))
      (expect (tzif-failure-reason (subseq octets 0 44)) :to-equal "missing TZif magic")
      (expect (princ-to-string (make-condition 'tzif-format-error :reason "truncated"))
              :to-equal "invalid TZif data: truncated"))))

(defun posix-tz-fields (text)
  (let ((zone (parse-posix-tz text)))
    (list (posix-tz-standard-name zone) (aitools.env.domain::posix-tz-standard-offset zone)
          (posix-tz-daylight-name zone) (aitools.env.domain::posix-tz-daylight-offset zone))))

(defun zone-offset-seconds (zone year month day hour minute second)
  (nth-value 0 (posix-tz-offset-at zone (floor (utc-ms year month day hour minute second) 1000))))

(describe "aitools.env.domain POSIX TZ syntax"
  (it-each (("EST+5:30:15" ("EST" -19815 nil nil))
            ("IST-5:30" ("IST" 19800 nil nil))
            ("<-03>3" ("-03" -10800 nil nil))
            ("EST5EDT4" ("EST" -18000 "EDT" -14400))
            ("EST5EDT" ("EST" -18000 "EDT" -14400))
            ("AAA0BBB-2,M3.5.0/1,M10.5.0/1" ("AAA" 0 "BBB" 7200)))
      "reads ~S as ~S"
      (text fields)
    (expect (posix-tz-fields text) :to-equal fields))

  (it "applies the US rules to a daylight name without rules, as tzcode does"
    (let ((zone (parse-posix-tz "EST5EDT4")))
      (expect (zone-offset-seconds zone 2026 3 8 6 59 59) :to-be -18000)
      (expect (zone-offset-seconds zone 2026 3 8 7 0 0) :to-be -14400)))

  (it "takes the last week of a month that has only four of the weekday (M2.5.0)"
    ;; February 2026 has four Sundays; the last is the 22nd.
    (let ((zone (parse-posix-tz "EST5EDT,M2.5.0,M11.1.0")))
      (expect (zone-offset-seconds zone 2026 2 22 6 59 59) :to-be -18000)
      (expect (zone-offset-seconds zone 2026 2 22 7 0 0) :to-be -14400)))

  (it "counts February 29 in a zero-based Julian day"
    ;; Day 59 is February 29 in a leap year, March 1 otherwise.
    (let ((zone (parse-posix-tz "AAA0BBB,59/0,300/0")))
      (expect (zone-offset-seconds zone 2028 2 28 12 0 0) :to-be 0)
      (expect (zone-offset-seconds zone 2028 2 29 12 0 0) :to-be 3600)
      (expect (zone-offset-seconds zone 2027 2 28 12 0 0) :to-be 0)
      (expect (zone-offset-seconds zone 2027 3 1 12 0 0) :to-be 3600)))

  (it "accepts a rule time up to the RFC 8536 limit of 167 hours"
    (let ((zone (parse-posix-tz "AAA0BBB,M3.1.0/167,M11.1.0/-1")))
      (expect (aitools.env.domain::tz-rule-time (aitools.env.domain::posix-tz-start zone)) :to-be (* 167 3600))
      (expect (aitools.env.domain::tz-rule-time (aitools.env.domain::posix-tz-end zone)) :to-be -3600)))

  (it-each (("") ("EST") ("E5") ("<AB>5") ("<ABC5") ("5") ("EST1234") ("EST25") ("EST5:60") ("EST5:30:60")
            ("EST5:") ("EST5EDT4;M3.2.0,M11.1.0") ("EST5EDT,M3.2.0") ("EST5EDT,M3.2.0,M11.1.0x")
            ("EST5EDT,M3.2,M11.1.0") ("EST5EDT,Mx.2.0,M11.1.0") ("EST5EDT,M0.2.0,M11.1.0")
            ("EST5EDT,M3.6.0,M11.1.0") ("EST5EDT,M3.2.7,M11.1.0") ("EST5EDT,J0,J365") ("EST5EDT,J1,J366")
            ("EST5EDT,0,366") ("EST5EDT,M3.2.0/168,M11.1.0") ("EST5EDT,") ("EST5EDT,M3") ("EST5EDT,M3.2.0,") ("EST5EDT,M3.2.0/2:60,M11.1.0") ("EST5EDT4x"))
      "rejects ~S"
      (text)
    (expect (handler-case (progn (parse-posix-tz text) :parsed)
              (posix-tz-syntax-error (condition)
                (princ-to-string condition)))
            :to-equal (format nil "invalid POSIX TZ string ~S" text))))
