;;;; t/unit/process/domain-test.lisp
;;;;
;;;; aitools.process.domain terminal text, output summaries, line patterns,
;;;; and bg ids. The records and the rest are in domain-records-test.lisp.
(in-package #:aitools.process.test)

(defparameter *dummy-token* "ghp_abcdefghijklmnopqrstuvwxyz0123456789"
  "A GitHub-token-shaped dummy value (a format redaction masks), not a real secret.")

(defun esc (text)
  "TEXT with every `^` replaced by an ESC character."
  (substitute (code-char 27) #\^ text))

(defun cr (text)
  "TEXT with every `|` replaced by a carriage return."
  (substitute #\Return #\| text))

(defun controls (text)
  "TEXT with `^` as ESC, `~` as BEL, and `{` as the 8-bit CSI."
  (map 'string (lambda (char)
                 (case char
                   (#\^ (code-char 27))
                   (#\~ (code-char 7))
                   (#\{ (code-char #x9b))
                   (t char)))
       text))

(describe "aitools.process.domain terminal text"
  (it "removes CSI color and erase sequences"
    (expect (aitools.process.domain:strip-ansi-escapes (esc "^[1;31merror^[0m done^[K"))
            :to-equal "error done"))

  (it "removes OSC sequences ended by BEL or ST"
    (expect (aitools.process.domain:strip-ansi-escapes
             (concatenate 'string (esc "^]0;title") (string (code-char 7)) "a" (esc "^]8;;x^\\b")))
            :to-equal "ab"))

  (it "removes two-character escapes and the 8-bit CSI"
    (expect (aitools.process.domain:strip-ansi-escapes
             (concatenate 'string (esc "^(Bx") (string (code-char #x9b)) "2Ky"))
            :to-equal "xy"))

  (it "leaves a line without escapes identical"
    (let ((line "plain text"))
      (expect (aitools.process.domain:strip-ansi-escapes line) :to-be line)))

  (it "keeps only the last redraw of a carriage-return progress line"
    (expect (aitools.process.domain:collapse-carriage-returns (cr "10%|50%|100%")) :to-equal "100%"))

  (it "drops the CR of a CRLF line end instead of emptying the line"
    (expect (aitools.process.domain:collapse-carriage-returns (cr "done|")) :to-equal "done"))

  (it "strips escapes and redraws together, as a progress bar emits them"
    (expect (aitools.process.domain:normalize-terminal-line (cr (esc "^[2K 10%|^[2K 99%|^[32mok^[0m")) t)
            :to-equal "ok"))

  (it "normalizes nothing when stripping is off"
    (let ((line (cr (esc "^[31mred^[0m|x"))))
      (expect (aitools.process.domain:normalize-terminal-line line nil) :to-equal line)))

  (it "splits on LF without inventing a trailing empty line"
    (expect (aitools.process.domain:split-output-lines (format nil "a~%b~%")) :to-equal '("a" "b"))
    (expect (aitools.process.domain:split-output-lines (format nil "a~%~%b")) :to-equal '("a" "" "b"))
    (expect (aitools.process.domain:split-output-lines "") :to-equal '()))

  (it "decodes malformed UTF-8 with U+FFFD instead of failing"
    (expect (aitools.process.domain:decode-output-octets
             (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(97 255 98)))
            :to-equal (coerce (list #\a (code-char #xfffd) #\b) 'string)))

  (it-each (("a^[2 qb" "ab" "a CSI with an intermediate byte")
            ("a^[12" "a" "an unterminated CSI")
            ("a{12" "a" "an unterminated 8-bit CSI")
            ("a^" "a" "a lone ESC at the end")
            ("a^]0;title" "a" "an unterminated OSC")
            ("a^]0;t^xz~b" "ab" "an OSC holding an ESC that does not start ST")
            ("a^(" "a" "an nF escape cut off before its final byte"))
      "strips ~S to ~S (~A)"
      (text expected description)
    (declare (ignore description))
    (expect (aitools.process.domain:strip-ansi-escapes (controls text)) :to-equal expected)))

(defun numbered-lines (count)
  (format nil "~{line ~D~%~}" (loop for n from 1 to count collect n)))

(defun summarize (text &rest options)
  (apply #'aitools.process.domain:summarize-output text
         (append options (list :head-count 2 :tail-count 3 :strip-p t :grep-limit 50))))

(describe "aitools.process.domain summarize-output"
  (it "keeps every line, head then tail without overlap, when they fit"
    (let ((report (summarize (numbered-lines 4))))
      (expect (aitools.process.domain:output-report-head report) :to-equal '("line 1" "line 2"))
      (expect (aitools.process.domain:output-report-tail report) :to-equal '("line 3" "line 4"))
      (expect (aitools.process.domain:output-report-truncated report) :to-be nil)))

  (it "drops the middle and marks truncation when lines exceed head+tail"
    (let ((report (summarize (numbered-lines 10))))
      (expect (aitools.process.domain:output-report-head report) :to-equal '("line 1" "line 2"))
      (expect (aitools.process.domain:output-report-tail report) :to-equal '("line 8" "line 9" "line 10"))
      (expect (aitools.process.domain:output-report-total-lines report) :to-be 10)
      (expect (aitools.process.domain:output-report-truncated report) :to-be t)))

  (it "greps the dropped middle lines with their original line numbers"
    (let ((report (summarize (numbered-lines 10)
                             :pattern (aitools.process.domain:compile-line-pattern "^line [56]$"))))
      (expect (aitools.process.domain:output-report-matches report) :to-equal '((5 . "line 5") (6 . "line 6")))
      (expect (aitools.process.domain:output-report-total-matches report) :to-be 2)))

  (it "stops collecting matches at the limit but keeps counting"
    (let ((report (aitools.process.domain:summarize-output
                   (numbered-lines 10) :head-count 1 :tail-count 1 :strip-p t
                   :pattern (aitools.process.domain:compile-line-pattern "line")
                   :grep-limit 3)))
      (expect (length (aitools.process.domain:output-report-matches report)) :to-be 3)
      (expect (aitools.process.domain:output-report-total-matches report) :to-be 10)
      (expect (aitools.process.domain:output-report-grep-exceeded-p report 3) :to-be t)
      (expect (aitools.process.domain:output-report-grep-exceeded-p report 10) :to-be nil)))

  (it "masks secrets and counts them before grep sees the line"
    (let ((report (summarize (format nil "token ~A~%" *dummy-token*)
                             :pattern (aitools.process.domain:compile-line-pattern "ghp_"))))
      (expect (aitools.process.domain:output-report-head report) :to-equal '("token [REDACTED_SECRET]"))
      (expect (aitools.process.domain:output-report-redactions report) :to-be 1)
      (expect (aitools.process.domain:output-report-total-matches report) :to-be 0)))

  (it "strips ANSI before matching so colored error lines are found"
    (let ((report (summarize (esc (format nil "ok~%^[31mERROR^[0m: boom~%"))
                             :pattern (aitools.process.domain:compile-line-pattern "^ERROR:"))))
      (expect (aitools.process.domain:output-report-matches report) :to-equal '((2 . "ERROR: boom")))))

  (it "caps a single huge line and marks truncation"
    (let* ((huge (make-string (1+ (* 1024 1024)) :initial-element #\x))
           (report (summarize (format nil "~A~%" huge))))
      (expect (length (first (aitools.process.domain:output-report-head report))) :to-be (* 1024 1024))
      (expect (aitools.process.domain:output-report-truncated report) :to-be t)))

  (it "reports no match fields when no pattern was given"
    (let ((json (aitools.process.domain:output-report-json (summarize "a"))))
      (expect (json-alist-value json "matches" :absent) :to-be :absent)
      (expect (json-alist-value json "truncated") :to-be json-kit:+json-false+))))

(describe "aitools.process.domain line patterns"
  (it "rejects an invalid pattern with its own condition"
    (expect (handler-case (progn (aitools.process.domain:compile-line-pattern "(") :compiled)
              (aitools.process.domain:invalid-line-pattern () :rejected))
            :to-be :rejected))

  (it "prints the kit's diagnosis as its report"
    (let ((condition (nth-value 1 (ignore-errors (aitools.process.domain:compile-line-pattern "(")))))
      (expect (typep condition 'aitools.process.domain:invalid-line-pattern) :to-be t)
      (expect (plusp (length (aitools.process.domain:invalid-line-pattern-message condition))) :to-be t)
      (expect (princ-to-string condition) :to-equal (aitools.process.domain:invalid-line-pattern-message condition))))

  (it "signals rather than reporting a non-match when a line exhausts the step budget"
    (let ((pattern (aitools.process.domain:compile-line-pattern "^(a+)+\\1$")))
      (expect (and (aitools.process.domain:line-pattern-matches-p pattern "aa") t) :to-be t)
      (expect (aitools.process.domain:line-pattern-matches-p pattern "ab") :to-be nil)
      (let ((condition (nth-value 1 (ignore-errors
                                     (aitools.process.domain:line-pattern-matches-p
                                      pattern (concatenate 'string (make-string 40 :initial-element #\a) "b"))))))
        (expect (typep condition 'aitools.process.domain:invalid-line-pattern) :to-be t)
        (expect (integerp (search "limit" (princ-to-string condition))) :to-be t)))))

(describe "aitools.process.domain bg ids"
  (it-each (("bg-1") ("bg-42") ("bg-123456789"))
      "accepts ~S"
      (id)
    (expect (aitools.process.domain:bg-id-p id) :to-be t))

  (it-each (("bg-0") ("bg-01") ("bg-") ("bg-1/../x") ("../bg-1") ("bg-1.json")
            ("BG-1") ("bg-１") ("bg-1234567890") (""))
      "rejects ~S"
      (id)
    (expect (aitools.process.domain:bg-id-p id) :to-be nil))

  (it "allocates the id after the highest one in use, ignoring other files"
    (expect (aitools.process.domain:next-bg-id '("bg-2" "bg-10" "notes")) :to-equal "bg-11")
    (expect (aitools.process.domain:next-bg-id '()) :to-equal "bg-1"))

  (it "reads ids from any of a process's files but records only from .json"
    (expect (aitools.process.domain:bg-file-id "bg-3.log") :to-equal "bg-3")
    (expect (aitools.process.domain:bg-record-id-from-file-name "bg-3.log") :to-be nil)
    (expect (aitools.process.domain:bg-record-id-from-file-name "bg-3.json") :to-equal "bg-3")
    (expect (aitools.process.domain:bg-file-id ".bg-3.tmp-1") :to-be nil))

  (it-each (("x") (".json") ("bg-01.log") ("bg-1.txt") ("bg-1.json.bak"))
      "reads no id from the file name ~S"
      (file-name)
    (expect (aitools.process.domain:bg-file-id file-name) :to-be nil)))
