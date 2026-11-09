(in-package #:aitools.process.test)

(describe "aitools.process.domain constructor defaults"
  (it "leaves optional bg-record fields absent when omitted"
    (let ((record (aitools.process.domain:make-bg-record
                   :id "bg-10" :argv '("sleep") :pid 4242 :started "now")))
      (expect (aitools.process.domain:bg-record-name record) :to-be nil)
      (expect (aitools.process.domain:bg-record-stop-signal record) :to-be nil)))

  (it "uses the documented log-slice defaults"
    (let ((slice (aitools.process.domain::make-log-slice)))
      (expect (aitools.process.domain:log-slice-lines slice) :to-be nil)
      (expect (aitools.process.domain:log-slice-next-offset slice) :to-be 0)
      (expect (aitools.process.domain:log-slice-truncated slice) :to-be nil)
      (expect (aitools.process.domain:log-slice-redactions slice) :to-be 0)))

  (it "uses the documented output-report defaults"
    (let ((report (aitools.process.domain::make-output-report)))
      (expect (aitools.process.domain:output-report-head report) :to-be nil)
      (expect (aitools.process.domain:output-report-tail report) :to-be nil)
      (expect (aitools.process.domain:output-report-total-lines report) :to-be 0)
      (expect (aitools.process.domain:output-report-truncated report) :to-be nil)
      (expect (aitools.process.domain:output-report-matches report) :to-be nil)
      (expect (aitools.process.domain:output-report-total-matches report) :to-be nil)
      (expect (aitools.process.domain:output-report-redactions report) :to-be 0)))

  (it "uses the documented process-outcome defaults"
    (let ((outcome (aitools.process.domain:make-process-outcome)))
      (expect (aitools.process.domain:process-outcome-exit-code outcome) :to-be nil)
      (expect (aitools.process.domain:process-outcome-signal outcome) :to-be nil)
      (expect (aitools.process.domain:process-outcome-timed-out outcome) :to-be nil)
      (expect (aitools.process.domain:process-outcome-duration-ms outcome) :to-be 0)
      (expect (aitools.process.domain:process-outcome-stdout outcome) :to-equal "")
      (expect (aitools.process.domain:process-outcome-stderr outcome) :to-equal "")
      (expect (aitools.process.domain:process-outcome-stdout-capped outcome) :to-be nil)
      (expect (aitools.process.domain:process-outcome-stderr-capped outcome) :to-be nil)
      (expect (aitools.process.domain:process-outcome-stdout-bytes outcome) :to-be nil)))

  (it "leaves non-port wait fields at their defaults"
    (let ((condition (aitools.process.domain:make-wait-condition :port 1)))
      (expect (aitools.process.domain:wait-condition-kind condition) :to-be :port)
      (expect (aitools.process.domain:wait-condition-path condition) :to-be nil)
      (expect (aitools.process.domain:wait-condition-pattern condition) :to-be nil)
      (expect (aitools.process.domain:wait-condition-bg-id condition) :to-be nil)
      (expect (aitools.process.domain:wait-condition-duration-text condition) :to-be nil)
      (expect (aitools.process.domain:wait-condition-duration-ms condition) :to-be nil))))

(describe "aitools.process.domain ASCII digits and signals"
  (it "accepts ASCII digit endpoints and rejects adjacent punctuation in bg IDs"
    (dolist (id '("bg-9" "bg-10"))
      (expect (aitools.process.domain:bg-id-p id) :to-be t))
    (dolist (id '("bg-/1" "bg-:1"))
      (expect (aitools.process.domain:bg-id-p id) :to-be nil)))

  (it "normalizes signal names and rejects unsupported input"
    (expect (aitools.process.domain:signal-number "term") :to-be 15)
    (expect (aitools.process.domain:signal-number "not-a-signal") :to-be nil)
    (expect (aitools.process.domain:signal-number 15) :to-be nil)))

(describe "aitools.process.domain terminal decoder boundaries"
  (it "strips CSI parameter, intermediate, and final bytes at range endpoints"
    (let ((escape (string (code-char 27))))
      (dolist (code '(#x30 #x3f))
        (expect (aitools.process.domain:strip-ansi-escapes
                 (concatenate 'string escape "[" (string (code-char code)) "mkept"))
                :to-equal "kept"))
      (dolist (code '(#x20 #x2f))
        (expect (aitools.process.domain:strip-ansi-escapes
                 (concatenate 'string escape "[" (string (code-char code)) "mkept"))
                :to-equal "kept")
        (expect (aitools.process.domain:strip-ansi-escapes
                 (concatenate 'string escape (string (code-char code)) "Bkept"))
                :to-equal "kept"))
      (dolist (code '(#x40 #x7e))
        (expect (aitools.process.domain:strip-ansi-escapes
                 (concatenate 'string escape "[" (string (code-char code)) "kept"))
                :to-equal "kept"))))

  (it "decodes only the requested octet range"
    (let ((octets (make-array 5 :element-type '(unsigned-byte 8)
                              :initial-contents '(120 65 66 121 122))))
      (expect (aitools.process.domain:decode-output-octets octets :start 1 :end 3)
              :to-equal "AB")))
)
