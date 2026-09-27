;;;; t/unit/process/domain-records-test.lisp
;;;;
;;;; aitools.process.domain bg records, log slices, wait conditions,
;;;; run-result fields, and shell words.
(in-package #:aitools.process.test)

(defun sample-record (&key (id "bg-1") stop-signal)
  (aitools.process.domain:make-bg-record :id id :name "web" :argv '("sleep" "60") :pid 4242
                                         :started "2026-09-26T00:00:00Z" :stop-signal stop-signal))

(defun record-rejected-p (text id)
  (handler-case (progn (aitools.process.domain:parse-bg-record text id) nil)
    (aitools.process.domain:invalid-bg-record () t)))

(describe "aitools.process.domain bg records"
  (it "round-trips through its JSON form"
    (let ((parsed (aitools.process.domain:parse-bg-record
                   (aitools.process.domain:serialize-bg-record (sample-record :stop-signal 9)) "bg-1")))
      (expect (aitools.process.domain:bg-record-argv parsed) :to-equal '("sleep" "60"))
      (expect (aitools.process.domain:bg-record-pid parsed) :to-be 4242)
      (expect (aitools.process.domain:bg-record-name parsed) :to-equal "web")
      (expect (aitools.process.domain:bg-record-stop-signal parsed) :to-be 9)))

  (it "rejects a record read under another id"
    (expect (record-rejected-p (aitools.process.domain:serialize-bg-record (sample-record)) "bg-2") :to-be t))

  (it-each (("not JSON" "not json")
            ("a non-object" "[]")
            ("a missing key" "{\"id\":\"bg-1\",\"name\":null,\"argv\":[\"a\"],\"pid\":10,\"started\":\"x\"}")
            ("an unknown key" "{\"id\":\"bg-1\",\"name\":null,\"argv\":[\"a\"],\"pid\":10,\"started\":\"x\",\"stop_signal\":null,\"cmd\":\"rm\"}")
            ("an empty argv" "{\"id\":\"bg-1\",\"name\":null,\"argv\":[],\"pid\":10,\"started\":\"x\",\"stop_signal\":null}")
            ("pid 1" "{\"id\":\"bg-1\",\"name\":null,\"argv\":[\"a\"],\"pid\":1,\"started\":\"x\",\"stop_signal\":null}")
            ("a negative pid" "{\"id\":\"bg-1\",\"name\":null,\"argv\":[\"a\"],\"pid\":-5,\"started\":\"x\",\"stop_signal\":null}")
            ("a string pid" "{\"id\":\"bg-1\",\"name\":null,\"argv\":[\"a\"],\"pid\":\"10\",\"started\":\"x\",\"stop_signal\":null}")
            ("a numeric id" "{\"id\":1,\"name\":null,\"argv\":[\"a\"],\"pid\":10,\"started\":\"x\",\"stop_signal\":null}")
            ("a numeric name" "{\"id\":\"bg-1\",\"name\":5,\"argv\":[\"a\"],\"pid\":10,\"started\":\"x\",\"stop_signal\":null}")
            ("a non-string argv entry" "{\"id\":\"bg-1\",\"name\":null,\"argv\":[\"a\",1],\"pid\":10,\"started\":\"x\",\"stop_signal\":null}")
            ("a numeric start time" "{\"id\":\"bg-1\",\"name\":null,\"argv\":[\"a\"],\"pid\":10,\"started\":5,\"stop_signal\":null}")
            ("stop_signal 0" "{\"id\":\"bg-1\",\"name\":null,\"argv\":[\"a\"],\"pid\":10,\"started\":\"x\",\"stop_signal\":0}")
            ("stop_signal 65" "{\"id\":\"bg-1\",\"name\":null,\"argv\":[\"a\"],\"pid\":10,\"started\":\"x\",\"stop_signal\":65}")
            ("a string stop_signal" "{\"id\":\"bg-1\",\"name\":null,\"argv\":[\"a\"],\"pid\":10,\"started\":\"x\",\"stop_signal\":\"9\"}"))
      "rejects a record with ~A"
      (description text)
    (declare (ignore description))
    (expect (record-rejected-p text "bg-1") :to-be t))

  (it "says which field it rejected when reported"
    (let ((condition (nth-value 1 (ignore-errors
                                   (aitools.process.domain:parse-bg-record
                                    "{\"id\":\"bg-1\",\"name\":null,\"argv\":[\"a\"],\"pid\":1,\"started\":\"x\",\"stop_signal\":null}"
                                    "bg-1")))))
      (expect (typep condition 'aitools.process.domain:invalid-bg-record) :to-be t)
      (expect (princ-to-string condition) :to-equal "bg record bg-1 has an invalid pid")))

  (it "reads a null name and stop_signal as absent"
    (let ((parsed (aitools.process.domain:parse-bg-record
                   "{\"id\":\"bg-1\",\"name\":null,\"argv\":[\"a\"],\"pid\":10,\"started\":\"x\",\"stop_signal\":null}"
                   "bg-1")))
      (expect (aitools.process.domain:bg-record-name parsed) :to-be nil)
      (expect (aitools.process.domain:bg-record-stop-signal parsed) :to-be nil)))

  (it "reads a shell exit status, mapping 128+N to signal N"
    (expect (multiple-value-list (aitools.process.domain:parse-exit-status (format nil "0~%"))) :to-equal '(0 nil))
    (expect (multiple-value-list (aitools.process.domain:parse-exit-status "3")) :to-equal '(3 nil))
    (expect (multiple-value-list (aitools.process.domain:parse-exit-status "143")) :to-equal '(nil 15))
    (expect (multiple-value-list (aitools.process.domain:parse-exit-status "")) :to-equal '(nil nil))
    (expect (multiple-value-list (aitools.process.domain:parse-exit-status "abc")) :to-equal '(nil nil)))

  (it "formats start times as UTC RFC 3339"
    (expect (aitools.process.domain:format-utc-timestamp (encode-universal-time 5 4 3 2 1 2026 0))
            :to-equal "2026-01-02T03:04:05Z"))

  (it "validates --name labels"
    (expect (aitools.process.domain:bg-name-valid-p "web server") :to-be t)
    (expect (aitools.process.domain:bg-name-valid-p "") :to-be nil)
    (expect (aitools.process.domain:bg-name-valid-p (format nil "a~%b")) :to-be nil)
    (expect (aitools.process.domain:bg-name-valid-p (make-string 65 :initial-element #\a)) :to-be nil)))

(defun slice (text base &rest options)
  (apply #'aitools.process.domain:slice-log (string-bytes text) base
         (append options (list :strip-p t))))

(describe "aitools.process.domain slice-log"
  (it "returns the last lines in tail mode and points next_offset at the end"
    (let ((slice (slice (numbered-lines 5) 0 :count 2 :final-p t)))
      (expect (aitools.process.domain:log-slice-lines slice) :to-equal '("line 4" "line 5"))
      (expect (aitools.process.domain:log-slice-truncated slice) :to-be t)
      (expect (aitools.process.domain:log-slice-next-offset slice) :to-be (length (numbered-lines 5)))))

  (it "pages forward from an offset without skipping or repeating a line"
    (let* ((text (numbered-lines 5))
           (first (slice text 0 :count 2 :from-p t :final-p t))
           (offset (aitools.process.domain:log-slice-next-offset first))
           (second (slice (subseq text offset) offset :count 10 :from-p t :final-p t)))
      (expect (aitools.process.domain:log-slice-lines first) :to-equal '("line 1" "line 2"))
      (expect (aitools.process.domain:log-slice-truncated first) :to-be t)
      (expect offset :to-be (length (format nil "line 1~%line 2~%")))
      (expect (aitools.process.domain:log-slice-lines second) :to-equal '("line 3" "line 4" "line 5"))
      (expect (aitools.process.domain:log-slice-truncated second) :to-be nil)))

  (it "holds back an unterminated last line while the writer may still extend it"
    (let ((running (slice (format nil "done~%partial") 100 :count 10 :from-p t :final-p nil))
          (ended (slice (format nil "done~%partial") 100 :count 10 :from-p t :final-p t)))
      (expect (aitools.process.domain:log-slice-lines running) :to-equal '("done"))
      (expect (aitools.process.domain:log-slice-next-offset running) :to-be 105)
      (expect (aitools.process.domain:log-slice-lines ended) :to-equal '("done" "partial"))
      (expect (aitools.process.domain:log-slice-next-offset ended) :to-be 112)))

  (it "keeps only matching lines under --grep"
    (let ((slice (slice (numbered-lines 12) 0 :count 10 :from-p t :final-p t
                        :pattern (aitools.process.domain:compile-line-pattern "^line 1[0-9]$"))))
      (expect (aitools.process.domain:log-slice-lines slice) :to-equal '("line 10" "line 11" "line 12"))))

  (it "drops the partial first line when earlier bytes were skipped"
    (let ((slice (slice (format nil "ne 1~%line 2~%") 3 :count 10 :final-p t :omitted-before-p t)))
      (expect (aitools.process.domain:log-slice-lines slice) :to-equal '("line 2"))
      (expect (aitools.process.domain:log-slice-truncated slice) :to-be t)))

  (it "counts redactions only in the lines it returns"
    (let ((slice (slice (format nil "~A~%ok~%" *dummy-token*) 0 :count 1 :final-p t)))
      (expect (aitools.process.domain:log-slice-lines slice) :to-equal '("ok"))
      (expect (aitools.process.domain:log-slice-redactions slice) :to-be 0))))

(describe "aitools.process.domain wait conditions"
  (it-each (((:file "a.log" :pattern "ready") :file-pattern)
            ((:port 8080) :port)
            ((:bg "bg-1" :pattern "up") :bg-pattern)
            ((:bg "bg-1" :exit t) :bg-exit)
            ((:duration-text "2s" :duration-ms 2000) :duration))
      "accepts ~S as ~S"
      (arguments kind)
    (expect (aitools.process.domain:wait-condition-kind (apply #'aitools.process.domain:make-wait-condition arguments))
            :to-be kind))

  (it-each ((())
            ((:file "a" :pattern "x" :port 80))
            ((:file "a"))
            ((:pattern "x"))
            ((:bg "bg-1"))
            ((:bg "bg-1" :pattern "x" :exit t))
            ((:port 80 :exit t))
            ((:port 80 :pattern "x")))
      "rejects ~S"
      (arguments)
    (multiple-value-bind (condition message) (apply #'aitools.process.domain:make-wait-condition arguments)
      (expect condition :to-be nil)
      (expect (stringp message) :to-be t)))

  (it "renders a condition back into its flags"
    (expect (aitools.process.domain:wait-condition-arguments
             (aitools.process.domain:make-wait-condition :bg "bg-2" :pattern "a b"))
            :to-equal '("--bg" "bg-2" "--pattern" "a b")))

  (it "names the misplaced --pattern in its rejection"
    (expect (nth-value 1 (aitools.process.domain:make-wait-condition :port 80 :pattern "x"))
            :to-equal "--pattern only applies to --file or --bg"))

  (it-each (((:file "a.log" :pattern "ready") ("--file" "a.log" "--pattern" "ready"))
            ((:port 8080) ("--port" "8080"))
            ((:bg "bg-1" :exit t) ("--bg" "bg-1" "--exit"))
            ((:duration-text "2s" :duration-ms 2000) ("--duration" "2s")))
      "renders ~S as ~S"
      (arguments flags)
    (expect (aitools.process.domain:wait-condition-arguments
             (apply #'aitools.process.domain:make-wait-condition arguments))
            :to-equal flags)))

(describe "aitools.process.domain run-result-fields"
  (it-each (((:stdout-capped t)) ((:stderr-capped t)))
      "marks capture_capped when ~S"
      (arguments)
    (let ((report (summarize "a")))
      (expect (field (aitools.process.domain:run-result-fields
                      (apply #'aitools.process.domain:make-process-outcome :exit-code 0 arguments) report report)
                     "capture_capped")
              :to-be t)))

  (it "leaves capture_capped out when both streams fit"
    (let ((report (summarize "a")))
      (expect (assoc "capture_capped"
                     (aitools.process.domain:run-result-fields
                      (aitools.process.domain:make-process-outcome :exit-code 0) report report)
                     :test #'string=)
              :to-be nil)))

  (it "names the --stdout-to file and its size instead of head and tail"
    (let* ((report (summarize "a"))
           (written (field (aitools.process.domain:run-result-fields
                            (aitools.process.domain:make-process-outcome :exit-code 0 :stdout-bytes 9) report report
                            :stdout-path "out.txt")
                           "stdout"))
           (unwritten (field (aitools.process.domain:run-result-fields
                              (aitools.process.domain:make-process-outcome :exit-code 0) report report
                              :stdout-path "out.txt")
                             "stdout")))
      (expect (json-alist-value written "path") :to-equal "out.txt")
      (expect (json-alist-value written "bytes") :to-be 9)
      (expect (json-alist-value written "head" :absent) :to-be :absent)
      (expect (json-alist-value unwritten "bytes") :to-be 0))))

(describe "aitools.process.domain shell words"
  (it "quotes only what a shell would interpret"
    (expect (aitools.process.domain:command-line "aitools" "bg" "logs" "bg-1" "--from" "12")
            :to-equal "aitools bg logs bg-1 --from 12")
    (expect (aitools.process.domain:command-line "aitools" "wait" (list "--pattern" "it's up"))
            :to-equal "aitools wait --pattern 'it'\\''s up'")
    (expect (aitools.process.domain:command-line "") :to-equal "''")))
