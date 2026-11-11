(in-package #:aitools.inspect.test)

(describe "read rendering redaction spans"
  (it "returns no span for unchanged text and bounds a changed secret"
    (expect (multiple-value-list
             (aitools.inspect.domain::%redacted-span "plain" "plain"))
            :to-equal '(nil))
    (expect (multiple-value-list
             (aitools.inspect.domain::%redacted-span
              "left secret right" "left [REDACTED_SECRET] right"))
            :to-equal '(5 11)))

  (it "masks only secret bytes in a widened hex window without mutating input"
    (let* ((secret (format nil "ghp_~A" (make-string 36 :initial-element #\a)))
           (text (format nil "GITHUB_TOKEN=~A~%" secret))
           (octets (string-bytes text))
           (token-start (search secret text))
           (original (copy-seq octets))
           (masked (aitools.inspect.domain::redact-hex-octets
                    octets (1+ token-start) (+ token-start 5))))
      (expect (not (eq masked octets)) :to-be t)
      (expect (every (lambda (index) (= (aref masked index) #x2A))
                     (loop for index from token-start below (+ token-start 4)
                           collect index))
              :to-be t)
      (expect (equalp octets original) :to-be t)
      (expect
       (let ((plain (string-bytes "ordinary text")))
         (eq (aitools.inspect.domain::redact-hex-octets
              plain 0 (length plain))
             plain))
       :to-be t))))

(describe "UTF-8 content-count boundaries"
  (it "counts a BOM-only file as empty and keeps U+3000 word boundaries"
    (let ((bom-only (coerce '(#xEF #xBB #xBF) '(vector (unsigned-byte 8))))
          (text (coerce '(#xEF #xBB #xBF #xE3 #x80 #x80 #x61 #x0D #x0A)
                        '(vector (unsigned-byte 8)))))
      (expect (multiple-value-list (text-content-counts bom-only))
              :to-equal '(0 0 0 0))
      (expect (multiple-value-list (text-content-counts text))
              :to-equal '(1 1 2 2)))))

(describe "format write nesting limits"
  (it "accepts the configured YAML depth and rejects the next container"
    (signals yaml-syntax-error
      (parse-yaml-write-document "items: [[1]]" :max-depth 2))
    (let ((value (parse-yaml-write-document "items: [[1]]" :max-depth 3)))
      (expect (length (json-object-get value "items")) :to-be 1)))

  (it "accepts the configured TOML depth and rejects the next container"
    (signals toml-syntax-error
      (parse-toml-write-document "items = [[1]]" :max-depth 2))
    (let ((value (parse-toml-write-document "items = [[1]]" :max-depth 3)))
      (expect (length (json-object-get value "items")) :to-be 1))))

(describe "Lisp delimiter scanner stop callback"
  (it "stops immediately after the delimiter requested by the callback"
    (let ((events nil))
      (expect (scan-lisp-delimiters
               (vector "(a)") :common-lisp
               (lambda (kind char line column)
                 (push (list kind char line column) events)
                 (when (eq kind :close) :stop)))
              :to-be :stopped)
      (expect (nreverse events)
              :to-equal (list (list :open #\( 0 0)
                                   (list :close #\) 0 2))))))
