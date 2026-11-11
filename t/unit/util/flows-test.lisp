;;;;
;;;; Application flows use cl-boundary-kit contexts with deterministic fakes.
;;;; TEST-BOUNDARIES makes both input readers fail when called, so a flow that
;;;; reads standard input without `--stdin` cannot pass.
(in-package #:aitools.util.test)

(defun request (&rest arguments)
  (apply #'make-input-request arguments))

(defun file-reader (files)
  "A cl-weave mock for synchronous file reads."
  (make-mock-function
   (lambda (path limit)
     (let ((entry (assoc path files :test #'string=)))
       (cond ((null entry) (values :missing))
             ((> (length (cdr entry)) limit) (values :too-large))
             (t (values :octets (cdr entry))))))))

(describe "aitools.util.application input rules"
  (it "fails with argument.invalid, without reading standard input, when no input is given"
    (let* ((stdin-reader (unused-reader "read-stdin-octets"))
           (boundaries (test-boundaries :read-stdin-octets stdin-reader)))
      (multiple-value-bind (kind fields)
          (run-flow #'util-encode-flow boundaries "base64" (request))
        (expect kind :to-be :error)
        (expect (getf fields :code) :to-equal "argument.invalid")
        (expect (getf fields :repairs) :not :to-be-falsy))
      (expect (mock-calls stdin-reader) :to-equal '())))

  (it "rejects more than one input source"
    (expect (getf (nth-value 1 (run-flow #'util-tokens-flow (test-boundaries) (request :content "a" :stdin t))) :code)
            :to-equal "argument.invalid"))

  (it "reads standard input only when --stdin is given"
    (multiple-value-bind (kind fields)
        (run-flow #'util-encode-flow (test-boundaries :read-stdin-octets (stdin-reader "hi")) "hex" (request :stdin t))
      (expect kind :to-be :ok)
      (expect (field fields "output") :to-equal "6869")))

  (it "rejects standard input that is not UTF-8 text"
    (let ((reader (make-mock-function
                 (lambda (limit)
                   (declare (ignore limit))
                   (values :octets (octets 255))))))
      (expect (getf (nth-value 1 (run-flow #'util-tokens-flow (test-boundaries :read-stdin-octets reader) (request :stdin t)))
                    :code)
              :to-equal "input.not-utf8")))

  (it "encodes --content-file bytes without decoding them"
    (let ((boundaries (test-boundaries :read-file-octets (file-reader (list (cons "bin" (octets 0 255 128)))))))
      (expect (field (nth-value 1 (run-flow #'util-encode-flow boundaries "base64" (request :content-file "bin"))) "output")
              :to-equal "AP+A")))

  (it "maps a missing --content-file to input.not-found and an oversized one to argument.invalid"
    (let ((boundaries (test-boundaries :read-file-octets (file-reader '()))))
      (expect (getf (nth-value 1 (run-flow #'util-encode-flow boundaries "hex" (request :content-file "nope"))) :code)
              :to-equal "input.not-found"))
    (let ((boundaries (test-boundaries :read-file-octets (make-mock-function
       (lambda (path limit)
         (declare (ignore path limit))
         (values :too-large))))))
      (expect (getf (nth-value 1 (run-flow #'util-encode-flow boundaries "hex" (request :content-file "big"))) :code)
              :to-equal "argument.invalid"))))

(describe "aitools.util.application util-encode-flow and util-decode-flow"
  (it-each (("base64") ("url") ("hex"))
      "round-trips text through ~A"
      (scheme)
    (let* ((text "héllo / wörld ✓")
           (encoded (field (nth-value 1 (run-flow #'util-encode-flow (test-boundaries) scheme (request :content text)))
                           "output")))
      (multiple-value-bind (kind fields) (run-flow #'util-decode-flow (test-boundaries) scheme (request :content encoded))
        (expect kind :to-be :ok)
        (expect (field fields "output") :to-equal text)
        (expect (assoc "binary" fields :test #'string=) :to-be nil))))

  (it "returns non-UTF-8 results as binary:true with output_hex and no output"
    (multiple-value-bind (kind fields) (run-flow #'util-decode-flow (test-boundaries) "base64" (request :content "/wCA"))
      (expect kind :to-be :ok)
      (expect (field fields "binary") :to-be t)
      (expect (field fields "output_hex") :to-equal "ff0080")
      (expect (field fields "bytes") :to-be 3)
      (expect (assoc "output" fields :test #'string=) :to-be nil)))

  (it "fails with input.syntax-error (exit code 1) on malformed input"
    (let ((code (getf (nth-value 1 (run-flow #'util-decode-flow (test-boundaries) "base64" (request :content "@@@@"))) :code)))
      (expect code :to-equal "input.syntax-error")
      (expect (aitools.protocol.domain:error-code-exit-code code) :to-be 1)))

  (it "rejects an unknown scheme from a direct caller"
    (expect (getf (nth-value 1 (run-flow #'util-encode-flow (test-boundaries) "rot13" (request :content "a"))) :code)
            :to-equal "argument.invalid")))

(describe "aitools.util.application util-redact-flow and util-tokens-flow"
  (it "masks known secret formats and counts them"
    (let ((fields (nth-value 1 (run-flow #'util-redact-flow (test-boundaries)
                                         (request :content "password=hunter2 key AKIAIOSFODNN7EXAMPLE")))))
      (expect (field fields "text") :to-equal "password=[REDACTED_SECRET] key [REDACTED_SECRET]")
      (expect (field fields "redactions") :to-be 2)))

  (it "reports the statistic fields in the documented order"
    (let ((fields (nth-value 1 (run-flow #'util-tokens-flow (test-boundaries) (request :content "a bc")))))
      (expect (mapcar #'car fields)
              :to-equal '("approx_tokens" "chars" "bytes" "lines" "words" "max_line_chars"))
      (expect (field fields "words") :to-be 2))))

(describe "aitools.util.application util-calc-flow"
  (it "returns input, result, and exact for a non-integer rational"
    (let ((fields (nth-value 1 (run-flow #'util-calc-flow (test-boundaries) "1/3 + 2**100" nil 10))))
      (expect (field fields "input") :to-equal "1/3 + 2**100")
      (expect (field fields "result") :to-equal "1267650600228229401496703205376.3333333333")
      (expect (field fields "exact") :to-equal "3802951800684688204490109616129/3")))

  (it "omits exact for an integer result"
    (let ((fields (nth-value 1 (run-flow #'util-calc-flow (test-boundaries) "2**100" nil 10))))
      (expect (field fields "result") :to-equal "1267650600228229401496703205376")
      (expect (assoc "exact" fields :test #'string=) :to-be nil)))

  (it "maps division by zero and syntax errors to input.syntax-error"
    (dolist (text '("1/0" "x + 1" "f(x) = x"))
      (expect (getf (nth-value 1 (run-flow #'util-calc-flow (test-boundaries) text nil 10)) :code)
              :to-equal "input.syntax-error")))

  (it "reads the expression from standard input only with --stdin"
    (let ((fields (nth-value 1 (run-flow #'util-calc-flow (test-boundaries :read-stdin-octets (stdin-reader (format nil "6 * 7~%")))
                                         nil t 10))))
      (expect (field fields "result") :to-equal "42"))
    (expect (getf (nth-value 1 (run-flow #'util-calc-flow (test-boundaries) nil nil 10)) :code) :to-equal "argument.invalid")
    (expect (getf (nth-value 1 (run-flow #'util-calc-flow (test-boundaries) "1" t 10)) :code) :to-equal "argument.invalid"))

  (it "rejects --decimals outside its range"
    (expect (getf (nth-value 1 (run-flow #'util-calc-flow (test-boundaries) "1" nil 1001)) :code) :to-equal "argument.invalid")))

(describe "aitools.util.application util-uuid-flow"
  (it "returns --count v4 values from the uuid-source boundary"
    (let* ((uuids '("00000000-0000-4000-8000-000000000001" "00000000-0000-4000-8000-000000000002"))
           (boundaries (test-boundaries :uuid-source (cl-boundary-kit:make-test-uuid-source :values uuids))))
      (expect (field (nth-value 1 (run-flow #'util-uuid-flow boundaries "v4" 2)) "values") :to-equal uuids)))

  (it "returns strictly increasing v7 values stamped with the clock boundary's time"
    (let* ((clock (cl-boundary-kit:make-fake-clock :start 1700000000000))
           (values (field (nth-value 1 (run-flow #'util-uuid-flow (test-boundaries :clock clock) "v7" 20)) "values")))
      (expect (length values) :to-be 20)
      (expect (loop for (a b) on values while b always (string< a b)) :to-be-truthy)
      (expect (every (lambda (uuid) (and (= (uuid-version uuid) 7) (= (uuid-variant-bits uuid) 2))) values)
              :to-be-truthy)
      (expect (uuid-v7-ms (first values)) :to-be 1700000000000)))

  (it "rejects --count outside 1..1000 and an unknown kind"
    (expect (getf (nth-value 1 (run-flow #'util-uuid-flow (test-boundaries) "v4" 0)) :code) :to-equal "argument.invalid")
    (expect (getf (nth-value 1 (run-flow #'util-uuid-flow (test-boundaries) "v4" 1001)) :code) :to-equal "argument.invalid")
    (expect (getf (nth-value 1 (run-flow #'util-uuid-flow (test-boundaries) "v1" 1)) :code) :to-equal "argument.invalid")))

(describe "aitools.util.application util-random-flow"
  (it "returns --count values of --length characters from the alphabet"
    (let ((values (field (nth-value 1 (run-flow #'util-random-flow (test-boundaries) 40 "base64url" 3)) "values")))
      (expect (length values) :to-be 3)
      (expect (every (lambda (value) (= (length value) 40)) values) :to-be-truthy)
      (expect (every (lambda (value)
                       (every (lambda (char) (or (alphanumericp char) (find char "-_"))) value))
                     values)
              :to-be-truthy)))

  (it "draws from the random-source boundary"
    (let* ((source (cl-boundary-kit:make-deterministic-random-source :seed 3))
           (original (symbol-function (quote cl-boundary-kit:random-source-bytes)))
           (draws nil))
      (cl-weave:with-mocked-functions
          (((symbol-function (quote cl-boundary-kit:random-source-bytes))
            (lambda (actual-source size)
              (when (eq actual-source source) (push size draws))
              (funcall original actual-source size))))
        (run-flow (function util-random-flow) (test-boundaries :random-source source) 8 "hex" 1))
      (expect draws :not :to-be-falsy)))

  (it "rejects out-of-range --length and an unknown alphabet"
    (expect (getf (nth-value 1 (run-flow #'util-random-flow (test-boundaries) 0 "hex" 1)) :code) :to-equal "argument.invalid")
    (expect (getf (nth-value 1 (run-flow #'util-random-flow (test-boundaries) 4097 "hex" 1)) :code) :to-equal "argument.invalid")
    (expect (getf (nth-value 1 (run-flow #'util-random-flow (test-boundaries) 8 "emoji" 1)) :code) :to-equal "argument.invalid")))

(defun outcome-reader (outcome &rest arguments)
  "A cl-weave mock returning one synchronous input-reader result."
  (make-mock-function
   (lambda (&rest call-arguments)
     (declare (ignore call-arguments))
     (if (eq outcome :failure)
         (values :failure (first arguments))
         (values outcome)))))

(describe "aitools.util.application input failures"
  (it-each ((:content-file :failure ("boom") "environment.io" "cannot read f: boom")
            (:stdin :too-large () "argument.invalid" "standard input exceeds the 67108864 byte input limit")
            (:stdin :failure ("boom") "environment.io" "cannot read standard input: boom"))
      "maps a ~S reader's ~S to ~*~A"
      (source outcome arguments code message)
    (let* ((reader (apply #'outcome-reader outcome arguments))
           (boundaries (if (eq source :stdin) (test-boundaries :read-stdin-octets reader) (test-boundaries :read-file-octets reader)))
           (request (if (eq source :stdin) (request :stdin t) (request :content-file "f"))))
      (multiple-value-bind (kind fields) (run-flow #'util-tokens-flow boundaries request)
        (expect kind :to-be :error)
        (expect (getf fields :code) :to-equal code)
        (expect (getf fields :message) :to-equal message)
        (expect (getf fields :repairs) :not :to-be-falsy)))))

(describe "aitools.util.application flows over --content-file bytes"
  (it "counts and redacts --content-file bytes decoded leniently"
    (let ((boundaries (test-boundaries :read-file-octets (file-reader (list (cons "f" (octets 97 255 10 98)))))))
      (let ((fields (nth-value 1 (run-flow #'util-tokens-flow boundaries (request :content-file "f")))))
        (expect (field fields "chars") :to-be 4)
        (expect (field fields "bytes") :to-be 4)
        (expect (field fields "lines") :to-be 2))
      (expect (field (nth-value 1 (run-flow #'util-redact-flow boundaries (request :content-file "f"))) "text")
              :to-equal (format nil "a~C~%b" (code-char #xfffd)))))

  (it "rejects an unknown decode scheme and an out-of-range random --count"
    (multiple-value-bind (kind fields) (run-flow #'util-decode-flow (test-boundaries) "rot13" (request :content "a"))
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "argument.invalid")
      (expect (getf fields :message) :to-contain "\"rot13\""))
    (dolist (count '(0 1001))
      (expect (getf (nth-value 1 (run-flow #'util-random-flow (test-boundaries) 8 "hex" count)) :code)
              :to-equal "argument.invalid"))))

