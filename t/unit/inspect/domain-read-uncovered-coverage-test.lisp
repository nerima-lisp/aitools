(in-package #:aitools.inspect.test)

(defun %domain-read-coverage-octets (&rest chunks)
  (coerce (apply #'concatenate '(vector (unsigned-byte 8)) chunks)
          '(simple-array (unsigned-byte 8) (*))))

(describe "UTF-8 continuation and range boundaries"
  (it "rejects missing and non-continuation bytes in multibyte sequences"
    (dolist (octets '((#xE2)
                      (#xE2 #x82)
                      (#xE2 #x41 #x80)
                      (#xF0)
                      (#xF0 #x90 #x80)
                      (#xF0 #x90 #x41 #x80)))
      (expect (multiple-value-list
               (aitools.inspect.domain::%valid-utf8-text-counts
                (coerce octets '(simple-array (unsigned-byte 8) (*))) 0))
              :to-equal '(0 0 0 0 nil))))

  (it "keeps a byte cut that is already at its lower bound"
    (let ((octets (%domain-read-coverage-octets #(0 0 0 226 130 172)))
          (start 3))
      (expect (aitools.inspect.domain::%utf8-cut octets start start)
              :to-be start)))
)

(describe "Lisp scanner empty and diagnostic limits"
  (it "returns no diagnostics when the requested limit is zero"
    (expect (aitools.inspect.domain:lisp-balance-diagnostics
             (lines-of ")" "(" ) :common-lisp :limit 0)
            :to-equal nil))

  (it "skips lines before the requested scanner start"
    (expect (aitools.inspect.domain:scan-lisp-delimiters
             #("(ignored)") :common-lisp
             (lambda (kind char line column)
               (declare (ignore kind char line column))
               nil)
             :start-line 1)
            :to-be nil))

  (it "recognizes a non-escaped Emacs Lisp character literal"
    (expect (aitools.inspect.domain:lisp-balance-diagnostics
             (lines-of "(list ?x)") :emacs-lisp)
            :to-be nil)))

(describe "definition and read rendering defaults"
  (it "constructs a definition with valid positive line numbers"
    (let ((definition
            (aitools.inspect.domain::make-definition 2 4 "function" "run")))
      (expect (aitools.inspect.domain:definition-line definition) :to-be 2)
      (expect (aitools.inspect.domain:definition-end-line definition) :to-be 4)))

  (it "returns no hexadecimal rows for an empty window"
    (expect (aitools.inspect.domain:hex-rows
             (%domain-read-coverage-octets #(1 2 3)) 2 2)
            :to-equal nil))

  (it "finds the changed span when redaction reaches both ends"
    (expect (multiple-value-list
             (aitools.inspect.domain::%redacted-span "secret" "[REDACTED]"))
            :to-equal '(0 6)))

  (it "does not emit strings when a chunk contains only controls"
    (let* ((emissions 0)
          (scan (aitools.inspect.domain::make-strings-scan
                 :min-length 2
                 :emit (lambda (offset text cut-offset)
                         (declare (ignore offset text cut-offset))
                         (incf emissions)))))
      (expect (aitools.inspect.domain:scan-strings-chunk
               scan (%domain-read-coverage-octets #(0 1 0)) 0 :final t)
              :to-be 3)
      (expect emissions :to-be 0))

    (let ((emissions nil))
      (expect (aitools.inspect.domain::extract-strings
               (%domain-read-coverage-octets #(0 1 0))
               :emit (lambda (offset text)
                       (push (list offset text) emissions)))
              :to-be nil)
      (expect emissions :to-be nil)))
)

(describe "similarity ranking results"
  (it "keeps nearest ties in their input order and drops distant items"
    (expect (aitools.inspect.domain:rank-similar
             "alpha" '("alpga" "alphi" "alpha" "omega")
             :count 2 :max-distance 1)
            :to-equal '("alpha" "alpga"))))

(describe "snapshot default options"
  (it "leaves omitted scan options at their defaults"
    (let ((snapshot (aitools.inspect.domain:make-snapshot "id" "created" nil)))
      (expect (list (aitools.inspect.domain:snapshot-glob snapshot)
                    (aitools.inspect.domain:snapshot-lang snapshot)
                    (aitools.inspect.domain:snapshot-no-ignore snapshot)
                    (aitools.inspect.domain:snapshot-skip-larger-than snapshot)
                    (aitools.inspect.domain:snapshot-newer snapshot))
              :to-equal '(nil nil nil nil nil)))))
