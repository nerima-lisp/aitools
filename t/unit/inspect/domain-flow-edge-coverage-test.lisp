(in-package #:aitools.inspect.test)

(describe "UTF-8 lead boundaries"
  (it "accepts the constrained leaders and rejects their invalid boundaries"
    (let ((valid (coerce '(#xE0 #xA0 #x80 #xED #x9F #xBF
                           #xF0 #x90 #x80 #x80 #xF4 #x8F #xBF #xBF)
                         '(simple-array (unsigned-byte 8) (*)))))
      (expect (multiple-value-list
               (aitools.inspect.domain::%valid-utf8-text-counts valid 0))
              :to-equal '(1 1 4 4 t)))
    (dolist (octets '((#xE0 #x80 #x80) (#xED #xA0 #x80)
                      (#xF0 #x80 #x80 #x80) (#xF4 #x90 #x80 #x80)))
      (expect (multiple-value-list
               (aitools.inspect.domain::%valid-utf8-text-counts
                (coerce octets '(simple-array (unsigned-byte 8) (*))) 0))
              :to-equal '(0 0 0 0 nil))))

  (it "cuts a requested prefix back to the character boundary"
    (let ((octets (coerce '(#x61 #xE2 #x82 #xAC #x62)
                          '(simple-array (unsigned-byte 8) (*)))))
      (expect (aitools.inspect.domain::%utf8-cut octets 0 3) :to-be 1)
      (expect (aitools.inspect.domain::%utf8-cut octets 0 2) :to-be 1))))

(describe "selector and snapshot domain contracts"
  (it "returns no candidate when a range starts past an empty file"
    (expect (resolve #() (aitools.kernel.domain:make-range-selector "1"))
            :to-be :no-match))

  (it "ranks a missing symbol against definitions in a supported language"
    (expect (resolve (lines-of "(defun present () 1)")
                     (aitools.kernel.domain:make-symbol-selector "absent")
                     :path "a.lisp")
            :to-be :no-match)
    (expect (aitools.inspect.domain:rank-similar "alpha" '("omega")
                                                  :max-distance 1)
            :to-be nil))

  (it "rejects a symbol selector when a supplied language has no definitions"
    (let ((outcome
            (aitools.inspect.domain:resolve-line-selector/k
             #("(f)")
             (aitools.kernel.domain:make-symbol-selector "f")
             :language (aitools.text.domain::make-language :name "unknown")
             :on-selected (constantly :selected)
             :on-no-match (constantly :no-match)
             :on-ambiguous (constantly :ambiguous)
             :on-invalid (lambda (code message)
                           (declare (ignore message))
                           code))))
      (expect outcome :to-equal "input.unsupported-language")))

  (it "uses default snapshot options and rejects a non-string file path"
    (let ((snapshot (make-snapshot "id" "created" '())))
      (expect (snapshot-glob snapshot) :to-equal nil))
    (expect (decode-snapshot-test-entry "\"t\"" "[1, 1, 0, \"h\"]")
            :to-be :invalid)))

(describe "Lisp scanner callback boundary"
  (it "stops at an opening delimiter when requested"
    (expect (aitools.inspect.domain:scan-lisp-delimiters
             #("(ignored)") :common-lisp
             (lambda (kind char line column)
               (declare (ignore char line column))
               (when (eq kind :open) :stop)))
            :to-be :stopped)))
