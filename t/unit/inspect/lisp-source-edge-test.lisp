(in-package #:aitools.inspect.test)

(describe "Lisp scanner source edges"
  (it "recognizes an escaped Emacs Lisp character after a carriage return"
    (let ((source (format nil "(list~C?~C))" #\Return #\\)))
      (expect (lisp-balance-diagnostics (lines-of source) :emacs-lisp) :to-be nil)))

  (it "does not treat a character introducer inside a token as a character literal"
    (expect (lisp-balance-diagnostics
             (lines-of (format nil "(list?~C))" #\\))
             :emacs-lisp)
            :to-equal '((1 9 "unexpected ')' with no open delimiter"))))

  (it "skips escaped delimiters in strings and escaped symbols"
    (expect (lisp-balance-diagnostics
             (lines-of (format nil "(list ~Cescaped ~C~C )~C)" #\" #\\ #\" #\"))
             :common-lisp)
            :to-be nil)
    (expect (lisp-balance-diagnostics
             (lines-of (format nil "(list |escaped ~C~C )~C)" #\\ #\| #\|))
             :common-lisp)
            :to-be nil))

  (it "finds the closing line after an unmatched close before the first form"
    (expect (sexp-end-line (lines-of ")" "(let ((x 1))" "x)") 0 :common-lisp)
            :to-be 2)))

(describe "outline edge cases"
  (it "ignores malformed headings and includes deeper Markdown headings"
    (let* ((language (aitools.text.domain:language-for-path "guide.md"))
           (definitions (find-definitions
                         (lines-of "# Topic" "body" "#not-heading" "### Child"
                                   "####### Too many" "# Next")
                         language)))
      (expect (mapcar (function definition-name) definitions)
              :to-equal (list "Topic" "Child" "Next"))
      (expect (mapcar (function definition-end-line) definitions)
              :to-equal (list 5 5 6))))

  (it "ignores a stray closing brace before the body starts"
    (let ((definitions
            (find-definitions
             (lines-of "function f()" "}" "{" "}" "function g() {}")
             (aitools.text.domain:language-for-path "a.js"))))
      (expect (mapcar #'definition-name definitions) :to-equal '("f" "g"))
      (expect (mapcar #'definition-end-line definitions) :to-equal '(4 5)))))

(describe "UTF-8 count boundaries"
  (it "counts generic three- and four-byte leads after a BOM"
    (let ((bytes (coerce '(#xEF #xBB #xBF #xE1 #x80 #x80 #xF3 #x80 #x80 #x80)
                         '(vector (unsigned-byte 8)))))
      (expect (multiple-value-list
               (aitools.inspect.domain::%valid-utf8-text-counts bytes 3))
              :to-equal '(1 1 2 2 t))
      (expect (multiple-value-list (text-content-counts bytes))
              :to-equal '(1 1 2 2)))))
