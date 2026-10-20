(in-package #:aitools.search.test)

(describe "aitools.search.domain definition extents"
  (it "skips character literals, strings, `;` and `#|` comments when closing a Lisp form"
    (expect (outline-of "/w/a.lisp" (lines "(defun f ()" "  #\\( \"a\\\"(\" ; )" "  #| ) |# 1)" "(defun g ())"))
            :to-equal '((1 3 "function" "f") (4 4 "function" "g"))))

  (it "ends an unclosed Lisp form on the last line of the file"
    (expect (outline-of "/w/a.lisp" (lines "(defun g ())" "(defun h (" "  x"))
            :to-equal '((1 1 "function" "g") (2 3 "function" "h"))))

  (it "skips strings, escapes, and comments when closing a brace definition"
    (expect (outline-of "/w/a.rs" (lines "fn a() {" "  let s = \"}\\\"\";" "  // }" "  /* } */" "}"
                                         "fn b() {" "  let s = \"abc" "}"))
            :to-equal '((1 5 "function" "a") (6 8 "function" "b"))))

  (it "follows a brace on a later line, and otherwise ends the definition at its own line"
    (expect (outline-of "/w/a.rs" (lines "fn c()" "" "  {" "}" "fn d()" "fn e() {}"))
            :to-equal '((1 4 "function" "c") (5 5 "function" "d") (6 6 "function" "e"))))

  (it "ends an unclosed brace definition, or one in an unclosed comment, at the end of the file"
    (expect (outline-of "/w/a.rs" (format nil "fn f() {~%/* never closed"))
            :to-equal '((1 2 "function" "f"))))

  (it "closes a brace definition at its outermost brace and stops at an unclosed string"
    (expect (outline-of "/w/a.rs" (format nil "fn f() { { } }~%fn g() { \"abc"))
            :to-equal '((1 1 "function" "f") (2 2 "function" "g")))
    (expect (outline-of "/w/a.lisp" (format nil "(defun f () \"abc"))
            :to-equal '((1 1 "function" "f"))))

  (it "ends a brace definition followed only by blank lines at its own line"
    (expect (outline-of "/w/a.rs" (format nil "fn g()~%~%")) :to-equal '((1 1 "function" "g"))))

  (it-property "finds the matching close of generated nested Lisp forms"
      ((depth (gen-integer :min 1 :max 20)))
    (let ((content (apply #'lines
                          (append (list "(defun probe ()")
                                  (loop repeat depth collect "  (list")
                                  (loop repeat depth collect "  )")
                                  (list "  )")))))
      (expect (outline-of "/w/probe.lisp" content)
              :to-equal (list (list 1 (+ 2 (* 2 depth)) "function" "probe")))))

  (it-property "hides headings between generated matching Markdown fences"
      ((opening-length (gen-integer :min 3 :max 12))
       (closing-extra (gen-integer :min 0 :max 12)))
    (let ((opening (make-string opening-length :initial-element #\`))
          (closing (make-string (+ opening-length closing-extra) :initial-element #\`)))
      (expect (outline-of "/w/a.md" (lines "# before" opening "# hidden" closing "# after"))
              :to-equal '((1 4 "heading" "before") (5 5 "heading" "after"))))))
