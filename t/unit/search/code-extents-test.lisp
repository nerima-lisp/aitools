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
              :to-equal '((1 4 "heading" "before") (5 5 "heading" "after")))))

  (it "stops an unterminated JavaScript string at a newline"
    (expect (outline-of "/w/a.js"
                        (lines "function a() {"
                               "  const close = '}';"
                               "  return close;"
                               "}"
                               "function b() {}"))
            :to-equal '((1 4 "function" "a") (5 5 "function" "b")))
    (expect (outline-of "/w/a.js"
                        (lines "function incomplete() {"
                               "  const close = 'unterminated"
                               "  }"
                               "function later() {}"))
            :to-equal '((1 3 "function" "incomplete") (4 4 "function" "later"))))

  (it "skips JavaScript template literals"
    (expect (outline-of "/w/a.js"
                        (lines "function a() {"
                               "  const close = `}`;"
                               "  return close;"
                               "}"
                               "function b() {}"))
            :to-equal '((1 4 "function" "a") (5 5 "function" "b"))))

  (it "skips nested JavaScript template substitutions"
    (expect (outline-of "/w/a.ts"
                        (lines "function a() {"
                               "  const value = `${`}`}`;"
                               "  return value;"
                               "}"
                               "function b() {}"))
            :to-equal '((1 4 "function" "a") (5 5 "function" "b"))))

  (it "skips Go rune literals"
    (expect (outline-of "/w/a.go"
                        (lines "func a() {"
                               "  close := '}'"
                               "}"
                               "func b() {}"))
            :to-equal '((1 3 "function" "a") (4 4 "function" "b"))))

  (it "skips Go raw string literals"
    (expect (outline-of "/w/a.go"
                        (lines "func a() {"
                               "  close := `}`"
                               "}"
                               "func b() {}"))
            :to-equal '((1 3 "function" "a") (4 4 "function" "b"))))

  (it "keeps Rust lifetimes in function signatures outside string skipping"
    (expect (outline-of "/w/a.rs"
                        (lines "fn borrow<'a>(x: &'a str) {"
                               "  use_value(x);"
                               "}"
                               "fn later() {}"))
            :to-equal '((1 3 "function" "borrow") (4 4 "function" "later"))))

  (it "handles complete and incomplete Rust character literals"
    (expect (outline-of "/w/a.rs"
                        (lines "fn close_char() {"
                               "  let close = '}';"
                               "  let unicode = '\\u{7d}';"
                               "  let hex = '\\x7d';"
                               "  let newline = '\\n';"
                               "  use_value(close);"
                               "}"
                               "fn later() {}"))
            :to-equal '((1 7 "function" "close_char") (8 8 "function" "later")))
    (expect (outline-of "/w/a.rs"
                        (lines "fn incomplete_char() {"
                               "  let value = '"
                               "  }"
                               "fn later() {}"))
            :to-equal '((1 3 "function" "incomplete_char") (4 4 "function" "later")))
    (expect (outline-of "/w/a.rs"
                        (lines "fn incomplete_escape() {"
                               (format nil "  let value = '~C" #\\)
                               "  }"
                               "fn later() {}"))
            :to-equal '((1 3 "function" "incomplete_escape") (4 4 "function" "later")))
    (expect (outline-of "/w/a.rs"
                        (lines "fn incomplete_unicode() {"
                               "  let value = '\\u{"
                               "  }'"
                               "fn later() {}"))
            :to-equal '((1 3 "function" "incomplete_unicode") (4 4 "function" "later"))))

  (it "skips Rust raw and byte strings with matching delimiters"
    (expect (outline-of "/w/a.rs"
                        (lines "fn close_raw() {"
                               "  let close = r#\"}\"#;"
                               "  let plain = r\"}\";"
                               "  let bytes = b\"}\";"
                               "  let byte_raw = br##\"}\"##;"
                               "  let shorter = r##\" } \"# } still inside \"##;"
                               "  use_value(close);"
                               "}"
                               "fn later() {}"))
            :to-equal '((1 8 "function" "close_raw") (9 9 "function" "later")))
    (expect (outline-of "/w/a.rs"
                        (lines "fn incomplete_raw() {"
                               "  let value = r##\" } \"#"
                               "  } inside"))
            :to-equal '((1 3 "function" "incomplete_raw"))))

  (it "skips nested Common Lisp and Scheme block comments"
    (expect (outline-of "/w/a.lisp"
                        (lines "(defun f ()"
                               "  #| outer #| inner |# ) |#"
                               "  (work)"
                               ")"
                               "(defun g ())"))
            :to-equal '((1 4 "function" "f") (5 5 "function" "g")))
    (expect (outline-of "/w/a.scm"
                        (lines "(define (f)"
                               "  #| outer #| inner |# ) |#"
                               "  (work)"
                               ")"
                               "(define (g))"))
            :to-equal '((1 4 "function" "f") (5 5 "function" "g"))))

  (it "skips nested Rust block comments"
    (expect (outline-of "/w/a.rs"
                        (lines "fn a() {"
                               "  /* outer /* inner */ } */"
                               "  work();"
                               "}"
                               "fn b() {}"))
            :to-equal '((1 4 "function" "a") (5 5 "function" "b"))))

  (it "keeps JavaScript block comments non-nesting"
    (expect (outline-of "/w/a.js"
                        (lines "function a() {"
                               "  /* outer /* inner */ }"
                               "function b() {}"))
            :to-equal '((1 2 "function" "a") (3 3 "function" "b"))))

  (it "skips shell single-quoted strings"
    (expect (outline-of "/w/a.sh"
                        (lines "function a() {"
                               "  value='}'"
                               "}"
                               "function b() {}"))
            :to-equal '((1 3 "function" "a") (4 4 "function" "b"))))

  (it "skips Nix indented strings"
    (expect (outline-of "/w/a.nix"
                        (lines "attrs = {"
                               "  text = ''"
                               "    } inside"
                               "    ''' } inside"
                               "    ''${x} } inside"
                               "    ''\\ } inside"
                               "  '';"
                               "}"
                               "next = {};"))
            :to-equal '((1 8 "attribute" "attrs")
                        (2 7 "attribute" "text")
                        (9 9 "attribute" "next"))))
)
