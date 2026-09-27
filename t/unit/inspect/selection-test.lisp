;;;; t/unit/inspect/selection-test.lisp
;;;;
;;;; The public selector API (the selector rules in
;;;; docs/src/reference/commands.md, "Conventions shared by many commands") and the
;;;; pieces under it: option parsing under the one-selector rule, each
;;;; selector's range, :SYMBOL extents per language, and the Lisp scanner's
;;;; dialect rules.
(in-package #:aitools.inspect.test)

(defun parse-options (command &rest options)
  "(VALUES :SELECTOR selector) / (VALUES :NONE) / (VALUES :INVALID message)."
  (apply #'parse-selector-options/k command
         :on-selector (lambda (selector) (values :selector selector))
         :on-none (lambda () (values :none))
         :on-invalid (lambda (message repairs) (declare (ignore repairs)) (values :invalid message))
         options))

(defun resolve (lines selector &key path)
  "(VALUES outcome payload) of RESOLVE-SELECTOR/K."
  (resolve-selector/k lines selector
                      :path path
                      :on-selected (lambda (ranges) (values :selected ranges))
                      :on-no-match (lambda (candidates) (values :no-match candidates))
                      :on-ambiguous (lambda (candidates) (values :ambiguous candidates))
                      :on-invalid (lambda (code message) (declare (ignore message)) (values :invalid code))))

(defun symbol-range (text path name &key kind)
  (multiple-value-bind (outcome ranges)
      (resolve (split-text-lines text) (aitools.kernel.domain:make-symbol-selector name :kind kind) :path path)
    (and (eq outcome :selected) (first ranges))))

(describe "parse-selector-options/k"
  (it "builds each selector kind and reports none"
    (expect (parse-options :read) :to-be :none)
    (expect (aitools.kernel.domain:selector-kind (nth-value 1 (parse-options :read :range "2:4"))) :to-be :range)
    (expect (aitools.kernel.domain:selector-kind (nth-value 1 (parse-options :edit :symbol "f"))) :to-be :symbol)
    (expect (aitools.kernel.domain:selector-exclusive (nth-value 1 (parse-options :read :between '("a" "b") :exclusive t)))
            :to-be t)
    (expect (aitools.kernel.domain:selector-invert (nth-value 1 (parse-options :read :match "x" :invert t))) :to-be t))

  (it-each (("two selectors" (:range "1" :match "x"))
       ("a selector with an extra exclusive flag" (:range "1" :extra-exclusive (("--tail" . 3))))
       ("--exclusive without --between" (:exclusive t))
       ("--invert without --match" (:invert t))
       ("--kind without --symbol" (:kind "function"))
       ("a malformed --range" (:range "5:2"))
       ("a --between with one pattern" (:between ("a")))
       ("an empty --match" (:match "")))
      "rejects ~A"
      (label options)
    (declare (ignore label))
    (expect (apply #'parse-options :read options) :to-be :invalid)))

(describe "resolve-selector/k"
  (let ((lines (lines-of "a" "b" "c" "d")))
    (it "selects --range N, S:E clamped, and S:"
      (expect (nth-value 1 (resolve lines (aitools.kernel.domain:make-range-selector "2"))) :to-equal '((2 . 2)))
      (expect (nth-value 1 (resolve lines (aitools.kernel.domain:make-range-selector "3:99"))) :to-equal '((3 . 4)))
      (expect (nth-value 1 (resolve lines (aitools.kernel.domain:make-range-selector "2:"))) :to-equal '((2 . 4))))

    (it "reports a start past the end as no match"
      (expect (resolve lines (aitools.kernel.domain:make-range-selector "5")) :to-be :no-match))

    (it "selects every --match line, or with --invert every other"
      (expect (nth-value 1 (resolve lines (aitools.kernel.domain:make-match-selector "[bd]"))) :to-equal '((2 . 2) (4 . 4)))
      (expect (nth-value 1 (resolve lines (aitools.kernel.domain:make-match-selector "[bd]" :invert t)))
              :to-equal '((1 . 1) (3 . 3))))

    (it "gives an empty range for --between --exclusive on adjacent lines"
      (expect (nth-value 1 (resolve lines (aitools.kernel.domain:make-between-selector "b" "c" :exclusive t)))
              :to-equal '((3 . 2))))

    (it "names similar lines when nothing matches"
      (multiple-value-bind (outcome candidates) (resolve (lines-of "alpha" "beta") (aitools.kernel.domain:make-match-selector "betx"))
        (expect outcome :to-be :no-match)
        (expect (json-object-get (first candidates) "line") :to-be 2)))

    (it "rejects --symbol on a file without a language"
      (expect (nth-value 1 (resolve lines (aitools.kernel.domain:make-symbol-selector "f") :path "notes.txt"))
              :to-equal "input.unsupported-language"))))

(describe "--symbol extents"
  (it "follows balanced parentheses in Lisp, skipping strings and characters"
    (expect (symbol-range "(defun f ()
  (format nil \")\" #\\())

(defun g () 2)" "a.lisp" "f")
            :to-equal '(1 . 2)))

  (it "follows braces in Rust and ends a bodiless item at its semicolon"
    (expect (symbol-range "fn main() {
    let s = \"}\";
}
fn other() {}" "m.rs" "main")
            :to-equal '(1 . 3))
    (expect (symbol-range "struct Unit;
fn f() {}" "u.rs" "Unit") :to-equal '(1 . 1)))

  (it "follows indentation in Python"
    (expect (symbol-range "def f(x):
    if x:
        return 1

    return 2

def g():
    pass" "p.py" "f")
            :to-equal '(1 . 5)))

  (it "runs a Markdown heading to the next heading of its level"
    (expect (symbol-range "# Title
## Setup
text
### Detail
more
## Usage
end" "r.md" "Setup")
            :to-equal '(2 . 5)))

  (it "narrows by --kind and reports ambiguity"
    (let ((text "(defvar x 1)
(defun x () 2)"))
      (expect (symbol-range text "a.lisp" "x" :kind "function") :to-equal '(2 . 2))
      (expect (resolve (split-text-lines text) (aitools.kernel.domain:make-symbol-selector "x") :path "a.lisp")
              :to-be :ambiguous))))

(describe "lisp-balance-diagnostics dialects"
  (it "treats #\\( as a character in Common Lisp and |...| as a symbol"
    (expect (lisp-balance-diagnostics (lines-of "(list #\\( |a)b|)") :common-lisp) :to-be nil))

  (it "treats \\( as a character in Clojure and pairs brackets"
    (expect (lisp-balance-diagnostics (lines-of "(str \\( [1 {:a 2}])") :clojure) :to-be nil)
    (expect (third (first (lisp-balance-diagnostics (lines-of "(let [a 1))") :clojure)))
            :to-contain "closes '['"))

  (it "treats ?( and ?\\) as characters in Emacs Lisp"
    (expect (lisp-balance-diagnostics (lines-of "(list ?( ?\\))") :emacs-lisp) :to-be nil))

  (it "skips nested block comments and reports an unterminated string at its start"
    (expect (lisp-balance-diagnostics (lines-of "#| ( #| ) |# ( |#" "(a)") :common-lisp) :to-be nil)
    (expect (lisp-balance-diagnostics (lines-of "(a" " \"open") :common-lisp)
            :to-equal '((1 1 "'(' is never closed") (2 2 "unterminated string")))))

(describe "read-render"
  (it "escapes invisible characters and leaves others alone"
    (expect (escape-invisible (format nil "a~Cb" #\Tab)) :to-equal "a\\u{0009}b")
    (expect (escape-invisible "plain ascii") :to-equal "plain ascii")
    (expect (escape-invisible (string (code-char #x3042))) :to-equal (string (code-char #x3042))))

  (it "finds UTF-8 runs with extract-strings"
    (let ((found '()))
      (extract-strings (coerce (concatenate '(vector (unsigned-byte 8)) #(1) (string-bytes (string (code-char #x3042))) (string-bytes "bcd") #(0))
                               '(simple-array (unsigned-byte 8) (*)))
                       :min-length 4 :emit (lambda (offset text) (push (cons offset text) found) nil))
      (expect found :to-equal (list (cons 1 (concatenate 'string (string (code-char #x3042)) "bcd")))))))

(describe "--symbol in Markdown code fences"
  (it "does not take a fenced line for a heading"
    (expect (symbol-range "# Guide
```sh
# Guide
```
text" "g.md" "Guide")
            :to-equal '(1 . 5))))

(describe "parse-selector-options/k command acceptance"
  (it "rejects a selector the command does not accept, naming both"
    (expect (multiple-value-list (parse-options :diff :range "1"))
            :to-equal '(:invalid "diff does not accept --range")))

  (it "rejects an empty --symbol"
    (expect (multiple-value-list (parse-options :read :symbol ""))
            :to-equal '(:invalid "--symbol needs a name"))))

(describe "read selection errors"
  (it "repairs an ambiguous --symbol with the first match's full line range"
    (multiple-value-bind (kind fields)
        (run-flow #'read-flow (make-test-ports :files '(("/work/a.lisp" . "(defvar x 1)
(defun x ()
  2)
"))) "a.lisp" :symbol "x")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "selection.ambiguous")
      (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools read a.lisp --range 1:1")))

  (it "repairs --symbol on a file without a definition table with a line range"
    (multiple-value-bind (kind fields)
        (run-flow #'read-flow (make-test-ports :files '(("/work/n.txt" . "plain"))) "n.txt" :symbol "f")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.unsupported-language")
      (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools read n.txt --range 1:80"))))

(describe "lisp-balance-diagnostics ordering and unterminated forms"
  (it "orders two problems on one line by column"
    (expect (mapcar (lambda (problem) (subseq problem 0 2)) (lisp-balance-diagnostics (lines-of "))") :common-lisp))
            :to-equal '((1 1) (1 2))))

  (it-each (("|abc" "unterminated |...| symbol")
            ("#| open" "unterminated #| comment"))
      "reports ~S as ~A"
      (text message)
    (expect (lisp-balance-diagnostics (lines-of "(a)" text) :common-lisp)
            :to-equal (list (list 2 1 message)))))

(describe "--symbol extents in brace languages"
  (it "skips braces inside line comments, block comments, and strings with escaped quotes"
    (expect (symbol-range "function f() {
  // }
  const s = \"\\\"}\";
  /* } { */
  return 1;
}
function g() {}" "a.js" "f")
            :to-equal '(1 . 6)))

  (it "treats a definition with no brace within 50 lines as one line"
    (expect (symbol-range (format nil "fn f()~%~{~A~%~}{~%}~%" (loop repeat 60 collect "x")) "a.rs" "f")
            :to-equal '(1 . 1)))

  (it "runs an unclosed body to the end of the file"
    (expect (symbol-range (format nil "fn f() {~%  1~%  2~%") "a.rs" "f") :to-equal '(1 . 3))))

(describe "--symbol extents by indentation with tabs"
  (it "counts a tab as indentation to the next multiple of 8"
    (expect (symbol-range (format nil "def f():~%~Creturn 1~%~%def g():~%~Cpass~%" #\Tab #\Tab) "p.py" "f")
            :to-equal '(1 . 2))))

(describe "--between patterns"
  (it-each (("(" "b") ("a" "("))
      "rejects an invalid start or end pattern ~S ~S as input.syntax-error"
      (start end)
    (expect (nth-value 1 (resolve (lines-of "a" "b") (aitools.kernel.domain:make-between-selector start end)))
            :to-equal "input.syntax-error")))

(describe "sexp-end-line"
  (it "ends at the last line when the form never closes"
    (expect (sexp-end-line (lines-of "(a" "b" "c") 0 :common-lisp) :to-be 2)))

(describe "similarity helpers"
  (it "quotes a single quote for the shell"
    (expect (shell-quote "it's") :to-equal "'it'\\''s'"))

  (it "compares only the first 256 characters of long strings"
    (let ((long (make-string 300 :initial-element #\a)))
      (expect (edit-distance long (concatenate 'string (make-string 256 :initial-element #\a) "bbbb")) :to-be 0))))
