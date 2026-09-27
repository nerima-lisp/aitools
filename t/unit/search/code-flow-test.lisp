;;;; t/unit/search/code-flow-test.lisp
;;;;
;;;; The `code` group: per-language outline and defs fixtures, word-bounded refs,
;;;; and overview.
(in-package #:aitools.search.test)

(defun lines (&rest lines)
  (format nil "~{~A~%~}" lines))

(defparameter *language-fixtures*
  `(("common-lisp" "/w/a.lisp"
     ,(lines "(defpackage #:demo (:use #:cl))" "" "(defun add (a b)" "  \"Add; (not a paren)\"" "  (+ a b))"
             "(defmacro with-x ((x) &body body)" "  `(let ((,x 1)) ,@body))" "(defstruct (point (:copier nil)) x y)")
     ((1 1 "package" "#:demo") (3 5 "function" "add") (6 7 "macro" "with-x") (8 8 "struct" "point")))
    ("emacs-lisp" "/w/a.el"
     ,(lines "(defvar my-count 0)" "(defun my-inc ()" "  (setq my-count (1+ my-count)))")
     ((1 1 "variable" "my-count") (2 3 "function" "my-inc")))
    ("scheme" "/w/a.scm"
     ,(lines "(define (square x)" "  (* x x))" "(define limit 10)")
     ((1 2 "function" "square") (3 3 "variable" "limit")))
    ("clojure" "/w/a.clj"
     ,(lines "(ns demo.core)" "(defn greet [name]" "  (str \"hi \" name))")
     ((1 1 "namespace" "demo.core") (2 3 "function" "greet")))
    ("rust" "/w/a.rs"
     ,(lines "pub struct Point {" "    x: i32," "}" "" "pub fn len(p: &Point) -> i32 {" "    p.x" "}" "const MAX: u32 = 3;")
     ((1 3 "struct" "Point") (5 7 "function" "len") (8 8 "constant" "MAX")))
    ("go" "/w/a.go"
     ,(lines "type Server struct {" "	port int" "}" "" "func (s *Server) Start() {" "	run()" "}" "func main() {" "}")
     ((1 3 "struct" "Server") (5 7 "method" "Start") (8 9 "function" "main")))
    ("python" "/w/a.py"
     ,(lines "class Greeter:" "    def hello(self):" "        return 1" "" "    def bye(self):" "        pass" "def top():" "    pass")
     ((1 6 "class" "Greeter") (2 3 "function" "hello") (5 6 "function" "bye") (7 8 "function" "top")))
    ("javascript" "/w/a.js"
     ,(lines "export function load(x) {" "  return x;" "}" "const run = async () => {" "  await load(1);" "};")
     ((1 3 "function" "load") (4 6 "function" "run")))
    ("typescript" "/w/a.ts"
     ,(lines "export interface Shape {" "  area(): number;" "}" "type Id = string;")
     ((1 3 "interface" "Shape") (4 4 "type" "Id")))
    ("nix" "/w/a.nix"
     ,(lines "{" "  name = \"demo\";" "  build = {" "    enable = true;" "  };" "}")
     ((2 2 "attribute" "name") (3 5 "attribute" "build") (4 4 "attribute" "enable")))
    ("shell" "/w/a.sh"
     ,(lines "greet() {" "  echo hi" "}" "function bye {" "  echo bye" "}")
     ((1 3 "function" "greet") (4 6 "function" "bye")))
    ("markdown" "/w/a.md"
     ,(lines "# Title" "text" "## Part" "```" "# not a heading" "```" "## Next" "end")
     ((1 8 "heading" "Title") (3 6 "heading" "Part") (7 8 "heading" "Next")))))

(defun outline-of (path content)
  (multiple-value-bind (kind fields)
      (run-flow #'code-outline/k (make-fake-ports :files (list (list path content))) :path path)
    (expect kind :to-be :ok)
    (mapcar (lambda (symbol)
              (list (jfield symbol "line") (jfield symbol "end_line") (jfield symbol "kind") (jfield symbol "name")))
            (field fields "symbols"))))

(describe "aitools.search.application code-outline/k per language"
  (it-each (("common-lisp") ("emacs-lisp") ("scheme") ("clojure") ("rust") ("go") ("python")
            ("javascript") ("typescript") ("nix") ("shell") ("markdown"))
      "outlines ~A definitions with estimated end lines"
      (language)
    (destructuring-bind (path content expected) (rest (assoc language *language-fixtures* :test #'string=))
      (expect (outline-of path content) :to-equal expected)))

  ;; The table's Rust `impl` pattern needs `(?:[^{]*\s+for\s+)?` to match
  ;; nothing for an inherent impl; cl-regex-kit v2.1.1 fixed the optional-group
  ;; bug that made it return no match, so `impl Point {` is now found.
  (it "outlines a Rust impl block, with and without a trait"
    (expect (outline-of "/w/a.rs" (lines "impl Point {" "}"))
            :to-equal '((1 2 "impl" "Point")))
    (expect (outline-of "/w/a.rs" (lines "impl Display for Point {" "}"))
            :to-equal '((1 2 "impl" "Point"))))

  (it "rejects a file in no supported language"
    (multiple-value-bind (kind fields)
        (run-flow #'code-outline/k (make-fake-ports :files '(("/w/a.txt" "x"))) :path "/w/a.txt")
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "input.unsupported-language")))

  (it "is partial past --limit"
    (multiple-value-bind (kind fields)
        (run-flow #'code-outline/k (make-fake-ports :files (list (list "/w/a.el" (lines "(defvar a 1)" "(defvar b 2)"))))
                  :path "a.el" :limit 1)
      (expect kind :to-be :partial)
      (expect (field fields "total") :to-be 2))))

(defparameter *code-files*
  (list (list "/w/src/core.lisp" (lines "(defun parse (x)" "  (parse-inner x))" "(defun parse-inner (x) x)"))
        (list "/w/src/use.lisp" (lines "(parse 1)" "(reparse 2)" "(list 'parse)"))
        (list "/w/notes.txt" "parse")))

(describe "aitools.search.application code-defs/k and code-refs/k"
  (it "finds definitions by exact name, by --prefix, and by --kind"
    (flet ((defs (&rest arguments)
             (mapcar (lambda (def) (list (jfield def "path") (jfield def "line") (jfield def "name")))
                     (field (nth-value 1 (apply #'run-flow #'code-defs/k (make-fake-ports :files *code-files*) arguments))
                            "defs"))))
      (expect (defs :name "parse") :to-equal '(("src/core.lisp" 1 "parse")))
      (expect (defs :name "parse" :prefix t) :to-equal '(("src/core.lisp" 1 "parse") ("src/core.lisp" 3 "parse-inner")))
      (expect (defs :name "parse" :kind "macro") :to-equal '())))

  (it "returns whole-identifier references only, marking the defining line def"
    (multiple-value-bind (kind fields) (run-flow #'code-refs/k (make-fake-ports :files *code-files*) :name "parse")
      (expect kind :to-be :ok)
      (expect (mapcar (lambda (ref) (list (jfield ref "path") (jfield ref "line") (jfield ref "kind")))
                      (field fields "refs"))
              :to-equal '(("src/core.lisp" 1 "def") ("src/use.lisp" 1 "ref") ("src/use.lisp" 3 "ref")))
      (expect (jfield (first (field fields "refs")) "text") :to-equal "(defun parse (x)"))))

(describe "aitools.search.application overview/k"
  (it "summarizes languages, build files, entries, and git state read from .git"
    (let ((ports (make-fake-ports
                  :files (list (list "/w/.git/HEAD" (format nil "ref: refs/heads/main~%"))
                               (list "/w/.git/refs/heads/main" (format nil "0123abcd~%"))
                               (list "/w/flake.nix" (lines "{" "}"))
                               (list "/w/demo.asd" (lines "(defsystem \"demo\")"))
                               (list "/w/src/a.lisp" (lines "(defun a ())" "(defun b ())" ""))
                               (list "/w/src/b.py" (lines "x = 1"))
                               (list "/w/logo.png" (coerce #(137 80 78 71 0 0) '(vector (unsigned-byte 8))))))))
      (multiple-value-bind (kind fields) (run-flow #'overview/k ports)
        (expect kind :to-be :ok)
        (expect (field fields "root") :to-equal "/w")
        (expect (field fields "ignore_source") :to-equal "gitignore")
        (expect (mapcar (lambda (row) (list (jfield row "lang") (jfield row "files") (jfield row "lines")))
                        (field fields "languages"))
                :to-equal '(("common-lisp" 2 4) ("nix" 1 2) ("python" 1 1)))
        (expect (field fields "build_files") :to-equal '("demo.asd" "flake.nix"))
        (expect (mapcar (lambda (entry) (jfield entry "name")) (field fields "entries"))
                :to-equal '("demo.asd" "flake.nix" "logo.png" "src"))
        (let ((git (field fields "git")))
          (expect (jfield git "branch") :to-equal "main")
          (expect (jfield git "head") :to-equal "0123abcd")
          (expect (jfield git "untracked") :to-be 5)))))

  (it "reports git as null outside a repository and cuts languages at --limit"
    (multiple-value-bind (kind fields)
        (run-flow #'overview/k (make-fake-ports :files '(("/w/a.lisp" "x") ("/w/b.py" "y"))) :limit 1)
      (expect kind :to-be :partial)
      (expect (field fields "git") :to-be json-kit:+json-null+)
      (expect (field fields "languages_total") :to-be 2))))

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
    (expect (outline-of "/w/a.rs" (format nil "fn g()~%~%")) :to-equal '((1 1 "function" "g")))))

(describe "aitools.search.application code-outline/k errors"
  (it "reports a missing file of a supported language as input.not-found with a find repair"
    (multiple-value-bind (kind fields) (run-flow #'code-outline/k (make-fake-ports :directories '("/w/src")) :path "src/nope.lisp")
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "input.not-found")
      (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools find nope.lisp")))

  (it "reports a binary file as input.unsupported-format with an info repair"
    (multiple-value-bind (kind fields)
        (run-flow #'code-outline/k (make-fake-ports :files (list (list "/w/bin.lisp" (coerce #(40 0 41) '(vector (unsigned-byte 8))))))
                  :path "bin.lisp")
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "input.unsupported-format")
      (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools info bin.lisp")))

  (it "names a file outside the workspace by its absolute path"
    (multiple-value-bind (kind fields)
        (run-flow #'code-outline/k (make-fake-ports :files (list (list "/other/a.lisp" (lines "(defun o ())"))) :directories '("/w"))
                  :path "/other/a.lisp")
      (expect kind :to-be :ok)
      (expect (field fields "path") :to-equal "/other/a.lisp"))))

(describe "aitools.search.application code-defs/k and code-refs/k scanning"
  (flet ((defs (ports &rest arguments)
           (multiple-value-bind (kind fields) (apply #'run-flow #'code-defs/k ports arguments)
             (values kind fields (mapcar (lambda (def) (list (jfield def "path") (jfield def "name")))
                                         (field fields "defs"))))))
    (it "keeps only names that start with a --prefix NAME, ignoring shorter ones"
      (expect (nth-value 2 (defs (make-fake-ports :files *code-files*) :name "parse-inner" :prefix t))
              :to-equal '(("src/core.lisp" "parse-inner"))))

    (it "scans below --path only, skipping files without the name and files that fail to read"
      (let ((ports (make-fake-ports :files (append *code-files*
                                                   (list (list "/w/src/other.lisp" (lines "(defun other ())"))
                                                         (list "/w/src/bad.lisp" (lines "(defun parse ())"))
                                                         (list "/w/lib/parse.lisp" (lines "(defun parse ())"))))
                                    :failing '("/w/src/bad.lisp"))))
        (multiple-value-bind (kind fields names) (defs ports :name "parse" :path "src")
          (expect kind :to-be :ok)
          (expect names :to-equal '(("src/core.lisp" "parse")))
          (expect (field fields "total") :to-be 1))))

    (it "is partial past --limit with a next command carrying --path, --prefix, and --kind"
      (multiple-value-bind (kind fields names)
          (defs (make-fake-ports :files *code-files*) :name "parse" :prefix t :kind "function" :path "src" :limit 1)
        (expect kind :to-be :partial)
        (expect names :to-equal '(("src/core.lisp" "parse")))
        (expect (field fields "total") :to-be 2)
        (expect (field fields "next_commands")
                :to-equal '("aitools code defs parse src --prefix --kind function --limit 2"))))

    (it "reports a --path that does not exist as input.not-found"
      (multiple-value-bind (kind fields)
          (run-flow #'code-defs/k (make-fake-ports :files *code-files*) :name "parse" :path "nope")
        (expect kind :to-be :error)
        (expect (getf fields :code) :to-equal "input.not-found")
        (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools find nope"))))

  (it "is partial past --limit for refs, with a next command carrying --path"
    (multiple-value-bind (kind fields)
        (run-flow #'code-refs/k (make-fake-ports :files *code-files*) :name "parse" :path "src" :limit 1)
      (expect kind :to-be :partial)
      (expect (length (field fields "refs")) :to-be 1)
      (expect (field fields "total") :to-be 3)
      (expect (field fields "next_commands") :to-equal '("aitools code refs parse src --limit 3"))))

  (it "bounds a reference at the file start and end, and by a non-ASCII identifier character"
    (multiple-value-bind (kind fields)
        (run-flow #'code-refs/k (make-fake-ports :files (list (list "/w/a.lisp" (format nil "parse~%(éparse 1)~%(x parse"))))
                  :name "parse")
      (expect kind :to-be :ok)
      (expect (mapcar (lambda (ref) (list (jfield ref "line") (jfield ref "text"))) (field fields "refs"))
              :to-equal '((1 "parse") (3 "(x parse"))))))

(defun git-index (&rest paths)
  "The bytes of a version 2 git index listing PATHS: zeroed stat data and
object ids, the name length in the flags, and each entry NUL-padded to a
multiple of 8 bytes, as git writes it."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (flet ((u32 (n) (loop for shift from 24 downto 0 by 8 do (vector-push-extend (ldb (byte 8 shift) n) out))))
      (map nil (lambda (char) (vector-push-extend (char-code char) out)) "DIRC")
      (u32 2)
      (u32 (length paths))
      (dolist (path paths)
        (let ((start (fill-pointer out)) (name (string-bytes path)))
          (dotimes (i 60) (vector-push-extend 0 out))
          (vector-push-extend (ldb (byte 8 8) (length name)) out)
          (vector-push-extend (ldb (byte 8 0) (length name)) out)
          (map nil (lambda (octet) (vector-push-extend octet out)) name)
          (loop do (vector-push-extend 0 out) until (zerop (mod (- (fill-pointer out) start) 8)))))
      (coerce out '(simple-array (unsigned-byte 8) (*))))))

(defun overview-git (files &rest arguments)
  (let ((fields (nth-value 1 (apply #'run-flow #'overview/k (make-fake-ports :files files) arguments))))
    (let ((git (field fields "git")))
      (mapcar (lambda (name) (jfield git name)) '("branch" "head" "untracked" "deleted")))))

(describe "aitools.search.application overview/k git summary"
  (flet ((head (text) (list "/w/.git/HEAD" text)))
    (it "resolves the branch through packed-refs when no loose ref exists"
      (expect (overview-git (list (head (format nil "ref: refs/heads/main~%"))
                                  (list "/w/.git/packed-refs"
                                        (lines "# pack-refs with: peeled" "aaaa refs/heads/other" "^bbbb" "cccc refs/heads/main"))))
              :to-equal '("main" "cccc" 0 0)))

    (it-each (("0123abc" "null" "0123abc") ("ref: refs/tags/v1" "refs/tags/v1" "null") ("" "null" "null"))
        "reads HEAD ~S as branch ~A and head ~A"
        (text branch sha)
      (flet ((value (text) (if (string= text "null") json-kit:+json-null+ text)))
        (expect (subseq (overview-git (list (head (format nil "~A~%" text)))) 0 2)
                :to-equal (list (value branch) (value sha)))))

    (it "counts untracked and deleted paths from the index, below the start path"
      (let ((files (list (head (format nil "ref: refs/heads/main~%"))
                         (list "/w/.git/index" (git-index "src/a.lisp" "src/gone.lisp" "top.txt" "zz/gone.txt"))
                         (list "/w/src/a.lisp" "(x)") (list "/w/src/new.lisp" "(y)") (list "/w/top.txt" "t"))))
        (expect (overview-git files) :to-equal (list "main" json-kit:+json-null+ 1 2))
        (expect (overview-git files :path "src") :to-equal (list "main" json-kit:+json-null+ 1 1))
        ;; A workspace root below the repository top sees the index paths below it.
        (expect (overview-git files :root "/w/src") :to-equal (list "main" json-kit:+json-null+ 1 1))))

    (it "treats an unreadable index as no tracked paths"
      (expect (overview-git (list (head (format nil "ref: refs/heads/main~%")) (list "/w/.git/index" "DIRX")
                                  (list "/w/a.lisp" "(x)")))
              :to-equal (list "main" json-kit:+json-null+ 1 0)))))

(describe "aitools.search.application overview/k scanning"
  (it "is partial past --limit with a next command carrying the path, and skips a file that fails to read"
    (multiple-value-bind (kind fields)
        (run-flow #'overview/k (make-fake-ports :files '(("/w/src/a.lisp" "x") ("/w/src/b.py" "y") ("/w/src/c.py" "z"))
                                                :failing '("/w/src/c.py"))
                  :path "src" :limit 1)
      (expect kind :to-be :partial)
      (expect (mapcar (lambda (row) (list (jfield row "lang") (jfield row "files"))) (field fields "languages"))
              :to-equal '(("common-lisp" 1)))
      (expect (field fields "languages_total") :to-be 2)
      (expect (length (field fields "entries")) :to-be 3)
      (expect (field fields "next_commands") :to-equal '("aitools overview src --limit 2"))))

  (it "reports a path that does not exist as input.not-found"
    (expect (getf (nth-value 1 (run-flow #'overview/k (make-fake-ports :directories '("/w")) :path "nope")) :code) :to-equal "input.not-found")))

(describe "aitools.search.domain overview parts"
  (it-each (("ref: refs/heads/main" "main" nil "refs/heads/main") ("ref: HEAD2" "HEAD2" nil "HEAD2")
            ("ref: " nil "ref:" nil) ("  0123abc  " nil "0123abc" nil) ("" nil nil nil))
      "parses HEAD ~S"
      (text branch sha ref)
    (expect (multiple-value-list (parse-head-file text)) :to-equal (list branch sha ref)))

  (it "finds a ref in packed-refs, ignoring comment, peeled, and malformed lines"
    (let ((text (lines "# pack-refs with: peeled" "^1111 refs/heads/main" "noseparator" "2222 refs/heads/main")))
      (expect (packed-ref-sha text "refs/heads/main") :to-equal "2222")
      (expect (packed-ref-sha text "refs/heads/other") :to-be nil)))

  (it "orders tally rows by lines, then bytes, then name"
    (let ((tally (make-language-tally)))
      (tally-file tally "b" 10 5)
      (tally-file tally "a" 10 5)
      (tally-file tally "c" 10 9)
      (tally-file tally "d" 20 1)
      (expect (mapcar #'first (aitools.search.domain::language-tally-rows tally)) :to-equal '("d" "c" "a" "b")))))
