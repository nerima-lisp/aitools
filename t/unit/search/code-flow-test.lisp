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

(describe "aitools.search.application Markdown fence boundaries"
  (it "requires a matching closing fence"
    (expect
     (outline-of "/w/a.md"
                 (lines "# title" "````" "```` not a close" "```"
                        "~~~" "# hidden" "````  " "## visible"))
     :to-equal (quote ((1 8 "heading" "title") (8 8 "heading" "visible"))))
    (expect
     (outline-of "/w/a.md"
                 (lines "# title" "```" "# hidden"
                        (format nil "```~C" #\Tab)
                        "## visible"))
     :to-equal (quote ((1 5 "heading" "title") (5 5 "heading" "visible")))))

  (it "rejects a backtick in a backtick fence info string"
    (expect
     (outline-of "/w/a.md" (lines "```lang`name" "# visible"))
     :to-equal '((2 2 "heading" "visible")))
    (expect
     (outline-of "/w/a.md" (lines "~~~lang`name" "# hidden" "~~~" "# visible"))
     :to-equal '((4 4 "heading" "visible"))))

  (it "rejects tab-indented opening and closing fences"
    (expect
     (outline-of "/w/a.md"
                 (lines (format nil "~C```" #\Tab)
                        "# visible after tab-indented opener"))
     :to-equal '((2 2 "heading" "visible after tab-indented opener")))
    (expect
     (outline-of "/w/a.md"
                 (lines "```" (format nil "~C```" #\Tab)
                        "# hidden" "```" "# visible"))
     :to-equal '((5 5 "heading" "visible")))))
