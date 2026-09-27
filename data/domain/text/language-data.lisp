;;;; data/domain/text/language-data.lisp
;;;;
;;;; The language table shared by `code outline|defs|refs`, `--symbol`, and
;;;; `transform --op comment|uncomment`, kept as data so that a language is
;;;; easy to add. Adding a language means adding one
;;;; plist here; the text domain (language.lisp) builds its lookup tables
;;;; from this list at load time.
;;;;
;;;; Keys of each plist:
;;;;   :NAME            the `--lang` value and the `lang` field in output
;;;;   :EXTENSIONS      lowercase file extensions without the dot
;;;;   :FILENAMES       exact base names (checked before extensions)
;;;;   :LINE-COMMENT    line comment marker, or NIL
;;;;   :BLOCK-COMMENT   (OPEN CLOSE) block comment markers, or NIL
;;;;   :EXTENT          how a definition's end line is estimated:
;;;;                    :SEXP (balanced parentheses from the definition line),
;;;;                    :BRACE (balanced braces), :INDENT (until a line
;;;;                    indented no deeper than the definition), :HEADING
;;;;                    (until the next heading of the same or higher level)
;;;;   :IDENTIFIER      a cl-regex-kit character class matching one
;;;;                    identifier character, for word-bounded `code refs`
;;;;   :DEFINITIONS     ((KIND PATTERN)...): cl-regex-kit patterns matched
;;;;                    against one line; each has a capture named `name`
(in-package #:aitools.data)

(defparameter *text-languages*
  '((:name "common-lisp"
     :extensions ("lisp" "lsp" "cl" "asd" "ros")
     :filenames ()
     :line-comment ";"
     :block-comment ("#|" "|#")
     :extent :sexp
     :identifier "[^\\s()'\"`,;]"
     :definitions
     (("function" "^\\s*\\((?:defun|defgeneric)\\s+(?<name>[^\\s()]+)")
      ("method" "^\\s*\\(defmethod\\s+(?<name>[^\\s()]+)")
      ("macro" "^\\s*\\((?:defmacro|define-compiler-macro|define-symbol-macro)\\s+(?<name>[^\\s()]+)")
      ("variable" "^\\s*\\((?:defvar|defparameter)\\s+(?<name>[^\\s()]+)")
      ("constant" "^\\s*\\(defconstant\\s+(?<name>[^\\s()]+)")
      ("class" "^\\s*\\((?:defclass|define-condition)\\s+(?<name>[^\\s()]+)")
      ("struct" "^\\s*\\(defstruct\\s+\\(?(?<name>[^\\s()]+)")
      ("type" "^\\s*\\(deftype\\s+(?<name>[^\\s()]+)")
      ("package" "^\\s*\\(defpackage\\s+(?<name>[^\\s()]+)")))
    (:name "emacs-lisp"
     :extensions ("el")
     :filenames (".emacs")
     :line-comment ";"
     :block-comment nil
     :extent :sexp
     :identifier "[^\\s()'\"`,;\\[\\]]"
     :definitions
     (("function" "^\\s*\\((?:defun|cl-defun|defsubst|cl-defgeneric|define-inline)\\s+(?<name>[^\\s()]+)")
      ("method" "^\\s*\\(cl-defmethod\\s+(?<name>[^\\s()]+)")
      ("macro" "^\\s*\\((?:defmacro|cl-defmacro)\\s+(?<name>[^\\s()]+)")
      ("variable" "^\\s*\\((?:defvar|defvar-local|defcustom|defface)\\s+(?<name>[^\\s()]+)")
      ("constant" "^\\s*\\(defconst\\s+(?<name>[^\\s()]+)")
      ("mode" "^\\s*\\((?:define-minor-mode|define-derived-mode|define-globalized-minor-mode)\\s+(?<name>[^\\s()]+)")
      ("struct" "^\\s*\\(cl-defstruct\\s+\\(?(?<name>[^\\s()]+)")))
    (:name "scheme"
     :extensions ("scm" "ss" "sld" "sls" "rkt")
     :filenames ()
     :line-comment ";"
     :block-comment ("#|" "|#")
     :extent :sexp
     :identifier "[^\\s()'\"`,;\\[\\]]"
     :definitions
     (("function" "^\\s*\\(define\\s+\\((?<name>[^\\s()]+)")
      ("variable" "^\\s*\\(define\\s+(?<name>[^\\s()]+)")
      ("macro" "^\\s*\\((?:define-syntax|define-macro)\\s+\\(?(?<name>[^\\s()]+)")
      ("struct" "^\\s*\\((?:define-record-type|struct)\\s+\\(?(?<name>[^\\s()]+)")))
    (:name "clojure"
     :extensions ("clj" "cljs" "cljc" "edn" "bb")
     :filenames ()
     :line-comment ";"
     :block-comment nil
     :extent :sexp
     :identifier "[^\\s()'\"`,;\\[\\]{}]"
     :definitions
     (("function" "^\\s*\\((?:defn|defn-)\\s+(?<name>[^\\s()\\[\\]]+)")
      ("macro" "^\\s*\\(defmacro\\s+(?<name>[^\\s()\\[\\]]+)")
      ("method" "^\\s*\\((?:defmulti|defmethod)\\s+(?<name>[^\\s()\\[\\]]+)")
      ("variable" "^\\s*\\((?:def|defonce)\\s+(?<name>[^\\s()\\[\\]]+)")
      ("type" "^\\s*\\((?:defprotocol|defrecord|deftype)\\s+(?<name>[^\\s()\\[\\]]+)")
      ("namespace" "^\\s*\\(ns\\s+(?<name>[^\\s()\\[\\]]+)")))
    (:name "rust"
     :extensions ("rs")
     :filenames ()
     :line-comment "//"
     :block-comment ("/*" "*/")
     :extent :brace
     :identifier "[A-Za-z0-9_]"
     :definitions
     (("function" "^\\s*(?:pub(?:\\([^)]*\\))?\\s+)?(?:(?:const|async|unsafe|default)\\s+)*(?:extern\\s+\"[^\"]*\"\\s+)?fn\\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)")
      ("struct" "^\\s*(?:pub(?:\\([^)]*\\))?\\s+)?struct\\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)")
      ("enum" "^\\s*(?:pub(?:\\([^)]*\\))?\\s+)?enum\\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)")
      ("trait" "^\\s*(?:pub(?:\\([^)]*\\))?\\s+)?(?:unsafe\\s+)?trait\\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)")
      ("impl" "^\\s*(?:unsafe\\s+)?impl(?:<[^>]*>)?\\s+(?:[^{]*\\s+for\\s+)?(?<name>[A-Za-z_][A-Za-z0-9_:]*)")
      ("module" "^\\s*(?:pub(?:\\([^)]*\\))?\\s+)?mod\\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)")
      ("constant" "^\\s*(?:pub(?:\\([^)]*\\))?\\s+)?(?:const|static)\\s+(?:mut\\s+)?(?<name>[A-Za-z_][A-Za-z0-9_]*)")
      ("type" "^\\s*(?:pub(?:\\([^)]*\\))?\\s+)?type\\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)")
      ("macro" "^\\s*macro_rules!\\s*(?<name>[A-Za-z_][A-Za-z0-9_]*)")))
    (:name "go"
     :extensions ("go")
     :filenames ()
     :line-comment "//"
     :block-comment ("/*" "*/")
     :extent :brace
     :identifier "[A-Za-z0-9_]"
     :definitions
     (("method" "^func\\s+\\([^)]*\\)\\s*(?<name>[A-Za-z_][A-Za-z0-9_]*)")
      ("function" "^func\\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)")
      ("struct" "^type\\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)\\s+struct\\b")
      ("interface" "^type\\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)\\s+interface\\b")
      ("type" "^type\\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)")
      ("constant" "^const\\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)")
      ("variable" "^var\\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)")))
    (:name "python"
     :extensions ("py" "pyi")
     :filenames ()
     :line-comment "#"
     :block-comment nil
     :extent :indent
     :identifier "[A-Za-z0-9_]"
     :definitions
     (("function" "^\\s*(?:async\\s+)?def\\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)")
      ("class" "^\\s*class\\s+(?<name>[A-Za-z_][A-Za-z0-9_]*)")))
    (:name "javascript"
     :extensions ("js" "mjs" "cjs" "jsx")
     :filenames ()
     :line-comment "//"
     :block-comment ("/*" "*/")
     :extent :brace
     :identifier "[A-Za-z0-9_$]"
     :definitions
     (("function" "^\\s*(?:export\\s+(?:default\\s+)?)?(?:async\\s+)?function\\s*\\*?\\s*(?<name>[A-Za-z_$][A-Za-z0-9_$]*)")
      ("class" "^\\s*(?:export\\s+(?:default\\s+)?)?class\\s+(?<name>[A-Za-z_$][A-Za-z0-9_$]*)")
      ("function" "^\\s*(?:export\\s+)?(?:const|let|var)\\s+(?<name>[A-Za-z_$][A-Za-z0-9_$]*)\\s*=\\s*(?:async\\s+)?(?:function\\b|\\([^)]*\\)\\s*=>|[A-Za-z_$][A-Za-z0-9_$]*\\s*=>)")))
    (:name "typescript"
     :extensions ("ts" "tsx" "mts" "cts")
     :filenames ()
     :line-comment "//"
     :block-comment ("/*" "*/")
     :extent :brace
     :identifier "[A-Za-z0-9_$]"
     :definitions
     (("function" "^\\s*(?:export\\s+(?:default\\s+)?)?(?:declare\\s+)?(?:async\\s+)?function\\s*\\*?\\s*(?<name>[A-Za-z_$][A-Za-z0-9_$]*)")
      ("class" "^\\s*(?:export\\s+(?:default\\s+)?)?(?:declare\\s+)?(?:abstract\\s+)?class\\s+(?<name>[A-Za-z_$][A-Za-z0-9_$]*)")
      ("interface" "^\\s*(?:export\\s+)?(?:declare\\s+)?interface\\s+(?<name>[A-Za-z_$][A-Za-z0-9_$]*)")
      ("type" "^\\s*(?:export\\s+)?(?:declare\\s+)?type\\s+(?<name>[A-Za-z_$][A-Za-z0-9_$]*)")
      ("enum" "^\\s*(?:export\\s+)?(?:declare\\s+)?(?:const\\s+)?enum\\s+(?<name>[A-Za-z_$][A-Za-z0-9_$]*)")
      ("function" "^\\s*(?:export\\s+)?(?:const|let|var)\\s+(?<name>[A-Za-z_$][A-Za-z0-9_$]*)(?:\\s*:[^=]+)?\\s*=\\s*(?:async\\s+)?(?:function\\b|\\([^)]*\\)\\s*(?::[^=]+)?=>|[A-Za-z_$][A-Za-z0-9_$]*\\s*=>)")))
    (:name "nix"
     :extensions ("nix")
     :filenames ()
     :line-comment "#"
     :block-comment ("/*" "*/")
     :extent :brace
     :identifier "[A-Za-z0-9_'-]"
     :definitions
     (("attribute" "^\\s*(?<name>[A-Za-z_][A-Za-z0-9_'-]*(?:\\.[A-Za-z_][A-Za-z0-9_'-]*)*)\\s*=(?!=)")))
    (:name "shell"
     :extensions ("sh" "bash" "zsh" "ksh")
     :filenames (".bashrc" ".bash_profile" ".profile" ".zshrc" ".envrc")
     :line-comment "#"
     :block-comment nil
     :extent :brace
     :identifier "[A-Za-z0-9_]"
     :definitions
     (("function" "^\\s*function\\s+(?<name>[A-Za-z_][A-Za-z0-9_:.-]*)")
      ("function" "^\\s*(?<name>[A-Za-z_][A-Za-z0-9_:.-]*)\\s*\\(\\s*\\)")))
    (:name "markdown"
     :extensions ("md" "markdown")
     :filenames ()
     :line-comment nil
     :block-comment ("<!--" "-->")
     :extent :heading
     :identifier "[^\\s#]"
     :definitions
     (("heading" "^(?<level>#{1,6})\\s+(?<name>.*?)\\s*#*\\s*$"))))
  "The language table; see the file header for each key.")

(export '*text-languages*)
