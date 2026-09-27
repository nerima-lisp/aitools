;;;; t/unit/text/guess-test.lisp
;;;;
;;;; `info`'s encoding_guess and mime, Unicode normalization, and the
;;;; language table.
(in-package #:aitools.text.test)

(defparameter *japanese-sample* "これは日本語のテキストです。ひらがなとカタカナと漢字を含みます。")

(defun legacy-bytes (string encoding)
  (encode-string/k string encoding :on-encoded (lambda (bytes replaced) (declare (ignore replaced)) bytes)
                                   :on-unmappable (lambda (index char) (fail (format nil "~A ~A" index char)))))

(describe "aitools.text.domain encoding guess"
  (it "takes ASCII and valid UTF-8 as utf-8"
    (expect (guess-encoding (string-bytes "plain ascii")) :to-be :utf-8)
    (expect (guess-encoding (string-bytes *japanese-sample*)) :to-be :utf-8))

  (it "recognizes Shift_JIS and EUC-JP Japanese text"
    (expect (guess-encoding (legacy-bytes *japanese-sample* :shift_jis)) :to-be :shift_jis)
    (expect (guess-encoding (legacy-bytes *japanese-sample* :euc-jp)) :to-be :euc-jp))

  (it "recognizes UTF-16 by BOM and by NUL-byte pattern"
    (expect (guess-encoding (octets #xFF #xFE 97 0)) :to-be :utf-16le)
    (expect (guess-encoding (octets #xFE #xFF 0 97)) :to-be :utf-16be)
    (expect (guess-encoding (legacy-bytes "hello world" :utf-16le)) :to-be :utf-16le)
    (expect (guess-encoding (legacy-bytes "hello world" :utf-16be)) :to-be :utf-16be))

  (it "returns unknown for binary data"
    (expect (guess-encoding (octets 0 0 0 1 2 3 0 0 255 254 0 0 0)) :to-be :unknown)))

(describe "aitools.text.domain MIME guess"
  (it "prefers magic signatures"
    (expect (guess-mime (octets #x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A 0 0)) :to-equal "image/png")
    (expect (guess-mime (octets #x1F #x8B 8 0)) :to-equal "application/gzip")
    (expect (guess-mime (octets #x50 #x4B 3 4 20 0)) :to-equal "application/zip")
    (let ((tar (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0)))
      (replace tar (string-bytes "ustar") :start1 257)
      (expect (guess-mime tar) :to-equal "application/x-tar")))

  (it "falls back to octet-stream for binary and to the extension or text/plain for text"
    (expect (guess-mime (octets 1 2 0 3)) :to-equal "application/octet-stream")
    (expect (guess-mime (string-bytes "{}") :path "a/b.json") :to-equal "application/json")
    (expect (guess-mime (string-bytes "x") :path "README") :to-equal "text/plain")))

(describe "aitools.text.domain normalization"
  (it "folds full-width letters and joins a separate voiced mark under NFKC"
    (expect (normalize-text (coerce (list (code-char #xFF21) (code-char #x304B) (code-char #x3099)) 'string) :nfkc)
            :to-equal (coerce (list #\A (code-char #x304C)) 'string)))

  (it "composes under NFC and decomposes under NFD"
    (let ((decomposed (coerce (list #\e (code-char #x301)) 'string)))
      (expect (normalize-text decomposed :nfc) :to-equal (string (code-char #xE9)))
      (expect (normalize-text (string (code-char #xE9)) :nfd) :to-equal decomposed))))

(describe "aitools.text.domain language table"
  (it "finds languages by extension and exact file name"
    (expect (language-name (language-for-path "src/a.lisp")) :to-equal "common-lisp")
    (expect (language-name (language-for-path "x/init.el")) :to-equal "emacs-lisp")
    (expect (language-name (language-for-path "a.RS")) :to-equal "rust")
    (expect (language-name (language-for-path "main.go")) :to-equal "go")
    (expect (language-name (language-for-path "a.py")) :to-equal "python")
    (expect (language-name (language-for-path "a.tsx")) :to-equal "typescript")
    (expect (language-name (language-for-path "a.mjs")) :to-equal "javascript")
    (expect (language-name (language-for-path "flake.nix")) :to-equal "nix")
    (expect (language-name (language-for-path "home/.bashrc")) :to-equal "shell")
    (expect (language-name (language-for-path "README.md")) :to-equal "markdown")
    (expect (language-name (language-for-path "a.clj")) :to-equal "clojure")
    (expect (language-name (language-for-path "a.scm")) :to-equal "scheme")
    (expect (language-for-path "Makefile") :to-be-falsy)
    (expect (language-for-path ".hidden") :to-be-falsy))

  (it "exposes comment markers"
    (expect (language-line-comment (find-language "common-lisp")) :to-equal ";")
    (expect (language-block-comment (find-language "rust")) :to-equal '("/*" "*/"))
    (expect (language-line-comment (find-language "markdown")) :to-be-falsy))

  (it "builds a path predicate for the scan's :LANG, NIL for unknown names"
    (let ((predicate (language-path-predicate "Python")))
      (expect (funcall predicate "a/b.py") :to-be-truthy)
      (expect (funcall predicate "a/b.lisp") :to-be-falsy))
    (expect (language-path-predicate "cobol") :to-be-falsy))

  (it "compiles every definition pattern with a `name` capture"
    (let ((problems '()))
      (dolist (name (language-names))
        (loop for (kind pattern) in (language-definitions (find-language name))
              do (handler-case
                     (unless (cl-regex-kit:regex-group-index (cl-regex-kit:compile-regex pattern) "name")
                       (push (list name kind :no-name) problems))
                   (error (condition) (push (list name kind (princ-to-string condition)) problems)))))
      (expect problems :to-equal nil)))

  (it-each (("common-lisp" "(defun foo-bar (x)" "foo-bar")
            ("common-lisp" "(defstruct (point (:copier nil))" "point")
            ("emacs-lisp" "(defcustom my-var nil" "my-var")
            ("scheme" "(define (square x)" "square")
            ("clojure" "(defn- helper [x]" "helper")
            ("rust" "pub(crate) async fn run_it(x: u8) {" "run_it")
            ("go" "func (s *Server) Serve() error {" "Serve")
            ("python" "    async def handle(self):" "handle")
            ("javascript" "export const add = (a, b) => a + b;" "add")
            ("typescript" "export interface Shape {" "Shape")
            ("nix" "  buildInputs = [ ];" "buildInputs")
            ("shell" "deploy() {" "deploy")
            ("markdown" "## Getting started ##" "Getting started"))
      "finds the defined name on a ~A sample line"
      (language line expected)
    (let ((found (loop for (nil pattern) in (language-definitions (find-language language))
                       for regex = (cl-regex-kit:compile-regex pattern)
                       for match = (cl-regex-kit:scan regex line)
                       when match
                         return (cl-regex-kit:match-group-string
                                 match (cl-regex-kit:regex-group-index regex "name") line))))
      (expect found :to-equal expected))))

(describe "aitools.text.domain guess-encoding tie-breaks and cut samples"
  (it "prefers EUC-JP when both legacy decodings succeed and it scores higher, Shift_JIS on a tie"
    (expect (guess-encoding (octets #xA4 #xA2 #xA4 #xA4 #xA4 #xA6)) :to-be :euc-jp)
    (expect (guess-encoding (octets #xA1 #xA1)) :to-be :shift_jis)
    (expect (guess-encoding (octets #xEF)) :to-be :unknown)
    (expect (guess-encoding (octets #xEF #xBB #x41)) :to-be :euc-jp)
    (expect (guess-encoding (octets #xEF #x41 #x41)) :to-be :unknown))

  (it "calls NUL-bearing data unknown when it has too few pairs or no UTF-16 pattern"
    (expect (guess-encoding (octets 0)) :to-be :unknown)
    (expect (guess-encoding (octets 0 0 0 0)) :to-be :unknown))

  (it "forgives a sequence cut by the sample's end"
    (let ((aitools.text.domain::*encoding-guess-sample* 5))
      (expect (guess-encoding (octets #x82 #xA0 #x82 #xA2 #x82 #xA4)) :to-be :shift_jis)
      (expect (guess-encoding (octets #xE3 #x81 #x82 #xE3 #x81 #x82)) :to-be :utf-8))))
