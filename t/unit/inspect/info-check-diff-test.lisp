;;;; t/unit/inspect/info-check-diff-test.lisp
;;;;
;;;; `info`, `check`, and `diff` through fake ports
;;;; Real symlinks, digests of
;;;; real files, and `diff --op` against a real journal are in
;;;; t/integration/inspect-cli-test.lisp.
(in-package #:aitools.inspect.test)

(defun info-of (files path &rest options &key symlinks &allow-other-keys)
  (apply #'run-flow #'info-flow (make-test-ports :files files :symlinks symlinks) path
         (loop for (key value) on options by #'cddr unless (eq key :symlinks) append (list key value))))

(describe "info"
  (it "reports path and content fields of a text file"
    (multiple-value-bind (kind fields) (info-of '(("/work/a.txt" . "one two
three  four five
")) "a.txt")
      (expect kind :to-be :ok)
      (expect (field fields "absolute") :to-equal "/work/a.txt")
      (expect (field fields "relative") :to-equal "a.txt")
      (expect (field fields "exists") :to-be t)
      (expect (field fields "kind") :to-equal "file")
      (expect (field fields "inside_workspace") :to-be t)
      (expect (field fields "lines") :to-be 2)
      (expect (field fields "words") :to-be 5)
      (expect (field fields "max_line_chars") :to-be 16)
      (expect (field fields "encoding_guess") :to-equal "utf-8")
      (expect (field fields "line_ending") :to-equal "lf")
      (expect (field fields "trailing_newline") :to-be t)
      (expect (field fields "mode") :to-equal "0644")
      (expect (field fields "mtime") :to-equal "2023-11-14T22:13:20Z")))

  (it "guesses Shift_JIS and UTF-16LE"
    (expect (field (nth-value 1 (info-of `(("/work/j.txt" . ,(coerce #(#x82 #xA0 #x82 #xA2 #x82 #xA4 #x0A) '(vector (unsigned-byte 8))))) "j.txt"))
                   "encoding_guess")
            :to-equal "shift_jis")
    (expect (field (nth-value 1 (info-of `(("/work/w.txt" . ,(coerce #(#xFF #xFE #x41 0 #x42 0) '(vector (unsigned-byte 8))))) "w.txt"))
                   "encoding_guess")
            :to-equal "utf-16le"))

  (it "adds the standard digest with --digest"
    (let ((digest (field (nth-value 1 (info-of '(("/work/a.txt" . "abc")) "a.txt" :digest "sha1")) "digest")))
      (expect (json-object-get digest "algorithm") :to-equal "sha1")
      (expect (json-object-get digest "value") :to-equal "a9993e364706816aba3e25717850c26c9cd0d89d")))

  (it "fails on a missing path with candidates, or reports exists:false with --allow-missing"
    (expect (error-code (nth-value 1 (info-of '(("/work/a.txt" . "x")) "b.txt"))) :to-equal "input.not-found")
    (multiple-value-bind (kind fields) (info-of '(("/work/a.txt" . "x")) "b.txt" :allow-missing t)
      (expect kind :to-be :ok)
      (expect (json-false-value-p (field fields "exists")) :to-be t)
      (expect (field fields "size") :to-be nil)))

  (it "sees a symlink leading out of the workspace"
    (multiple-value-bind (kind fields) (info-of '(("/outside/secret.txt" . "s")) "link"
                                                :symlinks '(("/work/link" . "/outside/secret.txt")))
      (expect kind :to-be :ok)
      (expect (field fields "relative") :to-equal "link")
      (expect (field fields "real") :to-equal "/outside/secret.txt")
      (expect (json-false-value-p (field fields "inside_workspace")) :to-be t))))

(defun check-of (files path &rest options)
  (apply #'run-flow #'check-flow (make-test-ports :files files) path options))

(describe "check"
  (it "accepts valid JSON and Lisp"
    (expect (field (nth-value 1 (check-of '(("/work/a.json" . "{\"a\": [1, 2]}")) "a.json")) "format") :to-equal "json")
    (expect (field (nth-value 1 (check-of '(("/work/a.lisp" . "(defun f () \")\" #\\( ; )
  1)")) "a.lisp")) "valid") :to-be t))

  (it "reports a JSON error with its line and column"
    (multiple-value-bind (kind fields) (check-of '(("/work/b.json" . "{
  \"a\": 1,
  \"b\": }")) "b.json")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.syntax-error")
      (let ((diagnostic (first (getf fields :diagnostics))))
        (expect (json-object-get diagnostic "line") :to-be 3)
        (expect (json-object-get diagnostic "col") :to-be 8))))

  (it "locates an unclosed paren and a stray closer"
    (let ((diagnostic (first (getf (nth-value 1 (check-of '(("/work/c.lisp" . "(defun f ()
  (list 1 2)")) "c.lisp"))
                                   :diagnostics))))
      (expect (json-object-get diagnostic "line") :to-be 1)
      (expect (json-object-get diagnostic "col") :to-be 1))
    (let ((diagnostic (first (getf (nth-value 1 (check-of '(("/work/d.lisp" . "(a)
 b)")) "d.lisp"))
                                   :diagnostics))))
      (expect (json-object-get diagnostic "line") :to-be 2)
      (expect (json-object-get diagnostic "col") :to-be 3)))

  (it "rejects an unknown extension unless --format is given"
    (expect (error-code (nth-value 1 (check-of '(("/work/x.txt" . "{}")) "x.txt"))) :to-equal "input.unsupported-format")
    (expect (field (nth-value 1 (check-of '(("/work/x.txt" . "{}")) "x.txt" :format "json")) "valid") :to-be t)))

(defun diff-of (files &rest options)
  (apply #'run-flow #'diff-flow (make-test-ports :files files) options))

(describe "diff"
  (let ((files '(("/work/a.txt" . "one
two
three
") ("/work/b.txt" . "one
2
three
four
"))))
    (it "renders a unified diff"
      (multiple-value-bind (kind fields) (diff-of files :a "a.txt" :b "b.txt")
        (expect kind :to-be :ok)
        (expect (json-false-value-p (field fields "identical")) :to-be t)
        (expect (field fields "diff") :to-equal "--- a.txt
+++ b.txt
@@ -1,3 +1,4 @@
 one
-two
+2
 three
+four
")))

    (it "counts lines with --output stat"
      (let ((fields (nth-value 1 (diff-of files :a "a.txt" :b "b.txt" :output "stat"))))
        (expect (field fields "added") :to-be 2)
        (expect (field fields "deleted") :to-be 1)))

    (it "compares line sets with --output set"
      (let ((fields (nth-value 1 (diff-of files :a "a.txt" :b "b.txt" :output "set"))))
        (expect (field fields "only_a") :to-equal '("two"))
        (expect (field fields "only_b") :to-equal '("2" "four"))
        (expect (field fields "both_count") :to-be 2))))

  (it "ignores whitespace and line endings on request"
    (let ((files `(("/work/a.txt" . "x = 1
") ("/work/b.txt" . ,(format nil "x=1~C~C" #\Return #\Newline)))))
      (expect (json-false-value-p (field (nth-value 1 (diff-of files :a "a.txt" :b "b.txt")) "identical")) :to-be t)
      (expect (json-false-value-p (field (nth-value 1 (diff-of files :a "a.txt" :b "b.txt" :ignore-eol t)) "identical"))
              :to-be t)
      ;; As with diff -w, the CR is whitespace too.
      (expect (field (nth-value 1 (diff-of files :a "a.txt" :b "b.txt" :ignore-whitespace t)) "identical") :to-be t)
      (let ((eol-only `(("/work/a.txt" . "x = 1
") ("/work/b.txt" . ,(format nil "x = 1~C~C" #\Return #\Newline)))))
        (expect (json-false-value-p (field (nth-value 1 (diff-of eol-only :a "a.txt" :b "b.txt")) "identical")) :to-be t)
        (expect (field (nth-value 1 (diff-of eol-only :a "a.txt" :b "b.txt" :ignore-eol t)) "identical") :to-be t))))

  (it "classifies directory entries"
    (multiple-value-bind (kind fields)
        (diff-of '(("/work/d1/same" . "s") ("/work/d1/gone" . "g") ("/work/d1/sub/changed" . "1")
                   ("/work/d2/same" . "s") ("/work/d2/new" . "n") ("/work/d2/sub/changed" . "2"))
                 :a "d1" :b "d2")
      (expect kind :to-be :ok)
      (expect (field fields "mode") :to-equal "directory")
      (expect (field fields "added") :to-equal '("new"))
      (expect (field fields "removed") :to-equal '("gone"))
      (expect (field fields "modified") :to-equal '("sub/changed"))
      (expect (field fields "identical_count") :to-be 1)))

  (it "rejects paths together with --op and a missing second path"
    (expect (error-code (nth-value 1 (diff-of '() :a "a" :op "op-1"))) :to-equal "argument.invalid")
    (expect (error-code (nth-value 1 (diff-of '() :a "a"))) :to-equal "argument.invalid")))

;;; contract-F1: a path that exists but denies its bytes (EACCES) is
;;; environment.io, not input.not-found; a genuinely absent path stays
;;; input.not-found.
(describe "an existing but unreadable file reports environment.io (contract-F1)"
  (flet ((ports () (make-test-ports :unreadable '(("/work/secret.txt" . "hidden")
                                                  ("/work/other.txt" . "hidden too"))
                                    :files '(("/work/readable.txt" . "1
2")))))
    (it "info reports environment.io, not input.not-found"
      (expect (error-code (nth-value 1 (run-flow #'info-flow (ports) "secret.txt"))) :to-equal "environment.io"))
    (it "check reports environment.io"
      (expect (error-code (nth-value 1 (run-flow #'check-flow (ports) "secret.txt" :format "json")))
              :to-equal "environment.io"))
    (it "diff reports environment.io when either side is unreadable"
      (expect (error-code (nth-value 1 (run-flow #'diff-flow (ports) :a "secret.txt" :b "other.txt")))
              :to-equal "environment.io")
      (expect (error-code (nth-value 1 (run-flow #'diff-flow (ports) :a "readable.txt" :b "other.txt")))
              :to-equal "environment.io"))
    (it "a genuinely missing path is still input.not-found"
      (expect (error-code (nth-value 1 (run-flow #'info-flow (ports) "nope.txt"))) :to-equal "input.not-found"))))

(defun %archive-octets ()
  (aitools.text.domain:write-zip
   (list (aitools.text.domain:make-archive-member :name "m.txt" :kind :file :mode #o644 :mtime 0
                                                  :data (%as-octets "m")))))

(defparameter *single-file-flows*
  `(("table read" ,#'table-read-flow ())
    ("table agg" ,#'table-agg-flow ())
    ("json get" ,#'json-get-flow (""))
    ("json select" ,#'json-select-flow (""))
    ("archive list" ,#'archive-list-flow ())
    ("archive read" ,#'archive-read-flow ("m.txt"))
    ("read" ,#'read-flow ())
    ("read --encoding" ,#'read-flow (:encoding "utf-8"))
    ("read --as hex" ,#'read-flow (:as "hex"))
    ("check" ,#'check-flow (:format "json")))
  "(label flow extra-arguments) for each single-file flow that reads its
target's bytes after probing it; EXTRA-ARGUMENTS follow the path.")

(describe "single-file flows on a file that cannot be read (contract-F1)"
  (it-each (("table read") ("table agg") ("json get") ("json select") ("archive list") ("archive read")
            ("read") ("read --encoding") ("read --as hex") ("check"))
      "~A reports environment.io for a file denied by permission"
      (label)
    (destructuring-bind (flow extra) (rest (assoc label *single-file-flows* :test #'string=))
      (multiple-value-bind (kind fields)
          (apply #'run-flow flow (make-test-ports :unreadable `(("/work/f.json" . ,(%archive-octets)))) "f.json" extra)
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "environment.io")
        (expect (getf fields :message) :to-equal "file f.json cannot be read: permission denied")
        (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools info f.json"))))

  (it-each (("table read") ("table agg") ("json get") ("json select") ("archive list") ("archive read")
            ("read") ("read --encoding") ("read --as hex") ("check"))
      "~A reports input.not-found for a file that vanished after it was probed"
      (label)
    (destructuring-bind (flow extra) (rest (assoc label *single-file-flows* :test #'string=))
      ;; The host still lists the file; the text source no longer has it.
      (let* ((present (make-fake-filesystem :files `(("/work/f.json" . ,(%archive-octets))) :directories '("/work")))
             (gone (make-fake-filesystem :directories '("/work")))
             (ports (make-inspect-ports :workspace-host (make-fake-host present) :text-source (make-fake-source gone)
                                        :open-store (lambda (root) (fail (format nil "open-store ~A" root)))
                                        :state-directory-function (constantly nil))))
        (multiple-value-bind (kind fields) (apply #'run-flow flow ports "f.json" extra)
          (expect kind :to-be :error)
          (expect (error-code fields) :to-equal "input.not-found")
          (expect (getf fields :message) :to-equal "file f.json does not exist")
          (expect (json-object-get (first (getf fields :candidates)) "path") :to-equal "f.json"))))))

(describe "diff edge cases"
  (it "compares binary files by bytes only"
    (let ((files `(("/work/a.bin" . ,(coerce #(0 1 2) '(vector (unsigned-byte 8))))
                   ("/work/b.bin" . ,(coerce #(0 1 3) '(vector (unsigned-byte 8))))
                   ("/work/c.bin" . ,(coerce #(0 1 2) '(vector (unsigned-byte 8)))))))
      (multiple-value-bind (kind fields) (diff-of files :a "a.bin" :b "b.bin")
        (expect kind :to-be :ok)
        (expect (field fields "binary") :to-be t)
        (expect (json-false-value-p (field fields "identical")) :to-be t)
        (expect (field fields "diff") :to-be nil))
      (expect (field (nth-value 1 (diff-of files :a "a.bin" :b "c.bin")) "identical") :to-be t)))

  (it-each (("the first" "gone.txt" "a.txt") ("the second" "a.txt" "gone.txt"))
      "reports ~A path missing as input.not-found"
      (label a b)
    (declare (ignore label))
    (multiple-value-bind (kind fields) (diff-of '(("/work/a.txt" . "x")) :a a :b b)
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (getf fields :message) :to-equal "file gone.txt does not exist")))

  (it "rejects a file compared with a directory"
    (multiple-value-bind (kind fields) (diff-of '(("/work/a.txt" . "x") ("/work/d/x" . "y")) :a "a.txt" :b "d")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")
      (expect (getf fields :message) :to-equal "a.txt and d must both be files or both be directories")
      (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools info a.txt")))

  (it "is partial past --limit hunks, counting them all"
    (let ((files `(("/work/a.txt" . ,(format nil "~{~A~%~}" (loop for i below 30 collect i)))
                   ("/work/b.txt" . ,(format nil "~{~A~%~}" (loop for i below 30 collect (if (member i '(2 25)) "x" i)))))))
      (multiple-value-bind (kind fields) (diff-of files :a "a.txt" :b "b.txt" :limit 1)
        (expect kind :to-be :partial)
        (expect (field fields "hunks") :to-be 2)
        (expect (search "+x" (field fields "diff")) :to-be-truthy)
        (expect (field fields "truncated") :to-be t))))

  (it "is partial past --limit entries per directory list"
    (multiple-value-bind (kind fields) (diff-of '(("/work/d1/k" . "k") ("/work/d2/k" . "k") ("/work/d2/n1" . "1")
                                                  ("/work/d2/n2" . "2"))
                                                :a "d1" :b "d2" :limit 1)
      (expect kind :to-be :partial)
      (expect (field fields "added") :to-equal '("n1")))))

(describe "check --format"
  (it "checks an unknown extension as Lisp with --format lisp"
    (multiple-value-bind (kind fields)
        (run-flow #'check-flow (make-test-ports :files '(("/work/x.conf" . "(a (b)"))) "x.conf" :format "lisp")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.syntax-error")
      (expect (json-object-get (first (getf fields :diagnostics)) "message") :to-equal "'(' is never closed"))))

(describe "diff of the workspace root"
  (it "reports the root directory as ."
    (let ((fields (nth-value 1 (diff-of '(("/work/k" . "k") ("/other/k" . "k")) :a "." :b "/other"))))
      (expect (field fields "a") :to-equal ".")
      (expect (field fields "b") :to-equal "/other"))))

(defun octets (&rest parts)
  "PARTS (strings and byte lists) concatenated as an octet vector."
  (coerce (loop for part in parts
                append (if (stringp part) (coerce (string-bytes part) 'list) part))
          '(simple-array (unsigned-byte 8) (*))))

(describe "text-content-counts on bytes"
  (it-each (("two-byte characters" ("caf" (#xC3 #xA9) " ok"))
            ("three-byte characters and U+3000 between words" ((#xE6 #x97 #xA5) (#xE3 #x80 #x80) "x"))
            ("E0 and ED lead bytes at their range edges" ((#xE0 #xA0 #x80) (#xED #x9F #xBF)))
            ("four-byte characters, F0 and F4 at their range edges" ((#xF0 #x90 #x80 #x80) " " (#xF4 #x8F #xBF #xBF)))
            ("CR runs before LF and at the end" ("a" (13 13 10) "b" (13)))
            ("a last line without a newline" ("a" (10) "bc")))
      "counts valid UTF-8 with ~A exactly as the decoded reference"
      (label parts)
    (declare (ignore label))
    (let ((bytes (apply #'octets parts)))
      (expect (nth-value 4 (aitools.inspect.domain::%valid-utf8-text-counts bytes 0)) :to-be t)
      (expect (multiple-value-list (text-content-counts bytes))
              :to-equal (multiple-value-list (aitools.inspect.domain::%decoded-text-counts bytes 0)))))

  (it-each (("a lone continuation byte" ((#x80)))
            ("a truncated two-byte sequence" ((#xC3)))
            ("an overlong E0 sequence" ((#xE0 #x80 #x80)))
            ("a UTF-16 surrogate" ((#xED #xA0 #x80)))
            ("an overlong F0 sequence" ((#xF0 #x80 #x80 #x80)))
            ("a code point past U+10FFFF" ((#xF4 #x90 #x80 #x80)))
            ("a byte that never starts a sequence" ((#xFF))))
      "falls back to the decoded count for ~A"
      (label parts)
    (declare (ignore label))
    (let ((bytes (apply #'octets "a b" (append parts (list (list 13 10) "c")))))
      (expect (nth-value 4 (aitools.inspect.domain::%valid-utf8-text-counts bytes 0)) :to-be nil)
      ;; Two lines; the invalid bytes become U+FFFD inside the first line's second word.
      (expect (subseq (multiple-value-list (text-content-counts bytes)) 0 2) :to-equal '(2 3))))

  (it-each ((:lf "lf") (:crlf "crlf") (:mixed "mixed") (:none "none"))
      "names line ending ~S ~S"
      (style name)
    (expect (line-ending-name style) :to-equal name)))
