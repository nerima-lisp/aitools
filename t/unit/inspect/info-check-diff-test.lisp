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
(describe "inspect error and path edge cases"
  (it "preserves diagnostics for an invalid first JSON diff operand"
    (multiple-value-bind (kind fields)
        (run-flow #'json-diff-flow
                  (make-test-ports :files '(("/work/a.json" . "{")
                                            ("/work/b.json" . "{}")))
                  "a.json" "b.json")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.unsupported-format")
      (expect (json-object-get (first (getf fields :diagnostics)) "line") :to-be 1)
      (expect (getf (first (getf fields :repairs)) :command)
              :to-equal "aitools check a.json --format json")))

  (it "preserves diagnostics for an invalid second JSON diff operand"
    (multiple-value-bind (kind fields)
        (run-flow #'json-diff-flow
                  (make-test-ports :files '(("/work/a.json" . "{}")
                                            ("/work/b.json" . "{")))
                  "a.json" "b.json")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.unsupported-format")
      (expect (json-object-get (first (getf fields :diagnostics)) "line") :to-be 1)
      (expect (getf (first (getf fields :repairs)) :command)
              :to-equal "aitools check b.json --format json")))

  (it "reports a missing second JSON diff operand"
    (multiple-value-bind (kind fields)
        (run-flow #'json-diff-flow
                  (make-test-ports :files '(("/work/a.json" . "{}")))
                  "a.json" "b.json")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (getf fields :message) :to-equal "file b.json does not exist")))

  (it "reports directory metadata without reading it as a file"
    (multiple-value-bind (kind fields)
        (run-flow #'info-flow (make-test-ports :directories '("/work/sub")) "sub")
      (expect kind :to-be :ok)
      (expect (field fields "path") :to-equal "sub")
      (expect (field fields "kind") :to-equal "directory")
      (expect (field fields "size") :to-be 0)
      (expect (field fields "mode") :to-equal "0755")))

  (it "preserves a broken symlink's path fields with --allow-missing"
    (multiple-value-bind (kind fields)
        (run-flow #'info-flow
                  (make-test-ports :symlinks '(("/work/link" . "/outside/missing")))
                  "link" :allow-missing t)
      (expect kind :to-be :ok)
      (expect (json-false-value-p (field fields "exists")) :to-be t)
      (expect (field fields "kind") :to-equal "symlink")
      (expect (field fields "real") :to-equal "/outside/missing")))

  (it "supports every documented info digest"
    (let ((files '(("/work/a.txt" . "abc"))))
      (expect (json-object-get (field (nth-value 1 (info-of files "a.txt" :digest "sha256")) "digest")
                               "value")
              :to-equal "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
      (expect (json-object-get (field (nth-value 1 (info-of files "a.txt" :digest "md5")) "digest")
                               "value")
              :to-equal "900150983cd24fb0d6963f7d28e17f72")))

  (it "rejects an invalid lock timeout before resolving the inspect root"
    (multiple-value-bind (kind fields)
        (run-flow #'info-flow (make-test-ports :files nil) "a.txt" :lock-timeout "fast")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")
      (expect (getf fields :message)
              :to-equal "--lock-timeout \"fast\" is not a duration"))))

;;; contract-F1: a path that exists but denies its bytes (EACCES) is
;;; environment.io, not input.not-found; a genuinely absent path stays
;;; input.not-found.
(describe "check --format"
  (it "checks an unknown extension as Lisp with --format lisp"
    (multiple-value-bind (kind fields)
        (run-flow #'check-flow (make-test-ports :files '(("/work/x.conf" . "(a (b)"))) "x.conf" :format "lisp")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.syntax-error")
      (expect (json-object-get (first (getf fields :diagnostics)) "message") :to-equal "'(' is never closed"))))

