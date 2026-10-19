;;;; t/unit/inspect/read-test.lisp
;;;;
;;;; `read` through fake ports.
(in-package #:aitools.inspect.test)
(defun numbered-lines (count)
  (format nil "~{line ~D~%~}" (loop for n from 1 to count collect n)))

(defun read-file (files path &rest options)
  (apply #'run-flow #'read-flow (make-test-ports :files files) path options))

(describe "read text mode"
  (it "returns start_line, lines, total_lines, hash, and no next command at the end"
    (multiple-value-bind (kind fields) (read-file '(("/work/a.txt" . "alpha
beta
")) "a.txt")
      (expect kind :to-be :ok)
      (expect (field fields "mode") :to-equal "text")
      (expect (field fields "start_line") :to-be 1)
      (expect (field fields "lines") :to-equal '("alpha" "beta"))
      (expect (field fields "total_lines") :to-be 2)
      (expect (field fields "hash") :to-equal (aitools.kernel.domain:sha256-hex (string-bytes "alpha
beta
")))
      (expect (field fields "next_commands") :to-be nil)))

  (it "stops at --max-lines as partial and names the next --range"
    (multiple-value-bind (kind fields) (read-file `(("/work/big.txt" . ,(numbered-lines 200))) "big.txt")
      (expect kind :to-be :partial)
      (expect (length (field fields "lines")) :to-be 80)
      (expect (json-false-value-p (field fields "truncated")) :to-be nil)
      (expect (field fields "next_commands") :to-equal '("aitools read big.txt --range 81:160"))))
  (it "keeps explicit UTF-8 line cuts byte-addressable"
    (multiple-value-bind (kind fields)
        (read-file `(("/work/long.txt" . ,(format nil "~A~%short~%" (make-string 20000 :initial-element #\a))))
                   "long.txt" :encoding "utf-8")
      (expect kind :to-be :partial)
      (expect (field fields "encoding") :to-equal "utf-8")
      (expect (length (first (field fields "lines"))) :to-be 16384)
      (expect (field fields "next_commands")
              :to-contain "aitools read long.txt --as hex --bytes 16384:16640")))

  (it "names the next window after an explicit --range that ends early, without truncating"
    (multiple-value-bind (kind fields) (read-file `(("/work/big.txt" . ,(numbered-lines 200))) "big.txt" :range "5:9")
      (expect kind :to-be :ok)
      (expect (field fields "start_line") :to-be 5)
      (expect (field fields "lines") :to-equal '("line 5" "line 6" "line 7" "line 8" "line 9"))
      (expect (field fields "next_commands") :to-equal '("aitools read big.txt --range 10:89"))))

  (it "reads --range N and S: to the end"
    (let ((files `(("/work/f.txt" . ,(numbered-lines 5)))))
      (expect (field (nth-value 1 (read-file files "f.txt" :range "3")) "lines") :to-equal '("line 3"))
      (expect (field (nth-value 1 (read-file files "f.txt" :range "4:")) "lines") :to-equal '("line 4" "line 5"))))

  (it "reads --tail N"
    (multiple-value-bind (kind fields) (read-file `(("/work/f.txt" . ,(numbered-lines 10))) "f.txt" :tail 3)
      (expect kind :to-be :ok)
      (expect (field fields "start_line") :to-be 8)
      (expect (field fields "lines") :to-equal '("line 8" "line 9" "line 10"))))

  (it "keeps the BOM and CR out of the lines"
    (let ((octets (concatenate '(vector (unsigned-byte 8)) #(#xEF #xBB #xBF) (string-bytes (format nil "a~C~Cb~C~C" #\Return #\Newline #\Return #\Newline)))))
      (expect (field (nth-value 1 (read-file `(("/work/crlf.txt" . ,octets)) "crlf.txt")) "lines") :to-equal '("a" "b"))))

  (it "counts invalid UTF-8 as encoding_errors"
    (let ((octets (concatenate '(vector (unsigned-byte 8)) (string-bytes "ok ") #(#xFF #xFE) (string-bytes (string #\Newline)))))
      (expect (field (nth-value 1 (read-file `(("/work/bad.txt" . ,octets)) "bad.txt")) "encoding_errors") :to-be 2)))

  (it "does not print a binary file's content"
    (multiple-value-bind (kind fields) (read-file `(("/work/b.bin" . ,(coerce #(0 1 2 3 0 255) '(vector (unsigned-byte 8))))) "b.bin")
      (expect kind :to-be :ok)
      (expect (field fields "binary") :to-be t)
      (expect (field fields "size") :to-be 6)
      (expect (field fields "mime") :to-equal "application/octet-stream")
      (expect (field fields "lines") :to-be nil))))

(describe "read selectors"
  (let ((files '(("/work/src/a.lisp" . "(defun alpha ()
  1)

(defun beta ()
  (list 2
        3))
;; START
middle
;; END
"))))
    (it "selects a definition with --symbol"
      (multiple-value-bind (kind fields) (read-file files "src/a.lisp" :symbol "beta")
        (expect kind :to-be :ok)
        (expect (field fields "start_line") :to-be 4)
        (expect (field fields "lines") :to-equal '("(defun beta ()" "  (list 2" "        3))"))))

    (it "offers similar definitions when --symbol matches nothing"
      (multiple-value-bind (kind fields) (read-file files "src/a.lisp" :symbol "betta")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "selection.no-match")
        (expect (json-object-get (first (getf fields :candidates)) "name") :to-equal "beta")))

    (it "selects --between inclusive and --exclusive"
      (expect (field (nth-value 1 (read-file files "src/a.lisp" :between '("START" "END"))) "lines")
              :to-equal '(";; START" "middle" ";; END"))
      (expect (field (nth-value 1 (read-file files "src/a.lisp" :between '("START" "END") :exclusive t)) "lines")
              :to-equal '("middle")))

    (it "selects --match lines with their numbers, and --invert the rest"
      (multiple-value-bind (kind fields) (read-file files "src/a.lisp" :match "defun")
        (expect kind :to-be :ok)
        (expect (field fields "lines") :to-equal '("(defun alpha ()" "(defun beta ()"))
        (expect (field fields "line_numbers") :to-equal '(1 4)))
      (expect (length (field (nth-value 1 (read-file files "src/a.lisp" :match "defun" :invert t)) "lines")) :to-be 7))

    (it "rejects two selectors, and a bad regex as input.syntax-error"
      (expect (error-code (nth-value 1 (read-file files "src/a.lisp" :range "1" :match "x"))) :to-equal "argument.invalid")
      (expect (error-code (nth-value 1 (read-file files "src/a.lisp" :range "1" :tail 2))) :to-equal "argument.invalid")
      (expect (error-code (nth-value 1 (read-file files "src/a.lisp" :match "(unclosed"))) :to-equal "input.syntax-error"))

    (it "reports selection.ambiguous with every --between block"
      (let ((twice '(("/work/t.txt" . "A
x
B
A
y
B
"))))
        (multiple-value-bind (kind fields) (read-file twice "t.txt" :between '("^A" "^B"))
          (expect kind :to-be :error)
          (expect (error-code fields) :to-equal "selection.ambiguous")
          (expect (length (getf fields :candidates)) :to-be 2))))

    (it "reports a --range past the end as selection.no-match"
      (expect (error-code (nth-value 1 (read-file files "src/a.lisp" :range "500"))) :to-equal "selection.no-match"))))

(describe "read other modes"
  (it "shows invisible characters with --escape-invisible"
    (let ((text (format nil "a~Cb~Cc~Cd~C~C" (code-char #x200B) (code-char #xA0) (code-char #x3000) #\Return #\Newline)))
      (expect (field (nth-value 1 (read-file `(("/work/i.txt" . ,text)) "i.txt" :escape-invisible t)) "lines")
              :to-equal '("a\\u{200B}b\\u{00A0}c\\u{3000}d\\u{000D}"))))

  (it "dumps hex rows of 16 bytes from --bytes"
    (let ((octets (coerce (loop for i below 40 collect i) '(vector (unsigned-byte 8)))))
      (multiple-value-bind (kind fields) (read-file `(("/work/h.bin" . ,octets)) "h.bin" :as "hex" :bytes "0:20")
        (expect kind :to-be :ok)
        (expect (field fields "mode") :to-equal "hex")
        (expect (length (field fields "rows")) :to-be 2)
        (expect (json-object-get (second (field fields "rows")) "hex") :to-equal "10 11 12 13"))))

  (it "lists printable runs with --as strings"
    (let ((octets (concatenate '(vector (unsigned-byte 8)) #(0 0) (string-bytes "hello") #(0 1) (string-bytes "ab") #(0))))
      (multiple-value-bind (kind fields) (read-file `(("/work/s.bin" . ,octets)) "s.bin" :as "strings")
        (expect kind :to-be :ok)
        (expect (mapcar (lambda (item) (json-object-get item "offset")) (field fields "strings")) :to-equal '(2))
        (expect (json-object-get (first (field fields "strings")) "text") :to-equal "hello"))))

  (it "decodes --encoding shift_jis"
    (let ((octets (coerce #(#x82 #xA0 #x0A) '(vector (unsigned-byte 8)))))
      (multiple-value-bind (kind fields) (read-file `(("/work/j.txt" . ,octets)) "j.txt" :encoding "shift_jis")
        (expect kind :to-be :ok)
        (expect (field fields "lines") :to-equal (list (string (code-char #x3042))))
        (expect (field fields "encoding") :to-equal "shift_jis"))))

  (it "rejects an unknown encoding as input.unsupported-format"
    (expect (error-code (nth-value 1 (read-file '(("/work/a.txt" . "x")) "a.txt" :encoding "ebcdic")))
            :to-equal "input.unsupported-format")))

(describe "read reports the path root-relative inside the workspace, absolute outside"
  (it "reports an in-root target as its workspace-root-relative path, even given an absolute path"
    (multiple-value-bind (kind fields) (read-file '(("/work/sub/a.txt" . "x")) "/work/sub/a.txt")
      (expect kind :to-be :ok)
      (expect (field fields "path") :to-equal "sub/a.txt")))

  (it "reports an out-of-root target (a symlink leaving the workspace) as its absolute real path"
    (multiple-value-bind (kind fields)
        (run-flow #'read-flow
                  (make-test-ports :files '(("/outside/secret.txt" . "s"))
                                   :symlinks '(("/work/link" . "/outside/secret.txt")))
                  "link")
      (expect kind :to-be :ok)
      (expect (field fields "path") :to-equal "/outside/secret.txt"))))

(describe "read of a missing path"
  (it "fails with input.not-found and near workspace paths as candidates"
    (multiple-value-bind (kind fields) (read-file '(("/work/src/reader.lisp" . "x") ("/work/src/writer.lisp" . "y"))
                                                  "src/raeder.lisp")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (json-object-get (first (getf fields :candidates)) "path") :to-equal "src/reader.lisp")
      (expect (plusp (length (getf (first (getf fields :repairs)) :command))) :to-be t)))

  (it "refuses a directory as refusal.not-a-file"
    (expect (error-code (nth-value 1 (read-file '(("/work/d/x" . "1")) "d"))) :to-equal "refusal.not-a-file")))

(describe "read of an unreadable file (contract-F1)"
  (flet ((ports () (make-test-ports :unreadable '(("/work/secret.txt" . "hidden")))))
    (it-each (("text") ("hex") ("strings"))
        "reports environment.io in --as ~A mode, not input.not-found"
        (as)
      (expect (error-code (nth-value 1 (run-flow #'read-flow (ports) "secret.txt" :as as))) :to-equal "environment.io"))))

(describe "read --match that exhausts the regex step budget"
  (it "answers input.syntax-error rather than internal.unexpected"
    (expect (error-code (nth-value 1 (read-file `(("/work/a.txt" . ,(make-string 40 :initial-element #\a)))
                                                "a.txt" :match "(a+)+(?=[bc])")))
            :to-equal "input.syntax-error")))

(describe "read --as hex masks secrets"
  (it "shows a secret token's bytes as the mask byte, not verbatim"
    (let* ((octets (string-bytes (format nil "GITHUB_TOKEN=ghp_abcdefghijklmnopqrstuvwxyz0123456789~%"))))
      (multiple-value-bind (kind fields)
          (run-flow #'read-flow (make-test-ports :files `(("/work/env" . ,octets))) "env"
                    :as "hex" :bytes (format nil "0:~D" (length octets)))
        (expect kind :to-be :ok)
        (let ((hex (format nil "~{~A~^ ~}" (mapcar (lambda (row) (json-object-get row "hex")) (field fields "rows")))))
          ;; "ghp_" is 67 68 70 5f; masked to '*' (2a), so its bytes never appear.
          (expect (search "67 68 70 5f" hex) :to-be nil)
          (expect (and (search "2a 2a 2a 2a" hex) t) :to-be t))))))

(describe "inspect context resolution"
  (it-each (("an unparsable --lock-timeout" (:lock-timeout "soon") "argument.invalid"
             "--lock-timeout \"soon\" is not a duration" "aitools schema")
            ("a --root that does not exist" (:root "/nope") "input.not-found"
             "workspace root /nope does not exist" "aitools info .")
            ("a --root that is a file" (:root "/work/a.txt") "input.not-found"
             "workspace root /work/a.txt is not a directory" "aitools info ."))
      "rejects ~A"
      (label options code message repair)
    (declare (ignore label))
    (multiple-value-bind (kind fields) (apply #'read-file '(("/work/a.txt" . "x")) "a.txt" options)
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal code)
      (expect (getf fields :message) :to-equal message)
      (expect (getf (first (getf fields :repairs)) :command) :to-equal repair))))

(describe "read of a missing path outside the workspace"
  (it "ranks workspace files by the missing file's base name"
    (multiple-value-bind (kind fields) (read-file '(("/work/src/notes.md" . "x")) "/elsewhere/note.md")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (json-object-get (first (getf fields :candidates)) "path") :to-equal "src/notes.md")
      (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools find note.md"))))

(describe "read of a directory"
  (it "says it is a directory and repairs with a one-level listing"
    (multiple-value-bind (kind fields) (read-file '(("/work/d/x" . "1")) "d")
      (expect kind :to-be :error)
      (expect (getf fields :message) :to-equal "d is a directory, not a file")
      (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools find --depth 1 d"))))

