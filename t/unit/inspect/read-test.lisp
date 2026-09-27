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

(describe "read windows and continuations"
  (it-each (("the byte path" () "aitools read big.txt --range 41:120")
            ("the decoded path" (:escape-invisible t) "aitools read big.txt --range 41:120 --escape-invisible"))
      "names the window before a truncated --tail on ~A"
      (label options next)
    (declare (ignore label))
    (multiple-value-bind (kind fields)
        (apply #'read-file `(("/work/big.txt" . ,(numbered-lines 200))) "big.txt" :tail 100 options)
      (expect kind :to-be :partial)
      (expect (field fields "start_line") :to-be 121)
      (expect (length (field fields "lines")) :to-be 80)
      (expect (field fields "next_commands") :to-equal (list next))))

  (it-each (("the byte path" ())
            ("the decoded path" (:escape-invisible t)))
      "reads an empty file as no lines on ~A"
      (label options)
    (declare (ignore label))
    (multiple-value-bind (kind fields) (apply #'read-file '(("/work/e.txt" . "")) "e.txt" options)
      (expect kind :to-be :ok)
      (expect (field fields "lines") :to-be nil)
      (expect (field fields "total_lines") :to-be 0)))

  (it "reports a --range on an empty file as selection.no-match without candidates"
    (multiple-value-bind (kind fields) (read-file '(("/work/e.txt" . "")) "e.txt" :range "1")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "selection.no-match")
      (expect (getf fields :candidates) :to-be nil)))

  (it "shows no lines for --between --exclusive around adjacent lines"
    (multiple-value-bind (kind fields) (read-file '(("/work/b.txt" . "a
b
c
")) "b.txt" :between '("^a" "^b") :exclusive t)
      (expect kind :to-be :ok)
      (expect (field fields "start_line") :to-be 2)
      (expect (field fields "lines") :to-be nil)))

  (it "names every --match line, keeping --invert, when --max-lines cuts them"
    (multiple-value-bind (kind fields) (read-file `(("/work/m.txt" . ,(numbered-lines 5))) "m.txt"
                                                  :match "line [12]" :invert t :max-lines 1)
      (expect kind :to-be :partial)
      (expect (field fields "line_numbers") :to-equal '(3))
      (expect (field fields "next_commands")
              :to-equal '("aitools read m.txt --match 'line [12]' --invert --max-lines 3")))))

(describe "read hex and strings arguments"
  (it-each (("--bytes without --as hex" (:bytes "0:4") "--bytes needs --as hex")
            ("--encoding with --as hex" (:as "hex" :encoding "shift_jis")
             "--encoding and --escape-invisible apply to --as text only")
            ("--escape-invisible with --as strings" (:as "strings" :escape-invisible t)
             "--encoding and --escape-invisible apply to --as text only")
            ("a reversed --bytes span" (:as "hex" :bytes "5:2") "--bytes \"5:2\" is not S:E or S:")
            ("a --bytes without a colon" (:as "hex" :bytes "12") "--bytes \"12\" is not S:E or S:"))
      "rejects ~A"
      (label options message)
    (declare (ignore label))
    (multiple-value-bind (kind fields) (apply #'read-file '(("/work/h.bin" . "0123456789")) "h.bin" options)
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")
      (expect (getf fields :message) :to-contain message)))

  (it "dumps --bytes S: to the end of the file"
    (multiple-value-bind (kind fields) (read-file '(("/work/h.bin" . "0123456789")) "h.bin" :as "hex" :bytes "6:")
      (expect kind :to-be :ok)
      (expect (list (field fields "start") (field fields "end")) :to-equal '(6 10))
      (expect (json-object-get (first (field fields "rows")) "hex") :to-equal "36 37 38 39")))

  (it "names the next 256 bytes after the default hex window"
    (let ((octets (make-array 300 :element-type '(unsigned-byte 8) :initial-element 65)))
      (multiple-value-bind (kind fields) (read-file `(("/work/h.bin" . ,octets)) "h.bin" :as "hex")
        (expect kind :to-be :ok)
        (expect (field fields "end") :to-be 256)
        (expect (field fields "next_commands") :to-equal '("aitools read h.bin --as hex --bytes 256:300")))))

  (it "names every run when --max-lines cuts --as strings"
    (let ((octets (concatenate '(vector (unsigned-byte 8)) (string-bytes "first") #(0) (string-bytes "second") #(0))))
      (multiple-value-bind (kind fields) (read-file `(("/work/s.bin" . ,octets)) "s.bin" :as "strings" :max-lines 1)
        (expect kind :to-be :partial)
        (expect (field fields "total") :to-be 2)
        (expect (field fields "next_commands")
                :to-equal '("aitools read s.bin --as strings --min-length 4 --max-lines 2"))))))

(describe "missing-path candidates on a large workspace"
  (it "ranks only the first 20000 scanned paths"
    (let ((files (cons '("/work/z/target-near.txt" . "late")
                       (loop for index below 20000
                             collect (cons (format nil "/work/a/f~5,'0D.txt" index) "")))))
      (multiple-value-bind (kind fields) (read-file files "z/target-nea.txt")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.not-found")
        ;; z/target-near.txt is the nearest path but lies past the scan bound.
        (expect (find "z/target-near.txt" (getf fields :candidates)
                      :key (lambda (candidate) (json-object-get candidate "path")) :test #'string=)
                :to-be nil))))

  (it "offers z/target-near.txt when the workspace is small"
    (multiple-value-bind (kind fields) (read-file '(("/work/z/target-near.txt" . "x") ("/work/a/f.txt" . ""))
                                                  "z/target-nea.txt")
      (expect kind :to-be :error)
      (expect (json-object-get (first (getf fields :candidates)) "path") :to-equal "z/target-near.txt"))))

(defun %recording-read-ports (octets reads &key range)
  "Ports over the one file /work/big.bin (OCTETS) whose TEXT-SOURCE pushes an
(operation . arguments) record onto (car READS) for every read. With RANGE
the source also has a ranged read; without it the port composes one."
  (let ((nodes (make-fake-filesystem :files `(("/work/big.bin" . ,octets)) :directories '("/work"))))
    (flet ((note (&rest record) (push record (car reads))))
      (make-inspect-ports
       :workspace-host (make-fake-host nodes)
       :text-source (apply #'aitools.text.application:make-text-source
                           :file-size (lambda (path) (note :size path) (length octets))
                           :read-prefix (lambda (path count)
                                          (note :prefix path count)
                                          (subseq octets 0 (min count (length octets))))
                           :read-octets (lambda (path) (note :all path) octets)
                           :call-with-chunks (lambda (path size function)
                                               (note :chunks path size)
                                               (loop for start from 0 below (length octets) by size
                                                     until (eq (funcall function
                                                                        (subseq octets start
                                                                                (min (length octets) (+ start size))))
                                                               :stop))
                                               t)
                           (when range
                             (list :read-range
                                   (lambda (path start end)
                                     (note :range path start end)
                                     (let ((end (min end (length octets))))
                                       (values (subseq octets (min start end) end) (length octets)))))))
       :open-store (lambda (root) (fail (format nil "open-store ~A" root)))
       :state-directory-function (constantly nil)))))

(defun %patterned-octets (size)
  (let ((octets (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (index size octets) (setf (aref octets index) (mod index 251)))))

(describe "read --as hex and --as strings read only what they show"
  (it-each (("a composed ranged read" nil) ("the source's ranged read" t))
      "dumps a --bytes window of a large file through ~A without reading the whole file"
      (label range)
    (declare (ignore label))
    (let ((reads (list nil)))
      (multiple-value-bind (kind fields)
          (run-flow #'read-flow (%recording-read-ports (%patterned-octets (* 1024 1024)) reads :range range)
                    "big.bin" :as "hex" :bytes "600000:600020")
        (expect kind :to-be :ok)
        (expect (list (field fields "size") (field fields "start") (field fields "end"))
                :to-equal (list (* 1024 1024) 600000 600020))
        (expect (mapcar (lambda (row) (json-object-get row "offset")) (field fields "rows")) :to-equal '(600000 600016))
        (expect (json-object-get (second (field fields "rows")) "hex")
                :to-equal (format nil "~{~(~2,'0x~)~^ ~}" (loop for index from 600016 below 600020 collect (mod index 251))))
        (expect (find :all (car reads) :key #'first) :to-be nil))))

  (it "asks the source's ranged read for the window and a bounded redaction margin only"
    (let ((reads (list nil)))
      (run-flow #'read-flow (%recording-read-ports (%patterned-octets (* 1024 1024)) reads :range t)
                "big.bin" :as "hex" :bytes "600000:600020")
      (let ((ranges (remove :range (car reads) :key #'first :test-not #'eq)))
        (expect (length ranges) :to-be 1)
        (destructuring-bind (operation path start end) (first ranges)
          (declare (ignore operation path))
          (expect (<= start 600000) :to-be t)
          (expect (>= end 600020) :to-be t)
          (expect (<= (- end start) (+ 20 (* 2 4096))) :to-be t)))))

  (it "lists --as strings of a large file chunk by chunk, keeping runs and characters split across chunks"
    (let* ((octets (make-array (* 3 65536) :element-type '(unsigned-byte 8) :initial-element 0))
           (straddling (string-bytes (format nil "straddle~Crun" (code-char #x3042))))
           (reads (list nil)))
      (replace octets (string-bytes "first") :start1 10)
      ;; The run starts before the 64 KiB chunk edge and its 3-byte character
      ;; sits across it.
      (replace octets straddling :start1 (- 65536 9))
      (replace octets (string-bytes "last") :start1 (- (* 3 65536) 4))
      (multiple-value-bind (kind fields)
          (run-flow #'read-flow (%recording-read-ports octets reads) "big.bin" :as "strings")
        (expect kind :to-be :ok)
        (expect (field fields "size") :to-be (* 3 65536))
        (expect (mapcar (lambda (item) (list (json-object-get item "offset") (json-object-get item "text")))
                        (field fields "strings"))
                :to-equal (list (list 10 "first")
                                (list (- 65536 9) (format nil "straddle~Crun" (code-char #x3042)))
                                (list (- (* 3 65536) 4) "last")))
        (expect (find :all (car reads) :key #'first) :to-be nil)))))

(describe "read cuts a line longer than the per-line byte cap"
  (flet ((long-file (line) `(("/work/long.txt" . ,(format nil "~A~%short~%" line)))))
    (it-each (("the byte path" ())
              ("the decoded path" (:escape-invisible t))
              ("a --match selection" (:match "a")))
        "returns the first 16384 bytes, marks the line, and names the --as hex rest on ~A"
        (label options)
      (declare (ignore label))
      (multiple-value-bind (kind fields)
          (apply #'read-file (long-file (make-string 50000 :initial-element #\a)) "long.txt" options)
        (expect kind :to-be :partial)
        (expect (length (first (field fields "lines"))) :to-be 16384)
        (expect (field fields "cut_lines") :to-equal '(1))
        (expect (json-false-value-p (field fields "truncated")) :to-be nil)
        (expect (field fields "next_commands") :to-contain "aitools read long.txt --as hex --bytes 16384:16640")))

    (it "cuts before a multi-byte character the cap would split"
      (multiple-value-bind (kind fields)
          (read-file (long-file (make-string 6000 :initial-element (code-char #x3042))) "long.txt")
        (expect kind :to-be :partial)
        (expect (field fields "lines") :to-equal (list (make-string 5461 :initial-element (code-char #x3042)) "short"))
        (expect (field fields "encoding_errors") :to-be 0)
        (expect (field fields "next_commands") :to-contain "aitools read long.txt --as hex --bytes 16383:16639")))

    (it "leaves a line of exactly 16384 bytes whole"
      (multiple-value-bind (kind fields) (read-file (long-file (make-string 16384 :initial-element #\a)) "long.txt")
        (expect kind :to-be :ok)
        (expect (length (first (field fields "lines"))) :to-be 16384)
        (expect (field fields "cut_lines") :to-be nil)))

    (it "cuts by characters with --encoding and still marks the line"
      (multiple-value-bind (kind fields)
          (read-file (long-file (make-string 20000 :initial-element #\a)) "long.txt" :encoding "iso-8859-1")
        (expect kind :to-be :partial)
        (expect (length (first (field fields "lines"))) :to-be 16384)
        (expect (field fields "cut_lines") :to-equal '(1))))))
