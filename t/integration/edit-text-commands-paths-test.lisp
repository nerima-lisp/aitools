;;;; t/integration/edit-text-commands-paths-test.lisp
;;;;
;;;; The text commands' remaining paths: apply's created, deleted and
;;;; renamed files, transform and move-lines positions, write, split and
;;;; transcode refusals, and files without matches.
(in-package #:aitools.edit.test)

(describe "aitools apply: created, deleted and renamed files"
  (it "creates a file from /dev/null, refusing one that exists"
    (with-workspace ()
      (let ((diff (format nil "--- /dev/null~%+++ b/new.txt~%@@ -0,0 +1,2 @@~%+one~%+two~%")))
        (setf *stdin* (bytes diff))
        (multiple-value-bind (kind fields) (run "apply" '() :stdin t)
          (expect kind :to-be :ok)
          (expect (changes fields) :to-equal '(("new.txt" "created"))))
        (expect (text "new.txt") :to-equal (format nil "one~%two~%"))
        (with-error (code message) (run "apply" '() :stdin t)
          (expect code :to-equal "refusal.exists")
          (expect message :to-equal "the patch creates new.txt, which exists")))))

  (it "refuses a creation hunk that expects existing lines"
    (with-workspace ()
      (setf *stdin* (bytes (format nil "--- /dev/null~%+++ b/new.txt~%@@ -1,1 +1,2 @@~% ctx~%+one~%")))
      (with-error (code message) (run "apply" '() :stdin t)
        (expect code :to-equal "selection.no-match")
        (expect message :to-equal "hunk 1 does not apply to new file new.txt"))
      (expect (kind "new.txt") :to-be :absent)))

  (it "deletes a file whose every line the patch removes, and refuses when content remains"
    (with-workspace ()
      (put "gone.txt" (format nil "a~%b~%"))
      (put "kept.txt" (format nil "a~%b~%c~%"))
      (setf *stdin* (bytes (format nil "--- a/kept.txt~%+++ /dev/null~%@@ -1,2 +0,0 @@~%-a~%-b~%")))
      (with-error (code message) (run "apply" '() :stdin t)
        (expect code :to-equal "selection.no-match")
        (expect message :to-equal "the patch deletes kept.txt but content remains"))
      (setf *stdin* (bytes (format nil "--- a/gone.txt~%+++ /dev/null~%@@ -1,2 +0,0 @@~%-a~%-b~%")))
      (multiple-value-bind (kind fields) (run "apply" '() :stdin t)
        (expect kind :to-be :ok)
        (expect (changes fields) :to-equal '(("gone.txt" "deleted"))))
      (expect (kind "gone.txt") :to-be :absent)))

  (it "renames a file the patch moves, and --reverse moves it back"
    (with-workspace ()
      (put "old.txt" (format nil "a~%b~%"))
      (setf *stdin* (bytes (format nil "--- a/old.txt~%+++ b/new.txt~%@@ -1,2 +1,2 @@~% a~%-b~%+B~%")))
      (expect (run "apply" '() :stdin t) :to-be :ok)
      (expect (kind "old.txt") :to-be :absent)
      (expect (text "new.txt") :to-equal (format nil "a~%B~%"))
      (expect (run "apply" '() :stdin t :reverse t) :to-be :ok)
      (expect (kind "new.txt") :to-be :absent)
      (expect (text "old.txt") :to-equal (format nil "a~%b~%"))))

  (it-each (("adds a final newline the old side lacked"
             "a~%b" "--- a/f.txt~%+++ b/f.txt~%@@ -1,2 +1,2 @@~% a~%-b~%\\ No newline at end of file~%+b~%" "a~%b~%")
            ("drops the final newline the new side lacks"
             "a~%b~%" "--- a/f.txt~%+++ b/f.txt~%@@ -1,2 +1,2 @@~% a~%-b~%+b~%\\ No newline at end of file~%" "a~%b"))
      "~A"
      (name before diff after)
    (declare (ignore name))
    (with-workspace ()
      (put "f.txt" (format nil before))
      (setf *stdin* (bytes (format nil diff)))
      (expect (run "apply" '() :stdin t) :to-be :ok)
      (expect (text "f.txt") :to-equal (format nil after)))))

(describe "aitools transform and move-lines: remaining selections and positions"
  (it "applies an op that changes the line count to each scattered --match run"
    (with-workspace ()
      (put "a.txt" (format nil "x1~%x1~%keep~%x2~%x2~%"))
      (multiple-value-bind (kind fields) (run "transform" '("a.txt") :op '("unique") :match "^x" :expect-count "4")
        (expect kind :to-be :ok)
        (expect (field fields "removed_lines") :to-be 2))
      (expect (text "a.txt") :to-equal (format nil "x1~%keep~%x2~%"))))

  (it "keeps the lines of a scattered --match in place when the op keeps their number"
    (with-workspace ()
      (put "a.txt" (format nil "b~%keep~%a~%"))
      (with-error (code) (run "transform" '("a.txt") :op '("sort") :match "^[ab]$" :expect-count "3")
        (expect code :to-equal "selection.count-mismatch"))
      (expect (run "transform" '("a.txt") :op '("sort") :match "^[ab]$" :expect-count "2") :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "a~%keep~%b~%"))))

  (it-each (("eol-lf" "a|~%b|~%" "a~%b~%")
            ("no-final-newline" "a~%b~%" "a~%b")
            ("strip-bom" "@a~%" "a~%"))
      "applies whole-file op ~A"
      (op before after)
    (flet ((expand (template)
             (substitute (code-char #xFEFF) #\@ (coerce (substitute #\Return #\| (format nil template)) '(vector character)))))
      (with-workspace ()
        (put "a.txt" (expand before))
        (expect (run "transform" '("a.txt") :op (list op)) :to-be :ok)
        (expect (text "a.txt") :to-equal (expand after)))))

  (it "leaves an empty file empty, reporting no change"
    (with-workspace ()
      (put "e.txt" "")
      (multiple-value-bind (kind fields) (run "transform" '("e.txt") :op '("sort"))
        (expect kind :to-be :ok)
        (expect (field fields "changes") :to-equal '()))))

  (it "comments a Markdown file with its block marker"
    (with-workspace ()
      (put "r.md" (format nil "a~%"))
      (expect (run "transform" '("r.md") :op '("comment")) :to-be :ok)
      (expect (text "r.md") :to-equal (format nil "<!-- a -->~%"))))

  (it "refuses a position past the end, inside the moved lines, or in a missing destination"
    (with-workspace ()
      (put "a.txt" (format nil "1~%2~%3~%"))
      (with-error (code message keys) (run "move-lines" '("a.txt") :range "1" :to-position "after:9"
                                                                   :expect-hash (list (hash "a.txt")))
        (expect code :to-equal "selection.no-match")
        (expect message :to-equal "a.txt has 3 lines; line 9 does not exist")
        (expect (json-field (first (getf keys :candidates)) "text") :to-equal "3"))
      (with-error (code message) (run "move-lines" '("a.txt") :range "1:2" :to-position "before:2" :expect-hash (list (hash "a.txt")))
        (expect code :to-equal "argument.invalid")
        (expect message :to-equal "--to-position falls inside the lines being moved"))
      (with-error (code message) (run "move-lines" '("a.txt") :range "1" :to "b.txt" :to-position "after:1"
                                                              :expect-hash (list (format nil "a.txt=~A" (hash "a.txt"))
                                                                                 "b.txt=0000"))
        (expect code :to-equal "refusal.target-changed")
        (expect (search "b.txt changed" message) :to-be-truthy))
      (expect (text "a.txt") :to-equal (format nil "1~%2~%3~%"))))

  (it "moves lines into a new file at its start, and before a symbol of another file"
    (with-workspace ()
      (put "a.txt" (format nil "1~%2~%"))
      (expect (run "move-lines" '("a.txt") :match "^2$" :expect-count "1" :to "new.txt" :to-position "start") :to-be :ok)
      (expect (text "new.txt") :to-equal (format nil "2~%"))
      (put "b.lisp" (format nil "(defun f ()~%  1)~%"))
      (expect (run "move-lines" '("a.txt") :range "1" :to "b.lisp" :to-position "before-symbol:f"
                                           :expect-hash (list (format nil "a.txt=~A" (hash "a.txt"))
                                                              (format nil "b.lisp=~A" (hash "b.lisp"))))
              :to-be :ok)
      (expect (text "b.lisp") :to-equal (format nil "1~%(defun f ()~%  1)~%"))
      (expect (text "a.txt") :to-equal ""))))

(describe "aitools write, split and transcode: remaining refusals"
  (it "refuses to write over a directory and writes --stdin bytes"
    (with-workspace ()
      (sb-posix:mkdir (disk "d") #o755)
      (with-error (code message) (run "write" '("d") :content '("x"))
        (expect code :to-equal "refusal.not-a-file")
        (expect message :to-equal "d is a directory"))
      (setf *stdin* (octet-vector 1 2 3))
      (expect (run "write" '("s.bin") :stdin t) :to-be :ok)
      (expect (octets-of "s.bin") :to-equalp (octet-vector 1 2 3))))

  (it "refuses more pieces than --suffix-digits can name and a --prefix outside the workspace"
    (with-workspace ()
      (put "a.txt" (format nil "~{~A~%~}" (loop for i below 11 collect i)))
      (let ((before (snapshot)))
        (with-error (code message) (run "split" '("a.txt") :lines "1" :suffix-digits "1")
          (expect code :to-equal "argument.invalid")
          (expect message :to-equal "11 pieces need more than --suffix-digits 1"))
        (with-error (code) (run "split" '("a.txt") :lines "20" :prefix "../piece-")
          (expect code :to-equal "refusal.outside-workspace"))
        (expect-unchanged before))))

  (it-each (("a binary file" (1 0 2 10 3))
            ("a file with no line ending" (97 98 99))
            ("a file that is not UTF-8" (97 #xFF 10 98 10)))
      "splits ~A by bytes"
      (name octets)
    (declare (ignore name))
    (with-workspace ()
      (put "f" (apply #'octet-vector octets))
      (expect (run "split" '("f") :bytes "2") :to-be :ok)
      (expect (octets-of "f.001") :to-equalp (apply #'octet-vector (subseq octets 0 2)))))

  (it "refuses a file whose encoding cannot be guessed or that is not valid in --from"
    (with-workspace ()
      (put "bin" (octet-vector 0 159 146 150 0 1 2 255 254 0))
      (with-error (code message) (run "transcode" '("bin"))
        (expect code :to-equal "input.unsupported-format")
        (expect message :to-equal "cannot tell bin's encoding; pass --from"))
      (put "bad.txt" (octet-vector 97 #xFF))
      (with-error (code message keys) (run "transcode" '("bad.txt") :from "utf-8" :to "utf-16le")
        (expect code :to-equal "input.syntax-error")
        (expect (json-field (first (getf keys :diagnostics)) "offset") :to-be 1))))

  (it "keeps a BOM into another Unicode encoding and writes nothing when the bytes stay the same"
    (with-workspace ()
      (put "b.txt" (format nil "~Ca~%" (code-char #xFEFF)))
      (expect (run "transcode" '("b.txt") :from "utf-8" :to "utf-16le") :to-be :ok)
      (expect (octets-of "b.txt") :to-equalp (octet-vector #xFF #xFE 97 0 10 0))
      (put "same.txt" (format nil "same~%"))
      (multiple-value-bind (kind fields) (run "transcode" '("same.txt") :from "utf-8" :to "utf-8")
        (expect kind :to-be :ok)
        (expect (field fields "changes") :to-equal '())))))

(describe "aitools text commands: remaining paths through the flows"
  (it "reports no strategy for several --old edits and a strategy for one"
    (with-workspace ()
      (put "a.txt" (format nil "a~%b~%"))
      (setf *stdin* (bytes "{\"edits\": [{\"old\": \"a\", \"new\": \"A\"}, {\"old\": \"b\", \"new\": \"B\"}]}"))
      (multiple-value-bind (kind fields) (run "edit" '("a.txt") :stdin t)
        (expect kind :to-be :ok)
        (expect (assoc "strategy" fields :test #'string=) :to-be nil))
      (setf *stdin* (bytes "{\"old\": \"A\", \"new\": \"x\"}"))
      (multiple-value-bind (kind fields) (run "edit" '("a.txt") :stdin t)
        (expect kind :to-be :ok)
        (expect (field fields "strategy") :to-equal "exact"))
      (expect (text "a.txt") :to-equal (format nil "x~%B~%"))))

  (it "offers no candidates below a path whose parent is a file"
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (with-error (code message keys) (run "edit" '("a.txt/x") :old "a" :new "b")
        (expect code :to-equal "input.not-found")
        (expect (getf keys :candidates) :to-equal '()))))

  (it "refuses --symbol in a file of no known language"
    (with-workspace ()
      (put "x.unknownext" (format nil "f~%"))
      (with-error (code message) (run "edit" '("x.unknownext") :symbol "f" :new "g" :expect-hash (list (hash "x.unknownext")))
        (expect code :to-equal "input.unsupported-language")
        (expect (search "--symbol" message) :to-be-truthy))))

  (it "applies a diff whose paths carry no a/ or b/ prefix, and refuses one for a missing file"
    (with-workspace ()
      (put "f.txt" (format nil "one~%"))
      (setf *stdin* (bytes (format nil "--- f.txt~%+++ f.txt~%@@ -1 +1 @@~%-one~%+ONE~%")))
      (expect (run "apply" '() :stdin t) :to-be :ok)
      (expect (text "f.txt") :to-equal (format nil "ONE~%"))
      (setf *stdin* (bytes (format nil "--- a/gone.txt~%+++ b/gone.txt~%@@ -1 +1 @@~%-one~%+ONE~%")))
      (with-error (code message) (run "apply" '() :stdin t)
        (expect code :to-equal "input.not-found")
        (expect message :to-equal "gone.txt does not exist"))))

  (it "refuses a split piece that a symlink would take outside the workspace"
    (with-workspace ()
      (put "a.txt" (format nil "1~%2~%"))
      (sb-posix:symlink "/tmp/aitools-outside-piece" (disk "a.txt.002"))
      (let ((before (snapshot)))
        (with-error (code) (run "split" '("a.txt") :lines "1")
          (expect code :to-equal "refusal.outside-workspace"))
        (expect-unchanged before))))

  (it "transcodes an empty file to nothing new"
    (with-workspace ()
      (put "e.txt" "")
      (multiple-value-bind (kind fields) (run "transcode" '("e.txt") :from "utf-8" :to "utf-16le")
        (expect kind :to-be :ok)
        (expect (field fields "changes") :to-equal '()))))

  (it "names an empty destination's end when a position is past it"
    (with-workspace ()
      (put "a.txt" (format nil "1~%"))
      (put "e.txt" "")
      (with-error (code message keys) (run "move-lines" '("a.txt") :range "1" :to "e.txt" :to-position "after:1"
                                                                   :expect-hash (list (format nil "a.txt=~A" (hash "a.txt"))
                                                                                      (format nil "e.txt=~A" (hash "e.txt"))))
        (expect code :to-equal "selection.no-match")
        (expect (json-field (first (getf keys :candidates)) "text") :to-equal ""))))

  (it "needs the destination's own PATH=HASH for a position-based move into another file"
    (with-workspace ()
      (put "a.txt" (format nil "1~%2~%"))
      (put "b.txt" (format nil "x~%y~%"))
      (let ((before (snapshot)))
        (with-error (code message) (run "move-lines" '("a.txt") :range "1" :to "b.txt" :to-position "after:1"
                                                                :expect-hash (list (hash "a.txt")))
          (expect code :to-equal "argument.invalid")
          (expect (search "--expect-hash b.txt=<hash>" message) :to-be-truthy))
        (with-error (code) (run "move-lines" '("a.txt") :range "1" :to "new.txt" :to-position "after:1"
                                                        :expect-hash (list (hash "a.txt") "new.txt=0000"))
          (expect code :to-equal "refusal.target-changed"))
        (expect-unchanged before)))))

(describe "aitools replace and apply: files without matches and short paths"
  (it "counts only the files with a match, scanning below subdirectories"
    (with-workspace ()
      (put "a.txt" (format nil "foo~%"))
      (put "b.txt" (format nil "bar~%"))
      (put "sub/c.txt" (format nil "foo~%"))
      (multiple-value-bind (kind fields) (run "replace" '("foo" "baz" "a.txt" "b.txt") :expect-count "1")
        (expect kind :to-be :ok)
        (expect (changes fields) :to-equal '(("a.txt" "modified"))))
      (multiple-value-bind (kind fields) (run "replace" '("foo" "qux") :expect-count "1")
        (expect kind :to-be :ok)
        (expect (changes fields) :to-equal '(("sub/c.txt" "modified"))))
      (expect (text "b.txt") :to-equal (format nil "bar~%"))))

  (it "applies a diff naming a one-letter file"
    (with-workspace ()
      (put "f" (format nil "one~%"))
      (setf *stdin* (bytes (format nil "--- f~%+++ f~%@@ -1 +1 @@~%-one~%+ONE~%")))
      (expect (run "apply" '() :stdin t) :to-be :ok)
      (expect (text "f") :to-equal (format nil "ONE~%")))))
