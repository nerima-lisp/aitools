;;;; t/integration/edit-text-commands-paths-test.lisp
;;;;
;;;; Text command path and selection cases.
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
