;;;; t/integration/edit-text-commands-test.lisp
;;;;
;;;; The text-editing commands after `edit` against a real temporary workspace: insert, replace, apply,
;;;; transform, move-lines, write, split and transcode, and their undo. The
;;;; shared helpers and the pipeline-level cases are in edit-text-test.lisp;
;;;; argument refusals and inputs are in edit-text-commands-inputs-test.lisp,
;;;; and the remaining paths in edit-text-commands-paths-test.lisp.
(in-package #:aitools.edit.test)

(describe "aitools insert"
  (it "inserts at the start, at the end, and around selected lines"
    (with-workspace ()
      (put "a.txt" (format nil "b~%d~%"))
      (multiple-value-bind (kind fields) (run "insert" '("a.txt") :at "start" :content "a")
        (expect kind :to-be :ok)
        (expect (field fields "inserted_at") :to-equal '(1)))
      (expect (run "insert" '("a.txt") :after t :range "2" :content "c" :expect-hash (list (hash "a.txt"))) :to-be :ok)
      (expect (run "insert" '("a.txt") :before t :match "^d$" :expect-count "1" :content "c2") :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "a~%b~%c~%c2~%d~%"))))

  (it "inserts after every --match line, checking the count"
    (with-workspace ()
      (put "a.txt" (format nil "x~%y~%x~%"))
      (with-error (code) (run "insert" '("a.txt") :after t :match "^x" :content "+" :expect-count "1")
        (expect code :to-equal "selection.count-mismatch"))
      (multiple-value-bind (kind fields) (run "insert" '("a.txt") :after t :match "^x" :content "+" :expect-count "2")
        (expect kind :to-be :ok)
        (expect (field fields "inserted_at") :to-equal '(2 5)))
      (expect (text "a.txt") :to-equal (format nil "x~%+~%y~%x~%+~%")))))

(describe "aitools replace"
  (it "replaces across files and writes nothing when the count differs"
    (with-workspace ()
      (put "a.txt" (format nil "foo foo~%"))
      (put "b.txt" (format nil "foo~%"))
      (let ((before (snapshot)))
        (with-error (code message keys) (run "replace" '("foo" "bar") :expect-count "2")
          (expect code :to-equal "selection.count-mismatch")
          (expect (json-field (first (getf keys :diagnostics)) "actual") :to-be 3))
        (expect-unchanged before))
      (multiple-value-bind (kind fields) (run "replace" '("foo" "bar") :expect-count "3")
        (expect kind :to-be :ok)
        (expect (mapcar (lambda (change) (json-field change "count")) (field fields "changes")) :to-equal '(2 1)))
      (expect (text "a.txt") :to-equal (format nil "bar bar~%"))))

  (it "honours --word, --nth, --multiline, --fixed and --ignore-case"
    (with-workspace ()
      (put "a.txt" (format nil "cat concat Cat~%"))
      (expect (run "replace" '("cat" "dog" "a.txt") :word t :ignore-case t :expect-count "2") :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "dog concat dog~%"))
      (put "n.txt" (format nil "x x x~%"))
      (expect (run "replace" '("x" "y" "n.txt") :nth "2" :expect-count "1") :to-be :ok)
      (expect (text "n.txt") :to-equal (format nil "x y x~%"))
      (put "m.txt" (format nil "begin~%body~%end~%"))
      (expect (run "replace" (list (format nil "begin\\nbody") "start" "m.txt") :multiline t :expect-count "1") :to-be :ok)
      (expect (text "m.txt") :to-equal (format nil "start~%end~%"))
      (put "f.txt" (format nil "a.b axb~%"))
      (expect (run "replace" '("a.b" "!" "f.txt") :fixed t :expect-count "1") :to-be :ok)
      (expect (text "f.txt") :to-equal (format nil "! axb~%"))))

  (it "limits a single file's replacement to the selection and refuses a selector with several files"
    (with-workspace ()
      (put "a.txt" (format nil "v~%v~%v~%"))
      (put "b.txt" (format nil "v~%"))
      (expect (run "replace" '("v" "w" "a.txt") :range "2" :expect-count "1" :expect-hash (list (hash "a.txt"))) :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "v~%w~%v~%"))
      (with-error (code) (run "replace" '("v" "w" "a.txt" "b.txt") :match "v" :expect-count "2")
        (expect code :to-equal "argument.invalid"))))

  (it "expands templates and filters, refuses \\1, and allows it with --literal-replacement"
    (with-workspace ()
      (put "a.txt" (format nil "item_007 getValue~%"))
      (expect (run "replace" '("item_(\\d+) (?<name>\\w+)" "item_${1:inc} ${name:snake}" "a.txt") :expect-count "1") :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "item_008 get_value~%"))
      (with-error (code message keys) (run "replace" '("(item)" "\\1x" "a.txt") :expect-count "1")
        (expect code :to-equal "argument.invalid")
        (expect (search "${1}x" (first (repair-commands keys))) :to-be-truthy))
      (expect (run "replace" '("(item)" "\\1x" "a.txt") :expect-count "1" :literal-replacement t) :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "\\1x_008 get_value~%"))
      (with-error (code) (run "replace" '("(get)" "${1:inc}" "a.txt") :expect-count "1")
        (expect code :to-equal "argument.invalid"))))

  (it "matches one non-ASCII character with ."
    (with-workspace ()
      (put "a.txt" (format nil "[あ]~%"))
      (expect (run "replace" '("\\[.\\]" "ok" "a.txt") :expect-count "1") :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "ok~%")))))

(defparameter +git-diff+
  (format nil "diff --git a/src/a.txt b/src/a.txt~%index 1111111..2222222 100644~%--- a/src/a.txt~%+++ b/src/a.txt~%@@ -1,3 +1,3 @@~% one~%-two~%+TWO~% three~%"))

(describe "aitools apply"
  (it "applies git diff output, stripping a/ and b/"
    (with-workspace ()
      (put "src/a.txt" (format nil "one~%two~%three~%"))
      (setf *stdin* (bytes +git-diff+))
      (multiple-value-bind (kind fields) (run "apply" '() :stdin t)
        (expect kind :to-be :ok)
        (expect (field fields "applied_hunks") :to-be 1))
      (expect (text "src/a.txt") :to-equal (format nil "one~%TWO~%three~%"))))

  (it "finds a shifted hunk within --fuzz, and --reverse undoes it"
    (with-workspace ()
      (put "src/a.txt" (format nil "new~%lines~%one~%two~%three~%"))
      (setf *stdin* (bytes +git-diff+))
      (expect (run "apply" '() :stdin t :fuzz "3") :to-be :ok)
      (expect (text "src/a.txt") :to-equal (format nil "new~%lines~%one~%TWO~%three~%"))
      (expect (run "apply" '() :stdin t :reverse t) :to-be :ok)
      (expect (text "src/a.txt") :to-equal (format nil "new~%lines~%one~%two~%three~%"))))

  (it "honours --strip"
    (with-workspace ()
      (put "a.txt" (format nil "one~%two~%three~%"))
      (setf *stdin* (bytes +git-diff+))
      (expect (run "apply" '() :stdin t :strip "2") :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "one~%TWO~%three~%"))))

  (it "writes nothing when one hunk of several files misses"
    (with-workspace ()
      (put "src/a.txt" (format nil "one~%two~%three~%"))
      (put "src/b.txt" (format nil "x~%"))
      (let ((before (snapshot)))
        (setf *stdin* (bytes (concatenate 'string +git-diff+
                                          (format nil "--- a/src/b.txt~%+++ b/src/b.txt~%@@ -1 +1 @@~%-nope~%+yes~%"))))
        (with-error (code message keys) (run "apply" '() :stdin t)
          (expect code :to-equal "selection.no-match")
          (expect (json-field (first (getf keys :candidates)) "path") :to-equal "src/b.txt"))
        (expect-unchanged before)))))

(describe "aitools transform"
  (it "applies ops in order, only inside the selection"
    (with-workspace ()
      (put "a.txt" (format nil "keep~%b~%a~%b~%end~%"))
      (multiple-value-bind (kind fields)
          (run "transform" '("a.txt") :op '("unique" "sort") :between '("^keep$" "^end$") :exclusive t)
        (expect kind :to-be :ok)
        (expect (field fields "removed_lines") :to-be 1))
      (expect (text "a.txt") :to-equal (format nil "keep~%a~%b~%end~%"))))

  (it "applies whole-file ops, refusing them with a selector"
    (with-workspace ()
      (put "a.txt" (format nil "a~%b"))
      (expect (run "transform" '("a.txt") :op '("eol-crlf" "final-newline")) :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "a~C~%b~C~%" #\Return #\Return))
      (with-error (code) (run "transform" '("a.txt") :op '("eol-lf") :range "1" :expect-hash (list (hash "a.txt")))
        (expect code :to-equal "argument.invalid"))))

  (it "requires --seed for shuffle and a known comment syntax for comment"
    (with-workspace ()
      (put "a.txt" (format nil "1~%2~%3~%"))
      (put "a.unknownext" (format nil "x~%"))
      (with-error (code) (run "transform" '("a.txt") :op '("shuffle")) (expect code :to-equal "argument.invalid"))
      (expect (run "transform" '("a.txt") :op '("shuffle") :seed "5") :to-be :ok)
      (let ((once (text "a.txt")))
        (put "a.txt" (format nil "1~%2~%3~%"))
        (run "transform" '("a.txt") :op '("shuffle") :seed "5")
        (expect (text "a.txt") :to-equal once))
      (with-error (code) (run "transform" '("a.unknownext") :op '("comment"))
        (expect code :to-equal "input.unsupported-language")))))

(describe "aitools move-lines"
  (it "moves lines within a file and into another file"
    (with-workspace ()
      (put "a.txt" (format nil "1~%2~%3~%4~%"))
      (put "b.txt" (format nil "x~%"))
      (expect (run "move-lines" '("a.txt") :range "1" :to-position "after:3" :expect-hash (list (hash "a.txt"))) :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "2~%3~%1~%4~%"))
      (expect (run "move-lines" '("a.txt") :match "^4$" :expect-count "1" :to "b.txt" :to-position "start") :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "2~%3~%1~%"))
      (expect (text "b.txt") :to-equal (format nil "4~%x~%"))))

  (it "refuses a position-based move when either hash is stale, changing neither file"
    (with-workspace ()
      (put "a.txt" (format nil "1~%2~%"))
      (put "b.txt" (format nil "x~%y~%"))
      (let ((before (snapshot)))
        (with-error (code) (run "move-lines" '("a.txt") :range "1" :to "b.txt" :to-position "before:2"
                                                        :expect-hash (list (format nil "a.txt=~A" (hash "a.txt")) "b.txt=0000"))
          (expect code :to-equal "refusal.target-changed"))
        (expect-unchanged before))
      (expect (run "move-lines" '("a.txt") :range "1" :to "b.txt" :to-position "before:2"
                                           :expect-hash (list (format nil "a.txt=~A" (hash "a.txt"))
                                                              (format nil "b.txt=~A" (hash "b.txt"))))
              :to-be :ok)
      (expect (text "b.txt") :to-equal (format nil "x~%1~%y~%")))))

(describe "aitools write, split and transcode"
  (it "refuses to replace a file without --overwrite and concatenates with --separator"
    (with-workspace ()
      (put "a.txt" "old")
      (with-error (code) (run "write" '("a.txt") :content '("new")) (expect code :to-equal "refusal.exists"))
      (put "p1" "one")
      (put "p2" "two")
      (expect (run "write" '("joined.txt") :content-file (list (disk "p1") (disk "p2")) :separator "--") :to-be :ok)
      (expect (text "joined.txt") :to-equal "one--two")
      (expect (run "write" '("a.txt") :content '("new") :overwrite t :expect-hash (list (hash "a.txt"))) :to-be :ok)
      (expect (text "a.txt") :to-equal "new")))

  (it "splits by lines and refuses when a piece exists"
    (with-workspace ()
      (put "a.txt" (format nil "1~%2~%3~%"))
      (multiple-value-bind (kind fields) (run "split" '("a.txt") :lines "2")
        (expect kind :to-be :ok)
        (expect (mapcar (lambda (change) (list (json-field change "path") (json-field change "start_line")
                                               (json-field change "lines")))
                        (field fields "changes"))
                :to-equal '(("a.txt.001" 1 2) ("a.txt.002" 3 1))))
      (expect (text "a.txt.002") :to-equal (format nil "3~%"))
      (let ((before (snapshot)))
        (with-error (code) (run "split" '("a.txt") :lines "1") (expect code :to-equal "refusal.exists"))
        (expect-unchanged before))))

  (it-each (("shift_jis") ("euc-jp") ("utf-16le") ("utf-16be"))
      "round-trips ~A through UTF-8 byte for byte"
      (encoding)
    (with-workspace ()
      (let ((original (aitools.text.domain:encode-string/k (format nil "日本語テキスト~%abc~%")
                                                            (aitools.text.domain:find-encoding encoding)
                                                            :on-encoded (lambda (octets replaced) (declare (ignore replaced)) octets))))
        (put "j.txt" original)
        (multiple-value-bind (kind fields) (run "transcode" '("j.txt") :from encoding)
          (expect kind :to-be :ok)
          (expect (field fields "to") :to-equal "utf-8"))
        (expect (text "j.txt") :to-equal (format nil "日本語テキスト~%abc~%"))
        (expect (run "transcode" '("j.txt") :from "utf-8" :to encoding) :to-be :ok)
        (expect (octets-of "j.txt") :to-equalp original))))

  (it "refuses unrepresentable characters unless --replace-unmappable"
    (with-workspace ()
      (put "e.txt" (format nil "caf~C 😀~%" (code-char #xE9)))
      (let ((before (snapshot)))
        (with-error (code) (run "transcode" '("e.txt") :to "iso-8859-1") (expect code :to-equal "argument.invalid"))
        (expect-unchanged before))
      (multiple-value-bind (kind fields) (run "transcode" '("e.txt") :to "iso-8859-1" :replace-unmappable t)
        (expect kind :to-be :ok)
        (expect (field fields "replaced") :to-be 1))
      (expect (octets-of "e.txt") :to-equalp (octet-vector 99 97 102 #xE9 32 63 10)))))

(describe "aitools text writes and undo"
  (it "restores the original bytes for each text command's op"
    (with-workspace ()
      (put "a.txt" (format nil "b~%a~%b~%"))
      (put "c.json" (format nil "{~%  \"k\": 1~%}~%"))
      (let ((before (snapshot)))
        (dolist (call (list (list "edit" '("a.txt") :old "a" :new "z")
                            (list "insert" '("a.txt") :at "start" :content "top")
                            (list "replace" '("b" "B" "a.txt") :expect-count "2")
                            (list "transform" '("a.txt") :op '("sort"))
                            (list "move-lines" '("a.txt") :match "^a$" :expect-count "1" :to-position "start")
                            (list "transcode" '("a.txt") :to "utf-16le")
                            (list "json.set" '("c.json" "/k" "2"))
                            (list "split" '("a.txt") :lines "1")
                            (list "write" '("new.txt") :content '("n"))))
          (multiple-value-bind (kind fields) (apply #'run call)
            (expect kind :to-be :ok)
            (expect (undo (field fields "op_id")) :to-be :committed)
            (expect (snapshot) :to-equal before)))))))
