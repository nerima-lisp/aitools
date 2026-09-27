;;;; t/integration/edit-text-test.lisp
;;;;
;;;; The text-editing commands against a real temporary workspace through the write pipeline: `edit`,
;;;; guards, inputs, output shape, and the workspace boundary and text
;;;; layout rules. The write-pipeline checks are in
;;;; edit-text-pipeline-test.lisp, the production ports and lock rechecks in
;;;; edit-text-ports-test.lisp, and the later commands in
;;;; edit-text-commands-test.lisp; all of them use the helpers defined here.
(in-package #:aitools.edit.test)

(defun expect-unchanged (before)
  (expect (snapshot) :to-equal before))

(defmacro with-error ((code &optional message keys) form &body body)
  "Run FORM (a RUN call), expect an error, bind CODE, MESSAGE, KEYS."
  (let ((kind (gensym "KIND"))
        (message (or message (gensym "MESSAGE")))
        (keys (or keys (gensym "KEYS"))))
    `(multiple-value-bind (,kind ,code ,message ,keys) ,form
       (declare (ignorable ,message ,keys))
       (expect ,kind :to-be :error)
       ,@body)))

(defun repair-commands (keys)
  (mapcar (lambda (repair) (getf repair :command)) (getf keys :repairs)))

(defun candidate-lines (keys)
  (mapcar (lambda (candidate) (json-field candidate "line")) (getf keys :candidates)))

(describe "aitools edit"
  (it "replaces an exact --old and reports changes, op_id and strategy"
    (with-workspace ()
      (put "a.txt" (format nil "one~%two~%"))
      (multiple-value-bind (kind fields) (run "edit" '("a.txt") :old "two" :new "2")
        (expect kind :to-be :ok)
        (expect (changes fields) :to-equal '(("a.txt" "modified")))
        (expect (field fields "strategy") :to-equal "exact")
        (expect (stringp (field fields "op_id")) :to-be t)
        (expect (json-field (first (field fields "changes")) "diff") :to-equal (format nil "@@ -1,2 +1,2 @@~% one~%-two~%+2~%")))
      (expect (text "a.txt") :to-equal (format nil "one~%2~%"))))

  (it "returns the lines of every match when --old is ambiguous, and similar places when it matches nothing"
    (with-workspace ()
      (put "a.txt" (format nil "x = 1~%y = 2~%x = 1~%"))
      (let ((before (snapshot)))
        (with-error (code message keys) (run "edit" '("a.txt") :old "x = 1" :new "z")
          (expect code :to-equal "selection.ambiguous")
          (expect (candidate-lines keys) :to-equal '(1 3)))
        (with-error (code message keys) (run "edit" '("a.txt") :old "y = 3" :new "z")
          (expect code :to-equal "selection.no-match")
          (expect (first (candidate-lines keys)) :to-be 2)
          (expect (repair-commands keys) :to-equal '("aitools read a.txt")))
        (expect-unchanged before))))

  (it "matches ignoring indentation and keeps the file's indentation"
    (with-workspace ()
      (put "f.py" (format nil "def f():~%        if x:~%            go()~%"))
      (multiple-value-bind (kind fields) (run "edit" '("f.py") :old (format nil "if x:~%    go()") :new (format nil "if y:~%    stop()"))
        (expect kind :to-be :ok)
        (expect (field fields "strategy") :to-equal "whitespace"))
      (expect (text "f.py") :to-equal (format nil "def f():~%        if y:~%            stop()~%"))))

  (it "replaces and deletes lines chosen by --range, --between, --match and --symbol"
    (with-workspace ()
      (put "a.txt" (format nil "1~%2~%3~%4~%"))
      (expect (run "edit" '("a.txt") :range "2:3" :new "x" :expect-hash (list (hash "a.txt"))) :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "1~%x~%4~%"))
      (expect (run "edit" '("a.txt") :between '("^1$" "^4$") :exclusive t :new "") :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "1~%4~%"))
      (put "b.txt" (format nil "keep~%drop me~%keep too~%drop again~%"))
      (expect (run "edit" '("b.txt") :match "^drop" :new "" :expect-count "2") :to-be :ok)
      (expect (text "b.txt") :to-equal (format nil "keep~%keep too~%"))
      (expect (run "edit" '("b.txt") :match "too" :invert t :new "" :expect-count "1") :to-be :ok)
      (expect (text "b.txt") :to-equal (format nil "keep too~%"))
      (put "c.lisp" (format nil "(defun a ()~%  1)~%~%(defun b ()~%  2)~%"))
      (expect (run "edit" '("c.lisp") :symbol "b" :new (format nil "(defun b ()~%  3)") :expect-hash (list (hash "c.lisp")))
              :to-be :ok)
      (expect (text "c.lisp") :to-equal (format nil "(defun a ()~%  1)~%~%(defun b ()~%  3)~%"))))

  (it "applies edits[] in order and writes nothing when one fails"
    (with-workspace ()
      (put "a.txt" (format nil "a~%b~%c~%"))
      (setf *stdin* (bytes "{\"edits\":[{\"old\":\"a\",\"new\":\"A\"},{\"match\":\"^b$\",\"new\":\"B\",\"expect_count\":1}]}"))
      (expect (run "edit" '("a.txt") :stdin t) :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "A~%B~%c~%"))
      (let ((before (snapshot)))
        (setf *stdin* (bytes "{\"edits\":[{\"old\":\"A\",\"new\":\"x\"},{\"old\":\"missing\",\"new\":\"y\"}]}"))
        (with-error (code) (run "edit" '("a.txt") :stdin t)
          (expect code :to-equal "selection.no-match"))
        (setf *stdin* (bytes "{\"edits\":[{\"range\":\"1\",\"new\":\"x\"}]}"))
        (with-error (code) (run "edit" '("a.txt") :stdin t)
          (expect code :to-equal "argument.invalid"))
        (expect-unchanged before))))

  (it "refuses --old '', two selectors, and a selector without --new"
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (with-error (code) (run "edit" '("a.txt") :old "" :new "x") (expect code :to-equal "argument.invalid"))
      (with-error (code) (run "edit" '("a.txt") :range "1" :match "a" :new "x") (expect code :to-equal "argument.invalid"))
      (with-error (code) (run "edit" '("a.txt") :old "a" :range "1" :new "x") (expect code :to-equal "argument.invalid"))
      (with-error (code) (run "edit" '("a.txt") :match "a") (expect code :to-equal "argument.invalid")))))

(describe "aitools write guards"
  (it "requires --expect-hash for position-based selectors and points at aitools info"
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (with-error (code message keys) (run "edit" '("a.txt") :range "1" :new "b")
        (expect code :to-equal "argument.invalid")
        (expect (repair-commands keys) :to-equal '("aitools info a.txt")))))

  (it "requires --expect-count for --match writes and replace, with a --dry-run repair"
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (with-error (code message keys) (run "edit" '("a.txt") :match "a" :new "b")
        (expect code :to-equal "argument.invalid")
        (expect (search "--dry-run" (first (repair-commands keys))) :to-be-truthy))
      (with-error (code message keys) (run "replace" '("a" "b" "a.txt"))
        (expect code :to-equal "argument.invalid")
        (expect (search "--dry-run" (first (repair-commands keys))) :to-be-truthy))))

  (it "fails with exit-2 codes and writes nothing on a hash or count mismatch"
    (with-workspace ()
      (put "a.txt" (format nil "a~%a~%"))
      (let ((before (snapshot)))
        (with-error (code message keys) (run "edit" '("a.txt") :range "1" :new "b" :expect-hash '("0000"))
          (expect code :to-equal "refusal.target-changed")
          (expect (json-field (first (getf keys :conflicts)) "current") :to-equal (hash "a.txt")))
        (with-error (code message keys) (run "edit" '("a.txt") :match "a" :new "b" :expect-count "3")
          (expect code :to-equal "selection.count-mismatch")
          (expect (json-field (first (getf keys :diagnostics)) "actual") :to-be 2))
        (expect (aitools.protocol.domain:error-code-exit-code "refusal.target-changed") :to-be 2)
        (expect (aitools.protocol.domain:error-code-exit-code "selection.count-mismatch") :to-be 2)
        (expect-unchanged before)))))

(describe "aitools write inputs and outputs"
  (it "never reads stdin without --stdin"
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (setf *stdin* nil)
      (with-error (code) (run "insert" '("a.txt") :at "end")
        (expect code :to-equal "argument.invalid"))))

  (it "passes --stdin text through unchanged: newlines, quotes, backslashes, $1"
    (with-workspace ()
      (put "a.txt" (format nil "x~%"))
      (let ((payload (format nil "line \"q\" \\ $1~%second")))
        (setf *stdin* (bytes payload))
        (expect (run "insert" '("a.txt") :at "end" :stdin t) :to-be :ok)
        (expect (text "a.txt") :to-equal (format nil "x~%~A~%" payload)))))

  (it "writes --content-file bytes unchanged"
    (with-workspace ()
      (let ((raw (octet-vector 0 255 13 10 27 128)))
        (put "in.bin" raw)
        (expect (run "write" '("out.bin") :content-file (list (disk "in.bin"))) :to-be :ok)
        (expect (octets-of "out.bin") :to-equalp raw))))

  (it "changes nothing and has no op_id with --dry-run"
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (let ((before (snapshot)))
        (multiple-value-bind (kind fields) (run "edit" '("a.txt") :old "a" :new "b" :dry-run t)
          (expect kind :to-be :ok)
          (expect (field fields "dry_run") :to-be t)
          (expect (assoc "op_id" fields :test #'string=) :to-be nil)
          (expect (changes fields) :to-equal '(("a.txt" "modified"))))
        (expect-unchanged before))))

  (it "cuts a long diff, marks it, and points at aitools diff --op"
    (with-workspace ()
      (put "a.txt" (format nil "~{~A~%~}" (loop for i below 300 collect i)))
      (multiple-value-bind (kind fields) (run "transform" '("a.txt") :op '("reverse"))
        (expect kind :to-be :ok)
        (expect (json-field (first (field fields "changes")) "diff_truncated") :to-be t)
        (expect (field fields "next_commands")
                :to-equal (list (format nil "aitools diff --op ~A" (field fields "op_id")))))))

  (it "refuses input carrying [REDACTED_SECRET]"
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (with-error (code) (run "edit" '("a.txt") :old "a" :new "key=[REDACTED_SECRET]")
        (expect code :to-equal "refusal.redacted-input"))
      (expect (text "a.txt") :to-equal (format nil "a~%")))))

(describe "aitools workspace boundary and text layout rules for writes"
  (it "refuses writes outside the root, through an escaping symlink, and into .git"
    (with-workspace ()
      (sb-posix:mkdir (disk ".git") #o755)
      (sb-posix:symlink "/tmp" (disk "escape"))
      (dolist (target (list "../outside.txt" "escape/x.txt" ".git/config"))
        (with-error (code) (run "write" (list target) :content (list "x"))
          (expect code :to-equal "refusal.outside-workspace")))))

  (it "allows writes below the mktemp area"
    (with-workspace ()
      (multiple-value-bind (kind fields) (run "mktemp" '() :suffix ".txt")
        (expect kind :to-be :ok)
        (let ((path (field fields "path")))
          (expect (run "write" (list path) :content (list "hi") :overwrite t
                                           :expect-hash (list (aitools.kernel.domain:content-hash (octet-vector))))
                  :to-be :ok)
          (expect (with-open-file (in path) (read-line in)) :to-equal "hi")))))

  (it "keeps CRLF, a missing final newline and a BOM across edits"
    (with-workspace ()
      (put "crlf.txt" (format nil "a~C~%b~C~%" #\Return #\Return))
      (put "open.txt" (format nil "a~%b"))
      (put "bom.txt" (format nil "~Chead~%tail~%" (code-char #xFEFF)))
      (expect (run "edit" '("crlf.txt") :old "b" :new (format nil "b~%c")) :to-be :ok)
      (expect (text "crlf.txt") :to-equal (format nil "a~C~%b~C~%c~C~%" #\Return #\Return #\Return))
      (expect (run "insert" '("open.txt") :at "end" :content "c") :to-be :ok)
      (expect (text "open.txt") :to-equal (format nil "a~%b~%c"))
      (expect (run "edit" '("bom.txt") :old "head" :new "HEAD") :to-be :ok)
      (expect (text "bom.txt") :to-equal (format nil "~CHEAD~%tail~%" (code-char #xFEFF)))))

  (it "refuses text edits of invalid UTF-8 and binary files, but writes bytes"
    (with-workspace ()
      (put "bad.txt" (octet-vector 97 #xFF 10))
      (put "bin.dat" (octet-vector 97 0 98))
      (with-error (code) (run "edit" '("bad.txt") :old "a" :new "b") (expect code :to-equal "input.not-utf8"))
      (with-error (code) (run "edit" '("bin.dat") :old "a" :new "b") (expect code :to-equal "refusal.not-a-file"))
      (expect (run "copy" '("bad.txt" "bad2.txt")) :to-be :ok)
      (expect (octets-of "bad2.txt") :to-equalp (octet-vector 97 #xFF 10)))))
