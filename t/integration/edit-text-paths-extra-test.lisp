(in-package #:aitools.edit.test)

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
