(in-package #:aitools.edit.test)

(describe "aitools JSON and table writes: stdin forms and repairs"
  (it "sets a value read from --stdin and a cell whose row, column and value are JSON numbers"
    (with-edit-workspace ()
      (put "c.json" "{\"a\":1}")
      (setf *stdin* (bytes "{\"pointer\": \"/b\", \"value\": [true]}"))
      (expect (run "json.set" '("c.json") :stdin t) :to-be :ok)
      (expect (text "c.json") :to-equal "{\"a\":1,\"b\":[true]}")
      (put "t.csv" (format nil "id,name~%1,x~%"))
      (setf *stdin* (bytes "{\"row\": 1, \"column\": 2, \"value\": 7}"))
      (multiple-value-bind (kind fields) (run "table.set" '("t.csv") :stdin t :expect-hash (list (hash "t.csv")))
        (expect kind :to-be :ok)
        (expect (field fields "previous") :to-equal "x"))
      (expect (text "t.csv") :to-equal (format nil "id,name~%1,7~%"))))

  (it "re-indents a one-line file with two spaces and repairs a failed test with json get of its pointer"
    (with-edit-workspace ()
      (put "c.json" "{\"a\":{\"b\":1}}")
      (expect (run "json.fmt" '("c.json")) :to-be :ok)
      (expect (text "c.json") :to-equal (format nil "{~%  \"a\": {~%    \"b\": 1~%  }~%}"))
      (setf *stdin* (bytes "[{\"op\":\"test\",\"path\":\"/a/b\",\"value\":2}]"))
      (with-error (code message keys) (run "json.patch" '("c.json") :stdin t)
        (expect code :to-equal "selection.no-match")
        (expect (repair-commands keys) :to-equal '("aitools json get c.json")))
      (with-error (code message keys) (run "json.set" '("c.json" "/a/b/c" "1"))
        (expect code :to-equal "input.not-found")
        (expect (repair-commands keys) :to-equal '("aitools json get c.json /a/b"))))))

(describe "aitools archive extract and create: formats, sources and refusals"
  (it "round-trips one file through .gz under its own name"
    (with-edit-workspace ()
      (put "notes.txt" (format nil "n~%"))
      (multiple-value-bind (kind fields) (run "archive.create" '("n.gz" "notes.txt"))
        (expect kind :to-be :ok)
        (expect (field fields "format") :to-equal "gz"))
      (expect (run "archive.extract" '("n.gz") :to "out") :to-be :ok)
      (expect (text "out/notes.txt") :to-equal (format nil "n~%"))))

  (it "archives a symlink as a link, and refuses an archive path that exists"
    (with-edit-workspace ()
      (put "src/a.txt" "a")
      (sb-posix:symlink "a.txt" (disk "src/l"))
      (expect (run "archive.create" '("o.tar" "src")) :to-be :ok)
      (with-error (code) (run "archive.create" '("o.tar" "src")) (expect code :to-equal "refusal.exists"))
      (expect (run "archive.extract" '("o.tar") :to "out") :to-be :ok)
      (expect (sb-posix:readlink (disk "out/src/l")) :to-equal "a.txt")))
  (it "reports a missing source when creating an archive"
    (with-edit-workspace ()
      (let ((before (snapshot)))
        (with-error (code)
            (run "archive.create" '("missing.zip" "missing-source"))
          (expect code :to-equal "input.not-found"))
        (expect-unchanged before))))

  (it "extracts only --entry names, from an archive outside the workspace"
    (with-edit-workspace ()
      (let ((outside (concatenate 'string *root* "/../outside.tar")))
        (with-open-file (out (sb-ext:parse-native-namestring outside) :direction :output :element-type '(unsigned-byte 8)
                                                                      :if-exists :supersede)
          (write-sequence (aitools.text.domain:write-tar (list (member-file "a.txt" "A") (member-file "b.txt" "B"))) out))
        (unwind-protect
             (progn
               (expect (run "archive.extract" (list outside) :to "out" :entry '("b.txt")) :to-be :ok)
               (expect (kind "out/a.txt") :to-be :absent)
               (expect (text "out/b.txt") :to-equal "B"))
          (delete-file (sb-ext:parse-native-namestring outside))))))

  (it "reports a missing archive and one that is no archive"
    (with-edit-workspace ()
      (put "plain.txt" "just text")
      (with-error (code message) (run "archive.extract" '("nope.zip") :to "out")
        (expect code :to-equal "input.not-found")
        (expect message :to-equal "archive nope.zip does not exist"))
      (with-error (code message) (run "archive.extract" '("plain.txt") :to "out")
        (expect code :to-equal "input.unsupported-format")
        (expect message :to-equal "plain.txt is not a zip, tar, tar.gz or gz archive"))))

  (it "refuses a hard link past --max-bytes as too large, not as a syntax error"
    (with-edit-workspace ()
      (put "h.tar" (retype-tar-entry (aitools.text.domain:write-tar (list (member-file "a" "0123456789") (member-file "h" "")))
                                     1024 #\1 :link "a"))
      (with-error (code) (run "archive.extract" '("h.tar") :to "out" :max-bytes "15")
        (expect code :to-equal "refusal.too-large"))
      (expect (run "archive.extract" '("h.tar") :to "out" :max-bytes "20") :to-be :ok)
      (expect (text "out/h") :to-equal "0123456789")))

  (it-each (("an unsupported gz method" (#x1F #x8B 9 0 0 0 0 0 0 255 1 2 3 4 5 6 7 8) "input.unsupported-format")
            ("a corrupt gz stream" (#x1F #x8B 8 0 0 0 0 0 0 255 255 255 255 0 0 0 0 0) "input.syntax-error"))
      "reports ~A as an input error"
      (name octets code)
    (declare (ignore name))
    (with-edit-workspace ()
      (put "x.gz" (apply #'octet-vector octets))
      (with-error (actual) (run "archive.extract" '("x.gz") :to "out") (expect actual :to-equal code))
      (expect (kind "out") :to-be :absent))))
