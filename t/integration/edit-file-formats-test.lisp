(in-package #:aitools.edit.test)
(describe "aitools copy source kind refusals"
  (it "refuses to copy a symlink staged in a transaction without changing its target"
    (with-edit-workspace ()
      (put "target.txt" "content")
      (let* ((tx (begin-tx))
             (before (snapshot)))
        (expect (run-in tx "link" '("target.txt" "l")) :to-be :ok)
        (with-error (code message)
            (run-in tx "copy" '("l" "copied"))
          (expect code :to-equal "refusal.not-a-file")
          (expect message :to-equal "l is a symlink"))
        (expect-unchanged before)
        (expect (commit-tx tx) :to-be :committed)
        (expect (kind "l") :to-be :symlink)
        (expect (sb-posix:readlink (disk "l")) :to-equal "target.txt")
        (expect (kind "copied") :to-be :absent)
        (expect (text "target.txt") :to-equal "content")))))

(describe "aitools json writes"
  (it "keeps key order and the file's indentation, appends with /-, and refuses a missing parent"
    (with-edit-workspace ()
      (put "c.json" (format nil "{~%    \"z\": 1,~%    \"a\": [1]~%}~%"))
      (expect (run "json.set" '("c.json" "/a/-" "2")) :to-be :ok)
      (expect (run "json.set" '("c.json" "/m" "{\"k\":true}")) :to-be :ok)
      (expect (text "c.json")
              :to-equal (format nil "{~%    \"z\": 1,~%    \"a\": [~%        1,~%        2~%    ],~%    \"m\": {~%        \"k\": true~%    }~%}~%"))
      (with-error (code) (run "json.set" '("c.json" "/nope/deeper" "1")) (expect code :to-equal "input.not-found"))
      (with-error (code) (run "json.set" '("c.json" "/z" "bare words")) (expect code :to-equal "argument.invalid"))
      (expect (run "json.delete" '("c.json" "/m")) :to-be :ok)
      (expect (text "c.json") :to-equal (format nil "{~%    \"z\": 1,~%    \"a\": [~%        1,~%        2~%    ]~%}~%"))))
  (it "refuses to edit a binary JSON file without changing it"
    (with-edit-workspace ()
      (put "bad.json" (octet-vector 123 34 97 34 58 0 125))
      (let ((before (snapshot)))
        (with-error (code)
            (run "json.set" '("bad.json" "/a" "1") :expect-hash (list (hash "bad.json")))
          (expect code :to-equal "refusal.not-a-file"))
        (expect-unchanged before))))

  (it "merges and patches from --stdin, writing nothing when a test op fails"
    (with-edit-workspace ()
      (put "c.json" (format nil "{\"a\": 1, \"b\": 2}~%"))
      (setf *stdin* (bytes "{\"b\": null, \"c\": 3}"))
      (expect (run "json.merge" '("c.json") :stdin t) :to-be :ok)
      (expect (text "c.json") :to-equal (format nil "{\"a\":1,\"c\":3}~%"))
      (let ((before (snapshot)))
        (setf *stdin* (bytes "[{\"op\":\"replace\",\"path\":\"/a\",\"value\":9},{\"op\":\"test\",\"path\":\"/c\",\"value\":4}]"))
        (with-error (code) (run "json.patch" '("c.json") :stdin t) (expect code :to-equal "selection.no-match"))
        (expect-unchanged before))
      (setf *stdin* (bytes "[{\"op\":\"test\",\"path\":\"/c\",\"value\":3},{\"op\":\"add\",\"path\":\"/d\",\"value\":[]}]"))
      (expect (run "json.patch" '("c.json") :stdin t) :to-be :ok)
      (expect (text "c.json") :to-equal (format nil "{\"a\":1,\"c\":3,\"d\":[]}~%"))))

  (it "formats with --indent, --minify and --sort-keys, and refuses non-JSON"
    (with-edit-workspace ()
      (put "c.json" "{\"b\":1,\"a\":{\"y\":2,\"x\":3}}")
      (expect (run "json.fmt" '("c.json") :indent "2" :sort-keys t) :to-be :ok)
      (expect (text "c.json") :to-equal (format nil "{~%  \"a\": {~%    \"x\": 3,~%    \"y\": 2~%  },~%  \"b\": 1~%}"))
      (expect (run "json.fmt" '("c.json") :minify t) :to-be :ok)
      (expect (text "c.json") :to-equal "{\"a\":{\"x\":3,\"y\":2},\"b\":1}")
      (put "n.json" "not json")
      (with-error (code) (run "json.fmt" '("n.json")) (expect code :to-equal "input.unsupported-format")))))

(describe "aitools table set"
  (it "sets one cell with a required hash, and undo restores the file"
    (with-edit-workspace ()
      (put "t.csv" (format nil "id,name~%1,ann~%2,bob~%"))
      (with-error (code) (run "table.set" '("t.csv") :row "2" :column "name" :value "x") (expect code :to-equal "argument.invalid"))
      (let ((before (snapshot)))
        (multiple-value-bind (kind fields) (run "table.set" '("t.csv") :row "2" :column "name" :value "b, o"
                                                                       :expect-hash (list (hash "t.csv")))
          (expect kind :to-be :ok)
          (expect (field fields "previous") :to-equal "bob")
          (expect (text "t.csv") :to-equal (format nil "id,name~%1,ann~%2,\"b, o\"~%"))
          (with-error (code) (run "table.set" '("t.csv") :row "9" :column "name" :value "x" :expect-hash (list (hash "t.csv")))
            (expect code :to-equal "input.not-found"))
          (expect (undo (field fields "op_id")) :to-be :committed))
        (expect (snapshot) :to-equal before))))
  (it "refuses to edit a binary table file without changing it"
    (with-edit-workspace ()
      (put "bad.csv" (octet-vector 105 100 0 10))
      (let ((before (snapshot)))
        (with-error (code)
            (run "table.set" '("bad.csv") :row "1" :column "name" :value "x"
                 :expect-hash (list (hash "bad.csv")))
          (expect code :to-equal "refusal.not-a-file"))
        (expect-unchanged before)))))

(describe "aitools archive extract and create"
  (it-each (("zip" "out.zip") ("tar" "out.tar") ("tar.gz" "out.tar.gz"))
      "round-trips a tree through ~A"
      (format name)
    (with-edit-workspace ()
      (put "src/a.txt" (format nil "alpha~%"))
      (put "src/bin/b.dat" (octet-vector 0 1 2 255) :mode #o755)
      (multiple-value-bind (kind fields) (run "archive.create" (list name "src") :format format)
        (expect kind :to-be :ok)
        (expect (field fields "entries") :to-be 3))
      (multiple-value-bind (kind fields) (run "archive.extract" (list name) :to "unpacked")
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be-truthy)
        (expect (octets-of "unpacked/src/a.txt") :to-equalp (octets-of "src/a.txt"))
        (expect (octets-of "unpacked/src/bin/b.dat") :to-equalp (octets-of "src/bin/b.dat"))
        (expect (mode "unpacked/src/bin/b.dat") :to-be #o755)
        (let ((before-undo (field fields "op_id")))
          (expect (undo before-undo) :to-be :committed)
          (expect (kind "unpacked") :to-be :absent)))))

  (it "writes nothing for zip slip, a bomb, or a collision"
    (with-edit-workspace ()
      (put "slip.tar" (aitools.text.domain:write-tar (list (member-file "ok.txt" "ok") (member-file "../evil" "x"))))
      (put "bomb.zip" (zip-of (aitools.text.domain:make-archive-member
                               :name "z" :kind :file :data (make-array 3000000 :element-type '(unsigned-byte 8)
                                                                               :initial-element 0))))
      (put "plain.zip" (zip-of (member-file "exists.txt" "new")))
      (put "out/exists.txt" "old")
      (let ((before (snapshot)))
        (with-error (code) (run "archive.extract" '("slip.tar") :to "out") (expect code :to-equal "refusal.outside-workspace"))
        (with-error (code) (run "archive.extract" '("bomb.zip") :to "out" :max-bytes "1MiB") (expect code :to-equal "refusal.too-large"))
        (with-error (code) (run "archive.extract" '("plain.zip") :to "out") (expect code :to-equal "refusal.exists"))
        (expect-unchanged before))))

  (it "reports a malformed gzip header as input.syntax-error through archive extract"
    (with-edit-workspace ()
      (put "broken.gz" (octet-vector 31 139 8 8 0 0 0 0 0 255 120))
      (with-error (code message)
          (run "archive.extract" '("broken.gz") :to "out")
        (expect code :to-equal "input.syntax-error")
        (expect message :to-equal "malformed archive data: unterminated gzip header string"))))

  (it "leaves ignored files out of archive create unless --no-ignore"
    (with-edit-workspace ()
      (sb-posix:mkdir (disk "node_modules") #o755)
      (put "node_modules/x.js" "x")
      (put "keep.txt" "k")
      (multiple-value-bind (kind fields) (run "archive.create" '("a.tar" "."))
        (expect kind :to-be :ok)
        (expect (field fields "entries") :to-be 1))))
  (it "records archive positionals through the direct write pipeline"
    (with-edit-workspace ()
      (put "src/a.txt" (format nil "alpha~%"))
      (let ((result nil)
            (host (aitools.edit.application::edit-ports-workspace-host *ports*)))
        (aitools.workspace.application:call-with-resolved-root/k
         host
         :root *root*
         :on-error (lambda (reason path)
                     (declare (ignore reason path))
                     (setf result (list :error)))
         :on-resolved
         (lambda (workspace-root)
           (funcall
            (gethash "archive.create" aitools.edit.application::*preparers*)
            *ports*
            (aitools.edit.application::make-command-env host workspace-root *ports*)
            (list "a.tar" "src")
            nil
            :on-error (lambda (code message &rest keys)
                        (declare (ignore message keys))
                        (setf result (list :error code)))
            :on-plan
            (lambda (plan)
              (expect
               (funcall (aitools.edit.application::write-plan-record-positionals plan)
                        (list "a.tar"))
               :to-equal
               (list "a.tar" "src"))
              (aitools.edit.application:run-write-command/k
               *ports* plan
               :root *root*
               :on-ok (lambda (fields)
                        (setf result (list :ok fields)))
               :on-error (lambda (code message &rest keys)
                           (declare (ignore message keys))
                           (setf result (list :error code))))))))
        (expect (first result) :to-be :ok)
        (let* ((fields (second result))
               (op-id (field fields "op_id"))
               (entry (aitools.store.application:find-journal-entry
                       (open-store *root*) op-id)))
          (expect (aitools.store.domain:journal-entry-argv entry) :to-equal (list "archive" "create" "a.tar" "src"))
          (expect (undo op-id) :to-be :committed)
          (expect (kind "a.tar") :to-be :absent)
          (expect (text "src/a.txt") :to-equal (format nil "alpha~%")))))))
