;;;; t/integration/edit-files-test.lisp
;;;;
;;;; File operations and the format writes: json, table set, archive extract/create; each with
;;;; its undo. Their refusals and special paths are in
;;;; edit-files-refusals-test.lisp; `--tx` staging and `tx rebase` replays
;;;; are in edit-tx-and-limits-test.lisp.
(in-package #:aitools.edit.test)

(describe "aitools move, copy, delete and mkdir"
  (it "renames a directory, refuses to merge into an existing one, and undoes the move"
    (with-workspace ()
      (put "d/a.txt" "a")
      (put "d/sub/b.txt" "b")
      (put "e/x" "x")
      (let ((before (snapshot)))
        (with-error (code) (run "move" '("d" "e")) (expect code :to-equal "refusal.exists"))
        (multiple-value-bind (kind fields) (run "move" '("d" "moved"))
          (expect kind :to-be :ok)
          (expect (changes fields) :to-equal '(("moved" "moved")))
          (expect (text "moved/sub/b.txt") :to-equal "b")
          (expect (kind "d") :to-be :absent)
          (expect (undo (field fields "op_id")) :to-be :committed))
        (expect (snapshot) :to-equal before))))

  (it "moves over a file only with --overwrite and a hash"
    (with-workspace ()
      (put "a" "A")
      (put "b" "B")
      (with-error (code) (run "move" '("a" "b")) (expect code :to-equal "refusal.exists"))
      (with-error (code) (run "move" '("a" "b") :overwrite t) (expect code :to-equal "argument.invalid"))
      (expect (run "move" '("a" "b") :overwrite t :expect-hash (list (format nil "b=~A" (hash "b")))) :to-be :ok)
      (expect (text "b") :to-equal "A")))

  (it "copies a tree recursively, skipping symlinks that leave the workspace, within --max-bytes"
    (with-workspace ()
      (put "src/a.txt" "aaaa" :mode #o755)
      (put "src/deep/b.txt" "bb")
      (sb-posix:symlink "a.txt" (disk "src/inside"))
      (sb-posix:symlink "/etc/hosts" (disk "src/outside"))
      (let ((before (snapshot)))
        (with-error (code) (run "copy" '("src" "dst")) (expect code :to-equal "argument.invalid"))
        (with-error (code) (run "copy" '("src" "dst") :recursive t :max-bytes "5") (expect code :to-equal "refusal.too-large"))
        (expect-unchanged before)
        (multiple-value-bind (kind fields) (run "copy" '("src" "dst") :recursive t)
          (expect kind :to-be :ok)
          (expect (field fields "files") :to-be 2)
          (expect (mapcar (lambda (skip) (json-field skip "path")) (field fields "skipped")) :to-equal '("src/outside"))
          (expect (text "dst/deep/b.txt") :to-equal "bb")
          (expect (mode "dst/a.txt") :to-be #o755)
          (expect (kind "dst/inside") :to-be :symlink)
          (expect (kind "dst/outside") :to-be :absent)
          (expect (undo (field fields "op_id")) :to-be :committed))
        (expect (snapshot) :to-equal before))))

  (it "deletes files and empty directories, refuses non-empty ones, and undo restores them"
    (with-workspace ()
      (put "full/x" "x")
      (sb-posix:mkdir (disk "empty") #o755)
      (put "f.txt" "f" :mode #o600)
      (let ((before (snapshot)))
        (with-error (code) (run "delete" '("full")) (expect code :to-equal "refusal.not-a-file"))
        (dolist (path '("empty" "f.txt"))
          (multiple-value-bind (kind fields) (run "delete" (list path))
            (expect kind :to-be :ok)
            (expect (changes fields) :to-equal (list (list path "deleted")))
            (expect (kind path) :to-be :absent)
            (expect (undo (field fields "op_id")) :to-be :committed)))
        (expect (snapshot) :to-equal before))))

  (it "creates directories with parents and treats an existing one as no change"
    (with-workspace ()
      (multiple-value-bind (kind fields) (run "mkdir" '("a/b"))
        (expect kind :to-be :ok)
        (expect (changes fields) :to-equal '(("a" "created") ("a/b" "created"))))
      (multiple-value-bind (kind fields) (run "mkdir" '("a/b"))
        (expect kind :to-be :ok)
        (expect (field fields "changes") :to-equal '())))))

(describe "aitools chmod, link and touch"
  (it "sets and clears execute bits and sets a mode, reporting the previous one"
    (with-workspace ()
      (put "s.sh" "echo" :mode #o644)
      (multiple-value-bind (kind fields) (run "chmod" '("s.sh") :exec t)
        (expect kind :to-be :ok)
        (expect (field fields "previous_mode") :to-equal "0644")
        (expect (mode "s.sh") :to-be #o755)
        (expect (changes fields) :to-equal '(("s.sh" "mode-changed")))
        (expect (undo (field fields "op_id")) :to-be :committed)
        (expect (mode "s.sh") :to-be #o644))
      (run "chmod" '("s.sh") :mode "750")
      (expect (mode "s.sh") :to-be #o750)
      (run "chmod" '("s.sh") :no-exec t)
      (expect (mode "s.sh") :to-be #o640)
      (with-error (code) (run "chmod" '("s.sh") :exec t :mode "700") (expect code :to-equal "argument.invalid"))))

  (it "links inside the workspace, refuses targets outside it, and replaces only symlinks"
    (with-workspace ()
      (put "target.txt" "t")
      (put "regular.txt" "r")
      (multiple-value-bind (kind fields) (run "link" '("target.txt" "l"))
        (expect kind :to-be :ok)
        (expect (changes fields) :to-equal '(("l" "linked")))
        (expect (sb-posix:readlink (disk "l")) :to-equal "target.txt"))
      (with-error (code) (run "link" '("../../etc/passwd" "bad")) (expect code :to-equal "refusal.outside-workspace"))
      (with-error (code) (run "link" '("target.txt" "regular.txt") :overwrite t
                                    :expect-hash (list (format nil "regular.txt=~A" (hash "regular.txt"))))
        (expect code :to-equal "refusal.exists"))
      (expect (text "regular.txt") :to-equal "r")
      (with-error (code) (run "link" '("regular.txt" "l")) (expect code :to-equal "refusal.exists"))
      ;; l leads to a regular file, so its hash is that file's content hash
      ;; (the value `info l` reports), not the hash of the link text.
      (with-error (code) (run "link" '("regular.txt" "l") :overwrite t
                                     :expect-hash (list (format nil "l=~A" (aitools.kernel.domain:content-hash
                                                                            (bytes "target.txt")))))
        (expect code :to-equal "refusal.target-changed"))
      (multiple-value-bind (kind fields) (run "link" '("regular.txt" "l") :overwrite t
                                                                          :expect-hash (list (format nil "l=~A" (hash "target.txt"))))
        (expect kind :to-be :ok)
        (expect (sb-posix:readlink (disk "l")) :to-equal "regular.txt")
        (expect (undo (field fields "op_id")) :to-be :committed)
        (expect (sb-posix:readlink (disk "l")) :to-equal "target.txt"))))

  (it "hashes a dangling symlink by its target text for --expect-hash"
    (with-workspace ()
      (put "regular.txt" "r")
      (sb-posix:symlink "missing.txt" (disk "l"))
      (expect (run "link" '("regular.txt" "l") :overwrite t
                   :expect-hash (list (format nil "l=~A" (aitools.kernel.domain:content-hash (bytes "missing.txt")))))
              :to-be :ok)
      (expect (sb-posix:readlink (disk "l")) :to-equal "regular.txt")))

  (it "creates an empty file (undo removes it) and sets an existing file's mtime"
    (with-workspace ()
      (multiple-value-bind (kind fields) (run "touch" '("new.txt"))
        (expect kind :to-be :ok)
        (expect (text "new.txt") :to-equal "")
        (expect (undo (field fields "op_id")) :to-be :committed)
        (expect (kind "new.txt") :to-be :absent))
      (put "old.txt" "keep")
      (sb-posix:utimes (disk "old.txt") 1000000000 1000000000)
      (multiple-value-bind (kind fields) (run "touch" '("old.txt") :mtime "2020-01-02T03:04:05Z")
        (expect kind :to-be :ok)
        (expect (field fields "mtime") :to-equal "2020-01-02T03:04:05Z")
        (expect (changes fields) :to-equal '(("old.txt" "modified")))
        (expect (sb-posix:stat-mtime (sb-posix:stat (disk "old.txt"))) :to-be 1577934245)
        (expect (text "old.txt") :to-equal "keep")
        (expect (undo (field fields "op_id")) :to-be :committed))
      (expect (sb-posix:stat-mtime (sb-posix:stat (disk "old.txt"))) :to-be 1000000000)
      (expect (text "old.txt") :to-equal "keep")))

  (it "creates a file with the --mtime given, and refuses a time before 1970"
    (with-workspace ()
      (expect (run "touch" '("new.txt") :mtime "@1577934245") :to-be :ok)
      (expect (sb-posix:stat-mtime (sb-posix:stat (disk "new.txt"))) :to-be 1577934245)
      (with-error (code) (run "touch" '("other.txt") :mtime "1969-12-31T23:59:59Z")
        (expect code :to-equal "argument.invalid"))
      (expect (kind "other.txt") :to-be :absent)))

  (it "stages an existing file's mtime in a tx and applies it at commit"
    (with-workspace ()
      (put "old.txt" "keep")
      (sb-posix:utimes (disk "old.txt") 1000000000 1000000000)
      (let ((tx (begin-tx)))
        (multiple-value-bind (kind fields) (run-in tx "touch" '("old.txt") :mtime "1577934245")
          (expect kind :to-be :ok)
          (expect (field fields "tx_op") :to-be 1))
        (expect (sb-posix:stat-mtime (sb-posix:stat (disk "old.txt"))) :to-be 1000000000)
        (expect (commit-tx tx) :to-be :committed)
        (expect (sb-posix:stat-mtime (sb-posix:stat (disk "old.txt"))) :to-be 1577934245)
        (expect (text "old.txt") :to-equal "keep")))))

(describe "aitools mktemp"
  (it "creates files and directories in tmp/ without a journal entry"
    (with-workspace ()
      (multiple-value-bind (kind fields) (run "mktemp" '() :suffix ".json")
        (expect kind :to-be :ok)
        (expect (search "/tmp/tmp." (field fields "path")) :to-be-truthy)
        (expect (string= ".json" (field fields "path") :start2 (- (length (field fields "path")) 5)) :to-be t)
        (expect (assoc "op_id" fields :test #'string=) :to-be nil))
      (multiple-value-bind (kind fields) (run "mktemp" '() :dir t)
        (expect kind :to-be :ok)
        (expect (sb-posix:s-isdir (sb-posix:stat-mode (sb-posix:stat (field fields "path")))) :to-be-truthy))
      (expect (aitools.store.application:read-journal (open-store *root*)) :to-equal '()))))

(describe "aitools json writes"
  (it "keeps key order and the file's indentation, appends with /-, and refuses a missing parent"
    (with-workspace ()
      (put "c.json" (format nil "{~%    \"z\": 1,~%    \"a\": [1]~%}~%"))
      (expect (run "json.set" '("c.json" "/a/-" "2")) :to-be :ok)
      (expect (run "json.set" '("c.json" "/m" "{\"k\":true}")) :to-be :ok)
      (expect (text "c.json")
              :to-equal (format nil "{~%    \"z\": 1,~%    \"a\": [~%        1,~%        2~%    ],~%    \"m\": {~%        \"k\": true~%    }~%}~%"))
      (with-error (code) (run "json.set" '("c.json" "/nope/deeper" "1")) (expect code :to-equal "input.not-found"))
      (with-error (code) (run "json.set" '("c.json" "/z" "bare words")) (expect code :to-equal "argument.invalid"))
      (expect (run "json.delete" '("c.json" "/m")) :to-be :ok)
      (expect (text "c.json") :to-equal (format nil "{~%    \"z\": 1,~%    \"a\": [~%        1,~%        2~%    ]~%}~%"))))

  (it "merges and patches from --stdin, writing nothing when a test op fails"
    (with-workspace ()
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
    (with-workspace ()
      (put "c.json" "{\"b\":1,\"a\":{\"y\":2,\"x\":3}}")
      (expect (run "json.fmt" '("c.json") :indent "2" :sort-keys t) :to-be :ok)
      (expect (text "c.json") :to-equal (format nil "{~%  \"a\": {~%    \"x\": 3,~%    \"y\": 2~%  },~%  \"b\": 1~%}"))
      (expect (run "json.fmt" '("c.json") :minify t) :to-be :ok)
      (expect (text "c.json") :to-equal "{\"a\":{\"x\":3,\"y\":2},\"b\":1}")
      (put "n.json" "not json")
      (with-error (code) (run "json.fmt" '("n.json")) (expect code :to-equal "input.unsupported-format")))))

(describe "aitools table set"
  (it "sets one cell with a required hash, and undo restores the file"
    (with-workspace ()
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
        (expect (snapshot) :to-equal before)))))

(describe "aitools archive extract and create"
  (it-each (("zip" "out.zip") ("tar" "out.tar") ("tar.gz" "out.tar.gz"))
      "round-trips a tree through ~A"
      (format name)
    (with-workspace ()
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
    (with-workspace ()
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
    (with-workspace ()
      (put "broken.gz" (octet-vector 31 139 8 8 0 0 0 0 0 255 120))
      (with-error (code message)
          (run "archive.extract" '("broken.gz") :to "out")
        (expect code :to-equal "input.syntax-error")
        (expect message :to-equal "malformed archive data: unterminated gzip header string"))))

  (it "leaves ignored files out of archive create unless --no-ignore"
    (with-workspace ()
      (sb-posix:mkdir (disk "node_modules") #o755)
      (put "node_modules/x.js" "x")
      (put "keep.txt" "k")
      (multiple-value-bind (kind fields) (run "archive.create" '("a.tar" "."))
        (expect kind :to-be :ok)
        (expect (field fields "entries") :to-be 1)))))
