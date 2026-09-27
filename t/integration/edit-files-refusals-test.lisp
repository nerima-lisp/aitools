;;;; t/integration/edit-files-refusals-test.lisp
;;;;
;;;; The file, JSON, table and archive commands' refusals, special paths,
;;;; stdin forms, and store races.
(in-package #:aitools.edit.test)

(defun put-file-fixtures ()
  (put "a.txt" (format nil "a~%"))
  (put "c.json" "{\"a\":1}")
  (put "t.csv" (format nil "id,name~%1,x~%"))
  (sb-posix:mkdir (disk "d") #o755)
  (put "d/in.txt" "in"))

(describe "aitools file, JSON, table and archive commands refuse malformed arguments"
  (it-each (("move with one path" "move" ("a.txt") () "argument.invalid" "move takes SRC and DST")
            ("copy with one path" "copy" ("a.txt") () "argument.invalid" "copy takes SRC and DST")
            ("delete with two paths" "delete" ("a.txt" "c.json") () "argument.invalid" "delete takes exactly one path")
            ("mkdir with no path" "mkdir" () () "argument.invalid" "mkdir takes exactly one path")
            ("chmod with two paths" "chmod" ("a.txt" "c.json") (:exec t) "argument.invalid" "chmod takes exactly one path")
            ("chmod with no change" "chmod" ("a.txt") () "argument.invalid" "chmod needs exactly one of --exec, --no-exec, --mode")
            ("chmod with a non-octal mode" "chmod" ("a.txt") (:mode "9") "argument.invalid" "--mode \"9\" is not an octal mode")
            ("chmod with a five-digit mode" "chmod" ("a.txt") (:mode "00644") "argument.invalid" "is not an octal mode")
            ("link with one path" "link" ("a.txt") () "argument.invalid" "link takes TARGET and LINK")
            ("touch with two paths" "touch" ("a.txt" "c.json") () "argument.invalid" "touch takes exactly one path")
            ("touch with an impossible date" "touch" ("a.txt") (:mtime "2024-02-30") "argument.invalid" "--mtime \"2024-02-30\" is not")
            ("mktemp with a / in --suffix" "mktemp" () (:suffix "a/b") "argument.invalid" "--suffix must not contain / or NUL")
            ("json set with no path" "json.set" () () "argument.invalid" "json set needs PATH")
            ("json set --stdin with a pointer argument" "json.set" ("c.json" "/a") (:stdin-data "{}") "argument.invalid" "pass only PATH")
            ("json set --stdin without a value" "json.set" ("c.json") (:stdin-data "{\"pointer\": \"/a\"}") "argument.invalid" "json set --stdin reads {\"pointer\": string")
            ("json set --stdin with a pointer that is not a string" "json.set" ("c.json") (:stdin-data "{\"pointer\": 1, \"value\": 2}") "argument.invalid" "json set --stdin reads")
            ("json set with two arguments" "json.set" ("c.json" "/a") () "argument.invalid" "json set takes PATH POINTER VALUE")
            ("json set with a malformed pointer" "json.set" ("c.json" "a" "1") () "argument.invalid" "JSON pointer \"a\"")
            ("json delete with one argument" "json.delete" ("c.json") () "argument.invalid" "json delete takes PATH POINTER")
            ("json delete with a malformed pointer" "json.delete" ("c.json" "a") () "argument.invalid" "JSON pointer \"a\"")
            ("json merge with two paths" "json.merge" ("c.json" "a.txt") (:stdin-data "{}") "argument.invalid" "json merge takes exactly one PATH")
            ("json patch without --stdin" "json.patch" ("c.json") () "argument.invalid" "json patch reads the patch from --stdin")
            ("json merge of --stdin that is not JSON" "json.merge" ("c.json") (:stdin-data "{nope") "input.syntax-error" "--stdin is not JSON")
            ("json fmt with two paths" "json.fmt" ("c.json" "a.txt") () "argument.invalid" "json fmt takes exactly one PATH")
            ("json fmt --indent 17" "json.fmt" ("c.json") (:indent "17") "argument.invalid" "--indent \"17\" must be 0 to 16")
            ("json fmt --indent x" "json.fmt" ("c.json") (:indent "x") "argument.invalid" "--indent \"x\" must be 0 to 16")
            ("json set --stdin that is not an object" "json.set" ("c.json") (:stdin-data "[1]") "argument.invalid" "json set --stdin reads")
            ("table set --stdin with a row that is not a number" "table.set" ("t.csv") (:stdin-data "{\"row\": \"one\", \"column\": \"id\", \"value\": \"2\"}") "argument.invalid" "--row \"one\" must be a positive integer")
            ("json fmt --indent with --minify" "json.fmt" ("c.json") (:indent "2" :minify t) "argument.invalid" "--indent and --minify cannot be combined")
            ("table set with two paths" "table.set" ("t.csv" "a.txt") (:row "1" :column "id" :value "2") "argument.invalid" "table set takes exactly one PATH")
            ("table set of a text file" "table.set" ("a.txt") (:row "1" :column "id" :value "2") "input.unsupported-format" "table set edits .csv and .tsv files, not a.txt")
            ("table set --row 0" "table.set" ("t.csv") (:row "0" :column "id" :value "2") "argument.invalid" "--row \"0\" must be a positive integer")
            ("table set without --column" "table.set" ("t.csv") (:row "1" :value "2") "argument.invalid" "table set needs --row, --column and --value")
            ("table set --stdin without a value" "table.set" ("t.csv") (:stdin-data "{\"row\": 1, \"column\": \"id\"}") "argument.invalid" "table set needs --row, --column and --value")
            ("archive extract with no archive" "archive.extract" () (:to "out") "argument.invalid" "archive extract takes exactly one archive PATH")
            ("archive extract without --to" "archive.extract" ("x.zip") () "argument.invalid" "archive extract needs --to <dir>")
            ("archive extract --max-bytes lots" "archive.extract" ("x.zip") (:to "out" :max-bytes "lots") "argument.invalid" "--max-bytes \"lots\" is not a size")
            ("archive extract --max-entries x" "archive.extract" ("x.zip") (:to "out" :max-entries "x") "argument.invalid" "--max-entries must be a non-negative integer")
            ("archive create with no source" "archive.create" ("o.zip") () "argument.invalid" "archive create takes PATH and at least one SRC")
            ("archive create of an unknown format" "archive.create" ("o.bin" "a.txt") () "argument.invalid" "cannot tell the format of o.bin")
            ("archive create of two files as .gz" "archive.create" ("o.gz" "a.txt" "c.json") () "argument.invalid" "a .gz archive holds exactly one file")
            ("archive create of nothing" "archive.create" ("o.zip" "d") (:glob ("*.none")) "selection.no-match" "no files to archive"))
      "refuses ~A"
      (name command positionals options code fragment)
    (declare (ignore name))
    (with-workspace ()
      (put-file-fixtures)
      (let ((before (snapshot)))
        (with-error (actual message) (apply #'run command positionals options)
          (expect actual :to-equal code)
          (expect (search fragment message) :to-be-truthy))
        (expect-unchanged before)))))

(describe "aitools file commands: missing, unreadable and special paths"
  (it-each (("move" ("nope.txt" "b.txt") ())
            ("copy" ("nope.txt" "b.txt") ())
            ("delete" ("nope.txt") ())
            ("chmod" ("nope.txt") (:exec t)))
      "~A names similar paths for a missing source"
      (command positionals options)
    (with-workspace ()
      (put "note.txt" "n")
      (with-error (code message keys) (apply #'run command positionals options)
        (expect code :to-equal "input.not-found")
        (expect message :to-equal "nope.txt does not exist")
        (expect (json-field (first (getf keys :candidates)) "path") :to-equal "note.txt"))))

  (it "refuses to copy a file past --max-bytes and a dangling symlink"
    (with-workspace ()
      (put "big.txt" "0123456789")
      (sb-posix:symlink "missing-target" (disk "dangling"))
      (with-error (code message) (run "copy" '("big.txt" "b.txt") :max-bytes "4")
        (expect code :to-equal "refusal.too-large")
        (expect message :to-equal "big.txt exceeds --max-bytes 4"))
      (with-error (code) (run "copy" '("dangling" "b.txt"))
        (expect code :to-equal "input.not-found"))
      (expect (kind "b.txt") :to-be :absent)))

  (it "refuses to chmod or touch through a dangling symlink and to touch a directory"
    (with-workspace ()
      (sb-posix:symlink "missing-target" (disk "dangling"))
      (sb-posix:mkdir (disk "d") #o755)
      (with-error (code) (run "chmod" '("dangling") :exec t) (expect code :to-equal "input.not-found"))
      (with-error (code message) (run "touch" '("d") :mtime "100")
        (expect code :to-equal "refusal.not-a-file")
        (expect message :to-equal "d is not a regular file"))))

  (it "reports no change when chmod asks for the mode a file already has"
    (with-workspace ()
      (put "f" "x" :mode #o644)
      (multiple-value-bind (kind fields) (run "chmod" '("f") :mode "644")
        (expect kind :to-be :ok)
        (expect (field fields "changes") :to-equal '())
        (expect (field fields "previous_mode") :to-equal "0644"))))

  (it "links to an absolute target inside the workspace"
    (with-workspace ()
      (put "t.txt" "t")
      (expect (run "link" (list (disk "t.txt") "l")) :to-be :ok)
      (expect (sb-posix:readlink (disk "l")) :to-equal (disk "t.txt"))))

  (it "stages a directory move holding a symlink per path in a tx"
    (with-workspace ()
      (put "d/a.txt" "a")
      (sb-posix:symlink "a.txt" (disk "d/l"))
      (let ((tx (begin-tx)))
        (expect (run-in tx "move" '("d" "m")) :to-be :ok)
        (expect (commit-tx tx) :to-be :committed)
        (expect (sb-posix:readlink (disk "m/l")) :to-equal "a.txt")
        (expect (text "m/a.txt") :to-equal "a")
        (expect (kind "d") :to-be :absent))))

  (it "reports environment.io when the mktemp area is a file, and input.not-found for a missing root"
    (with-workspace ()
      (let ((temporary (mktemp-path)))
        (let ((tmp (subseq temporary 0 (position #\/ temporary :from-end t))))
          (uiop:delete-directory-tree (sb-ext:parse-native-namestring (concatenate 'string tmp "/")) :validate t)
          (with-open-file (out (sb-ext:parse-native-namestring tmp) :direction :output :if-does-not-exist :create))
          (with-error (code) (run "mktemp" '())
            (expect code :to-equal "environment.io"))))
      (let ((result nil))
        (run-edit-command *ports* "mktemp" '() '() :root (disk "missing")
                          :on-ok (lambda (fields) (setf result fields))
                          :on-error (lambda (code message &key repairs &allow-other-keys)
                                      (declare (ignore message))
                                      (setf result (list code (getf (first repairs) :command)))))
        (expect result :to-equal '("input.not-found" "aitools schema mktemp"))))))

(describe "aitools JSON and table writes: stdin forms and repairs"
  (it "sets a value read from --stdin and a cell whose row, column and value are JSON numbers"
    (with-workspace ()
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
    (with-workspace ()
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
    (with-workspace ()
      (put "notes.txt" (format nil "n~%"))
      (multiple-value-bind (kind fields) (run "archive.create" '("n.gz" "notes.txt"))
        (expect kind :to-be :ok)
        (expect (field fields "format") :to-equal "gz"))
      (expect (run "archive.extract" '("n.gz") :to "out") :to-be :ok)
      (expect (text "out/notes.txt") :to-equal (format nil "n~%"))))

  (it "archives a symlink as a link, and refuses an archive path that exists"
    (with-workspace ()
      (put "src/a.txt" "a")
      (sb-posix:symlink "a.txt" (disk "src/l"))
      (expect (run "archive.create" '("o.tar" "src")) :to-be :ok)
      (with-error (code) (run "archive.create" '("o.tar" "src")) (expect code :to-equal "refusal.exists"))
      (expect (run "archive.extract" '("o.tar") :to "out") :to-be :ok)
      (expect (sb-posix:readlink (disk "out/src/l")) :to-equal "a.txt")))

  (it "extracts only --entry names, from an archive outside the workspace"
    (with-workspace ()
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
    (with-workspace ()
      (put "plain.txt" "just text")
      (with-error (code message) (run "archive.extract" '("nope.zip") :to "out")
        (expect code :to-equal "input.not-found")
        (expect message :to-equal "archive nope.zip does not exist"))
      (with-error (code message) (run "archive.extract" '("plain.txt") :to "out")
        (expect code :to-equal "input.unsupported-format")
        (expect message :to-equal "plain.txt is not a zip, tar, tar.gz or gz archive"))))

  (it "refuses a hard link past --max-bytes as too large, not as a syntax error"
    (with-workspace ()
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
    (with-workspace ()
      (put "x.gz" (apply #'octet-vector octets))
      (with-error (actual) (run "archive.extract" '("x.gz") :to "out") (expect actual :to-equal code))
      (expect (kind "out") :to-be :absent))))

(describe "aitools file commands: remaining refusals and areas"
  (it "refuses to merge a directory into a file and to overwrite a directory"
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (sb-posix:mkdir (disk "d") #o755)
      (with-error (code message) (run "move" '("d" "a.txt") :overwrite t :expect-hash (list (format nil "a.txt=~A" (hash "a.txt"))))
        (expect code :to-equal "refusal.exists")
        (expect message :to-equal "a.txt already exists (directories are never merged)"))
      (with-error (code message) (run "move" '("a.txt" "d") :overwrite t)
        (expect code :to-equal "refusal.exists")
        (expect message :to-equal "d already exists"))))

  (it "refuses a .gz of a symlink"
    (with-workspace ()
      (put "a.txt" "a")
      (sb-posix:symlink "a.txt" (disk "lnk"))
      (with-error (code) (run "archive.create" '("s.gz" "lnk"))
        (expect code :to-equal "argument.invalid"))))

  (it "extracts into the workspace root itself"
    (with-workspace ()
      (put "a.tar" (aitools.text.domain:write-tar (list (member-file "x.txt" "X"))))
      (multiple-value-bind (kind fields) (run "archive.extract" '("a.tar") :to ".")
        (expect kind :to-be :ok)
        (expect (changes fields) :to-equal '(("x.txt" "created"))))
      (expect (text "x.txt") :to-equal "X")))

  (it "refuses a --root that is a file for mktemp"
    (with-workspace ()
      (put "a.txt" "a")
      (let ((result nil))
        (run-edit-command *ports* "mktemp" '() '() :root (disk "a.txt")
                          :on-ok (lambda (fields) (setf result fields))
                          :on-error (lambda (code message &rest keys) (declare (ignore keys)) (setf result (list code message))))
        (expect (first result) :to-equal "argument.invalid")
        (expect (search "is not a usable directory" (second result)) :to-be-truthy))))

  (it "keeps going when an expired mktemp entry cannot be removed"
    (with-workspace ()
      (let* ((temporary (mktemp-path :dir t))
             (inner (concatenate 'string temporary "/kept")))
        (with-open-file (out (sb-ext:parse-native-namestring inner) :direction :output :if-does-not-exist :create))
        (sb-posix:utimes temporary 0 0)
        (sb-posix:chmod temporary #o555)
        (unwind-protect
             (progn
               (expect (search "/tmp/tmp." (mktemp-path)) :to-be-truthy)
               (if (zerop (sb-posix:getuid))
                   (expect (probe-file inner) :to-be nil) ; root removes it regardless of the mode
                   (expect (probe-file inner) :to-be-truthy)))
          (when (probe-file (concatenate 'string temporary "/"))
            (sb-posix:chmod temporary #o755))))))

  (it "refuses a --content-file larger than the input limit without reading it"
    (with-workspace ()
      (let ((path (disk "huge.bin")))
        (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output :if-does-not-exist :create))
        (sb-posix:truncate path (1+ aitools.edit.application:+max-input-bytes+))
        (with-error (code message) (run "write" '("out.bin") :content-file (list "huge.bin"))
          (expect code :to-equal "refusal.too-large")
          (expect (search "--content-file huge.bin exceeds" message) :to-be-truthy))
        (expect (kind "out.bin") :to-be :absent)))))

(defun call-with-store-io (overrides function)
  "Call FUNCTION with *PORTS* opening stores whose I/O port is the production
one with OVERRIDES (COPY-STORE-IO keywords) in place."
  (let ((*ports* (make-edit-ports
                  :workspace-host (aitools.edit.application::edit-ports-workspace-host *ports*)
                  :open-store (lambda (root)
                                (aitools.store.application:make-store
                                 (apply #'aitools.store.application:copy-store-io
                                        (aitools.store.infrastructure:make-posix-store-io) overrides)
                                 root *home* :temporary (%temporary-area-p root)))
                  :text-source (aitools.edit.application::edit-ports-text-source *ports*)
                  :read-stdin-octets #'%stdin-port
                  :unix-now #'aitools.edit.infrastructure:unix-now)))
    (funcall function)))

(describe "aitools mktemp against a racing mktemp area"
  (it "draws another name when the first is taken"
    (with-workspace ()
      (let* ((first (mktemp-path))
             (tmp (subseq first 0 (position #\/ first :from-end t)))
             (taken (concatenate 'string tmp "/tmp.aaaaaaaaaaaa"))
             (draws (list "aaaaaaaaaaaa" "bbbbbbbbbbbb")))
        (with-open-file (out (sb-ext:parse-native-namestring taken) :direction :output :if-does-not-exist :create)
          (write-string "kept" out))
        (call-with-store-io (list :random-hex (lambda (count) (declare (ignore count)) (pop draws)))
                            (lambda ()
                              (expect (mktemp-path) :to-equal (concatenate 'string tmp "/tmp.bbbbbbbbbbbb"))))
        (expect (with-open-file (in (sb-ext:parse-native-namestring taken)) (read-line in)) :to-equal "kept"))))

  (it "accepts the area another process created between its check and its mkdir"
    (with-workspace ()
      (let ((real-mkdir (aitools.store.application:store-io-mkdir (aitools.store.infrastructure:make-posix-store-io))))
        (call-with-store-io
         (list :mkdir (lambda (path &rest keys)
                        (apply real-mkdir path keys)
                        (if (search "/tmp" path :from-end t :start2 (max 0 (- (length path) 4)))
                            (error 'aitools.store.application:store-io-error :operation "mkdir" :path path
                                                                             :detail "File exists")
                            nil)))
         (lambda ()
           (let ((path (mktemp-path)))
             (expect (probe-file path) :to-be-truthy))))))))

(describe "aitools copy --recursive of the workspace root"
  (it "places the root's entries below the destination directory"
    (with-workspace ()
      (put "a.txt" "a")
      (put "d/b.txt" "b")
      (multiple-value-bind (kind fields) (run "copy" '("." "x") :recursive t)
        (expect kind :to-be :ok)
        (expect (field fields "files") :to-be 2))
      (expect (text "x/a.txt") :to-equal "a")
      (expect (text "x/d/b.txt") :to-equal "b")
      (expect (kind "xa.txt") :to-be :absent))))
