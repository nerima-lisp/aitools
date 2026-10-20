(in-package #:aitools.edit.test)

(describe "aitools chmod, link and touch"
  (it "sets and clears execute bits and sets a mode, reporting the previous one"
    (with-edit-workspace ()
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
    (with-edit-workspace ()
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
    (with-edit-workspace ()
      (put "regular.txt" "r")
      (sb-posix:symlink "missing.txt" (disk "l"))
      (expect (run "link" '("regular.txt" "l") :overwrite t
                   :expect-hash (list (format nil "l=~A" (aitools.kernel.domain:content-hash (bytes "missing.txt")))))
              :to-be :ok)
      (expect (sb-posix:readlink (disk "l")) :to-equal "regular.txt")))

  (it "creates an empty file (undo removes it) and sets an existing file's mtime"
    (with-edit-workspace ()
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
    (with-edit-workspace ()
      (expect (run "touch" '("new.txt") :mtime "@1577934245") :to-be :ok)
      (expect (sb-posix:stat-mtime (sb-posix:stat (disk "new.txt"))) :to-be 1577934245)
      (with-error (code) (run "touch" '("other.txt") :mtime "1969-12-31T23:59:59Z")
        (expect code :to-equal "argument.invalid"))
      (expect (kind "other.txt") :to-be :absent)))

  (it "stages an existing file's mtime in a tx and applies it at commit"
    (with-edit-workspace ()
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
    (with-edit-workspace ()
      (multiple-value-bind (kind fields) (run "mktemp" '() :suffix ".json")
        (expect kind :to-be :ok)
        (expect (search "/tmp/tmp." (field fields "path")) :to-be-truthy)
        (expect (string= ".json" (field fields "path") :start2 (- (length (field fields "path")) 5)) :to-be t)
        (expect (assoc "op_id" fields :test #'string=) :to-be nil))
      (multiple-value-bind (kind fields) (run "mktemp" '() :dir t)
        (expect kind :to-be :ok)
        (expect (sb-posix:s-isdir (sb-posix:stat-mode (sb-posix:stat (field fields "path")))) :to-be-truthy))
      (expect (aitools.store.application:read-journal (open-store *root*)) :to-equal '()))))
