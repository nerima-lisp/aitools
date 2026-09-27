;;;; t/integration/edit-security-test.lisp
;;;;
;;;; Regressions for the edit context's fix pass: the redaction-marker
;;;; refusal on decoded/expanded/transcoded bytes, chmod/touch hard-link
;;;; and setuid guards, the .git extraction/copy boundary, v7 tar
;;;; acceptance, a damaged tx record's error surface, the move/copy
;;;; destination repair, pointer-aware JSON repairs, and placeholder
;;;; refusal on every write entry point.
(in-package #:aitools.edit.test)

(defun utf16le-bytes (string)
  "STRING as little-endian UTF-16 octets (ASCII range)."
  (apply #'octet-vector (loop for char across string append (list (char-code char) 0))))

(defun to-v7-tar (octets)
  "A single-member ustar tar rewritten as a pre-POSIX v7 tar: no ustar magic,
with the header checksum recomputed, so DETECT-ARCHIVE-FORMAT accepts it by
checksum alone."
  (let ((data (copy-seq (coerce octets '(simple-array (unsigned-byte 8) (*))))))
    (fill data 0 :start 257 :end 265)
    (fill data 32 :start 148 :end 156)
    (let* ((sum (loop for i from 0 below 512 sum (aref data i)))
           (digits (format nil "~6,'0O" sum)))
      (dotimes (i 6) (setf (aref data (+ 148 i)) (char-code (char digits i))))
      (setf (aref data 154) 0 (aref data 155) 32))
    data))

(describe "aitools refuses a redaction marker introduced after the raw input check"
  (it "refuses a json set whose escaped value decodes to the marker"
    (with-workspace ()
      (put "d.json" "{\"k\":\"v\"}")
      (let ((before (snapshot)))
        (with-error (code) (run "json.set" '("d.json" "/k" "\"[REDACTED\\u005fSECRET]\""))
          (expect code :to-equal "refusal.redacted-input"))
        (expect-unchanged before))))

  (it "refuses a transcode whose decoded text contains the marker the raw bytes hid"
    (with-workspace ()
      (put "u.txt" (utf16le-bytes "[REDACTED_SECRET]"))
      (let ((before (snapshot)))
        (with-error (code) (run "transcode" '("u.txt") :from "utf-16le" :to "utf-8")
          (expect code :to-equal "refusal.redacted-input"))
        (expect-unchanged before)))))

(describe "aitools chmod rejects setuid, setgid and sticky bits"
  (it "refuses a mode with a high bit and applies only the low nine bits"
    (with-workspace ()
      (put "f" "x" :mode #o644)
      (dolist (bad '("4755" "2755" "1777"))
        (with-error (code) (run "chmod" '("f") :mode bad) (expect code :to-equal "argument.invalid")))
      (expect (mode "f") :to-be #o644)
      (expect (run "chmod" '("f") :mode "0755") :to-be :ok)
      (expect (mode "f") :to-be #o755))))

(describe "aitools chmod and touch refuse a hard-linked regular file"
  (it "refuses because the change would reach a link outside the workspace"
    (with-workspace ()
      (put "a" "x" :mode #o644)
      (sb-posix:link (disk "a") (disk "b"))
      (with-error (code) (run "chmod" '("a") :mode "600") (expect code :to-equal "refusal.not-a-file"))
      (with-error (code) (run "touch" '("a") :mtime "100") (expect code :to-equal "refusal.not-a-file"))
      (expect (mode "a") :to-be #o644))))

(describe "aitools archive extract and copy keep .git out of the write set"
  (it-each ((".git/config") (".GIT/config") ("sub/.git/hooks/pre-commit"))
      "refuses an archive entry named ~S"
      (name)
    (with-workspace ()
      (put "a.tar" (aitools.text.domain:write-tar (list (member-file name "x"))))
      (let ((before (snapshot)))
        (with-error (code) (run "archive.extract" '("a.tar") :to "out")
          (expect code :to-equal "refusal.outside-workspace"))
        (expect-unchanged before))))

  (it "refuses copy --recursive of a tree that carries a nested .git"
    (with-workspace ()
      (put "src/keep.txt" "k")
      (put "src/.git/config" "gitdir")
      (let ((before (snapshot)))
        (with-error (code) (run "copy" '("src" "dst") :recursive t)
          (expect code :to-equal "refusal.outside-workspace"))
        (expect-unchanged before)))))

(describe "aitools archive extract drops group and other write from extracted modes"
  (it "masks a 0777 entry to 0755 and a 0666 entry to 0644"
    (with-workspace ()
      (put "a.tar" (aitools.text.domain:write-tar
                    (list (member-file "world.sh" "x" :mode #o777)
                          (member-file "group.txt" "y" :mode #o666))))
      (expect (run "archive.extract" '("a.tar") :to "out") :to-be :ok)
      ;; Before the fix these were 0777 and 0666 (masked only by #o777).
      (expect (mode "out/world.sh") :to-be #o755)
      (expect (mode "out/group.txt") :to-be #o644)
      ;; No group/other write and no setuid/setgid/sticky on either file.
      (expect (logand (mode "out/world.sh") #o7022) :to-be 0)
      (expect (logand (mode "out/group.txt") #o7022) :to-be 0))))

(describe "aitools archive extract accepts a pre-POSIX v7 tar"
  (it "extracts a v7 tar the same as a ustar one"
    (with-workspace ()
      (put "v7.tar" (to-v7-tar (aitools.text.domain:write-tar (list (member-file "hello.txt" "hi")))))
      (multiple-value-bind (kind fields) (run "archive.extract" '("v7.tar") :to "out")
        (declare (ignore fields))
        (expect kind :to-be :ok)
        (expect (text "out/hello.txt") :to-equal "hi")))))

(describe "aitools surfaces a damaged tx record cleanly"
  (it "reports environment.io with a tx repair rather than internal.unexpected"
    (with-workspace ()
      (put "a.txt" (format nil "one~%"))
      (let ((tx (begin-tx)))
        (expect (run-in tx "edit" '("a.txt") :old "one" :new "two") :to-be :ok)
        (let ((index (first (directory (concatenate 'string *home* "/*/tx/*/index.json")))))
          (expect index :to-be-truthy)
          (with-open-file (out index :direction :output :if-exists :supersede)
            (write-string "{ this is not valid json" out)))
        (with-error (code message keys) (run-in tx "write" '("new.txt") :content (list "x"))
          (expect code :to-equal "environment.io")
          (expect (some (lambda (repair) (search "tx " (or (getf repair :command) "")))
                        (getf keys :repairs))
                  :to-be-truthy))))))

(describe "aitools move and copy point the exists repair at the destination"
  (it "repairs refusal.exists with the destination path, not the source"
    (with-workspace ()
      (put "a" "A")
      (put "b" "B")
      (with-error (code message keys) (run "move" '("a" "b"))
        (expect code :to-equal "refusal.exists")
        (expect (some (lambda (command) (search "aitools info b" command)) (repair-commands keys))
                :to-be-truthy)))))

(describe "aitools JSON writes repair with json get, not the generic file repairs"
  (it "points a missing pointer at json get of its parent"
    (with-workspace ()
      (put "d.json" "{\"a\":{\"b\":1}}")
      (with-error (code message keys) (run "json.set" '("d.json" "/a/missing/deeper" "1"))
        (expect code :to-equal "input.not-found")
        (expect (some (lambda (command) (search "aitools json get d.json" command)) (repair-commands keys))
                :to-be-truthy)))))

(describe "aitools refuses raw redaction markers on every write entry point"
  (it "refuses write --content-file whose bytes hold the marker"
    (with-workspace ()
      (put "masked" "[REDACTED_SECRET]")
      (with-error (code) (run "write" '("out") :content-file (list "masked"))
        (expect code :to-equal "refusal.redacted-input"))))

  (it "refuses write --stdin whose bytes hold the marker"
    (with-workspace ()
      (setf *stdin* (bytes "prefix [REDACTED_SECRET] suffix"))
      (with-error (code) (run "write" '("out") :stdin t)
        (expect code :to-equal "refusal.redacted-input"))))

  (it "refuses json set whose raw value holds the marker"
    (with-workspace ()
      (put "d.json" "{\"k\":1}")
      (with-error (code) (run "json.set" '("d.json" "/k" "\"[REDACTED_SECRET]\""))
        (expect code :to-equal "refusal.redacted-input")))))
