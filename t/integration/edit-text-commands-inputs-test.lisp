;;;; t/integration/edit-text-commands-inputs-test.lisp
;;;;
;;;; The text commands' stdin edits, missing targets, and replace scan
;;;; options.
(in-package #:aitools.edit.test)

(describe "aitools edit and insert: stdin edits, missing targets and selections"
  (it "applies edits[] chosen by between and by match with its own expect_count"
    (with-edit-workspace ()
      (put "a.txt" (format nil "begin~%x~%end~%y1~%y2~%z~%"))
      (setf *stdin* (bytes (concatenate 'string
                                        "{\"edits\": [{\"between\": [\"^begin$\", \"^end$\"], \"exclusive\": true, \"new\": \"X\"},"
                                        " {\"match\": \"^z$\", \"invert\": true, \"expect_count\": 4, \"new\": \"-\"}]}")))
      (with-error (code) (run "edit" '("a.txt") :stdin t) (expect code :to-equal "selection.count-mismatch"))
      (setf *stdin* (bytes (concatenate 'string
                                        "{\"edits\": [{\"between\": [\"^begin$\", \"^end$\"], \"exclusive\": true, \"new\": \"X\"},"
                                        " {\"match\": \"^y\", \"expect_count\": 2, \"new\": \"Y\"}]}")))
      (multiple-value-bind (kind fields) (run "edit" '("a.txt") :stdin t)
        (expect kind :to-be :ok)
        (expect (assoc "strategy" fields :test #'string=) :to-be nil))
      (expect (text "a.txt") :to-equal (format nil "begin~%X~%end~%Y~%Y~%z~%"))))

  (it "names similar files for a missing target and refuses a directory"
    (with-edit-workspace ()
      (put "notes.txt" "n")
      (put "src/main.lisp" "m")
      (sb-posix:mkdir (disk "dir") #o755)
      (with-error (code message keys) (run "edit" '("note.txt") :old "a" :new "b")
        (expect code :to-equal "input.not-found")
        (expect (mapcar (lambda (candidate) (json-field candidate "path")) (getf keys :candidates))
                :to-equal '("notes.txt" "dir" "src")))
      (with-error (code message keys) (run "edit" '("src/mian.lisp") :old "a" :new "b")
        (expect code :to-equal "input.not-found")
        (expect (json-field (first (getf keys :candidates)) "path") :to-equal "src/main.lisp"))
      (with-error (code message) (run "edit" '("dir") :old "a" :new "b")
        (expect code :to-equal "refusal.not-a-file")
        (expect message :to-equal "dir is a directory, not a file"))))

  (it "refuses a --symbol that names two definitions, and one that names none"
    (with-edit-workspace ()
      (put "c.lisp" (format nil "(defun a () 1)~%(defun a () 2)~%"))
      (with-error (code message keys) (run "edit" '("c.lisp") :symbol "a" :new "x" :expect-hash (list (hash "c.lisp")))
        (expect code :to-equal "selection.ambiguous")
        (expect (length (getf keys :candidates)) :to-be 2))
      (with-error (code) (run "edit" '("c.lisp") :symbol "zz" :new "x" :expect-hash (list (hash "c.lisp")))
        (expect code :to-equal "selection.no-match"))))

  (it "inserts a --content-file's text"
    (with-edit-workspace ()
      (put "a.txt" (format nil "a~%"))
      (put "in.txt" (format nil "b~%"))
      (expect (run "insert" '("a.txt") :at "end" :content-file "in.txt") :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "a~%b~%"))))
  (it "refuses to insert into a binary target without changing it"
    (with-edit-workspace ()
      (put "bad.bin" (octet-vector 97 0 98))
      (let ((before (snapshot)))
        (with-error (code message)
            (run "insert" (list "bad.bin") :at "end" :content "x")
          (expect code :to-equal "refusal.not-a-file")
          (expect message :to-equal "bad.bin is binary; text edits refuse it"))
        (expect-unchanged before))))
  (it "reports an unreadable content file without creating the output"
    (with-edit-workspace ()
      (put "in.txt" "input")
      (cl-weave:with-mocked-functions
          (((symbol-function (quote aitools.text.application:source-file-size))
            (lambda (source path)
              (declare (ignore source path))
              (values nil :unreadable))))
        (with-error (code message)
            (run "write" (list "out.txt") :content-file (list "in.txt"))
          (expect code :to-equal "environment.io")
          (expect message :to-equal "cannot read in.txt")))
      (expect (kind "out.txt") :to-be :absent)))
  (it "reports a content file that disappears during reading as an I/O failure"
    (with-edit-workspace ()
      (put "in.txt" "input")
      (cl-weave:with-mocked-functions
          (((symbol-function (quote aitools.text.application:source-file-size))
            (lambda (source path)
              (declare (ignore source path))
              (values 5 nil)))
           ((symbol-function (quote aitools.text.application:source-read-octets))
            (lambda (source path)
              (declare (ignore source path))
              nil)))
        (with-error (code message)
            (run "write" (list "out.txt") :content-file (list "in.txt"))
          (expect code :to-equal "environment.io")
          (expect message :to-equal "cannot read in.txt")))
      (expect (kind "out.txt") :to-be :absent))))

(describe "aitools replace: stdin, scan options and unreadable files"
  (it "reads the pattern and replacement from --stdin"
    (with-edit-workspace ()
      (put "a.txt" (format nil "cat~%"))
      (setf *stdin* (bytes "{\"pattern\": \"c(a)t\", \"replacement\": \"d${1}g\"}"))
      (expect (run "replace" '("a.txt") :stdin t :expect-count "1") :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "dag~%"))))

  (it "points a refusal's repair at the searched path, never at the pattern"
    (with-edit-workspace ()
      (sb-posix:mkdir (disk "src") #o755)
      (put "src/a.txt" (format nil "v~%"))
      (with-error (code message keys) (run "replace" '("v" "w" "src") :lang "klingon" :expect-count "1")
        (expect code :to-equal "input.unsupported-language")
        (expect (repair-commands keys) :to-equal '("aitools read src")))
      (with-error (code message keys) (run "replace" '("v" "w") :lang "klingon" :expect-count "1")
        (expect code :to-equal "input.unsupported-language")
        (expect (repair-commands keys) :to-equal '("aitools schema replace")))
      (setf *stdin* (bytes "{\"pattern\": \"v\", \"replacement\": \"w\"}"))
      (with-error (code message keys) (run "replace" '("src") :stdin t :lang "klingon" :expect-count "1")
        (expect code :to-equal "input.unsupported-language")
        (expect (repair-commands keys) :to-equal '("aitools read src")))))

  (it "refuses a named binary file but skips binary and non-UTF-8 files found by the scan"
    (with-edit-workspace ()
      (put "a.txt" (format nil "foo~%"))
      (put "b.bin" (octet-vector 102 111 111 0))
      (put "c.txt" (octet-vector 102 111 111 #xFF))
      (with-error (code) (run "replace" '("foo" "bar" "b.bin") :expect-count "1")
        (expect code :to-equal "refusal.not-a-file"))
      (expect (run "replace" '("foo" "bar") :expect-count "1") :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "bar~%"))
      (expect (octets-of "b.bin") :to-equalp (octet-vector 102 111 111 0))))

  (it "skips a scanned file whose selector does not match instead of refusing"
    (with-edit-workspace ()
      (put "a.txt" (format nil "k v~%"))
      (expect (run "replace" (list "v" "w") :between (list "^nope" "^never") :expect-count "0")
              :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "k v~%"))))

  (it "limits the scan with --newer by duration and by a reference file, and with --lang"
    (with-edit-workspace ()
      (put "old.txt" (format nil "v~%"))
      (put "new.lisp" (format nil "v~%"))
      (sb-posix:utimes (disk "old.txt") 0 0)
      (put "ref" "r")
      (sb-posix:utimes (disk "ref") 1000 1000)
      (expect (run "replace" '("v" "w") :newer "1h" :expect-count "1") :to-be :ok)
      (expect (text "new.lisp") :to-equal (format nil "w~%"))
      (expect (run "replace" '("w" "x") :newer "ref" :expect-count "1") :to-be :ok)
      (expect (text "new.lisp") :to-equal (format nil "x~%"))
      (expect (run "replace" '("v" "y") :lang "common-lisp" :expect-count "0") :to-be :ok)
      (expect (text "old.txt") :to-equal (format nil "v~%")))))

  (it "uses the injected edit clock for --newer durations"
    (with-edit-workspace ()
      (put "old.txt" "v")
      (put "new.txt" "v")
      (sb-posix:utimes (disk "old.txt") 1000 1000)
      (sb-posix:utimes (disk "new.txt") 99000 99000)
      (cl-weave:with-mocked-functions
          (((symbol-function 'aitools.edit.infrastructure:unix-now) (lambda () 100000)))
        (let ((*ports* (make-edit-ports
                        :workspace-host
                        (aitools.edit.application::edit-ports-workspace-host *ports*)
                        :open-store
                        (aitools.edit.application::edit-ports-open-store *ports*)
                        :text-source
                        (aitools.edit.application::edit-ports-text-source *ports*)
                        :unix-now
                        (symbol-function 'aitools.edit.infrastructure:unix-now))))
          (expect (run "replace" '("v" "w") :newer "1h" :expect-count "1") :to-be :ok)
          (expect (text "old.txt") :to-equal "v")
          (expect (text "new.txt") :to-equal "w")))))

  (it "treats an existing duration-shaped --newer path as a path"
    (with-edit-workspace ()
      (put "7d" "v")
      (put "new.txt" "v")
      (sb-posix:utimes (disk "7d") 1000 1000)
      (sb-posix:utimes (disk "new.txt") 1500 1500)
      (expect (run "replace" '("v" "w") :newer "7d" :expect-count "1") :to-be :ok)
      (expect (text "7d") :to-equal "v")
      (expect (text "new.txt") :to-equal "w")))

  (it "reports an oversized file skipped by a path-based --newer scan"
    (with-edit-workspace ()
      (put "7d" "ref")
      (put "small.txt" "v")
      (put "big.txt" (make-string 2048 :initial-element #\v))
      (sb-posix:utimes (disk "7d") 1000 1000)
      (sb-posix:utimes (disk "small.txt") 1500 1500)
      (sb-posix:utimes (disk "big.txt") 1500 1500)
      (multiple-value-bind (kind fields)
          (run "replace" '("v" "w") :newer "7d" :skip-larger-than "1KiB" :expect-count "1")
        (expect kind :to-be :ok)
        (expect (changes fields) :to-equal '(("small.txt" "modified")))
        (expect (mapcar (lambda (entry) (json-field entry "path")) (field fields "skipped"))
                :to-equal '("big.txt"))
        (expect (json-field (first (field fields "skipped")) "reason") :to-equal "too-large")
        (expect (text "small.txt") :to-equal "w")
        (expect (text "big.txt") :to-equal (make-string 2048 :initial-element #\v)))))

(describe "aitools transform target errors"
  (it "returns file suggestions for a missing target"
    (with-edit-workspace ()
      (put "notes.txt" (format nil "one~%"))
      (let ((before (snapshot)))
        (with-error (code message keys)
            (run "transform" (list "note.txt") :op (list "sort"))
          (expect code :to-equal "input.not-found")
          (expect message :to-equal "note.txt does not exist")
          (expect (mapcar (lambda (candidate) (json-field candidate "path"))
                          (getf keys :candidates))
                  :to-equal (list "notes.txt")))
        (expect-unchanged before))))
  (it "preserves the document when a line range matches nothing"
    (with-edit-workspace ()
      (put "a.txt" (format nil "only line~%"))
      (let ((before (snapshot)))
        (with-error (code message)
            (run "transform" (list "a.txt") :op (list "sort") :range "2" :expect-hash (list (hash "a.txt")))
          (expect code :to-equal "selection.no-match")
          (expect message :to-equal "the selector matches nothing in a.txt"))
        (expect-unchanged before)))))
