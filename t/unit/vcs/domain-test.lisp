;;;; t/unit/vcs/domain-test.lisp
;;;;
;;;; Parser inputs are shaped after real git 2.x output (porcelain blame,
;;;; `log -z`, `diff` patches) captured while writing the parsers.
(in-package #:aitools.vcs.test)

(defun lines-text (&rest lines)
  (format nil "~{~A~%~}" lines))

(defun tab (text)
  (concatenate 'string (string #\Tab) text))

(defun nul-join (&rest fields)
  (with-output-to-string (out)
    (dolist (field fields)
      (write-string field out)
      (write-char (code-char 0) out))))

(defun octets (&rest values)
  (make-array (length values) :element-type '(unsigned-byte 8) :initial-contents values))

(describe "aitools.vcs.domain epoch-seconds-to-iso8601"
  (it "renders the author's own offset"
    (expect (epoch-seconds-to-iso8601 1704132245 "+0900") :to-equal "2024-01-02T03:04:05+09:00"))
  (it "renders a negative half-hour offset"
    (expect (epoch-seconds-to-iso8601 1704184445 "-0530") :to-equal "2024-01-02T03:04:05-05:30"))
  (it "renders UTC as +00:00"
    (expect (epoch-seconds-to-iso8601 1704164645 "+0000") :to-equal "2024-01-02T03:04:05+00:00"))
  (it "rejects a malformed offset, including non-ASCII digits"
    (signals error (epoch-seconds-to-iso8601 0 "+09"))
    (signals error (epoch-seconds-to-iso8601 0 (format nil "+~C900" (code-char #xFF10))))))

(describe "aitools.vcs.domain command-line"
  (it "leaves safe words alone and quotes the rest"
    (expect (command-line "src/a.lisp") :to-equal "src/a.lisp")
    (expect (command-line "a b") :to-equal "'a b'")
    (expect (command-line "it's") :to-equal "'it'\\''s'")
    (expect (command-line "") :to-equal "''"))
  (it "drops NIL words"
    (expect (command-line "aitools" "git" "log" nil "--limit" "5") :to-equal "aitools git log --limit 5")))

(describe "aitools.vcs.domain status-fields"
  (it "splits index and work-tree changes and reports untracked paths"
    (let ((fields (status-fields
                   (list :branch "main" :upstream "origin/main" :ahead 2 :behind -3
                         :entries (list (list :kind :ordinary :index "M" :worktree "M" :path "a.txt")
                                        (list :kind :rename-or-copy :index "R" :worktree "." :path "new.txt"
                                              :original-path "old.txt")
                                        (list :kind :unmerged :index "U" :worktree "U" :path "c.txt")
                                        (list :kind :untracked :path "u.txt")
                                        (list :kind :ignored :path "i.txt"))))))
      (expect (cdr (assoc "branch" fields :test #'string=)) :to-equal "main")
      (expect (cdr (assoc "ahead" fields :test #'string=)) :to-equal 2)
      ;; cl-vcs-kit hands `# branch.ab +2 -3` over as -3.
      (expect (cdr (assoc "behind" fields :test #'string=)) :to-equal 3)
      (let ((staged (cdr (assoc "staged" fields :test #'string=)))
            (unstaged (cdr (assoc "unstaged" fields :test #'string=))))
        (expect (mapcar (lambda (item) (json-alist-value item "path")) staged) :to-equal '("a.txt" "new.txt"))
        (expect (json-alist-value (second staged) "from") :to-equal "old.txt")
        (expect (mapcar (lambda (item) (json-alist-value item "status")) unstaged) :to-equal '("M" "U")))
      (expect (cdr (assoc "untracked" fields :test #'string=)) :to-equal '("u.txt"))))
  (it "reports null upstream counts without an upstream"
    (let ((fields (status-fields (list :branch "main"))))
      (expect (json-kit:json-null-p (cdr (assoc "upstream" fields :test #'string=))) :to-be-truthy)
      (expect (json-kit:json-null-p (cdr (assoc "ahead" fields :test #'string=))) :to-be-truthy))))

(describe "aitools.vcs.domain map-log-records"
  (it "reads NUL-terminated five-field records in order"
    (let (records)
      (map-log-records (nul-join "aaa" "Ada" "1704132245" "+0900" "second: with	tab"
                                 "bbb" "Bob" "1704164645" "+0000" "first")
                       (lambda (&rest record) (push record records)))
      (expect (nreverse records)
              :to-equal '(("aaa" "Ada" "2024-01-02T03:04:05+09:00" "second: with	tab")
                          ("bbb" "Bob" "2024-01-02T03:04:05+00:00" "first")))))
  (it "stops when the continuation returns :stop"
    (let ((count 0))
      (map-log-records (nul-join "a" "A" "0" "+0000" "s" "b" "B" "0" "+0000" "t")
                       (lambda (&rest record) (declare (ignore record)) (incf count) :stop))
      (expect count :to-equal 1)))
  (it "rejects output cut inside a record"
    (signals error (map-log-records (nul-join "a" "A") (lambda (&rest r) (declare (ignore r)))))))

(defparameter *blame-porcelain*
  (lines-text "07e5d895a032520e317659bd3c63a0b9b71ce4ca 1 1 2"
              "author A B" "author-mail <a@b>" "author-time 1704132245" "author-tz +0900"
              "committer A B" "committer-mail <a@b>" "committer-time 1704132245" "committer-tz +0900"
              "summary first commit" "boundary" "filename a.txt"
              (tab "one")
              "07e5d895a032520e317659bd3c63a0b9b71ce4ca 2 2"
              (tab (format nil "two~C" #\Return))
              "0000000000000000000000000000000000000000 3 3 1"
              "author Not Committed Yet" "author-mail <not.committed.yet>" "author-time 1790370921"
              "author-tz -0530" "committer Not Committed Yet" "committer-mail <not.committed.yet>"
              "committer-time 1790370921" "committer-tz -0530" "summary Version of a.txt from a.txt"
              "previous 07e5d895a032520e317659bd3c63a0b9b71ce4ca a.txt" "filename a.txt"
              (tab "	three")))

(describe "aitools.vcs.domain map-blame-lines"
  (it "reuses a commit's headers for its later groups and strips CR"
    (let (lines)
      (map-blame-lines *blame-porcelain* (lambda (&rest line) (push line lines)))
      (expect (nreverse lines)
              :to-equal '((1 "07e5d895a032520e317659bd3c63a0b9b71ce4ca" "A B" "2024-01-02T03:04:05+09:00" "one")
                          (2 "07e5d895a032520e317659bd3c63a0b9b71ce4ca" "A B" "2024-01-02T03:04:05+09:00" "two")
                          (3 "0000000000000000000000000000000000000000" "Not Committed Yet"
                           "2026-09-25T15:45:21-05:30" "	three")))))
  (it "stops when the continuation returns :stop"
    (let ((count 0))
      (map-blame-lines *blame-porcelain* (lambda (&rest line) (declare (ignore line)) (incf count) :stop))
      (expect count :to-equal 1))))

(defparameter *two-file-patch*
  (lines-text "diff --git a/q.sql b/q.sql"
              "index 1111111..2222222 100644"
              "--- a/q.sql"
              "+++ b/q.sql"
              "@@ -1,3 +1,3 @@"
              " select 1;"
              "--- old comment"
              "+++ new comment"
              " select 2;"
              "@@ -10 +10 @@"
              "-x"
              "+y"
              "\\ No newline at end of file"
              "diff --git a/b.bin b/b.bin"
              "new file mode 100644"
              "index 0000000..badc806"
              "Binary files /dev/null and b/b.bin differ"))

(defparameter *two-file-records*
  (list (list :path "q.sql" :added 2 :deleted 2)
        (list :path "b.bin" :binary t)))

(defun diff-outcome (records patches &rest keys)
  "(VALUES KIND FILES CHARACTERS SUMMARY-PATH SUMMARY-LINES) from DIFF-FILES/K."
  (apply #'diff-files/k records patches
         :on-complete (lambda (files characters) (values :complete files characters))
         :on-truncated (lambda (files characters path lines) (values :truncated files characters path lines))
         keys))

(describe "aitools.vcs.domain diff shaping"
  (it "splits a patch at each diff --git line"
    (let ((patches (split-patch-by-file *two-file-patch*)))
      (expect (length patches) :to-equal 2)
      (expect (first (second patches)) :to-equal "diff --git a/b.bin b/b.bin")))

  (it "reads hunk bodies by count, so `--- `/`+++ ` body lines stay inside the hunk"
    (multiple-value-bind (kind files) (diff-outcome *two-file-records* (split-patch-by-file *two-file-patch*))
      (expect kind :to-be :complete)
      (let* ((file (first files)) (hunks (json-alist-value file "hunks")))
        (expect (json-alist-value file "mode") :to-equal "hunks")
        (expect (length hunks) :to-equal 2)
        (expect (json-alist-value (first hunks) "lines")
                :to-equal '(" select 1;" "--- old comment" "+++ new comment" " select 2;"))
        (expect (json-alist-value (second hunks) "old_count") :to-equal 1)
        (expect (json-alist-value (second hunks) "lines")
                :to-equal '("-x" "+y" "\\ No newline at end of file")))))

  (it "reports a binary file with null counts and no hunks"
    (multiple-value-bind (kind files) (diff-outcome *two-file-records* (split-patch-by-file *two-file-patch*))
      (declare (ignore kind))
      (let ((file (second files)))
        (expect (json-alist-value file "binary") :to-be t)
        (expect (json-kit:json-null-p (json-alist-value file "added")) :to-be-truthy)
        (expect (json-alist-value file "hunks") :to-equal nil))))

  (it "summarizes the first file over --max-lines and every file after it"
    (multiple-value-bind (kind files characters path lines)
        (diff-outcome *two-file-records* (split-patch-by-file *two-file-patch*) :max-lines 5)
      (expect kind :to-be :truncated)
      (expect (mapcar (lambda (file) (json-alist-value file "mode")) files) :to-equal '("summary" "summary"))
      (expect (json-alist-value (first files) "hunks" :absent) :to-be :absent)
      (expect characters :to-equal 0)
      (expect path :to-equal "q.sql")
      ;; Two hunk headers plus four and three body lines.
      (expect lines :to-equal 9)))

  (it "keeps files that fit before the summarized one"
    (let ((records (list (list :path "a" :added 1 :deleted 0) (list :path "b" :added 3 :deleted 0)))
          (patches (list (list "diff --git a/a b/a" "--- a/a" "+++ b/a" "@@ -0,0 +1 @@" "+1")
                         (list "diff --git a/b b/b" "--- a/b" "+++ b/b" "@@ -0,0 +1,3 @@" "+1" "+2" "+3"))))
      (multiple-value-bind (kind files characters path lines) (diff-outcome records patches :max-lines 3)
        (expect kind :to-be :truncated)
        (expect (mapcar (lambda (file) (json-alist-value file "mode")) files) :to-equal '("hunks" "summary"))
        (expect characters :to-equal 3)
        (expect path :to-equal "b")
        (expect lines :to-equal 4))))

  (it "gives counts only in stat mode"
    (multiple-value-bind (kind files) (diff-outcome *two-file-records* nil :output :stat)
      (expect kind :to-be :complete)
      (expect (json-alist-value (first files) "mode") :to-equal "stat")
      (expect (json-alist-value (first files) "added") :to-equal 2)
      (expect (json-alist-value (first files) "hunks" :absent) :to-be :absent))))

(describe "aitools.vcs.domain line-window/k"
  (flet ((window (total &rest keys)
           (apply #'line-window/k total
                  :on-window (lambda (&rest window) (cons :window window))
                  keys)))
    (it "caps an unbounded window and names the next range"
      (expect (window 200 :max-lines 80) :to-equal '(:window 1 80 t "81:160")))
    (it "returns an explicit range whole when it fits"
      (expect (window 200 :start 10 :end 20 :max-lines 80) :to-equal '(:window 10 20 nil nil)))
    (it "clamps the end to the last line"
      (expect (window 5 :start 3 :end 99) :to-equal '(:window 3 5 nil nil)))
    (it "continues inside the requested range only"
      (expect (window 200 :start 1 :end 100 :max-lines 80) :to-equal '(:window 1 80 t "81:100")))
    (it "returns an empty window for an empty document"
      (expect (window 0 :max-lines 80) :to-equal '(:window 1 0 nil nil)))))

(describe "aitools.vcs.domain split-object-spec"
  (it "splits at the first colon"
    (expect (multiple-value-list (split-object-spec "HEAD~1:src/a:b.lisp")) :to-equal '("HEAD~1" "src/a:b.lisp")))
  (it "returns NIL without a colon"
    (expect (split-object-spec "HEAD") :to-be nil)))

(describe "aitools.vcs.domain repository paths"
  (it "resolves a working-directory path against the repository top"
    (expect (repository-relative-path "a.txt" "/r/sub" "/r") :to-equal "sub/a.txt")
    (expect (repository-relative-path "../b/./c" "/r/sub" "/r") :to-equal "b/c")
    (expect (repository-relative-path "/r/x" "/elsewhere" "/r") :to-equal "x")
    (expect (repository-relative-path "." "/r" "/r") :to-equal "."))
  (it "returns NIL outside the top, including a sibling with a shared prefix"
    (expect (repository-relative-path "../x" "/r" "/r") :to-be nil)
    (expect (repository-relative-path "/rr/x" "/" "/r") :to-be nil))
  (it "renders a top-relative path from the working directory"
    (expect (path-from-directory "a/b.txt" "/r" "/r") :to-equal "a/b.txt")
    (expect (path-from-directory "a/b.txt" "/r" "/r/c/d") :to-equal "../../a/b.txt")
    (expect (path-from-directory "a/b.txt" "/r" "/r/a") :to-equal "b.txt")))

(defun condition-text (thunk)
  (handler-case (progn (funcall thunk) :returned)
    (error (condition) (princ-to-string condition))))

(describe "aitools.vcs.domain parsers on unusual git output"
  (it "rejects log output whose last field has no terminating NUL"
    (expect (condition-text (lambda ()
                              (map-log-records (concatenate 'string (nul-join "a" "A" "0" "+0000") "subject")
                                               (lambda (&rest record) (declare (ignore record))))))
            :to-equal "git log output ends inside a record"))

  (it "rejects a blame header without a line number"
    (expect (condition-text (lambda () (map-blame-lines (lines-text "garbage") (lambda (&rest line) (declare (ignore line))))))
            :to-equal "malformed blame header: \"garbage\""))

  (it "reads an empty blamed line and ignores blank header lines"
    (let (lines)
      (map-blame-lines (lines-text "07e5d895a032520e317659bd3c63a0b9b71ce4ca 1 1 1" "" "author A" "author-time 0"
                                   "author-tz +0000" (tab ""))
                       (lambda (&rest line) (push line lines)))
      (expect lines :to-equal '((1 "07e5d895a032520e317659bd3c63a0b9b71ce4ca" "A" "1970-01-01T00:00:00+00:00" "")))))

  (it "drops text before the first diff --git line"
    (expect (split-patch-by-file (lines-text "warning: preamble" "diff --git a/x b/x" "index 1..2"))
            :to-equal '(("diff --git a/x b/x" "index 1..2"))))

  (it "names a renamed file's source as from"
    (let ((shaped :not-called))
      (diff-files/k (list (list :path "new.txt" :original-path "old.txt" :added 0 :deleted 0)) nil
                    :output :stat
                    :on-complete (lambda (files characters) (declare (ignore characters)) (setf shaped files))
                    :on-truncated (lambda (&rest arguments) (fail "stat mode truncated: ~S" arguments)))
      (expect (mapcar (lambda (file) (list (json-alist-value file "path") (json-alist-value file "from"))) shaped)
              :to-equal '(("new.txt" "old.txt")))))

  (it "reports a null branch when the snapshot has none"
    (expect (json-kit:json-null-p (cdr (assoc "branch" (status-fields (list :entries nil)) :test #'string=)))
            :to-be-truthy)))

(describe "aitools.vcs.domain repository paths at their edges"
  (it "reads an empty path as the working directory"
    (expect (repository-relative-path "" "/r/sub" "/r") :to-equal "sub"))
  (it "returns NIL for a path above the top"
    (expect (repository-relative-path "/" "/r" "/r") :to-be nil))
  (it "renders the working directory itself as ."
    (expect (path-from-directory "sub" "/r" "/r/sub") :to-equal ".")))
