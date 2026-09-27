;;;; t/unit/kernel/unified-diff-test.lisp
(in-package #:aitools.kernel.test)

(describe "aitools.kernel.domain unified diff"
  (it "returns an empty diff for identical files"
    (let ((a (list "one" "two" "three")))
      (expect (generate-unified-diff a a :path-a "f" :path-b "f") :to-equal "")
      (expect (generate-diff-hunks a a) :to-be-falsy)))

  (it "generates, parses, and applies a round trip"
    (let* ((a (list "one" "two" "three" "four" "five"))
           (b (list "one" "TWO" "three" "four" "five" "six"))
           (diff (generate-unified-diff a b :path-a "f" :path-b "f"))
           (patch (first (parse-unified-diff diff))))
      (expect diff :to-contain "-two")
      (expect diff :to-contain "+TWO")
      (expect diff :to-contain "+six")
      (apply-hunks/k a (file-patch-hunks patch)
                     :on-applied (lambda (lines newline count)
                                   (declare (ignore newline))
                                   (expect count :to-be 1)
                                   (expect lines :to-equal b))
                     :on-conflict (lambda (i h nearby)
                                    (declare (ignore i h nearby))
                                    (fail "expected the hunk to apply")))))

  (it "applies the reverse of a patch to get back the original"
    (let* ((a (list "one" "two" "three"))
           (b (list "one" "TWO" "three" "four"))
           (diff (generate-unified-diff a b :path-a "f" :path-b "f"))
           (patch (first (parse-unified-diff diff))))
      (apply-hunks/k b (file-patch-hunks patch) :reverse t
                     :on-applied (lambda (lines newline count)
                                   (declare (ignore newline count))
                                   (expect lines :to-equal a))
                     :on-conflict (lambda (i h nearby)
                                    (declare (ignore i h nearby))
                                    (fail "expected the reverse hunk to apply")))))

  (it "applies within --fuzz lines when the target has shifted"
    (let* ((a (list "one" "two" "three" "four" "five"))
           (b (list "one" "TWO" "three" "four" "five"))
           (shifted-a (append (list "X" "Y") a))
           (diff (generate-unified-diff a b :path-a "f" :path-b "f"))
           (patch (first (parse-unified-diff diff))))
      (apply-hunks/k shifted-a (file-patch-hunks patch) :fuzz 3
                     :on-applied (lambda (lines newline count)
                                   (declare (ignore newline count))
                                   (expect lines :to-equal (append (list "X" "Y") b)))
                     :on-conflict (lambda (i h nearby)
                                    (declare (ignore i h nearby))
                                    (fail "expected fuzz to find the shifted hunk")))))

  (it "reports a conflict, with nearby content, when a hunk cannot be placed"
    (let* ((a (list "one" "two" "three"))
           (b (list "one" "TWO" "three"))
           (diff (generate-unified-diff a b :path-a "f" :path-b "f"))
           (patch (first (parse-unified-diff diff)))
           (different (list "one" "DIFFERENT" "three")))
      (apply-hunks/k different (file-patch-hunks patch)
                     :on-applied (lambda (lines newline count)
                                   (declare (ignore lines newline count))
                                   (fail "expected a conflict"))
                     :on-conflict (lambda (index hunk nearby)
                                    (declare (ignore hunk))
                                    (expect index :to-be 0)
                                    (expect nearby :to-contain "DIFFERENT")))))

  (it "strips a/ and b/ path prefixes by default"
    (let ((diff (format nil "--- a/src/foo.lisp~%+++ b/src/foo.lisp~%@@ -1,1 +1,1 @@~%-old~%+new~%")))
      (expect (file-patch-old-path (first (parse-unified-diff diff))) :to-equal "src/foo.lisp")
      (expect (file-patch-new-path (first (parse-unified-diff diff))) :to-equal "src/foo.lisp")))

  (it "keeps paths verbatim with --strip 0"
    (let ((diff (format nil "--- a/src/foo.lisp~%+++ b/src/foo.lisp~%@@ -1,1 +1,1 @@~%-old~%+new~%")))
      (expect (file-patch-old-path (first (parse-unified-diff diff :strip 0))) :to-equal "a/src/foo.lisp")))

  (it "marks only the true last line as having no trailing newline"
    (let* ((a (list "one" "two"))
           (b (list "one" "TWO"))
           (diff (generate-unified-diff a b :path-a "f" :path-b "f"
                                        :final-newline-a nil :final-newline-b nil)))
      (expect diff :to-contain "\\ No newline at end of file")
      (let ((patch (first (parse-unified-diff diff))))
        (apply-hunks/k a (file-patch-hunks patch)
                       :on-applied (lambda (lines newline count)
                                     (declare (ignore count))
                                     (expect lines :to-equal b)
                                     (expect newline :to-be-falsy))
                       :on-conflict (lambda (i h nearby)
                                      (declare (ignore i h nearby))
                                      (fail "expected the hunk to apply"))))))

  (it "splits distant edits into separate hunks and merges nearby ones"
    (let* ((a (loop for i from 1 to 20 collect (format nil "line~D" i)))
           (b (append (list "HEAD") (subseq a 1 18) (list "TAIL")))
           (diff (generate-unified-diff a b :path-a "f" :path-b "f" :context 2))
           (patch (first (parse-unified-diff diff))))
      (expect (length (file-patch-hunks patch)) :to-be 2)
      (apply-hunks/k a (file-patch-hunks patch)
                     :on-applied (lambda (lines newline count)
                                   (declare (ignore newline))
                                   (expect count :to-be 2)
                                   (expect lines :to-equal b))
                     :on-conflict (lambda (i h nearby)
                                    (declare (ignore i h nearby))
                                    (fail "expected both hunks to apply")))))

  (it "reads hunk lines that look like file headers as deletions and insertions"
    (let* ((a (list "select 1;" "-- old comment" "select 2;"))
           (b (list "select 1;" "++ counter" "select 2;"))
           (diff (format nil "--- a/q.sql~%+++ b/q.sql~%@@ -1,3 +1,3 @@~% select 1;~%--- old comment~%+++ counter~% select 2;~%"))
           (patches (parse-unified-diff diff)))
      (expect (length patches) :to-be 1)
      (expect (mapcar #'diff-line-op (diff-hunk-lines (first (file-patch-hunks (first patches)))))
              :to-equal '(:context :delete :insert :context))
      (apply-hunks/k a (file-patch-hunks (first patches))
                     :on-applied (lambda (lines newline count)
                                   (declare (ignore newline count))
                                   (expect lines :to-equal b))
                     :on-conflict (lambda (i h nearby)
                                    (declare (ignore i h nearby))
                                    (fail "expected the hunk to apply")))))

  (it "starts the next file after a hunk whose line counts are used up"
    (let ((patches (parse-unified-diff
                    (format nil "--- a/x~%+++ b/x~%@@ -1 +1 @@~%-a~%+b~%--- a/y~%+++ b/y~%@@ -1 +1 @@~%-c~%+d~%"))))
      (expect (mapcar #'file-patch-old-path patches) :to-equal (list "x" "y"))
      (expect (mapcar (lambda (patch) (length (file-patch-hunks patch))) patches) :to-equal (list 1 1))))

  (it "falls back to one whole-file hunk past the LCS cell limit"
    (let* ((a (list "a1" "a2" "a3" "a4"))
           (b (list "b1" "b2" "b3" "b4" "b5"))
           (hunks (generate-diff-hunks a b :max-cells 10)))
      (expect (length hunks) :to-be 1)
      (expect (diff-hunk-old-count (first hunks)) :to-be 4)
      (expect (diff-hunk-new-count (first hunks)) :to-be 5))))

(describe "aitools.kernel.domain unified diff parsing edges"
  (it-each (("@@ bogus @@")          ; no - or + range
            ("@@ -1,2 +3,4")         ; no closing @@
            ("@@ -a +1 @@")          ; a non-numeric start
            ("@@ -1,x +1 @@")        ; a non-numeric count
            ("@@ -1,-2 +1,0 @@")     ; a negative count
            ("@@ -1 +1,+1 @@")      ; a signed count
            ("@@ -1, +1 @@"))       ; an empty count
      "rejects the hunk header ~S"
      (header)
    (signals simple-error
      (parse-unified-diff (format nil "--- a/x~%+++ b/x~%~A~%-a~%+b~%" header))))

  (it "defaults a hunk range with no count to one line"
    (let ((hunk (first (file-patch-hunks
                        (first (parse-unified-diff (format nil "--- a/x~%+++ b/x~%@@ -3 +4 @@~%-c~%+d~%")))))))
      (expect (list (diff-hunk-old-start hunk) (diff-hunk-old-count hunk)
                    (diff-hunk-new-start hunk) (diff-hunk-new-count hunk))
              :to-equal '(3 1 4 1))))

  (it "splits text into lines without inventing a trailing empty line"
    (expect (split-diff-lines "") :to-equal '())
    (expect (split-diff-lines (format nil "a~%~%b")) :to-equal '("a" "" "b"))
    (expect (split-diff-lines (format nil "a~%b~%")) :to-equal '("a" "b")))

  (it "strips at most as many path components as the path has"
    (expect (strip-path-components "a/b/c" 0) :to-equal "a/b/c")
    (expect (strip-path-components "a/b/c" 2) :to-equal "c")
    (expect (strip-path-components "a/b/c" 5) :to-equal "c")))

(describe "aitools.kernel.domain unified diff body edges"
  (it "reads an empty hunk line as an empty context line and skips text outside hunks"
    (let* ((diff (format nil "diff --git a/x b/x~%~%--- a/x~%+++ b/x~%@@ -1,3 +1,3 @@~% a~%~%-b~%+c~%x~%"))
           (hunk (first (file-patch-hunks (first (parse-unified-diff diff))))))
      (expect (mapcar (lambda (line) (list (diff-line-op line) (diff-line-text line))) (diff-hunk-lines hunk))
              :to-equal '((:context "a") (:context "") (:delete "b") (:insert "c"))))))
