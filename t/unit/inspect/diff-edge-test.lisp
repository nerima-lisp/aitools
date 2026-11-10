(in-package #:aitools.inspect.test)

(describe "diff edge cases"
  (it "compares binary files by bytes only"
    (let ((files `(("/work/a.bin" . ,(coerce #(0 1 2) '(vector (unsigned-byte 8))))
                   ("/work/b.bin" . ,(coerce #(0 1 3) '(vector (unsigned-byte 8))))
                   ("/work/c.bin" . ,(coerce #(0 1 2) '(vector (unsigned-byte 8)))))))
      (multiple-value-bind (kind fields) (diff-of files :a "a.bin" :b "b.bin")
        (expect kind :to-be :ok)
        (expect (field fields "binary") :to-be t)
        (expect (json-false-value-p (field fields "identical")) :to-be t)
        (expect (field fields "diff") :to-be nil))
      (expect (field (nth-value 1 (diff-of files :a "a.bin" :b "c.bin")) "identical") :to-be t)))

  (it-each (("the first" "gone.txt" "a.txt") ("the second" "a.txt" "gone.txt"))
      "reports ~A path missing as input.not-found"
      (label a b)
    (declare (ignore label))
    (multiple-value-bind (kind fields) (diff-of '(("/work/a.txt" . "x")) :a a :b b)
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (getf fields :message) :to-equal "file gone.txt does not exist")))

  (it "rejects a file compared with a directory"
    (multiple-value-bind (kind fields) (diff-of '(("/work/a.txt" . "x") ("/work/d/x" . "y")) :a "a.txt" :b "d")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "argument.invalid")
      (expect (getf fields :message) :to-equal "a.txt and d must both be files or both be directories")
      (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools info a.txt")))

  (it "is partial past --limit hunks, counting them all"
    (let ((files `(("/work/a.txt" . ,(format nil "~{~A~%~}" (loop for i below 30 collect i)))
                   ("/work/b.txt" . ,(format nil "~{~A~%~}" (loop for i below 30 collect (if (member i '(2 25)) "x" i)))))))
      (multiple-value-bind (kind fields) (diff-of files :a "a.txt" :b "b.txt" :limit 1)
        (expect kind :to-be :partial)
        (expect (field fields "hunks") :to-be 2)
        (expect (search "+x" (field fields "diff")) :to-be-truthy)
        (expect (field fields "truncated") :to-be t))))

  (it "is partial past --limit entries per directory list"
    (multiple-value-bind (kind fields) (diff-of '(("/work/d1/k" . "k") ("/work/d2/k" . "k") ("/work/d2/n1" . "1")
                                                  ("/work/d2/n2" . "2"))
                                                :a "d1" :b "d2" :limit 1)
      (expect kind :to-be :partial)
      (expect (field fields "added") :to-equal '("n1"))))

  (it "ignores .git metadata and temporary files in directory diffs"
    (multiple-value-bind (kind fields)
        (diff-of
         (list (cons "/work/d1/keep" "old")
               (cons "/work/d1/size" "x")
               (cons "/work/d1/.git/config" "left")
               (cons "/work/d1/.aitools-op-1.tmp" "left")
               (cons "/work/d2/keep" "new")
               (cons "/work/d2/size" "long")
               (cons "/work/d2/.git/config" "right")
               (cons "/work/d2/.aitools-op-1.tmp" "right"))
         :a "d1" :b "d2")
      (expect kind :to-be :ok)
      (expect (field fields "added") :to-equal nil)
      (expect (field fields "removed") :to-equal nil)
      (expect (field fields "modified") :to-equal '("keep" "size"))
      (expect (field fields "identical_count") :to-be 0)))
  (it "retexts ignored-whitespace hunks for additions, deletions, and context"
    (let* ((insert (first (compare-lines #() #("new") :ignore-whitespace t)))
           (delete (first (compare-lines #("old") #() :ignore-whitespace t)))
           (change (first (compare-lines #("before  here" "old" "after") #("before here" "new" "after") :ignore-whitespace t))))
      (expect (list (aitools.kernel.domain:diff-hunk-old-count insert)
                    (aitools.kernel.domain:diff-hunk-new-count insert))
              :to-equal '(0 1))
      (expect (mapcar #'aitools.kernel.domain:diff-line-op
                      (aitools.kernel.domain:diff-hunk-lines insert))
              :to-equal '(:insert))
      (expect (mapcar #'aitools.kernel.domain:diff-line-text
                      (aitools.kernel.domain:diff-hunk-lines insert))
              :to-equal '("new"))
      (expect (list (aitools.kernel.domain:diff-hunk-old-count delete)
                    (aitools.kernel.domain:diff-hunk-new-count delete))
              :to-equal '(1 0))
      (expect (mapcar #'aitools.kernel.domain:diff-line-op
                      (aitools.kernel.domain:diff-hunk-lines delete))
              :to-equal '(:delete))
      (expect (mapcar #'aitools.kernel.domain:diff-line-text
                      (aitools.kernel.domain:diff-hunk-lines delete))
              :to-equal '("old"))
      (expect (mapcar #'aitools.kernel.domain:diff-line-op
                      (aitools.kernel.domain:diff-hunk-lines change))
              :to-equal '(:context :delete :insert :context))
      (expect (mapcar #'aitools.kernel.domain:diff-line-text
                      (aitools.kernel.domain:diff-hunk-lines change))
              :to-equal '("before  here" "old" "new" "after"))
      (expect (aitools.inspect.domain:diff-key "" :ignore-eol t) :to-equal ""))))

(describe "diff of the workspace root"
  (it "reports the root directory as ."
    (let ((fields (nth-value 1 (diff-of '(("/work/k" . "k") ("/other/k" . "k")) :a "." :b "/other"))))
      (expect (field fields "a") :to-equal ".")
      (expect (field fields "b") :to-equal "/other"))))
