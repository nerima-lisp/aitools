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
      (expect (field fields "added") :to-equal '("n1")))))

(describe "diff of the workspace root"
  (it "reports the root directory as ."
    (let ((fields (nth-value 1 (diff-of '(("/work/k" . "k") ("/other/k" . "k")) :a "." :b "/other"))))
      (expect (field fields "a") :to-equal ".")
      (expect (field fields "b") :to-equal "/other"))))
