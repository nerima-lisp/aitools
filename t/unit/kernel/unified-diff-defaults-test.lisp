(in-package #:aitools.kernel.test)

(describe "aitools.kernel.domain default diff options"
  (it-each ((8 2 7) (6 1 6))
    "uses three context lines by default for ~D input lines"
    (length expected-start expected-count)
    (let* ((a (loop for index below length collect (format nil "line~D" index)))
           (b (copy-list a)))
      (setf (nth (floor length 2) b) "changed")
      (let* ((hunks (generate-diff-hunks a b))
             (hunk (first hunks))
             (diff (generate-unified-diff a b :path-a "old" :path-b "new"))
             (patch (first (parse-unified-diff diff)))
             (parsed-hunk (first (file-patch-hunks patch))))
        (expect (list (diff-hunk-old-start hunk) (diff-hunk-old-count hunk))
                :to-equal (list expected-start expected-count))
        (expect (diff-hunk-old-count parsed-hunk) :to-be expected-count)
        (expect (file-patch-old-path patch) :to-equal "old")
        (expect (file-patch-new-path patch) :to-equal "new")
        (expect (loop for current-hunk in hunks
                      thereis (some #'diff-line-no-newline
                                    (diff-hunk-lines current-hunk)))
                :to-be-falsy))))

  (it "does not search for a shifted hunk when fuzz is omitted"
    (let* ((diff (generate-unified-diff '("old" "tail") '("new" "tail")
                                        :path-a "f" :path-b "f" :context 0))
           (patch (first (parse-unified-diff diff)))
           (result (apply-hunks/k '("prefix" "old" "tail")
                                  (file-patch-hunks patch)
                                  :on-applied (lambda (lines final-newline count)
                                                (declare (ignore lines final-newline count))
                                                :applied)
                                  :on-conflict (lambda (index hunk nearby)
                                                 (declare (ignore index hunk nearby))
                                                 :conflict))))
      (expect result :to-be :conflict)))
)