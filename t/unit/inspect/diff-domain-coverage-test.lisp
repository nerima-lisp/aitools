(in-package #:aitools.inspect.test)

(describe "diff domain normalization"
  (it "strips one trailing carriage return only when requested"
    (expect (diff-key (format nil "line~C" #\Return) :ignore-eol t) :to-equal "line")
    (expect (diff-key "line" :ignore-eol t) :to-equal "line")
    (expect (diff-key (format nil "a ~C~C" #\Tab #\Return)
                      :ignore-whitespace t :ignore-eol t)
            :to-equal "a"))

  (it "retexts EOL-normalized changes and ignores final-newline differences"
    (let* ((old-line (format nil "old~C" #\Return))
           (hunks (compare-lines (vector old-line) #("new")
                                 :ignore-eol t
                                 :final-newline-a nil
                                 :final-newline-b t))
           (lines (aitools.kernel.domain:diff-hunk-lines (first hunks))))
      (expect (mapcar #'aitools.kernel.domain:diff-line-op lines)
              :to-equal '(:delete :insert))
      (expect (mapcar #'aitools.kernel.domain:diff-line-text lines)
              :to-equal (list old-line "new"))
      (expect (compare-lines (vector (format nil "same~C" #\Return))
                             #("same")
                             :ignore-eol t
                             :final-newline-a nil
                             :final-newline-b t)
              :to-be nil))))
