(in-package #:aitools.vcs.test)

(describe "aitools.vcs.domain repository paths at their edges"
  (it "reads an empty path as the working directory"
    (expect (repository-relative-path "" "/r/sub" "/r") :to-equal "sub"))
  (it "returns NIL for a path above the top"
    (expect (repository-relative-path "/" "/r" "/r") :to-be nil))
  (it "renders the working directory itself as ."
    (expect (path-from-directory "sub" "/r" "/r/sub") :to-equal ".")))

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

(defun condition-text (thunk)
  (handler-case (progn (funcall thunk) :returned)
    (error (condition) (princ-to-string condition))))
