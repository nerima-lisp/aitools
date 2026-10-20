(in-package #:aitools.search.test)

(defun git-index (&rest paths)
  "The bytes of a version 2 git index listing PATHS: zeroed stat data and
object ids, the name length in the flags, and each entry NUL-padded to a
multiple of 8 bytes, as git writes it."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (flet ((u32 (n) (loop for shift from 24 downto 0 by 8 do (vector-push-extend (ldb (byte 8 shift) n) out))))
      (map nil (lambda (char) (vector-push-extend (char-code char) out)) "DIRC")
      (u32 2)
      (u32 (length paths))
      (dolist (path paths)
        (let ((start (fill-pointer out)) (name (string-bytes path)))
          (dotimes (i 60) (vector-push-extend 0 out))
          (vector-push-extend (ldb (byte 8 8) (length name)) out)
          (vector-push-extend (ldb (byte 8 0) (length name)) out)
          (map nil (lambda (octet) (vector-push-extend octet out)) name)
          (loop do (vector-push-extend 0 out) until (zerop (mod (- (fill-pointer out) start) 8)))))
      (coerce out '(simple-array (unsigned-byte 8) (*))))))

(defun overview-git (files &rest arguments)
  (let ((fields (nth-value 1 (apply #'run-flow #'overview/k (make-fake-ports :files files) arguments))))
    (let ((git (field fields "git")))
      (mapcar (lambda (name) (jfield git name)) '("branch" "head" "untracked" "deleted")))))

(describe "aitools.search.application overview/k git summary"
  (flet ((head (text) (list "/w/.git/HEAD" text)))
    (it "resolves the branch through packed-refs when no loose ref exists"
      (expect (overview-git (list (head (format nil "ref: refs/heads/main~%"))
                                  (list "/w/.git/packed-refs"
                                        (lines "# pack-refs with: peeled" "aaaa refs/heads/other" "^bbbb" "cccc refs/heads/main"))))
              :to-equal '("main" "cccc" 0 0)))

    (it-each (("0123abc" "null" "0123abc") ("ref: refs/tags/v1" "refs/tags/v1" "null") ("" "null" "null"))
        "reads HEAD ~S as branch ~A and head ~A"
        (text branch sha)
      (flet ((value (text) (if (string= text "null") json-kit:+json-null+ text)))
        (expect (subseq (overview-git (list (head (format nil "~A~%" text)))) 0 2)
                :to-equal (list (value branch) (value sha)))))

    (it "counts untracked and deleted paths from the index, below the start path"
      (let ((files (list (head (format nil "ref: refs/heads/main~%"))
                         (list "/w/.git/index" (git-index "src/a.lisp" "src/gone.lisp" "top.txt" "zz/gone.txt"))
                         (list "/w/src/a.lisp" "(x)") (list "/w/src/new.lisp" "(y)") (list "/w/top.txt" "t"))))
        (expect (overview-git files) :to-equal (list "main" json-kit:+json-null+ 1 2))
        (expect (overview-git files :path "src") :to-equal (list "main" json-kit:+json-null+ 1 1))
        ;; A workspace root below the repository top sees the index paths below it.
        (expect (overview-git files :root "/w/src") :to-equal (list "main" json-kit:+json-null+ 1 1))))

    (it "treats an unreadable index as no tracked paths"
      (expect (overview-git (list (head (format nil "ref: refs/heads/main~%")) (list "/w/.git/index" "DIRX")
                                  (list "/w/a.lisp" "(x)")))
              :to-equal (list "main" json-kit:+json-null+ 1 0)))))

(describe "aitools.search.application overview/k scanning"
  (it "is partial past --limit with a next command carrying the path, and skips a file that fails to read"
    (multiple-value-bind (kind fields)
        (run-flow #'overview/k (make-fake-ports :files '(("/w/src/a.lisp" "x") ("/w/src/b.py" "y") ("/w/src/c.py" "z"))
                                                :failing '("/w/src/c.py"))
                  :path "src" :limit 1)
      (expect kind :to-be :partial)
      (expect (mapcar (lambda (row) (list (jfield row "lang") (jfield row "files"))) (field fields "languages"))
              :to-equal '(("common-lisp" 1)))
      (expect (field fields "languages_total") :to-be 2)
      (expect (length (field fields "entries")) :to-be 3)
      (expect (field fields "next_commands") :to-equal '("aitools overview src --limit 2"))))

  (it "reports a path that does not exist as input.not-found"
    (expect (getf (nth-value 1 (run-flow #'overview/k (make-fake-ports :directories '("/w")) :path "nope")) :code) :to-equal "input.not-found")))

(describe "aitools.search.domain overview parts"
  (it-each (("ref: refs/heads/main" "main" nil "refs/heads/main") ("ref: HEAD2" "HEAD2" nil "HEAD2")
            ("ref: " nil "ref:" nil) ("  0123abc  " nil "0123abc" nil) ("" nil nil nil))
      "parses HEAD ~S"
      (text branch sha ref)
    (expect (multiple-value-list (parse-head-file text)) :to-equal (list branch sha ref)))

  (it "finds a ref in packed-refs, ignoring comment, peeled, and malformed lines"
    (let ((text (lines "# pack-refs with: peeled" "^1111 refs/heads/main" "noseparator" "2222 refs/heads/main")))
      (expect (packed-ref-sha text "refs/heads/main") :to-equal "2222")
      (expect (packed-ref-sha text "refs/heads/other") :to-be nil)))

  (it "orders tally rows by lines, then bytes, then name"
    (let ((tally (make-language-tally)))
      (tally-file tally "b" 10 5)
      (tally-file tally "a" 10 5)
      (tally-file tally "c" 10 9)
      (tally-file tally "d" 20 1)
      (expect (mapcar #'first (aitools.search.domain::language-tally-rows tally)) :to-equal '("d" "c" "a" "b")))))
