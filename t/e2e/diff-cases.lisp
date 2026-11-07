(in-package #:aitools.e2e.test)

;;;; Correspondence table row 11: diff output and comparison.

(define-row-case (11 "cmp -s's status is diff's identical" :foreign ("cmp")) (ws)
  (put-diff-pair ws)
  (put ws "x2.txt" (file-octets ws "x.txt"))
  (dolist (other '("x2.txt" "y.txt"))
    (expect (if (jget (run-ok ws (list "diff" "x.txt" other)) "identical") "0" "1")
            :to-equal (string-trim '(#\Newline)
                                   (oracle ws '("cmp") (format nil "cmp -s x.txt ~A; echo $?" other) nil)))))

(define-row-case (11 "comm is diff --output set" :foreign ("comm")) (ws)
  (put ws "x.txt" (format nil "a~%b~%c~%e~%"))
  (put ws "y.txt" (format nil "B~%a~%c~%d~%e~%"))
  (let ((envelope (run-ok ws '("diff" "x.txt" "y.txt" "--output" "set"))))
    (expect (sorted (jlist (jget envelope "only_a")))
            :to-equal (text-lines (oracle ws '("comm" "sort") "sort x.txt > xs; sort y.txt > ys; comm -23 xs ys" nil)))
    (expect (sorted (jlist (jget envelope "only_b")))
            :to-equal (text-lines (oracle ws '("comm" "sort") "sort x.txt > xs; sort y.txt > ys; comm -13 xs ys" nil)))
    (expect (jget envelope "both_count")
            :to-be (length (text-lines (oracle ws '("comm" "sort") "sort x.txt > xs; sort y.txt > ys; comm -12 xs ys" nil))))))

(define-row-case (11 "diff -w's status is diff --ignore-whitespace's identical" :foreign ("diff")) (ws)
  (put ws "w1.txt" (format nil "a  b~%c~%"))
  (put ws "w2.txt" (format nil "a b~%c~%"))
  (put ws "w3.txt" (format nil "a b~%d~%"))
  (dolist (other '("w2.txt" "w3.txt"))
    (expect (if (jget (run-ok ws (list "diff" "w1.txt" other "--ignore-whitespace")) "identical") "0" "1")
            :to-equal (string-trim '(#\Newline)
                                   (oracle ws '("diff") (format nil "diff -w w1.txt ~A >/dev/null; echo $?" other) nil)))))

(define-row-case (11 "diff -u is diff" :foreign ("diff")) (ws)
  (put-diff-pair ws)
  (let ((diff (jget (run-ok ws '("diff" "x.txt" "y.txt")) "diff")))
    (expect (subseq diff (1+ (position #\Newline diff :start (1+ (position #\Newline diff)))))
            :to-equal (oracle ws '("diff" "tail") "diff -u x.txt y.txt | tail -n +3" nil
                              :expect-status 0))))

