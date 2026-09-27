;;;; t/e2e/read-search-cases.lisp
;;;;
;;;; Correspondence table rows 1-5: reading and inspecting. Rows 6-11 are in
;;;; search-find-cases.lisp, which uses the helpers defined here.
(in-package #:aitools.e2e.test)

(defparameter +five-lines+ (format nil "alpha~%beta~%gamma~%delta~%epsilon~%"))

(defun read-text (workspace &rest arguments)
  (lines-text (jget (run-ok workspace (list* "read" arguments)) "lines")))

(defun sorted (strings)
  (sort (copy-list strings) #'string<))

(defun strip-dot-slash (path)
  (if (and (>= (length path) 2) (string= "./" path :end2 2)) (subseq path 2) path))

(defun oracle-lines (workspace tools script fixed)
  "The oracle's output lines with a leading ./ removed, sorted."
  (sorted (mapcar #'strip-dot-slash (text-lines (oracle workspace tools script fixed)))))

;;; Row 1

(define-row-case (1 "cat is read" :foreign ("cat")) (ws)
  (put ws "f.txt" +five-lines+)
  (expect (read-text ws "f.txt") :to-equal (oracle ws '("cat") "cat f.txt" +five-lines+)))

(define-row-case (1 "head -n 3 is read --range 1:3" :foreign ("head")) (ws)
  (put ws "f.txt" +five-lines+)
  (expect (read-text ws "f.txt" "--range" "1:3")
          :to-equal (oracle ws '("head") "head -n 3 f.txt" (format nil "alpha~%beta~%gamma~%"))))

(define-row-case (1 "tail -n 2 is read --tail 2" :foreign ("tail")) (ws)
  (put ws "f.txt" +five-lines+)
  (expect (read-text ws "f.txt" "--tail" "2")
          :to-equal (oracle ws '("tail") "tail -n 2 f.txt" (format nil "delta~%epsilon~%"))))

(define-row-case (1 "sed -n '2,4p' is read --range 2:4" :foreign ("sed")) (ws)
  (put ws "f.txt" +five-lines+)
  (expect (read-text ws "f.txt" "--range" "2:4")
          :to-equal (oracle ws '("sed") "sed -n '2,4p' f.txt" (format nil "beta~%gamma~%delta~%"))))

(define-row-case (1 "nl -ba numbering is read's start_line and lines" :foreign ("nl")) (ws)
  (put ws "f.txt" +five-lines+)
  (let* ((envelope (run-ok ws '("read" "f.txt" "--range" "2:3")))
         (numbered (loop for line in (jlist (jget envelope "lines"))
                         for n from (jget envelope "start_line")
                         collect (format nil "~6D~C~A~%" n #\Tab line))))
    (expect (apply #'concatenate 'string numbered)
            :to-equal (oracle ws '("nl" "sed") "nl -ba f.txt | sed -n '2,3p'"
                              (format nil "     2~Cbeta~%     3~Cgamma~%" #\Tab #\Tab)))))

;;; Row 2

(define-row-case (2 "sed -n '/A/,/B/p' is read --between A B" :foreign ("sed")) (ws)
  (put ws "f.txt" +five-lines+)
  (expect (read-text ws "f.txt" "--between" "^b" "^d")
          :to-equal (oracle ws '("sed") "sed -n '/^b/,/^d/p' f.txt" (format nil "beta~%gamma~%delta~%"))))

;;; Row 3

(defparameter +invisible-fixture+
  (concatenate 'string
               "plain" (string #\Newline)
               "zw" (string (code-char #x200B)) "x" (string #\Newline)
               "nb" (string (code-char #xA0)) "sp" (string #\Newline)
               "id" (string (code-char #x3000)) "sp" (string #\Newline)
               "cr" (string #\Return) (string #\Newline)
               "tab" (string #\Tab) "here" (string #\Newline))
  "Every non-ASCII character here is one of the characters `read --escape-invisible` spells out, so
`cat -vet` under LC_ALL=C flags exactly the lines aitools must escape.")

(define-row-case (3 "cat -A flags the same lines read --escape-invisible escapes" :foreign ("cat")) (ws)
  (put ws "inv.txt" +invisible-fixture+)
  (let* ((raw (text-lines +invisible-fixture+))
         (escaped (jlist (jget (run-ok ws '("read" "inv.txt" "--escape-invisible")) "lines")))
         (shown (mapcar (lambda (line) (subseq line 0 (1- (length line))))
                        (text-lines (oracle ws '("cat") "cat -vet inv.txt" nil)))))
    (expect (length escaped) :to-be (length raw))
    (expect (loop for line in raw for other in escaped for n from 1
                  unless (string= line other) collect n)
            :to-equal (loop for line in raw for other in shown for n from 1
                            unless (string= line other) collect n))))

(define-row-case (3 "read --escape-invisible renders each invisible character as perl does" :foreign ("perl")) (ws)
  (put ws "inv.txt" +invisible-fixture+)
  (expect (read-text ws "inv.txt" "--escape-invisible")
          :to-equal (oracle ws '("perl")
                            "perl -CSD -pe 's/([\\x{0}-\\x{8}\\x{9}\\x{B}-\\x{1F}\\x{7F}\\x{A0}\\x{200B}\\x{3000}])/sprintf(\"\\\\u{%04X}\",ord $1)/ge' inv.txt"
                            (format nil "plain~%zw\\u{200B}x~%nb\\u{00A0}sp~%id\\u{3000}sp~%cr\\u{000D}~%tab\\u{0009}here~%"))))

;;; Row 4

(defparameter +strings-fixture+
  (concatenate '(vector (unsigned-byte 8))
               (octets "abcdefg") #(0 1 2) (octets "xyz") #(255) (octets "hijklmn") #(0 10)))

(define-row-case (4 "xxd -p is read --as hex" :foreign ("xxd")) (ws)
  (put ws "b.bin" +strings-fixture+)
  (let ((rows (jlist (jget (run-ok ws '("read" "b.bin" "--as" "hex")) "rows"))))
    (expect (remove #\Space (apply #'concatenate 'string (mapcar (lambda (row) (jget row "hex")) rows)))
            :to-equal (remove #\Newline (oracle ws '("xxd") "xxd -p b.bin"
                                                (format nil "6162636465666700010278797aff68696a6b6c6d6e000a~%"))))
    (expect (mapcar (lambda (row) (jget row "offset")) rows) :to-equal '(0 16))))

(define-row-case (4 "od -An -tx1 is read --as hex" :foreign ("od")) (ws)
  (put ws "b.bin" +strings-fixture+)
  (let ((rows (jlist (jget (run-ok ws '("read" "b.bin" "--as" "hex")) "rows"))))
    (expect (format nil "~{~A~^ ~}" (mapcar (lambda (row) (jget row "hex")) rows))
            :to-equal (format nil "~{~A~^ ~}"
                              (remove "" (uiop:split-string
                                          (oracle ws '("od") "od -An -v -tx1 b.bin" nil)
                                          :separator '(#\Space #\Newline #\Tab))
                                      :test #'string=)))))

(define-row-case (4 "strings -a -t d is read --as strings" :foreign ("strings")) (ws)
  (put ws "b.bin" +strings-fixture+)
  (expect (mapcar (lambda (entry) (format nil "~D ~A" (jget entry "offset") (jget entry "text")))
                  (jlist (jget (run-ok ws '("read" "b.bin" "--as" "strings")) "strings")))
          :to-equal (mapcar (lambda (line) (string-left-trim " " line))
                            (text-lines (oracle ws '("strings") "strings -a -n 4 -t d b.bin"
                                                (format nil "      0 abcdefg~%     14 hijklmn~%"))))))

;;; Row 5

(define-row-case (5 "wc -l -w -c is info's lines, words, and size" :foreign ("wc")) (ws)
  (put ws "f.txt" (format nil "one two~%three four five~%six~%"))
  (let ((envelope (run-ok ws '("info" "f.txt"))))
    (expect (list (jget envelope "lines") (jget envelope "words") (jget envelope "size"))
            :to-equal (mapcar #'parse-integer
                              (remove "" (uiop:split-string (oracle ws '("wc") "wc -l -w -c < f.txt" nil)
                                                            :separator '(#\Space #\Tab #\Newline))
                                      :test #'string=)))))

(define-row-case (5 "stat's permission bits are info's mode" :foreign ("stat")) (ws)
  (put ws "f.txt" "x" :mode #o640)
  (expect (string-left-trim "0" (jget (run-ok ws '("info" "f.txt")) "mode"))
          :to-equal (string-trim '(#\Newline)
                                 (oracle ws '("stat") "stat -c %a f.txt 2>/dev/null || stat -f %Lp f.txt" (format nil "640~%")))))

(define-row-case (5 "file --mime-type is info's mime" :foreign ("file")) (ws)
  (put ws "f.txt" (format nil "plain text~%"))
  (expect (jget (run-ok ws '("info" "f.txt")) "mime")
          :to-equal (string-trim '(#\Newline) (oracle ws '("file") "file -b --mime-type f.txt" (format nil "text/plain~%")))))

(define-row-case (5 "sha256sum is info --digest sha256" :foreign ("sha256sum")) (ws)
  (put ws "f.txt" (format nil "digest me~%"))
  (expect (jget (run-ok ws '("info" "f.txt" "--digest" "sha256")) "digest" "value")
          :to-equal (subseq (oracle ws '("sha256sum") "sha256sum f.txt"
                                    (format nil "d9a84c53949e4ee500bb3ccf6e91cf52d02124d0d488cbf358ea3e4df6366ae3  f.txt~%"))
                            0 64)))

(define-row-case (5 "md5sum is info --digest md5" :foreign ("md5sum")) (ws)
  (put ws "f.txt" (format nil "digest me~%"))
  (expect (jget (run-ok ws '("info" "f.txt" "--digest" "md5")) "digest" "value")
          :to-equal (subseq (oracle ws '("md5sum") "md5sum f.txt" (format nil "a1fb764e160806df2e85218e082d0194  f.txt~%")) 0 32)))

(define-row-case (5 "test -e is info --allow-missing's exists" :foreign ("test")) (ws)
  (put ws "f.txt" "x")
  (dolist (path '("f.txt" "missing.txt"))
    (expect (if (jget (run-ok ws (list "info" path "--allow-missing")) "exists") "0" "1")
            :to-equal (string-trim '(#\Newline)
                                   (oracle ws '() (format nil "test -e ~A; echo $?" path) nil)))))

(define-row-case (5 "realpath and readlink -f are info's real" :foreign ("realpath" "readlink")) (ws)
  (put ws "d/f.txt" "x")
  (shell ws "ln -s d/f.txt link")
  (let ((real (jget (run-ok ws '("info" "link")) "real")))
    (expect real :to-equal (string-trim '(#\Newline) (oracle ws '("realpath") "realpath link" nil)))
    (expect real :to-equal (string-trim '(#\Newline) (oracle ws '("readlink") "readlink -f link" nil)))))
