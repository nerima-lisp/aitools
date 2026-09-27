;;;; t/e2e/search-find-cases.lisp
;;;;
;;;; Correspondence table rows 6-11: searching, finding, code navigation, and comparing.
(in-package #:aitools.e2e.test)

;;; Row 6

(defun put-search-tree (workspace)
  (put workspace "c.txt" (format nil "foo~%"))
  (put workspace "d/a.txt" (format nil "foo bar~%baz foo~%qux~%a.b axb~%"))
  (put workspace "d/sub/b.txt" (format nil "nothing~%")))

(defun matched-lines (envelope)
  "path:line:text for every matched line of a blocks-mode search, sorted."
  (sorted (loop for block in (jlist (jget envelope "blocks"))
                for start = (jget block "start_line")
                append (loop for n in (jlist (jget block "match_lines"))
                             collect (format nil "~A:~D:~A" (jget block "path") n
                                             (jget block "lines" (- n start)))))))

(define-row-case (6 "grep -rn is search" :foreign ("grep")) (ws)
  (put-search-tree ws)
  (expect (matched-lines (run-ok ws '("search" "foo")))
          :to-equal (oracle-lines ws '("grep") "grep -rn foo ." nil)))

(define-row-case (6 "rg -n is search" :foreign ("rg")) (ws)
  (put-search-tree ws)
  (expect (matched-lines (run-ok ws '("search" "foo")))
          :to-equal (oracle-lines ws '("rg") "rg -n --no-heading --sort path foo ."
                                  (format nil "./c.txt:1:foo~%./d/a.txt:1:foo bar~%./d/a.txt:2:baz foo~%"))))

(define-row-case (6 "grep -v is search --invert" :foreign ("grep")) (ws)
  (put-search-tree ws)
  (expect (matched-lines (run-ok ws '("search" "foo" "d/a.txt" "--invert" "--context" "0")))
          :to-equal (sorted (mapcar (lambda (line) (format nil "d/a.txt:~A" line))
                                    (text-lines (oracle ws '("grep") "grep -vn foo d/a.txt" nil))))))

(define-row-case (6 "grep -x is search --line-regexp" :foreign ("grep")) (ws)
  (put-search-tree ws)
  (expect (matched-lines (run-ok ws '("search" "foo" "--line-regexp")))
          :to-equal (oracle-lines ws '("grep") "grep -rnx foo ." nil)))

(define-row-case (6 "grep -e A -e B is search --pattern A --pattern B" :foreign ("grep")) (ws)
  (put-search-tree ws)
  (expect (matched-lines (run-ok ws '("search" "--pattern" "qux" "--pattern" "nothing")))
          :to-equal (oracle-lines ws '("grep") "grep -rn -e qux -e nothing ." nil)))

(define-row-case (6 "egrep is search with an alternation" :foreign ("egrep")) (ws)
  (put-search-tree ws)
  (expect (matched-lines (run-ok ws '("search" "ba[rz]|qux")))
          :to-equal (oracle-lines ws '("egrep") "egrep -rn 'ba[rz]|qux' ." nil)))

(define-row-case (6 "fgrep is search --fixed" :foreign ("fgrep")) (ws)
  (put-search-tree ws)
  (expect (matched-lines (run-ok ws '("search" "a.b" "--fixed")))
          :to-equal (oracle-lines ws '("fgrep") "fgrep -rn a.b ." nil)))

;;; Row 7

(define-row-case (7 "grep -c is search --output count" :foreign ("grep")) (ws)
  (put-search-tree ws)
  (expect (jget (run-ok ws '("search" "foo" "d/a.txt" "--output" "count")) "counts" 0 "count")
          :to-be (parse-integer (oracle ws '("grep") "grep -c foo d/a.txt" nil))))

(define-row-case (7 "grep -rl is search --output files" :foreign ("grep")) (ws)
  (put-search-tree ws)
  (expect (sorted (jlist (jget (run-ok ws '("search" "foo" "--output" "files")) "paths")))
          :to-equal (oracle-lines ws '("grep") "grep -rl foo ." nil)))

(define-row-case (7 "grep -rL is search --output files-without-match" :foreign ("grep")) (ws)
  (put-search-tree ws)
  (expect (sorted (jlist (jget (run-ok ws '("search" "foo" "--output" "files-without-match")) "paths")))
          :to-equal (oracle-lines ws '("grep") "grep -rL foo ." nil)))

(define-row-case (7 "grep -o is search --output matches" :foreign ("grep")) (ws)
  (put-search-tree ws)
  (expect (sorted (mapcar (lambda (match) (format nil "~A:~A" (jget match "path") (jget match "text")))
                          (jlist (jget (run-ok ws '("search" "ba." "--output" "matches")) "matches"))))
          :to-equal (oracle-lines ws '("grep") "grep -ro 'ba.' ." nil)))

(define-row-case (7 "grep -oP with \\K is a search --output matches group" :foreign ("grep")) (ws)
  (put ws "kv.txt" (format nil "x=12 y=3 x=45~%"))
  (expect (mapcar (lambda (match) (jget match "groups" 0))
                  (jlist (jget (run-ok ws '("search" "x=(\\d+)" "kv.txt" "--output" "matches")) "matches")))
          :to-equal (text-lines (oracle ws '("grep") "grep -oP 'x=\\K\\d+' kv.txt" (format nil "12~%45~%")
                                        :probe "echo x=1 | grep -oP 'x=\\K\\d'"))))

;;; Row 8

(defun put-find-tree (workspace)
  (put workspace "c.txt" "cccc")
  (put workspace "d/a.txt" (make-string 20 :initial-element #\a))
  (put workspace "d/sub/b.txt" "bbbbbbbb"))

(defun find-paths (workspace &rest arguments)
  (sorted (mapcar (lambda (item) (jget item "path"))
                  (jlist (jget (run-ok workspace (list* "find" arguments)) "items")))))

(define-row-case (8 "find is find" :foreign ("find")) (ws)
  (put-find-tree ws)
  (expect (find-paths ws) :to-equal (oracle-lines ws '("find") "find . -mindepth 1" nil)))

(define-row-case (8 "fd is find" :foreign ("fd")) (ws)
  (put-find-tree ws)
  (expect (find-paths ws "--type" "file")
          :to-equal (oracle-lines ws '("fd") "fd --type f ."
                                  (format nil "c.txt~%d/a.txt~%d/sub/b.txt~%"))))

(define-row-case (8 "ls -A is find --depth 1" :foreign ("ls")) (ws)
  (put-find-tree ws)
  (expect (find-paths ws "--depth" "1") :to-equal (oracle-lines ws '("ls") "ls -A" nil)))

(define-row-case (8 "ls -R lists what find lists" :foreign ("ls")) (ws)
  (put-find-tree ws)
  (expect (sorted (mapcar (lambda (path) (subseq path (1+ (or (position #\/ path :from-end t) -1))))
                          (find-paths ws)))
          :to-equal (sorted (remove-if (lambda (line) (or (string= line "") (search ":" line)))
                                       (text-lines (oracle ws '("ls") "ls -R" nil))))))

(defun tree-paths (node prefix)
  (loop for child in (jlist (jget node "children"))
        for path = (if prefix (format nil "~A/~A" prefix (jget child "name")) (jget child "name"))
        collect path
        append (tree-paths child path)))

(define-row-case (8 "tree's hierarchy is find --output tree" :foreign ("tree")) (ws)
  (put-find-tree ws)
  (expect (sorted (tree-paths (jget (run-ok ws '("find" "--output" "tree")) "tree") nil))
          :to-equal (sorted (mapcar #'strip-dot-slash
                                    (text-lines (oracle ws '("tree") "tree -afi --noreport . | tail -n +2"
                                                        (format nil "./c.txt~%./d~%./d/a.txt~%./d/sub~%./d/sub/b.txt~%")))))))

(define-row-case (8 "du's apparent byte total is find --sizes" :foreign ("du")) (ws)
  (put-find-tree ws)
  (let ((directory (find "d" (jlist (jget (run-ok ws '("find" "--sizes")) "items"))
                         :key (lambda (item) (jget item "path")) :test #'string=)))
    (expect (jget directory "size")
            :to-be (parse-integer (oracle ws '("find" "cat" "wc") "find d -type f -exec cat {} + | wc -c" nil)
                                  :junk-allowed t))))

;;; Row 9

(defun put-attribute-tree (workspace)
  (put workspace "empty.txt" "")
  (ensure-directories-exist (ws-path workspace "emptydir/"))
  (put workspace "run.sh" (format nil "#!/bin/sh~%") :mode #o755)
  (put workspace "data.txt" "data")
  (shell workspace "ln -s data.txt link")
  (sb-posix:utimes (native (ws-path workspace "data.txt")) 1577836800 1577836800)
  (sb-posix:utimes (native (ws-path workspace "empty.txt")) 1577836800 1577836800)
  (sb-posix:utimes (native (ws-path workspace "run.sh")) 1577836800 1577836800)
  (put workspace "ref" "r")
  (sb-posix:utimes (native (ws-path workspace "ref")) 1600000000 1600000000)
  (put workspace "fresh.txt" "new"))

(define-row-case (9 "find -empty is find --empty" :foreign ("find")) (ws)
  (put-attribute-tree ws)
  (expect (find-paths ws "--empty") :to-equal (oracle-lines ws '("find") "find . -mindepth 1 -empty" nil)))

(define-row-case (9 "find -perm is find --executable" :foreign ("find")) (ws)
  (put-attribute-tree ws)
  (expect (find-paths ws "--executable")
          :to-equal (oracle-lines ws '("find") "find . -type f -perm -u+x" nil)))

(define-row-case (9 "find -type l is find --type symlink" :foreign ("find")) (ws)
  (put-attribute-tree ws)
  (expect (find-paths ws "--type" "symlink") :to-equal (oracle-lines ws '("find") "find . -type l" nil)))

(define-row-case (9 "find -newer is find --newer" :foreign ("find")) (ws)
  (put-attribute-tree ws)
  (expect (find-paths ws "--newer" "ref" "--type" "file")
          :to-equal (oracle-lines ws '("find") "find . -type f -newer ref" nil)))

;;; Row 10

(defparameter +lisp-fixture+
  (format nil "(defun foo (x) x)~%(defmacro bar () nil)~%(defun foobar () (foo 1))~%(foobar)~%"))

(define-row-case (10 "a definition grep is code outline" :foreign ("ctags" "grep")) (ws)
  (put ws "s.lisp" +lisp-fixture+)
  (expect (mapcar (lambda (symbol) (format nil "~D:~A" (jget symbol "line") (jget symbol "name")))
                  (jlist (jget (run-ok ws '("code" "outline" "s.lisp")) "symbols")))
          :to-equal (text-lines (oracle ws '("grep" "sed")
                                        "grep -nE '^\\((defun|defmacro) ' s.lisp | sed -E 's/^([0-9]+):\\((defun|defmacro) ([^ ]+).*/\\1:\\3/'"
                                        (format nil "1:foo~%2:bar~%3:foobar~%")))))

(define-row-case (10 "a definition grep for one name is code defs" :foreign ("grep")) (ws)
  (put ws "s.lisp" +lisp-fixture+)
  (expect (mapcar (lambda (def) (format nil "~A:~D" (jget def "path") (jget def "line")))
                  (jlist (jget (run-ok ws '("code" "defs" "foo")) "defs")))
          :to-equal (text-lines (oracle ws '("grep" "cut") "grep -rn '^(defun foo ' . | cut -d: -f1,2 | sed 's|^\\./||'" nil))))

(define-row-case (10 "grep -w for a name is code refs" :foreign ("grep")) (ws)
  (put ws "s.lisp" +lisp-fixture+)
  (expect (mapcar (lambda (ref) (format nil "~D:~A" (jget ref "line") (jget ref "text")))
                  (jlist (jget (run-ok ws '("code" "refs" "foo")) "refs")))
          :to-equal (text-lines (oracle ws '("grep") "grep -nw foo s.lisp" nil))))

;;; Row 11

(defun put-diff-pair (workspace)
  (put workspace "x.txt" (format nil "a~%b~%c~%e~%"))
  (put workspace "y.txt" (format nil "a~%B~%c~%d~%e~%")))

(define-row-case (11 "diff -u is diff" :foreign ("diff")) (ws)
  (put-diff-pair ws)
  (let ((diff (jget (run-ok ws '("diff" "x.txt" "y.txt")) "diff")))
    (expect (subseq diff (1+ (position #\Newline diff :start (1+ (position #\Newline diff)))))
            :to-equal (oracle ws '("diff" "tail") "diff -u x.txt y.txt | tail -n +3" nil
                              :expect-status 0))))

(define-row-case (11 "diff -w's status is diff --ignore-whitespace's identical" :foreign ("diff")) (ws)
  (put ws "w1.txt" (format nil "a  b~%c~%"))
  (put ws "w2.txt" (format nil "a b~%c~%"))
  (put ws "w3.txt" (format nil "a b~%d~%"))
  (dolist (other '("w2.txt" "w3.txt"))
    (expect (if (jget (run-ok ws (list "diff" "w1.txt" other "--ignore-whitespace")) "identical") "0" "1")
            :to-equal (string-trim '(#\Newline)
                                   (oracle ws '("diff") (format nil "diff -w w1.txt ~A >/dev/null; echo $?" other) nil)))))

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
