;;;; t/e2e/edit-cases.lisp
;;;;
;;;; Correspondence table rows 12-21: text edits, inserts, writes, and patches. An in-place
;;;; edit runs aitools on a.txt and the shell command on o.txt, both written
;;;; from the same fixture; the shell script ends by printing o.txt, and that
;;;; output is the expected content of a.txt. Rows 22-30 are in
;;;; transform-file-cases.lisp, which uses the helpers defined here.
(in-package #:aitools.e2e.test)

(defun sed-in-place (expression file)
  "A `sed -i` that behaves the same under GNU and BSD sed: both accept an
attached backup suffix, which the script then removes."
  (format nil "sed -i.bak ~A ~A && rm ~A.bak" expression file file))

(defun info-hash (workspace path)
  (jget (run-ok workspace (list "info" path)) "hash"))

(defun expect-same-edit (workspace content tools script fixed arguments &key stdin probe)
  "Write CONTENT to a.txt and o.txt, take SCRIPT's output (which must print
o.txt after editing it) as the expected text, run aitools with ARGUMENTS
(each :HASH replaced by a.txt's current hash), and compare a.txt."
  (put workspace "a.txt" content)
  (put workspace "o.txt" content)
  (let ((expected (if probe
                      (oracle workspace tools script fixed :probe probe)
                      (oracle workspace tools script fixed))))
    (run-ok workspace (substitute (info-hash workspace "a.txt") :hash arguments) :stdin stdin)
    (expect (file-text workspace "a.txt") :to-equal expected)))

;;; Row 12

(define-row-case (12 "sed -i 's/a/b/' on one occurrence is edit --old a --new b" :foreign ("sed")) (ws)
  (expect-same-edit ws (format nil "one a two~%three~%") '("sed")
                    (format nil "~A && cat o.txt" (sed-in-place "'s/a/b/'" "o.txt"))
                    (format nil "one b two~%three~%")
                    '("edit" "a.txt" "--old" "a" "--new" "b")))

;;; Row 13

(define-row-case (13 "sed -i 's/x/y/g' is replace --expect-count N" :foreign ("sed")) (ws)
  (expect-same-edit ws (format nil "x x x~%x~%") '("sed")
                    (format nil "~A && cat o.txt" (sed-in-place "'s/x/y/g'" "o.txt"))
                    (format nil "y y y~%y~%")
                    '("replace" "--expect-count" "4" "x" "y" "a.txt")))

(define-row-case (13 "perl -pi -e 's/x+/y/g' is replace --expect-count N" :foreign ("perl")) (ws)
  (expect-same-edit ws (format nil "xx x axxxb~%x~%") '("perl")
                    "perl -pi -e 's/x+/y/g' o.txt && cat o.txt"
                    (format nil "y y ayb~%y~%")
                    '("replace" "--expect-count" "4" "x+" "y" "a.txt")))

(define-row-case (13 "replace x y a.txt --expect-count 4 takes the guard after the paths, as the table writes it" :foreign ("sed")) (ws)
  (expect-same-edit ws (format nil "x x x~%x~%") '("sed")
                    (format nil "~A && cat o.txt" (sed-in-place "'s/x/y/g'" "o.txt"))
                    (format nil "y y y~%y~%")
                    '("replace" "x" "y" "a.txt" "--expect-count" "4")))

(define-row-case (13 "sed 's/x/y/2' on one line is replace --nth 2" :foreign ("sed")) (ws)
  (expect-same-edit ws (format nil "x x x~%") '("sed")
                    (format nil "~A && cat o.txt" (sed-in-place "'s/x/y/2'" "o.txt"))
                    (format nil "x y x~%")
                    '("replace" "--nth" "2" "--expect-count" "1" "x" "y" "a.txt")))

;;; Row 14

(define-row-case (14 "perl -0pi -e 's/foo\\nbar/X/s' is replace --multiline" :foreign ("perl")) (ws)
  (expect-same-edit ws (format nil "foo~%bar~%baz~%") '("perl")
                    "perl -0pi -e 's/foo\\nbar/X/s' o.txt && cat o.txt"
                    (format nil "X~%baz~%")
                    '("replace" "--multiline" "--expect-count" "1" "foo\\nbar" "X" "a.txt")))

(define-row-case (14 "paste -sd, is replace --multiline joining lines" :foreign ("paste")) (ws)
  (expect-same-edit ws (format nil "a~%b~%c~%") '("paste")
                    "paste -sd, o.txt > joined && mv joined o.txt && cat o.txt"
                    (format nil "a,b,c~%")
                    '("replace" "--multiline" "--expect-count" "2" "\\n(.)" ",${1}" "a.txt")))

;;; Row 15

(define-row-case (15 "perl -pi -e 's/(\\w+)/\\U$1/' is replace with ${1:upper}" :foreign ("perl")) (ws)
  (expect-same-edit ws (format nil "alpha~%beta~%") '("perl")
                    "perl -pi -e 's/(\\w+)/\\U$1/' o.txt && cat o.txt"
                    (format nil "ALPHA~%BETA~%")
                    '("replace" "--expect-count" "2" "(\\w+)" "${1:upper}" "a.txt")))

(define-row-case (15 "incrementing numbers with perl s///e is replace with ${1:inc}" :foreign ("perl")) (ws)
  (expect-same-edit ws (format nil "v1 v9 v09~%") '("perl")
                    "perl -pi -e 's/v(\\d+)/sprintf(\"v%0*d\", length($1), $1+1)/ge' o.txt && cat o.txt"
                    (format nil "v2 v10 v10~%")
                    '("replace" "--expect-count" "3" "v(\\d+)" "v${1:inc}" "a.txt")))

;;; Row 16

(define-row-case (16 "sed -i 'S,Es/x/y/' is replace --range S:E" :foreign ("sed")) (ws)
  (expect-same-edit ws (format nil "x~%x~%x~%x~%") '("sed")
                    (format nil "~A && cat o.txt" (sed-in-place "'2,3s/x/y/'" "o.txt"))
                    (format nil "x~%y~%y~%x~%")
                    '("replace" "--range" "2:3" "--expect-hash" :hash "--expect-count" "2" "x" "y" "a.txt")))

;;; Row 17

(define-row-case (17 "sed -i 'Nd' is edit --range N --new ''" :foreign ("sed")) (ws)
  (expect-same-edit ws (format nil "a~%b~%c~%") '("sed")
                    (format nil "~A && cat o.txt" (sed-in-place "'2d'" "o.txt"))
                    (format nil "a~%c~%")
                    '("edit" "a.txt" "--range" "2" "--new" "" "--expect-hash" :hash)))

(define-row-case (17 "sed -i 'Nc text' is edit --range N --new text" :foreign ("sed")) (ws)
  (expect-same-edit ws (format nil "a~%b~%c~%") '("sed")
                    (format nil "~A && cat o.txt" (sed-in-place (format nil "'2c\\~%text'") "o.txt"))
                    (format nil "a~%text~%c~%")
                    '("edit" "a.txt" "--range" "2" "--new" "text" "--expect-hash" :hash)))

;;; Row 18

(define-row-case (18 "sed -i '/re/d' is edit --match re --new '' --expect-count N" :foreign ("sed")) (ws)
  (expect-same-edit ws (format nil "keep~%drop me~%keep too~%drop~%") '("sed")
                    (format nil "~A && cat o.txt" (sed-in-place "'/drop/d'" "o.txt"))
                    (format nil "keep~%keep too~%")
                    '("edit" "a.txt" "--match" "drop" "--new" "" "--expect-count" "2")))

(define-row-case (18 "grep -v re > tmp && mv is edit --match re --new ''" :foreign ("grep" "mv")) (ws)
  (expect-same-edit ws (format nil "keep~%drop me~%keep too~%drop~%") '("grep" "mv")
                    "grep -v drop o.txt > tmp && mv tmp o.txt && cat o.txt"
                    (format nil "keep~%keep too~%")
                    '("edit" "a.txt" "--match" "drop" "--new" "" "--expect-count" "2")))

;;; Row 19

(defparameter +insert-fixture+ (format nil "x~%re1~%y~%re2~%"))

(define-row-case (19 "sed -i '/re/a text' is insert --after --match re" :foreign ("sed")) (ws)
  (expect-same-edit ws +insert-fixture+ '("sed")
                    (format nil "~A && cat o.txt" (sed-in-place (format nil "'/re/a\\~%T'") "o.txt"))
                    (format nil "x~%re1~%T~%y~%re2~%T~%")
                    '("insert" "a.txt" "--after" "--match" "re" "--content" "T" "--expect-count" "2")))

(define-row-case (19 "sed -i '/re/i text' is insert --before --match re" :foreign ("sed")) (ws)
  (expect-same-edit ws +insert-fixture+ '("sed")
                    (format nil "~A && cat o.txt" (sed-in-place (format nil "'/re/i\\~%T'") "o.txt"))
                    (format nil "x~%T~%re1~%y~%T~%re2~%")
                    '("insert" "a.txt" "--before" "--match" "re" "--content" "T" "--expect-count" "2")))

(define-row-case (19 "sed -i '1i text' is insert --at start" :foreign ("sed")) (ws)
  (expect-same-edit ws +insert-fixture+ '("sed")
                    (format nil "~A && cat o.txt" (sed-in-place (format nil "'1i\\~%FIRST'") "o.txt"))
                    (format nil "FIRST~%x~%re1~%y~%re2~%")
                    '("insert" "a.txt" "--at" "start" "--content" "FIRST")))

;;; Row 20

(defparameter +literal-content+ (format nil "hello~%$1 \"q\" \\n `x`~%")
  "Newlines, quotes, a backslash, and $1, all of which must arrive
unchanged through --stdin.")

(define-row-case (20 "cat > file <<'EOF' is write --stdin" :foreign ("cat")) (ws)
  (let ((expected (oracle ws '("cat") (format nil "cat > o.txt <<'EOF'~%~AEOF~%cat o.txt" +literal-content+)
                          +literal-content+)))
    (run-ok ws '("write" "a.txt" "--stdin") :stdin +literal-content+)
    (expect (file-text ws "a.txt") :to-equal expected)))

(define-row-case (20 "tee is write --stdin" :foreign ("tee")) (ws)
  (let ((expected (oracle ws '("tee") "tee o.txt > /dev/null && cat o.txt" +literal-content+
                          :stdin +literal-content+)))
    (run-ok ws '("write" "a.txt" "--stdin") :stdin +literal-content+)
    (expect (file-text ws "a.txt") :to-equal expected)))

(define-row-case (20 "echo >> file is insert --at end" :foreign ("echo")) (ws)
  (expect-same-edit ws (format nil "a~%b~%") '()
                    "echo tail >> o.txt && cat o.txt"
                    (format nil "a~%b~%tail~%")
                    '("insert" "a.txt" "--at" "end" "--content" "tail")))

(define-row-case (20 "cat a b > c is write --content-file a --content-file b" :foreign ("cat")) (ws)
  (put ws "p1" (format nil "first~%"))
  (put ws "p2" (concatenate '(vector (unsigned-byte 8)) (octets "second") #(0 255 10)))
  (let ((expected (oracle ws '("cat") "cat p1 p2 > o.bin && cat o.bin" nil :octets t)))
    (run-ok ws '("write" "c.bin" "--content-file" "p1" "--content-file" "p2"))
    (expect (file-octets ws "c.bin") :to-equalp expected)))

;;; Row 21

(defparameter +patch-before+ (format nil "one~%two~%three~%"))
(defparameter +patch-after+ (format nil "one~%TWO~%three~%"))

(defun unified-patch (old new)
  (format nil "--- ~A~%+++ ~A~%@@ -1,3 +1,3 @@~% one~%-two~%+TWO~% three~%" old new))

(defun expect-same-patch (workspace tool script patch arguments start expected-fixed)
  "Apply PATCH to a.txt holding START with SCRIPT (run in o/, which holds the
same a.txt) and with aitools ARGUMENTS in the workspace root."
  (put workspace "a.txt" start)
  (put workspace "o/a.txt" start)
  (put workspace "p.diff" patch)
  (let ((expected (oracle workspace (list tool) (format nil "cd o && ~A && cat a.txt" script) expected-fixed)))
    (run-ok workspace arguments :stdin patch)
    (expect (file-text workspace "a.txt") :to-equal expected)))

(define-row-case (21 "patch is apply" :foreign ("patch")) (ws)
  (expect-same-patch ws "patch" "patch -s -p0 < ../p.diff" (unified-patch "a.txt" "a.txt")
                     '("apply" "--stdin") +patch-before+ +patch-after+))

(define-row-case (21 "git apply is apply --strip 1" :foreign ("git")) (ws)
  (expect-same-patch ws "git" "git apply ../p.diff" (unified-patch "a/a.txt" "b/a.txt")
                     '("apply" "--strip" "1" "--stdin") +patch-before+ +patch-after+))

(define-row-case (21 "patch -R is apply --reverse" :foreign ("patch")) (ws)
  (expect-same-patch ws "patch" "patch -s -R -p0 < ../p.diff" (unified-patch "a.txt" "a.txt")
                     '("apply" "--reverse" "--stdin") +patch-after+ +patch-before+))

(define-row-case (21 "patch -p1 is apply --strip 1" :foreign ("patch")) (ws)
  (expect-same-patch ws "patch" "patch -s -p1 < ../p.diff" (unified-patch "a/a.txt" "b/a.txt")
                     '("apply" "--strip" "1" "--stdin") +patch-before+ +patch-after+))
