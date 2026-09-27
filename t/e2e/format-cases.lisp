;;;; t/e2e/format-cases.lisp
;;;;
;;;; Correspondence table rows 31-37: JSON, tabular text, and archives. JSON results are
;;;; compared structurally with jq's output parsed by json-kit; archives are
;;;; built and checked with the real tar, zip, unzip, and gzip.
(in-package #:aitools.e2e.test)

(defun parse-json-text (text)
  (json-kit:parse text :false-value nil :null-value nil))

(defun jq (workspace filter file fixed &rest flags)
  "jq -c FILTER FILE's single output value, parsed."
  (parse-json-text (oracle workspace '("jq") (format nil "jq -c~{ ~A~} '~A' ~A" flags filter file) fixed)))

(defun jq-stream (workspace filter file fixed)
  "Every value jq -c FILTER FILE prints, parsed, as a list."
  (mapcar #'parse-json-text
          (text-lines (oracle workspace '("jq") (format nil "jq -c '~A' ~A" filter file) fixed))))

(defparameter +json-fixture+
  (format nil "{\"b\":{\"c\":\"hi\\n\",\"d\":[1,2]},\"a\":[1,2,3],\"name\":\"x\"}~%"))

(defparameter +json-array-fixture+
  (format nil "[{\"x\":2,\"n\":\"q\"},{\"x\":1,\"n\":\"p\"},{\"x\":3,\"n\":\"s\"},{\"x\":1,\"n\":\"r\"}]~%"))

;;; Row 31

(define-row-case (31 "jq '.b.d' is json get" :foreign ("jq")) (ws)
  (put ws "f.json" +json-fixture+)
  (expect (json= (jget (run-ok ws '("json" "get" "f.json" "/b/d")) "value")
                 (jq ws ".b.d" "f.json" (format nil "[1,2]~%")))
          :to-be t))

(define-row-case (31 "jq 'keys_unsorted' is json get --keys, and jq 'keys' is the same set" :foreign ("jq")) (ws)
  (put ws "f.json" +json-fixture+)
  (let ((keys (jlist (jget (run-ok ws '("json" "get" "f.json" "" "--keys")) "keys"))))
    (expect (json= (coerce keys 'vector) (jq ws "keys_unsorted" "f.json" (format nil "[\"b\",\"a\",\"name\"]~%")))
            :to-be t)
    (expect (json= (coerce (sorted keys) 'vector) (jq ws "keys" "f.json" (format nil "[\"a\",\"b\",\"name\"]~%")))
            :to-be t)))

(define-row-case (31 "jq 'length' is json get's length" :foreign ("jq")) (ws)
  (put ws "f.json" +json-fixture+)
  (dolist (pair '(("" . "") ("/a" . ".a") ("/name" . ".name")))
    (expect (jget (run-ok ws (list "json" "get" "f.json" (car pair))) "length")
            :to-be (jq ws (format nil "~A | length" (if (string= (cdr pair) "") "." (cdr pair))) "f.json" nil))))

(define-row-case (31 "jq -r is json get --raw" :foreign ("jq")) (ws)
  (put ws "f.json" +json-fixture+)
  (expect (format nil "~A~%" (jget (run-ok ws '("json" "get" "f.json" "/b/c" "--raw")) "text"))
          :to-equal (oracle ws '("jq") "jq -r '.b.c' f.json" (format nil "hi~%~%"))))

;;; Row 32

(define-row-case (32 "jq '.[] | select(.x == 1)' is json select --where" :foreign ("jq")) (ws)
  (put ws "f.json" +json-array-fixture+)
  (let ((items (mapcar (lambda (item) (jget item "value"))
                       (jlist (jget (run-ok ws '("json" "select" "f.json" "" "--where" "/x=1")) "items")))))
    (expect (json= (coerce items 'vector)
                   (coerce (jq-stream ws ".[] | select(.x == 1)" "f.json" nil) 'vector))
            :to-be t)))

(define-row-case (32 "jq 'sort_by(.x)' is json select --sort-by" :foreign ("jq")) (ws)
  (put ws "f.json" +json-array-fixture+)
  (let ((items (mapcar (lambda (item) (jget item "value"))
                       (jlist (jget (run-ok ws '("json" "select" "f.json" "" "--sort-by" "/x")) "items")))))
    (expect (json= (coerce items 'vector) (jq ws "sort_by(.x)" "f.json" nil)) :to-be t)))

(define-row-case (32 "jq 'map(select(...)) | length' is json select --output count" :foreign ("jq")) (ws)
  (put ws "f.json" +json-array-fixture+)
  (expect (jget (run-ok ws '("json" "select" "f.json" "" "--where" "/x=1" "--output" "count")) "count")
          :to-be (jq ws "map(select(.x == 1)) | length" "f.json" (format nil "2~%"))))

;;; Row 33

(defun expect-same-json-edit (workspace filter arguments &key stdin)
  "Apply jq FILTER to o.json and aitools ARGUMENTS to a.json (both
+JSON-FIXTURE+) and compare the documents structurally."
  (put workspace "a.json" +json-fixture+)
  (put workspace "o.json" +json-fixture+)
  (let ((expected (jq workspace filter "o.json" nil)))
    (run-ok workspace arguments :stdin stdin)
    (expect (json= (parse-json-text (file-text workspace "a.json")) expected) :to-be t)))

(define-row-case (33 "jq '.a = 1' is json set" :foreign ("jq")) (ws)
  (expect-same-json-edit ws ".a = 1" '("json" "set" "a.json" "/a" "1")))

(define-row-case (33 "jq '.a += [4]' is json set /a/-" :foreign ("jq")) (ws)
  (expect-same-json-edit ws ".a += [4]" '("json" "set" "a.json" "/a/-" "4")))

(define-row-case (33 "jq 'del(.b)' is json delete" :foreign ("jq")) (ws)
  (expect-same-json-edit ws "del(.b)" '("json" "delete" "a.json" "/b")))

(define-row-case (33 "jq '. * {...}' is json merge" :foreign ("jq")) (ws)
  (expect-same-json-edit ws ". * {\"b\":{\"e\":true},\"z\":\"new\"}" '("json" "merge" "a.json" "--stdin")
                         :stdin "{\"b\":{\"e\":true},\"z\":\"new\"}"))

(define-row-case (33 "jq . is json fmt, byte for byte" :foreign ("jq")) (ws)
  (let ((compact (format nil "{\"b\":1,\"a\":{\"c\":[1,2],\"s\":\"x\"}}~%")))
    (put ws "a.json" compact)
    (put ws "o.json" compact)
    (let ((expected (oracle ws '("jq") "jq . o.json"
                            (format nil "{~%  \"b\": 1,~%  \"a\": {~%    \"c\": [~%      1,~%      2~%    ],~%    \"s\": \"x\"~%  }~%}~%"))))
      (run-ok ws '("json" "fmt" "a.json"))
      (expect (file-text ws "a.json") :to-equal expected))))

;;; Row 34

(defun table-column (envelope)
  (format nil "~{~A~%~}" (mapcar (lambda (row) (aref row 0)) (jlist (jget envelope "rows")))))

(defparameter +passwd-fixture+ (format nil "root:x:0:0~%bin:x:1:1~%daemon:x:2:2~%"))

(define-row-case (34 "cut -d: -f1 is table read --format sep --delimiter ':'" :foreign ("cut")) (ws)
  (put ws "p.txt" +passwd-fixture+)
  (expect (table-column (run-ok ws '("table" "read" "p.txt" "--format" "sep" "--delimiter" ":" "--no-header" "--columns" "1")))
          :to-equal (oracle ws '("cut") "cut -d: -f1 p.txt" (format nil "root~%bin~%daemon~%"))))

(define-row-case (34 "awk -F: '{print $1}' is table read --format sep --delimiter ':'" :foreign ("awk")) (ws)
  (put ws "p.txt" +passwd-fixture+)
  (expect (table-column (run-ok ws '("table" "read" "p.txt" "--format" "sep" "--delimiter" ":" "--no-header" "--columns" "1")))
          :to-equal (oracle ws '("awk") "awk -F: '{print $1}' p.txt" (format nil "root~%bin~%daemon~%"))))

(define-row-case (34 "awk '{print $3}' is table read --format ws --columns 3" :foreign ("awk")) (ws)
  (put ws "w.txt" (format nil "a b c~%d  e   f~%  g h i j~%"))
  (expect (table-column (run-ok ws '("table" "read" "w.txt" "--format" "ws" "--no-header" "--columns" "3")))
          :to-equal (oracle ws '("awk") "awk '{print $3}' w.txt" (format nil "c~%f~%i~%"))))

;;; Row 35

(define-row-case (35 "awk '{s+=$2} END {print s}' is table agg --sum" :foreign ("awk")) (ws)
  (put ws "s.txt" (format nil "x 3~%y 4~%z 5.5~%"))
  (expect (jget (run-ok ws '("table" "agg" "s.txt" "--format" "ws" "--no-header" "--sum" "2")) "groups" 0 "sum")
          :to-equal (parse-json-text (oracle ws '("awk") "awk '{s+=$2} END {print s}' s.txt" (format nil "12.5~%")))))

(define-row-case (35 "sort | uniq -c | sort -rn is table agg --group-by line --count --sort count --desc" :foreign ("sort" "uniq")) (ws)
  (put ws "l.txt" (format nil "b~%a~%b~%c~%b~%a~%"))
  (expect (mapcar (lambda (group) (format nil "~D ~A" (jget group "count") (jget group "key" "line")))
                  (jlist (jget (run-ok ws '("table" "agg" "l.txt" "--format" "lines" "--group-by" "line"
                                            "--count" "--sort" "count" "--desc"))
                               "groups")))
          ;; No fixed value: GNU uniq -c pads the count to width 7 and BSD to
          ;; width 4, so a byte-exact constant cannot match both. The
          ;; string-left-trim below already normalizes that padding away, and a
          ;; sandbox without sort/uniq takes a counted skip.
          :to-equal (mapcar (lambda (line) (string-left-trim " " line))
                            (text-lines (oracle ws '("sort" "uniq") "sort l.txt | uniq -c | sort -rn" nil)))))

(define-row-case (35 "sort | uniq -d is table agg --min-count 2" :foreign ("uniq")) (ws)
  (put ws "l.txt" (format nil "b~%a~%b~%c~%b~%a~%"))
  (expect (sorted (mapcar (lambda (group) (jget group "key" "line"))
                          (jlist (jget (run-ok ws '("table" "agg" "l.txt" "--format" "lines" "--group-by" "line"
                                                    "--count" "--min-count" "2"))
                                       "groups"))))
          :to-equal (text-lines (oracle ws '("sort" "uniq") "sort l.txt | uniq -d" (format nil "a~%b~%")))))

;;; Row 36

(defun put-archive-sources (workspace)
  (put workspace "src/a.txt" (format nil "hello~%"))
  (put workspace "src/sub/b.txt" (format nil "world~%second~%")))

(defun make-archives (workspace)
  "Build t.tar, t.tgz, z.zip, and a.gz from src/ with the real tools."
  (put-archive-sources workspace)
  (shell workspace "tar -cf t.tar src && tar -czf t.tgz src && zip -qr z.zip src && gzip -c src/a.txt > a.gz"))

(defun archive-paths (workspace archive)
  (sorted (mapcar (lambda (item) (jget item "path"))
                  (jlist (jget (run-ok workspace (list "archive" "list" archive)) "items")))))

(defun without-trailing-slash (lines)
  (sorted (mapcar (lambda (line) (string-right-trim "/" line)) lines)))

(define-row-case (36 "tar -tf is archive list" :foreign ("tar")) (ws)
  (make-archives ws)
  (expect (archive-paths ws "t.tar")
          :to-equal (without-trailing-slash (text-lines (oracle ws '("tar") "tar -tf t.tar" nil))))
  (expect (archive-paths ws "t.tgz")
          :to-equal (without-trailing-slash (text-lines (oracle ws '("tar") "tar -tzf t.tgz" nil)))))

(define-row-case (36 "unzip -l's names are archive list" :foreign ("unzip")) (ws)
  (make-archives ws)
  (expect (archive-paths ws "z.zip")
          :to-equal (without-trailing-slash (text-lines (oracle ws '("unzip") "unzip -Z1 z.zip" nil)))))

(define-row-case (36 "zcat is archive read on a .gz" :foreign ("zcat")) (ws)
  (make-archives ws)
  (let ((entry (first (archive-paths ws "a.gz"))))
    (expect (lines-text (jget (run-ok ws (list "archive" "read" "a.gz" entry)) "lines"))
            :to-equal (oracle ws '("zcat") "zcat < a.gz" (format nil "hello~%")))))

(define-row-case (36 "unzip -p is archive read" :foreign ("unzip")) (ws)
  (make-archives ws)
  (expect (lines-text (jget (run-ok ws '("archive" "read" "z.zip" "src/sub/b.txt")) "lines"))
          :to-equal (oracle ws '("unzip") "unzip -p z.zip src/sub/b.txt" (format nil "world~%second~%"))))

(define-row-case (36 "tar -xOf is archive read" :foreign ("tar")) (ws)
  (make-archives ws)
  (expect (lines-text (jget (run-ok ws '("archive" "read" "t.tgz" "src/sub/b.txt")) "lines"))
          :to-equal (oracle ws '("tar") "tar -xzOf t.tgz src/sub/b.txt" (format nil "world~%second~%"))))

;;; Row 37

(define-row-case (37 "tar -xzf is archive extract --to" :foreign ("tar")) (ws)
  (make-archives ws)
  (oracle ws '("tar") "mkdir o && tar -xzf t.tgz -C o" nil)
  (run-ok ws '("archive" "extract" "t.tgz" "--to" "a"))
  (expect (mapcar #'butlast (tree-snapshot ws "a/")) :to-equalp (mapcar #'butlast (tree-snapshot ws "o/")))
  (expect (mapcar #'fourth (tree-snapshot ws "a/")) :to-equalp (mapcar #'fourth (tree-snapshot ws "o/"))))

(define-row-case (37 "unzip is archive extract --to" :foreign ("unzip")) (ws)
  (make-archives ws)
  (oracle ws '("unzip") "unzip -q z.zip -d o" nil)
  (run-ok ws '("archive" "extract" "z.zip" "--to" "a"))
  (expect (mapcar (lambda (entry) (list (first entry) (second entry) (fourth entry))) (tree-snapshot ws "a/"))
          :to-equalp (mapcar (lambda (entry) (list (first entry) (second entry) (fourth entry))) (tree-snapshot ws "o/"))))

(define-row-case (37 "archive create's .tar.gz is what tar -czf makes, read back by tar -xzf" :foreign ("tar")) (ws)
  (put-archive-sources ws)
  (run-ok ws '("archive" "create" "new.tar.gz" "src"))
  (oracle ws '("tar") "mkdir o && tar -czf ref.tgz src && tar -xzf ref.tgz -C o && mkdir a && tar -xzf new.tar.gz -C a" nil)
  (expect (mapcar #'fourth (tree-snapshot ws "a/")) :to-equalp (mapcar #'fourth (tree-snapshot ws "o/")))
  (expect (mapcar #'first (tree-snapshot ws "a/")) :to-equal (mapcar #'first (tree-snapshot ws "o/"))))

(define-row-case (37 "archive create --format zip is what zip -r makes, read back by unzip" :foreign ("zip" "unzip")) (ws)
  (put-archive-sources ws)
  (run-ok ws '("archive" "create" "--format" "zip" "new.zip" "src"))
  (oracle ws '("zip" "unzip") "zip -qr ref.zip src && unzip -q ref.zip -d o && unzip -q new.zip -d a" nil)
  (expect (mapcar (lambda (entry) (list (first entry) (fourth entry))) (tree-snapshot ws "a/"))
          :to-equalp (mapcar (lambda (entry) (list (first entry) (fourth entry))) (tree-snapshot ws "o/"))))

(define-row-case (37 "archive create's .gz is what gzip makes, read back by gzip -dc" :foreign ("gzip")) (ws)
  (put-archive-sources ws)
  (run-ok ws '("archive" "create" "a.gz" "src/a.txt"))
  (expect (utf8 (oracle ws '("gzip") "gzip -dc a.gz" nil :octets t))
          :to-equal (utf8 (oracle ws '("gzip") "gzip -c src/a.txt | gzip -dc" nil :octets t))))
