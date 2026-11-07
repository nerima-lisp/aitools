;;;;
;;;; Correspondence table rows 31-32 and 34-36: JSON queries, tables, and archive reads.
;;;; JSON results are compared structurally with jq's output parsed by json-kit;
;;;; archives are built and checked with the real tar, zip, unzip, and gzip.
(in-package #:aitools.e2e.test)

(defun jq-stream (workspace filter file fixed)
  "Every value jq -c FILTER FILE prints, parsed, as a list."
  (mapcar #'parse-json-text
          (text-lines (oracle workspace '("jq") (format nil "jq -c '~A' ~A" filter file) fixed))))

(defparameter +json-array-fixture+
  (format nil "[{\"x\":2,\"n\":\"q\"},{\"x\":1,\"n\":\"p\"},{\"x\":3,\"n\":\"s\"},{\"x\":1,\"n\":\"r\"}]~%"))

;;; Row 31

(define-row-case (31 "jq '.b.d' is doc get" :foreign ("jq")) (ws)
  (put ws "f.json" +json-fixture+)
  (expect (json= (jget (run-ok ws '("doc" "get" "f.json" "/b/d")) "value")
                 (jq ws ".b.d" "f.json" (format nil "[1,2]~%")))
          :to-be t))

(define-row-case (31 "jq 'keys_unsorted' is doc get --keys, and jq 'keys' is the same set" :foreign ("jq")) (ws)
  (put ws "f.json" +json-fixture+)
  (let ((keys (jlist (jget (run-ok ws '("doc" "get" "f.json" "" "--keys")) "keys"))))
    (expect (json= (coerce keys 'vector) (jq ws "keys_unsorted" "f.json" (format nil "[\"b\",\"a\",\"name\"]~%")))
            :to-be t)
    (expect (json= (coerce (sorted keys) 'vector) (jq ws "keys" "f.json" (format nil "[\"a\",\"b\",\"name\"]~%")))
            :to-be t)))

(define-row-case (31 "jq 'length' is doc get's length" :foreign ("jq")) (ws)
  (put ws "f.json" +json-fixture+)
  (dolist (pair '(("" . "") ("/a" . ".a") ("/name" . ".name")))
    (expect (jget (run-ok ws (list "doc" "get" "f.json" (car pair))) "length")
            :to-be (jq ws (format nil "~A | length" (if (string= (cdr pair) "") "." (cdr pair))) "f.json" nil))))

(define-row-case (31 "jq -r is doc get --raw" :foreign ("jq")) (ws)
  (put ws "f.json" +json-fixture+)
  (expect (format nil "~A~%" (jget (run-ok ws '("doc" "get" "f.json" "/b/c" "--raw")) "text"))
          :to-equal (oracle ws '("jq") "jq -r '.b.c' f.json" (format nil "hi~%~%"))))

;;; Row 32

(define-row-case (32 "jq '.[] | select(.x == 1)' is doc select --where" :foreign ("jq")) (ws)
  (put ws "f.json" +json-array-fixture+)
  (let ((items (mapcar (lambda (item) (jget item "value"))
                       (jlist (jget (run-ok ws '("doc" "select" "f.json" "" "--where" "/x=1")) "items")))))
    (expect (json= (coerce items 'vector)
                   (coerce (jq-stream ws ".[] | select(.x == 1)" "f.json" nil) 'vector))
            :to-be t)))

(define-row-case (32 "jq 'sort_by(.x)' is doc select --sort-by" :foreign ("jq")) (ws)
  (put ws "f.json" +json-array-fixture+)
  (let ((items (mapcar (lambda (item) (jget item "value"))
                       (jlist (jget (run-ok ws '("doc" "select" "f.json" "" "--sort-by" "/x")) "items")))))
    (expect (json= (coerce items 'vector) (jq ws "sort_by(.x)" "f.json" nil)) :to-be t)))

(define-row-case (32 "jq 'map(select(...)) | length' is doc select --output count" :foreign ("jq")) (ws)
  (put ws "f.json" +json-array-fixture+)
  (expect (jget (run-ok ws '("doc" "select" "f.json" "" "--where" "/x=1" "--output" "count")) "count")
          :to-be (jq ws "map(select(.x == 1)) | length" "f.json" (format nil "2~%"))))

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

