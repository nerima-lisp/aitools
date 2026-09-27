;;;; t/unit/edit/format-edge-test.lisp
;;;;
;;;; Edge cases and remaining branches of the JSON, table-cell and
;;;; archive-extraction models.
(in-package #:aitools.edit.test)

(describe "aitools.edit.domain JSON edge cases"
  (it "refuses to serialize a value outside the edit value model"
    (expect (refusal-code (lambda () (serialize-json (vector 1)))) :to-equal "argument.invalid")
    (expect (refusal-code (lambda () (serialize-json (vector 1.5)))) :to-equal "argument.invalid")
    (expect (refusal-code (lambda () (serialize-json :keyword))) :to-equal "argument.invalid"))

  (it-each (("a value on one line" "{\"a\": 1}" nil)
            ("the first indented line's whitespace" "{~%   \"a\": [~%      1]~%}" "   ")
            ("two spaces when no line is indented" "{~%\"a\": 1~%}" "  ")
            ("two spaces when the indented lines are blank" "[~%  ~%1]" "  "))
      "detects ~A"
      (name template expected)
    (declare (ignore name))
    (expect (detect-json-indent (format nil template)) :to-equal expected))

  (it "walks pointers through arrays and objects, refusing what is not there"
    (let ((value (j "{\"a\": [{\"b\": 1}, 2], \"s\": \"text\"}")))
      (expect (json-num-text (json-pointer-get value (parse-json-pointer "/a/0/b"))) :to-equal "1")
      (expect (json-pointer-get value '()) :to-be value)
      (dolist (pointer '("/a/2" "/a/x" "/a/-" "/s/0" "/missing"))
        (expect (refusal-code (lambda () (json-pointer-get value (parse-json-pointer pointer))))
                :to-equal "input.not-found"))))

  (it "sets, inserts and removes below an array element without touching the original"
    (let* ((text "{\"a\": [{\"b\": 1}, [5, 6]]}")
           (value (j text)))
      (expect (js (json-add value (parse-json-pointer "/a/0/c") (j "2"))) :to-equal "{\"a\":[{\"b\":1,\"c\":2},[5,6]]}")
      (expect (js (json-add value (parse-json-pointer "/a/1/0") (j "4"))) :to-equal "{\"a\":[{\"b\":1},[4,5,6]]}")
      (expect (js (json-add value '() (j "7"))) :to-equal "7")
      (expect (js (json-remove value (parse-json-pointer "/a/1/1"))) :to-equal "{\"a\":[{\"b\":1},[5]]}")
      (expect (js (json-replace value (parse-json-pointer "/a/1/0") (j "9"))) :to-equal "{\"a\":[{\"b\":1},[9,6]]}")
      (expect (js (json-replace value '() (j "[]"))) :to-equal "[]")
      (expect (js value) :to-equal "{\"a\":[{\"b\":1},[5,6]]}")))

  (it-each (("a missing parent below an array" "/a/9/b" "input.not-found")
            ("a non-index token into an array parent" "/a/x/b" "input.not-found")
            ("a scalar parent" "/n/b/c" "input.not-found")
            ("an index past the end" "/a/3" "input.not-found")
            ("a member of a string" "/n/x" "input.not-found"))
      "json-add refuses ~A"
      (name pointer code)
    (declare (ignore name))
    (expect (refusal-code (lambda () (json-add (j "{\"a\": [1, 2], \"n\": \"s\"}") (parse-json-pointer pointer) (j "0"))))
            :to-equal code))

  (it-each (("the root value" "" "argument.invalid")
            ("a missing member" "/nope" "input.not-found")
            ("an index past the end" "/a/5" "input.not-found")
            ("a member of a scalar" "/n/x" "input.not-found"))
      "json-remove refuses ~A"
      (name pointer code)
    (declare (ignore name))
    (expect (refusal-code (lambda () (json-remove (j "{\"a\": [1], \"n\": 1}") (parse-json-pointer pointer))))
            :to-equal code))

  (it-each (("a document that is not an array" "{\"op\":\"add\"}" "argument.invalid")
            ("an operation that is not an object" "[1]" "argument.invalid")
            ("an operation without op" "[{\"path\":\"/a\"}]" "argument.invalid")
            ("a path that is not a string" "[{\"op\":\"remove\",\"path\":1}]" "argument.invalid")
            ("add without value" "[{\"op\":\"add\",\"path\":\"/b\"}]" "argument.invalid")
            ("a move into its own child" "[{\"op\":\"move\",\"from\":\"/a\",\"path\":\"/a/x\"}]" "argument.invalid")
            ("replace of a missing member" "[{\"op\":\"replace\",\"path\":\"/zz\",\"value\":1}]" "input.not-found")
            ("copy from a missing member" "[{\"op\":\"copy\",\"from\":\"/zz\",\"path\":\"/b\"}]" "input.not-found"))
      "json-apply-patch refuses ~A"
      (name patch code)
    (declare (ignore name))
    (expect (refusal-code (lambda () (json-apply-patch (j "{\"a\":{\"x\":1}}") (j patch)))) :to-equal code))

  (it "moves a value onto its own path as a no-op and replaces an array element"
    (expect (js (json-apply-patch (j "{\"a\":1}") (j "[{\"op\":\"move\",\"from\":\"/a\",\"path\":\"/a\"}]")))
            :to-equal "{\"a\":1}")
    (expect (js (json-apply-patch (j "{\"a\":[1,2]}") (j "[{\"op\":\"replace\",\"path\":\"/a/1\",\"value\":3}]")))
            :to-equal "{\"a\":[1,3]}"))

  (it "compares strings and literals by kind in test"
    (expect (json-equal "x" "x") :to-be t)
    (expect (json-equal (j "true") (j "true")) :to-be t)
    (expect (json-equal (j "false") (j "null")) :to-be nil)
    (expect (json-equal (j "null") (j "null")) :to-be t)
    (expect (json-equal (j "null") (j "0")) :to-be nil)))

(describe "aitools.edit.domain table cell edge cases"
  ;; The tables are FORMAT templates, with | standing for a carriage return.
  (it-each (("a quoted header name and escaped quotes" "\"na\"\"me\",v~%x,1~%" 1 "na\"me" "y" "\"na\"\"me\",v~%y,1~%" "x")
            ("a column given by 1-based index" "a,b~%1,2~%" 1 "2" "9" "a,b~%1,9~%" "2")
            ("CRLF records" "a,b|~%1,2|~%3,4|~%" 2 "a" "7" "a,b|~%1,2|~%7,4|~%" "3")
            ("a file without a final newline" "a~%1" 1 "a" "2" "a~%2" "1")
            ("an empty field" "a,b~%,2~%" 1 "a" "z" "a,b~%z,2~%" ""))
      "sets a cell with ~A"
      (name text row column value expected previous)
    (declare (ignore name))
    (flet ((expand (template) (substitute #\Return #\| (format nil template))))
      (multiple-value-bind (result old) (table-set-cell (expand text) #\, row column value)
        (expect result :to-equal (expand expected))
        (expect old :to-equal previous))))

  (it "replaces a malformed or unterminated quoted field whole, leaving the rest"
    (expect (table-set-cell (format nil "a,b~%\"q\"x,2~%") #\, 1 "a" "z") :to-equal (format nil "a,b~%z,2~%"))
    (expect (table-set-cell (format nil "a~%\"open") #\, 1 "a" "z") :to-equal (format nil "a~%z")))

  (it-each (("row 0" 0 "a") ("a column index past the header" 1 "3") ("a column index of 0" 1 "0")
            ("a short row" 2 "b"))
      "refuses ~A"
      (name row column)
    (declare (ignore name))
    (expect (refusal-code (lambda () (table-set-cell (format nil "a,b~%1,2~%3~%") #\, row column "x")))
            :to-equal "input.not-found"))

  (it "chooses the delimiter by extension, case-insensitively"
    (expect (table-delimiter-for-path "x.CSV") :to-be #\,)
    (expect (table-delimiter-for-path "dir.v/x.tsv") :to-be #\Tab)
    (expect (table-delimiter-for-path "x.txt") :to-be nil)
    (expect (table-delimiter-for-path "noext") :to-be nil)))

(defun retype-tar-entry (octets header type &key link)
  "OCTETS (a tar) with the header at byte HEADER given typeflag TYPE and
LINK as its link name, checksum recomputed: the hard links and device
entries WRITE-TAR does not produce."
  (let ((data (copy-seq octets)))
    (setf (aref data (+ header 156)) (char-code type))
    (when link
      (replace data (bytes link) :start1 (+ header 157)))
    (fill data 32 :start (+ header 148) :end (+ header 156))
    (let ((digits (format nil "~6,'0O" (loop for i from header below (+ header 512) sum (aref data i)))))
      (dotimes (i 6) (setf (aref data (+ header 148 i)) (char-code (char digits i))))
      (setf (aref data (+ header 154)) 0 (aref data (+ header 155)) 32))
    data))

(describe "aitools.edit.domain archive extraction plans: formats and entry kinds"
  (it "extracts a gz member under its stored name, else the archive's name without .gz"
    (let ((named (aitools.text.domain:gzip-compress (bytes "hello") :name "inner.txt"))
          (anonymous (aitools.text.domain:gzip-compress (bytes "hello"))))
      (flet ((plan (octets archive-path)
               (plan-archive-extraction octets :gz "out" :archive-path archive-path :max-bytes 100 :max-entries 10
                                                         :lookup-kind (constantly :absent))))
        (let ((step (first (plan named "x/a.gz"))))
          (expect (extract-step-path step) :to-equal "out/inner.txt")
          (expect (extract-step-data step) :to-equalp (bytes "hello"))
          (expect (extract-step-mode step) :to-be #o644))
        (expect (extract-step-path (first (plan anonymous "x/log.GZ"))) :to-equal "out/log")
        (expect (extract-step-path (first (plan anonymous "blob"))) :to-equal "out/blob.out")
        (expect (extract-step-path (first (plan anonymous ".gz"))) :to-equal "out/.gz.out"))))

  (it "refuses a gz member that exists or inflates past --max-bytes"
    (let ((octets (aitools.text.domain:gzip-compress (make-array 5000 :element-type '(unsigned-byte 8) :initial-element 7)
                                                     :name "big")))
      (expect (refusal-code (lambda () (plan-archive-extraction octets :gz "" :max-bytes 100 :max-entries 1
                                                                          :lookup-kind (constantly :absent))))
              :to-equal "refusal.too-large")
      (expect (refusal-code (lambda () (plan-archive-extraction octets :gz "" :max-bytes 10000 :max-entries 1
                                                                          :lookup-kind (constantly :file))))
              :to-equal "refusal.exists")))

  (it "plans a tar.gz, refusing one that inflates past --max-bytes"
    (let ((octets (aitools.text.domain:gzip-compress
                   (aitools.text.domain:write-tar (list (member-file "a.txt" "A"))))))
      (expect (mapcar #'extract-step-path (extract-plan octets :tar-gz)) :to-equal '("out/a.txt"))
      (expect (refusal-code (lambda () (extract-plan octets :tar-gz :max-bytes 600))) :to-equal "refusal.too-large")))

  (it "extracts only the --entry names, ./-prefixed or not, and refuses one the archive lacks"
    (let ((octets (aitools.text.domain:write-tar
                   (list (member-file "./a.txt" "A") (member-file "b.txt" "B")
                         (aitools.text.domain:make-archive-member :name "." :kind :directory)))))
      (expect (mapcar #'extract-step-path (extract-plan octets :tar)) :to-equal '("out/a.txt" "out/b.txt"))
      (expect (mapcar #'extract-step-path (extract-plan octets :tar :selected '("a.txt"))) :to-equal '("out/a.txt"))
      (expect (refusal-code (lambda () (extract-plan octets :tar :selected '("c.txt")))) :to-equal "input.not-found")))

  (it "plans directories, merging into an existing one, and symlinks inside the destination"
    (let* ((octets (aitools.text.domain:write-tar
                    (list (aitools.text.domain:make-archive-member :name "d" :kind :directory :mode #o755)
                          (aitools.text.domain:make-archive-member :name "d/l" :kind :symlink :link-target "../a"))))
           (steps (extract-plan octets :tar :lookup (lambda (path) (if (string= path "out/d") :directory :absent)))))
      (expect (mapcar (lambda (step) (list (extract-step-kind step) (extract-step-path step) (extract-step-target step)))
                      steps)
              :to-equal '((:directory "out/d" nil) (:symlink "out/d/l" "../a")))
      (expect (refusal-code (lambda () (extract-plan octets :tar :lookup (lambda (path) (if (string= path "out/d") :file :absent)))))
              :to-equal "refusal.exists")))

  (it "resolves a tar hard link to the file it names and refuses one naming no file"
    (let* ((tar (aitools.text.domain:write-tar (list (member-file "a.txt" "shared" :mode #o600) (member-file "h" ""))))
           (linked (retype-tar-entry tar 1024 #\1 :link "a.txt"))
           (dangling (retype-tar-entry tar 1024 #\1 :link "zz"))
           (escaping (retype-tar-entry tar 1024 #\1 :link "../a.txt"))
           (steps (extract-plan linked :tar)))
      (expect (mapcar #'extract-step-path steps) :to-equal '("out/a.txt" "out/h"))
      (expect (extract-step-data (second steps)) :to-equalp (bytes "shared"))
      (expect (extract-step-mode (second steps)) :to-be #o600)
      (expect (refusal-code (lambda () (extract-plan dangling :tar))) :to-equal "refusal.outside-workspace")
      (expect (refusal-code (lambda () (extract-plan escaping :tar))) :to-equal "refusal.outside-workspace")))

  (it "refuses a hard link to a directory and a device or special entry"
    (let ((tar (aitools.text.domain:write-tar
                (list (aitools.text.domain:make-archive-member :name "d" :kind :directory) (member-file "h" "")))))
      (expect (refusal-code (lambda () (extract-plan (retype-tar-entry tar 512 #\1 :link "d") :tar)))
              :to-equal "refusal.outside-workspace")
      (expect (refusal-code (lambda () (extract-plan (retype-tar-entry tar 512 #\3) :tar)))
              :to-equal "input.unsupported-format")))

  (it "counts a hard link's bytes against --max-bytes"
    (let* ((tar (aitools.text.domain:write-tar (list (member-file "a" "0123456789") (member-file "h" ""))))
           (linked (retype-tar-entry tar 1024 #\1 :link "a")))
      (expect (length (extract-plan linked :tar :max-bytes 20)) :to-be 2)
      (expect (refusal-code (lambda () (extract-plan linked :tar :max-bytes 15))) :to-equal "refusal.too-large")))

  (it-each (("x.zip" :zip) ("x.TGZ" :tar-gz) ("x.tar.gz" :tar-gz) ("x.tar" :tar) ("x.gz" :gz) ("x.txt" nil) ("" nil))
      "infers the create format of ~S"
      (path format)
    (expect (archive-format-for-path path) :to-be format))

  (it "builds a gz archive holding one member under the given name"
    (let ((octets (build-archive :gz (list (member-file "a.txt" "payload")) :name "a.txt")))
      (expect (aitools.text.domain:gzip-decompress octets) :to-equalp (bytes "payload"))
      (expect (aitools.text.domain:gzip-member-header octets) :to-equal "a.txt"))))

(describe "aitools.edit.domain remaining branches of the value models"
  (it "refuses pointers through numbers and strings, and a patch that is a string"
    (let ((value (j "{\"n\": 1, \"s\": \"t\"}")))
      (expect (refusal-code (lambda () (json-pointer-get value (parse-json-pointer "/n/x")))) :to-equal "input.not-found")
      (expect (refusal-code (lambda () (json-add value (parse-json-pointer "/n/b/c") (j "1")))) :to-equal "input.not-found")
      (expect (refusal-code (lambda () (json-remove value (parse-json-pointer "/s/x")))) :to-equal "input.not-found")
      (expect (refusal-code (lambda () (json-apply-patch value "ops"))) :to-equal "argument.invalid")))

  (it-each (("a quoted field ending in an escaped quote" "a~%\"x\"\"\"~%" 1 "a" "z" "a~%z~%" "x\"")
            ("a last record ending in an empty field" "a,b~%1," 1 "b" "z" "a,b~%1,z" "")
            ("a quoted field closing at the end of the text" "a~%\"x\"" 1 "a" "z" "a~%z" "x"))
      "sets ~A"
      (name text row column value expected previous)
    (declare (ignore name))
    (multiple-value-bind (result old) (table-set-cell (format nil text) #\, row column value)
      (expect result :to-equal (format nil expected))
      (expect old :to-equal previous)))

  (it "names a gz member after the archive when its stored name is a path"
    (let ((octets (aitools.text.domain:gzip-compress (bytes "x") :name "dir/evil")))
      (expect (extract-step-path (first (plan-archive-extraction octets :gz "out" :archive-path "safe.gz" :max-bytes 10
                                                                          :max-entries 1 :lookup-kind (constantly :absent))))
              :to-equal "out/safe"))))
