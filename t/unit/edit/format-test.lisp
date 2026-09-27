;;;; t/unit/edit/format-test.lisp
;;;;
;;;; JSON (RFC 6901, 6902, 7386, key order and indentation), table cells, and
;;;; archive extraction plans (zip slip, bombs, collisions). Their edge cases
;;;; are in format-edge-test.lisp, which uses the helpers defined here.
(in-package #:aitools.edit.test)

(defun j (text) (parse-json-text text))

(defun js (value) (serialize-json value))

(describe "aitools.edit.domain JSON values"
  (it "serializes with the original key order and number text"
    (expect (js (j "{\"b\": 1.50, \"a\": [1e3, true, null, false], \"c\": {}}"))
            :to-equal "{\"b\":1.50,\"a\":[1e3,true,null,false],\"c\":{}}")
    (expect (serialize-json (j "{\"b\":1,\"a\":[2]}") :indent "  ")
            :to-equal (format nil "{~%  \"b\": 1,~%  \"a\": [~%    2~%  ]~%}"))
    (expect (serialize-json (j "{\"b\":1,\"a\":2}") :sort-keys t) :to-equal "{\"a\":2,\"b\":1}"))

  (it "detects the file's indentation unit"
    (expect (detect-json-indent (format nil "{~%    \"a\": 1~%}")) :to-equal "    ")
    (expect (detect-json-indent (format nil "{~%~C\"a\": 1~%}" #\Tab)) :to-equal (string #\Tab))
    (expect (detect-json-indent "{\"a\": 1}") :to-be nil))

  (it "reads RFC 6901 pointers with ~0 and ~1 escapes"
    (let ((value (j "{\"a/b\": {\"m~n\": [10, 20]}}")))
      (expect (json-num-text (json-pointer-get value (parse-json-pointer "/a~1b/m~0n/1"))) :to-equal "20")
      (expect (format-json-pointer (parse-json-pointer "/a~1b/m~0n")) :to-equal "/a~1b/m~0n")
      (expect (refusal-code (lambda () (parse-json-pointer "a"))) :to-equal "argument.invalid")))

  (it "sets members, appends with /-, and refuses missing parents"
    (let ((value (j "{\"items\": [1], \"name\": \"x\"}")))
      (expect (js (json-add value (parse-json-pointer "/items/-") (j "2") :array-mode :replace))
              :to-equal "{\"items\":[1,2],\"name\":\"x\"}")
      (expect (js (json-add value (parse-json-pointer "/name") (j "\"y\"") :array-mode :replace))
              :to-equal "{\"items\":[1],\"name\":\"y\"}")
      (expect (js (json-add value (parse-json-pointer "/items/0") (j "9") :array-mode :replace))
              :to-equal "{\"items\":[9],\"name\":\"x\"}")
      (expect (refusal-code (lambda () (json-add value (parse-json-pointer "/missing/key") (j "1"))))
              :to-equal "input.not-found")
      (expect (js (json-remove value (parse-json-pointer "/name"))) :to-equal "{\"items\":[1]}")))

  ;; RFC 7386 section 3's example.
  (it "merges RFC 7386's example"
    (expect (js (json-merge-patch
                 (j "{\"title\":\"Goodbye!\",\"author\":{\"givenName\":\"John\",\"familyName\":\"Doe\"},\"tags\":[\"example\",\"sample\"],\"content\":\"This will be unchanged\"}")
                 (j "{\"title\":\"Hello!\",\"phoneNumber\":\"+01-123-456-7890\",\"author\":{\"familyName\":null},\"tags\":[\"example\"]}")))
            :to-equal "{\"title\":\"Hello!\",\"author\":{\"givenName\":\"John\"},\"tags\":[\"example\"],\"content\":\"This will be unchanged\",\"phoneNumber\":\"+01-123-456-7890\"}"))

  (it-each (("{\"a\":\"b\"}" "{\"a\":\"c\"}" "{\"a\":\"c\"}")
            ("{\"a\":\"b\"}" "{\"b\":\"c\"}" "{\"a\":\"b\",\"b\":\"c\"}")
            ("{\"a\":\"b\"}" "{\"a\":null}" "{}")
            ("{\"a\":[{\"b\":\"c\"}]}" "{\"a\":[1]}" "{\"a\":[1]}")
            ("[\"a\",\"b\"]" "[\"c\",\"d\"]" "[\"c\",\"d\"]")
            ("{\"a\":\"foo\"}" "null" "null")
            ("{\"e\":null}" "{\"a\":1}" "{\"e\":null,\"a\":1}")
            ("[1,2]" "{\"a\":\"b\",\"c\":null}" "{\"a\":\"b\"}")
            ("{}" "{\"a\":{\"bb\":{\"ccc\":null}}}" "{\"a\":{\"bb\":{}}}"))
      "merges RFC 7386 appendix A: ~A + ~A"
      (target patch expected)
    (expect (js (json-merge-patch (j target) (j patch))) :to-equal expected))

  ;; RFC 6902 appendix A.
  (it-each (("{\"foo\":\"bar\"}" "[{\"op\":\"add\",\"path\":\"/baz\",\"value\":\"qux\"}]" "{\"foo\":\"bar\",\"baz\":\"qux\"}")
            ("{\"foo\":[\"bar\",\"baz\"]}" "[{\"op\":\"add\",\"path\":\"/foo/1\",\"value\":\"qux\"}]" "{\"foo\":[\"bar\",\"qux\",\"baz\"]}")
            ("{\"baz\":\"qux\",\"foo\":\"bar\"}" "[{\"op\":\"remove\",\"path\":\"/baz\"}]" "{\"foo\":\"bar\"}")
            ("{\"foo\":[\"bar\",\"qux\",\"baz\"]}" "[{\"op\":\"remove\",\"path\":\"/foo/1\"}]" "{\"foo\":[\"bar\",\"baz\"]}")
            ("{\"baz\":\"qux\",\"foo\":\"bar\"}" "[{\"op\":\"replace\",\"path\":\"/baz\",\"value\":\"boo\"}]" "{\"baz\":\"boo\",\"foo\":\"bar\"}")
            ("{\"foo\":{\"bar\":\"baz\",\"waldo\":\"fred\"},\"qux\":{\"corge\":\"grault\"}}"
             "[{\"op\":\"move\",\"from\":\"/foo/waldo\",\"path\":\"/qux/thud\"}]"
             "{\"foo\":{\"bar\":\"baz\"},\"qux\":{\"corge\":\"grault\",\"thud\":\"fred\"}}")
            ("{\"foo\":[\"all\",\"grass\",\"cows\",\"eat\"]}" "[{\"op\":\"move\",\"from\":\"/foo/1\",\"path\":\"/foo/3\"}]"
             "{\"foo\":[\"all\",\"cows\",\"eat\",\"grass\"]}")
            ("{\"baz\":\"qux\",\"foo\":[\"a\",2,\"c\"]}"
             "[{\"op\":\"test\",\"path\":\"/baz\",\"value\":\"qux\"},{\"op\":\"test\",\"path\":\"/foo/1\",\"value\":2}]"
             "{\"baz\":\"qux\",\"foo\":[\"a\",2,\"c\"]}")
            ("{\"foo\":\"bar\"}" "[{\"op\":\"add\",\"path\":\"/child\",\"value\":{\"grandchild\":{}}}]"
             "{\"foo\":\"bar\",\"child\":{\"grandchild\":{}}}")
            ("{\"foo\":[\"bar\"]}" "[{\"op\":\"add\",\"path\":\"/foo/-\",\"value\":[\"abc\",\"def\"]}]"
             "{\"foo\":[\"bar\",[\"abc\",\"def\"]]}")
            ("{\"/\":9,\"~1\":10}" "[{\"op\":\"test\",\"path\":\"/~01\",\"value\":10}]" "{\"/\":9,\"~1\":10}")
            ("{\"foo\":\"bar\"}" "[{\"op\":\"copy\",\"from\":\"/foo\",\"path\":\"/baz\"}]" "{\"foo\":\"bar\",\"baz\":\"bar\"}"))
      "applies RFC 6902 appendix A patch ~*~A"
      (document patch expected)
    (expect (js (json-apply-patch (j document) (j patch))) :to-equal expected))

  (it-each (("{\"baz\":\"qux\"}" "[{\"op\":\"test\",\"path\":\"/baz\",\"value\":\"bar\"}]" "selection.no-match")
            ("{\"/\":9,\"~1\":10}" "[{\"op\":\"test\",\"path\":\"/~01\",\"value\":\"10\"}]" "selection.no-match")
            ("{\"foo\":\"bar\"}" "[{\"op\":\"add\",\"path\":\"/baz/bat\",\"value\":\"qux\"}]" "input.not-found")
            ("{\"foo\":\"bar\"}" "[{\"op\":\"remove\",\"path\":\"/nope\"}]" "input.not-found")
            ("{\"foo\":\"bar\"}" "[{\"op\":\"frobnicate\",\"path\":\"/foo\"}]" "argument.invalid")
            ("{\"foo\":\"bar\"}" "{\"op\":\"add\"}" "argument.invalid"))
      "refuses RFC 6902 failure ~*~A as ~A"
      (document patch code)
    (expect (refusal-code (lambda () (json-apply-patch (j document) (j patch)))) :to-equal code))

  (it "compares numbers by value and objects regardless of order in test"
    (expect (json-equal (j "{\"a\":1.0,\"b\":[1e2]}") (j "{\"b\":[100],\"a\":1}")) :to-be t)
    (expect (json-equal (j "[1,2]") (j "[2,1]")) :to-be nil)))

(describe "aitools.edit.domain table cells"
  (it "rewrites only the addressed cell, quoting when needed"
    (multiple-value-bind (text previous)
        (table-set-cell (format nil "name,note~%a,\"x, y\"~%b,z~%") #\, 2 "note" "has \"quotes\", commas")
      (expect text :to-equal (format nil "name,note~%a,\"x, y\"~%b,\"has \"\"quotes\"\", commas\"~%"))
      (expect previous :to-equal "z"))
    (expect (table-set-cell (format nil "k~Cv~%1~C2~%" #\Tab #\Tab) #\Tab 1 "v" "3")
            :to-equal (format nil "k~Cv~%1~C3~%" #\Tab #\Tab)))

  (it "refuses a missing row or column"
    (expect (refusal-code (lambda () (table-set-cell (format nil "a~%1~%") #\, 2 "a" "x"))) :to-equal "input.not-found")
    (expect (refusal-code (lambda () (table-set-cell (format nil "a~%1~%") #\, 1 "b" "x"))) :to-equal "input.not-found")))

(defun zip-of (&rest members)
  (aitools.text.domain:write-zip members))

(defun member-file (name text &key (mode #o644))
  (aitools.text.domain:make-archive-member :name name :kind :file :data (bytes text) :mode mode))

(defun extract-plan (octets format &key (lookup (constantly :absent)) (max-bytes 1000000) (max-entries 1000) selected)
  (plan-archive-extraction octets format "out" :archive-path "a.zip" :max-bytes max-bytes :max-entries max-entries
                                               :selected selected :lookup-kind lookup))

(describe "aitools.edit.domain archive extraction plans"
  (it "plans every entry below the destination"
    (let ((steps (extract-plan (zip-of (member-file "a.txt" "A") (member-file "d/b.txt" "B" :mode #o755)) :zip)))
      (expect (mapcar #'extract-step-path steps) :to-equal '("out/a.txt" "out/d/b.txt"))
      (expect (extract-step-mode (second steps)) :to-be #o755)))

  (it-each (("../evil" "refusal.outside-workspace")
            ("/etc/passwd" "refusal.outside-workspace")
            ("a/../../evil" "refusal.outside-workspace")
            ("a\\..\\evil" "refusal.outside-workspace"))
      "refuses zip slip name ~S"
      (name code)
    (expect (refusal-code (lambda () (extract-plan (aitools.text.domain:write-tar (list (member-file name "x"))) :tar)))
            :to-equal code))

  (it "refuses a symlink leading out of the destination"
    (expect (refusal-code
             (lambda ()
               (extract-plan (aitools.text.domain:write-tar
                              (list (aitools.text.domain:make-archive-member :name "link" :kind :symlink
                                                                             :link-target "../../etc")))
                             :tar)))
            :to-equal "refusal.outside-workspace"))

  (it "refuses a decompression bomb by its real output size and by entry count"
    (let ((bomb (zip-of (aitools.text.domain:make-archive-member
                         :name "zeros" :kind :file :data (make-array 200000 :element-type '(unsigned-byte 8)
                                                                            :initial-element 0)))))
      (expect (< (length bomb) 5000) :to-be t)
      (expect (refusal-code (lambda () (extract-plan bomb :zip :max-bytes 100000))) :to-equal "refusal.too-large"))
    (expect (refusal-code (lambda () (extract-plan (zip-of (member-file "a" "1") (member-file "b" "2")) :zip
                                                   :max-entries 1)))
            :to-equal "refusal.too-large"))

  (it "refuses collisions with existing paths and duplicate names"
    (expect (refusal-code (lambda () (extract-plan (zip-of (member-file "a.txt" "A")) :zip
                                                   :lookup (lambda (path) (if (string= path "out/a.txt") :file :absent)))))
            :to-equal "refusal.exists")
    (expect (refusal-code (lambda () (extract-plan (zip-of (member-file "a.txt" "A") (member-file "a.txt" "B")) :zip)))
            :to-equal "refusal.exists")))
