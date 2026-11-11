;;;;
;;;; JSON (RFC 6901, 6902, 7386, key order and indentation).
;;;; Table cases are in table-test.lisp.
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

  (it "parses and formats RFC 6901 pointers with ~0 and ~1 escapes"
    (expect (parse-json-pointer "/a~1b/m~0n") :to-equal (list "a/b" "m~n"))
    (expect
     (format-json-pointer (parse-json-pointer "/a~1b/m~0n"))
     :to-equal
     "/a~1b/m~0n")
    (expect
     (refusal-code (lambda () (parse-json-pointer "a")))
     :to-equal
     "argument.invalid"))

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
    (expect (aitools.edit.domain::json-equal (j "{\"a\":1.0,\"b\":[1e2]}") (j "{\"b\":[100],\"a\":1}")) :to-be t)
    (expect (aitools.edit.domain::json-equal (j "[1,2]") (j "[2,1]")) :to-be nil)))

(describe "aitools.edit JSON codec properties"
  (it-property "round-trips generated JSON string values"
      ((value (gen-string :min-length 0 :max-length 80
                          :alphabet (format nil "aB09\"\\~%~C漢字" #\Tab))))
    (expect (j (js value)) :to-equal value)))

(describe "aitools.edit.domain JSON edge cases"
  (it "prints refusal details for values outside the edit JSON model"
    (expect (serialize-json (vector (make-json-num "1"))) :to-equal "[1]")
    (expect (refusal-code (lambda () (serialize-json (vector 1.5)))) :to-equal "argument.invalid")
    (expect (refusal-code (lambda () (serialize-json :keyword))) :to-equal "argument.invalid")
    (let ((report
            (handler-case
                (progn (serialize-json :keyword) "")
              (edit-refusal (condition) (princ-to-string condition)))))
      (expect report :to-equal "cannot serialize :KEYWORD")))

  (it-each (("a value on one line" "{\"a\": 1}" nil)
            ("the first indented line's whitespace" "{~%   \"a\": [~%      1]~%}" "   ")
            ("two spaces when no line is indented" "{~%\"a\": 1~%}" "  ")
            ("two spaces when the indented lines are blank" "[~%  ~%1]" "  "))
      "detects ~A"
      (name template expected)
    (declare (ignore name))
    (expect (detect-json-indent (format nil template)) :to-equal expected))

  (it "sets, inserts and removes below an array element without touching the original"
    (let* ((text "{\"a\": [{\"b\": 1}, [5, 6]]}")
           (value (j text)))
      (expect (js (json-add value (parse-json-pointer "/a/0/c") (j "2"))) :to-equal "{\"a\":[{\"b\":1,\"c\":2},[5,6]]}")
      (expect (js (json-add value (parse-json-pointer "/a/1/0") (j "4"))) :to-equal "{\"a\":[{\"b\":1},[4,5,6]]}")
      (expect (js (json-add value '() (j "7"))) :to-equal "7")
      (expect (js (json-remove value (parse-json-pointer "/a/1/1"))) :to-equal "{\"a\":[{\"b\":1},[5]]}")
      (expect (js value) :to-equal "{\"a\":[{\"b\":1},[5,6]]}")))
  (it "appends with default array mode and escapes JSON strings"
    (expect (js (json-add (j "[1]") (parse-json-pointer "/1") (j "2")))
            :to-equal "[1,2]")
    (expect (js (json-add (j "[1,2]") (parse-json-pointer "/1") (j "9")))
            :to-equal "[1,9,2]")
    (expect (json-string-literal (format nil "a~Cb" #\\Newline))
            :to-equal "\"a\\nb\""))

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
    (expect (aitools.edit.domain::json-equal "x" "x") :to-be t)
    (expect (aitools.edit.domain::json-equal (j "true") (j "true")) :to-be t)
    (expect (aitools.edit.domain::json-equal (j "false") (j "null")) :to-be nil)
    (expect (aitools.edit.domain::json-equal (j "null") (j "null")) :to-be t)
    (expect (aitools.edit.domain::json-equal (j "null") (j "0")) :to-be nil)))

