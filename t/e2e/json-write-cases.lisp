(in-package #:aitools.e2e.test)

;;;; Correspondence table row 33: JSON modification and formatting.

(define-row-case (33 "jq . is doc fmt, byte for byte" :foreign ("jq")) (ws)
  (let ((compact (format nil "{\"b\":1,\"a\":{\"c\":[1,2],\"s\":\"x\"}}~%")))
    (put ws "a.json" compact)
    (put ws "o.json" compact)
    (let ((expected (oracle ws '("jq") "jq . o.json"
                            (format nil "{~%  \"b\": 1,~%  \"a\": {~%    \"c\": [~%      1,~%      2~%    ],~%    \"s\": \"x\"~%  }~%}~%"))))
      (run-ok ws '("doc" "fmt" "a.json"))
      (expect (file-text ws "a.json") :to-equal expected))))

(define-row-case (33 "jq '. * {...}' is doc merge" :foreign ("jq")) (ws)
  (expect-same-json-edit ws ". * {\"b\":{\"e\":true},\"z\":\"new\"}" '("doc" "merge" "a.json" "--stdin")
                         :stdin "{\"b\":{\"e\":true},\"z\":\"new\"}"))

(define-row-case (33 "jq 'del(.b)' is doc delete" :foreign ("jq")) (ws)
  (expect-same-json-edit ws "del(.b)" '("doc" "delete" "a.json" "/b")))

(define-row-case (33 "jq '.a += [4]' is doc set /a/-" :foreign ("jq")) (ws)
  (expect-same-json-edit ws ".a += [4]" '("doc" "set" "a.json" "/a/-" "4")))

(define-row-case (33 "jq '.a = 1' is doc set" :foreign ("jq")) (ws)
  (expect-same-json-edit ws ".a = 1" '("doc" "set" "a.json" "/a" "1")))

