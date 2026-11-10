(in-package #:aitools.edit.test)

(describe "aitools JSON repair commands through dispatch"
  (it "runs explicit JSON repairs for extensionless paths"
    (call-with-cli-workspace
     (lambda ()
       (multiple-value-bind (code envelope)
           (run-aitools "doc" "fmt" "nodata" "--format" "json")
         (expect code :to-be 1)
         (expect (envelope-value envelope "error" "code")
                 :to-equal "input.not-found")
         (expect (envelope-value
                  (aref (envelope-value envelope "error" "repairs") 0)
                  "command")
                 :to-equal (format nil "aitools doc get nodata ~C~C --format json"
                                   (code-char 39) (code-char 39))))
       (put "nodata" "{\"value\":1}")
       (multiple-value-bind (code envelope)
           (run-aitools "doc" "get" "nodata" "" "--format" "json")
         (expect code :to-be 0)
         (expect (envelope-value envelope "status") :to-equal "ok"))
       (put "data" "{\"a\":{\"b\":[]}}")
       (multiple-value-bind (code envelope)
           (run-aitools "doc" "set" "data" "/a/b/c" "1" "--format" "json")
         (expect code :to-be 1)
         (expect (envelope-value envelope "error" "code")
                 :to-equal "input.not-found")
         (expect (envelope-value
                  (aref (envelope-value envelope "error" "repairs") 0)
                  "command")
                 :to-equal "aitools doc get data /a/b --format json"))
       (multiple-value-bind (code envelope)
           (run-aitools "doc" "get" "data" "/a/b" "--format" "json")
         (expect code :to-be 0)
         (expect (envelope-value envelope "status") :to-equal "ok")))))
  (it "routes malformed JSON writes through the repair continuation"
    (call-with-edit-workspace
     (lambda ()
       (put "broken.json" "{")
       (let ((before (octets-of "broken.json")))
         (with-error (code nil keys)
             (run "doc.set" (list "broken.json" "/a" "1"))
           (expect code :to-equal "input.unsupported-format")
           (expect (repair-commands keys)
                   :to-equal
                   (list "aitools doc get broken.json /a")))
         (expect (octets-of "broken.json")
                 :to-equalp
                 before))))))
