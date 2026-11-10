(in-package #:aitools.inspect.test)

(describe "document command flow forwarding"
  (it "passes a limited get through the dispatcher as a partial result"
    (let ((boundaries
            (make-test-boundaries
             :files (list (cons "/work/doc.json" "{\"name\":\"Ada\"}")))))
      (multiple-value-bind (kind fields)
          (run-flow (function doc-inspect-flow)
                    boundaries
                    "get"
                    (list "doc.json")
                    (list "doc.json" "/name")
                    nil
                    (list :max-bytes "1"))
        (expect kind :to-be :partial)
        (expect (field fields "value_preview") :to-be-truthy))))

  (it "forwards a missing-pointer repair through the dispatcher"
    (let ((boundaries
            (make-test-boundaries
             :files (list (cons "/work/doc.json" "{\"name\":\"Ada\"}")))))
      (multiple-value-bind (kind fields)
          (run-flow (function doc-inspect-flow)
                    boundaries
                    "get"
                    (list "doc.json")
                    (list "doc.json" "/missing")
                    nil
                    (list :max-bytes "16KiB"))
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "input.not-found")
        (expect (getf (first (getf fields :repairs)) :command)
                :to-equal "aitools doc get doc.json '' --keys")))))
