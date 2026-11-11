(in-package #:aitools.inspect.test)

(defun %remaining-flow-context (boundaries)
  (aitools.inspect.application::%make-inspect-context
   :boundaries boundaries
   :root (aitools.workspace.domain:make-workspace-root
          :path "/work" :real "/work")))

(describe "inspect application flow remaining paths"
  (it "lists and reads an archive through the application flows"
    (let ((boundaries (make-test-boundaries :files (archive-files))))
      (multiple-value-bind (kind fields)
          (run-flow #'archive-list-flow boundaries "a.zip")
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 3))
      (multiple-value-bind (kind fields)
          (run-flow #'archive-read-flow boundaries "a.zip" "docs/readme.txt")
        (expect kind :to-be :ok)
        (expect (field fields "lines") :to-equal '("one" "two" "three")))))

  (it "compares a file and rejects the reverse directory-file pairing"
    (let ((boundaries
            (make-test-boundaries
             :files '(( "/work/a.txt" . "before")
                       ("/work/sub/b.txt" . "child")))))
      (multiple-value-bind (kind fields)
          (run-flow #'diff-flow boundaries :a "a.txt" :b "a.txt")
        (expect kind :to-be :ok)
        (expect (field fields "mode") :to-equal "unified")
        (expect (field fields "identical") :to-be t))
      (multiple-value-bind (kind fields)
          (run-flow #'diff-flow boundaries :a "sub" :b "a.txt")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "argument.invalid"))))

  (it "suggests a workspace basename for an outside missing path"
    (multiple-value-bind (kind fields)
        (run-flow #'info-flow
                  (make-test-boundaries :files '(( "/work/target.csv" . "name")))
                  "/else/target.csv")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (json-object-get (first (getf fields :candidates)) "path")
              :to-equal "target.csv")))

  (it "preserves a root-relative file path while probing a target"
    (let* ((context
             (%remaining-flow-context
              (make-test-boundaries :files '(( "/work/sub/file.txt" . "text")))))
           (target
             (aitools.inspect.application::probe-target context "sub/file.txt")))
      (expect (aitools.inspect.application::file-target-kind target) :to-be :file)
      (expect (aitools.inspect.application::file-target-relative target)
              :to-equal "sub/file.txt")))

  (it "dispatches TOML document reads and JSON query flows"
    (let ((boundaries
            (make-test-boundaries
             :files (list (cons "/work/doc.toml" "name = \"Ada\"")
                          (cons "/work/data.json"
                                "{\"items\":[{\"id\":1},{\"id\":2}],\"enabled\":true}")
                          (cons "/work/a.json" "{\"x\":1}")
                          (cons "/work/b.json" "{\"x\":2}")))))
      (multiple-value-bind (kind fields)
          (run-flow #'json-get-flow boundaries "doc.toml" "/name" :format "toml")
        (expect kind :to-be :ok)
        (expect (field fields "value") :to-equal "Ada"))
      (multiple-value-bind (kind fields)
          (run-flow #'json-get-flow boundaries "data.json" "/enabled")
        (expect kind :to-be :ok)
        (expect (field fields "value") :to-be t))
      (multiple-value-bind (kind fields)
          (run-flow #'json-select-flow boundaries "data.json" "/items")
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 2))
      (multiple-value-bind (kind fields)
          (run-flow #'json-diff-flow boundaries "a.json" "b.json")
        (expect kind :to-be :ok)
        (expect (field fields "total") :to-be 1))))

  (it "forwards partial text views and omits invert from an ordinary match continuation"
    (let ((boundaries
            (make-test-boundaries
             :files (list (cons "/work/lines.txt"
                                (format nil "first~%second~%third~%"))
                          (cons "/work/hits.txt"
                                (format nil "hit~%keep~%hit~%"))))))
      (multiple-value-bind (kind fields)
          (run-flow #'read-flow boundaries "lines.txt" :max-lines 1)
        (expect kind :to-be :partial)
        (expect (field fields "lines") :to-equal '("first")))
      (multiple-value-bind (kind fields)
          (run-flow #'read-flow boundaries "hits.txt" :match "hit" :max-lines 1)
        (expect kind :to-be :partial)
        (expect (field fields "line_numbers") :to-equal '(1))
        (expect (search "--invert" (first (field fields "next_commands")))
                :to-be nil))))

  (it "finishes a strings scan when the last chunk ends with a delimiter"
    (multiple-value-bind (kind fields)
        (run-flow #'read-flow
                  (make-test-boundaries
                   :files (list (cons "/work/strings.bin"
                                      (%as-octets #(115 104 111 114 116 0)))))
                  "strings.bin" :as "strings")
      (expect kind :to-be :ok)
      (expect (field fields "total") :to-be 1)
      (expect (json-object-get (first (field fields "strings")) "text")
              :to-equal "short")))

  (it "validates table options and reads and aggregates default table input"
    (let ((boundaries
            (make-test-boundaries
             :files (list (cons "/work/people.csv"
                                (format nil "city,count~%Tokyo,3~%Osaka,2~%"))))))
      (multiple-value-bind (kind fields)
          (run-flow #'table-read-flow boundaries "people.csv")
        (expect kind :to-be :ok)
        (expect (field fields "total_rows") :to-be 2))
      (multiple-value-bind (kind fields)
          (run-flow #'table-agg-flow boundaries "people.csv")
        (expect kind :to-be :ok)
        (expect (json-object-get (first (field fields "groups")) "count")
                :to-be 2))
      (multiple-value-bind (kind fields)
          (run-flow #'table-read-flow boundaries "people.csv" :format "sep")
        (expect kind :to-be :error)
        (expect (error-code fields) :to-equal "argument.invalid")
        (expect (getf (first (getf fields :repairs)) :command)
                :to-equal "aitools schema table read"))))

  (it "registers an optional composition boundary when the others are omitted"
    (let* ((host (make-test-boundaries))
           (boundaries
             (aitools.inspect.composition::make-inspect-dependencies
              :workspace-host host)))
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :inspect-workspace-host)
              :to-be host)
      (expect (cl-boundary-kit:boundary-context-require
               boundaries :inspect-text-context)
              :to-be nil)))
  (it "classifies the workspace root and an out-of-root target"
    (let* ((context
             (%remaining-flow-context
              (make-test-boundaries
               :files (list (cons "/else/outside.txt" "outside")))))
           (root-target
             (aitools.inspect.application::probe-target context "."))
           (target
             (aitools.inspect.application::probe-target
              context "/else/outside.txt")))
      (expect (aitools.inspect.application::file-target-kind root-target)
              :to-be :directory)
      (expect (aitools.inspect.application::file-target-relative root-target)
              :to-equal "")
      (expect (aitools.inspect.application::file-target-kind target)
              :to-be :file)
      (expect (aitools.inspect.application::file-target-relative target)
              :to-be nil)
      (expect (aitools.inspect.application::target-display-path target)
              :to-equal "/else/outside.txt")))
  (it "rejects an inspect context tx value outside its declared type"
    (signals type-error
      (aitools.inspect.application::%make-inspect-context
       :tx (parse-integer "7"))))
  (it "defaults text-view continuation word lists to empty"
    (let ((options
            (aitools.inspect.application::make-text-view-options)))
      (expect (aitools.inspect.application::text-view-options-command-words
               options)
              :to-equal nil)
      (expect (aitools.inspect.application::text-view-options-extra-words
               options)
              :to-equal nil))))
