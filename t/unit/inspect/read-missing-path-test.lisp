(in-package #:aitools.inspect.test)

(describe "read of a missing path"
  (it "fails with input.not-found and near workspace paths as candidates"
    (multiple-value-bind (kind fields) (read-file '(("/work/src/reader.lisp" . "x") ("/work/src/writer.lisp" . "y"))
                                                  "src/raeder.lisp")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (json-object-get (first (getf fields :candidates)) "path") :to-equal "src/reader.lisp")
      (expect (plusp (length (getf (first (getf fields :repairs)) :command))) :to-be t)))
  (it "uses an outside missing path basename to find workspace candidates"
    (multiple-value-bind (kind fields)
        (read-file (quote (("/work/src/reader.lisp" . "x")))
                   "/outside/src/reader.lisp")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (json-object-get (first (getf fields :candidates)) "path")
              :to-equal "src/reader.lisp")))

  (it "refuses a directory as refusal.not-a-file"
    (expect (error-code (nth-value 1 (read-file '(("/work/d/x" . "1")) "d"))) :to-equal "refusal.not-a-file")))

(describe "read of a missing path outside the workspace"
  (it "ranks workspace files by the missing file's base name"
    (multiple-value-bind (kind fields) (read-file '(("/work/src/notes.md" . "x")) "/elsewhere/note.md")
      (expect kind :to-be :error)
      (expect (error-code fields) :to-equal "input.not-found")
      (expect (json-object-get (first (getf fields :candidates)) "path") :to-equal "src/notes.md")
      (expect (getf (first (getf fields :repairs)) :command) :to-equal "aitools find note.md"))))
