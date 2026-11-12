(in-package #:aitools.e2e.test)

(describe "aitools retired command repairs"
  (cl-weave:it-each
   (("delete" ("delete" "never-created-target") "aitools rm")
    ("git status" ("git" "status") "aitools run -- git")
    ("json get" ("json" "get" "never-created.json" "/a") "aitools doc get")
    ("yaml get" ("yaml" "get" "never-created.yaml" "/a") "aitools doc get")
    ("toml get" ("toml" "get" "never-created.toml" "/a") "aitools doc get"))
   "reports argument.invalid for ~A and suggests the replacement command"
   (name arguments repair-command)
   (call-with-workspace
    (lambda (workspace)
      (let* ((result (aitools workspace arguments))
             (envelope (aitools-envelope result)))
        (expect (aitools-exit-code result) :to-be 1)
        (expect (jget envelope "status") :to-equal "error")
        (expect (jget envelope "command") :to-equal (first arguments))
        (expect (jget envelope "error" "code") :to-equal "argument.invalid")
        (expect
         (map (quote list)
              (lambda (repair)
                (jget repair "command"))
              (jget envelope "error" "repairs"))
         :to-equal
         (list repair-command)))))
   :prefix
   "aitools-retired-command"))
