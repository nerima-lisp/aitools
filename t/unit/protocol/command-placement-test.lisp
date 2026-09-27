;;;; t/unit/protocol/command-placement-test.lisp
(in-package #:aitools.protocol.test)

(describe "aitools.protocol.domain command-placement"
  (it "recognizes a top-level command"
    (expect (top-level-command-p "read") :to-be-truthy)
    (expect (top-level-command-p "edit") :to-be-truthy))

  (it "does not recognize a group name as a top-level command"
    (expect (top-level-command-p "util") :to-be-falsy))

  (it "recognizes a group name"
    (expect (group-command-p "util") :to-be-truthy)
    (expect (group-command-p "json") :to-be-truthy))

  (it "gives repairs pointing at `aitools read` for cat"
    (let ((repairs (repairs-for-unknown-name "cat")))
      (expect repairs :not :to-be-falsy)
      (expect (getf (first repairs) :command) :to-equal "aitools read")))

  (it "gives repairs pointing at `aitools search` for grep"
    (expect (getf (first (repairs-for-unknown-name "grep")) :command) :to-equal "aitools search"))

  (it "gives repairs pointing at `aitools find` for ls"
    (expect (getf (first (repairs-for-unknown-name "ls")) :command) :to-equal "aitools find"))

  (it "points a bare group subcommand name at its group form"
    (expect (getf (first (repairs-for-unknown-name "uuid")) :command) :to-equal "aitools util uuid"))

  (it "falls back to `aitools schema` for a name with no correspondence entry"
    (let ((repairs (repairs-for-unknown-name "totally-made-up-command")))
      (expect repairs :not :to-be-falsy)
      (expect (getf (first repairs) :command) :to-equal "aitools schema"))))

(describe "aitools.protocol.domain correspondence-name-p"
  (it-each (("fmt") ("grep") ("cat"))
      "knows ~S from the correspondence table"
      (name)
    (expect (aitools.protocol.domain:correspondence-name-p name) :to-be t))

  (it-each (("totally-made-up-command") ("read") (""))
      "does not know ~S"
      (name)
    (expect (aitools.protocol.domain:correspondence-name-p name) :to-be-falsy))

  (it "offers exactly one browse-commands repair for an unknown name"
    (expect (repairs-for-unknown-name "totally-made-up-command")
            :to-equal (list (list :action "browse-commands" :detail "List every implemented command."
                                  :command "aitools schema")))))
