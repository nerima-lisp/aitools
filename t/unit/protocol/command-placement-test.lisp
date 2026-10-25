(in-package #:aitools.protocol.test)

(describe "aitools.protocol.domain unknown-name repairs"
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

  (it-each (("cp" "aitools copy")
            ("mv" "aitools move")
            ("rm" "aitools rm")
            ("top" "aitools sys metrics")
            ("rmdir" "aitools delete")
            ("mkdir" "aitools mkdir")
            ("chmod" "aitools chmod")
            ("ln" "aitools link")
            ("touch" "aitools touch")
            ("mktemp" "aitools mktemp")
            ("ps" "aitools sys procs")
            ("lsof" "aitools sys ports"))
      "maps ~S to its matching operation ~S"
      (name command)
    (expect (mapcar (lambda (repair) (getf repair :command))
                    (repairs-for-unknown-name name))
            :to-equal (list command)))

  (it-each (("rm" "--recursive")
            ("rmdir" "empty directory")
            ("ln" "symbolic link")
            ("lsof" "listening TCP"))
      "states the supported scope for ~S: ~S"
      (name scope)
    (expect (search scope (getf (first (repairs-for-unknown-name name)) :detail))
            :to-be-truthy))

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
