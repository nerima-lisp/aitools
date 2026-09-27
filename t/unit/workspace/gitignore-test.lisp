;;;; t/unit/workspace/gitignore-test.lisp
;;;;
;;;; .gitignore syntax and precedence (gitignore(5)); expected verdicts come
;;;; from gitignore(5)'s own examples and git's dir.c rules.
(in-package #:aitools.workspace.test)

(defun verdict (lines path &key directory base casefold)
  (ignore-list-verdict (parse-ignore-lines lines :base (or base "")) path directory :casefold casefold))

(describe "aitools.workspace.domain gitignore parsing"
  (it "skips blank lines and comments, keeps escaped # and !"
    (let ((list (parse-ignore-lines '("" "# comment" "\\#hash" "\\!bang"))))
      (expect (length (ignore-list-patterns list)) :to-be 2)
      (expect (ignore-list-verdict list "#hash" nil) :to-be :ignored)
      (expect (ignore-list-verdict list "!bang" nil) :to-be :ignored)))

  (it "trims unescaped trailing spaces but keeps an escaped one"
    (expect (verdict '("foo   ") "foo") :to-be :ignored)
    (expect (verdict '("foo\\ ") "foo ") :to-be :ignored)
    (expect (verdict '("foo\\ ") "foo") :to-be-falsy))

  (it "splits octets on LF, drops CR before LF and a UTF-8 BOM"
    (let ((list (parse-ignore-octets (concatenate '(vector (unsigned-byte 8))
                                                  #(#xEF #xBB #xBF)
                                                  (string-bytes (format nil "a.log~C~%b.log" #\Return))))))
      (expect (ignore-list-verdict list "a.log" nil) :to-be :ignored)
      (expect (ignore-list-verdict list "b.log" nil) :to-be :ignored))))

(describe "aitools.workspace.domain gitignore matching"
  (it "lets the last matching pattern win, so negation re-includes"
    (expect (verdict '("*.log" "!keep.log") "keep.log") :to-be :included)
    (expect (verdict '("*.log" "!keep.log") "other.log") :to-be :ignored)
    (expect (verdict '("!keep.log" "*.log") "keep.log") :to-be :ignored))

  (it "applies a trailing-slash pattern to directories only"
    (expect (verdict '("build/") "build" :directory t) :to-be :ignored)
    (expect (verdict '("build/") "build") :to-be-falsy)
    (expect (verdict '("build/") "src/build" :directory t) :to-be :ignored))

  (it "matches a slash-free pattern against the base name at any depth"
    (expect (verdict '("frotz") "a/b/frotz") :to-be :ignored))

  (it "anchors a pattern with a leading or middle slash to the file's directory"
    (expect (verdict '("/root.txt") "root.txt") :to-be :ignored)
    (expect (verdict '("/root.txt") "sub/root.txt") :to-be-falsy)
    (expect (verdict '("doc/frotz") "doc/frotz") :to-be :ignored)
    (expect (verdict '("doc/frotz") "a/doc/frotz") :to-be-falsy))

  (it "resolves anchored patterns relative to a nested .gitignore's base"
    (expect (verdict '("/x") "sub/x" :base "sub") :to-be :ignored)
    (expect (verdict '("/x") "x" :base "sub") :to-be-falsy)
    (expect (verdict '("a/b") "sub/a/b" :base "sub") :to-be :ignored)
    (expect (verdict '("a/b") "other/a/b" :base "sub") :to-be-falsy))

  (it "gives ** its three gitignore(5) meanings"
    (expect (verdict '("**/foo") "foo") :to-be :ignored)
    (expect (verdict '("**/foo") "a/b/foo") :to-be :ignored)
    (expect (verdict '("abc/**") "abc/x/y") :to-be :ignored)
    (expect (verdict '("abc/**") "abc") :to-be-falsy)
    (expect (verdict '("a/**/b") "a/b") :to-be :ignored)
    (expect (verdict '("a/**/b") "a/x/y/b") :to-be :ignored))

  (it "folds case when core.ignoreCase is on"
    (expect (verdict '("FOO") "foo") :to-be-falsy)
    (expect (verdict '("FOO") "foo" :casefold t) :to-be :ignored)
    (expect (verdict '("/Sub/x") "sub/x" :base "" :casefold t) :to-be :ignored))

  (it "consults the stack in precedence order: first list with a match decides"
    (let ((deeper (parse-ignore-lines '("!important.log") :base "sub"))
          (shallower (parse-ignore-lines '("*.log"))))
      (expect (ignore-stack-verdict (list deeper shallower) "sub/important.log" nil) :to-be :included)
      (expect (ignore-stack-verdict (list deeper shallower) "sub/other.log" nil) :to-be :ignored)
      (expect (ignore-stack-verdict (list deeper shallower) "sub/readme" nil) :to-be-falsy))))

(describe "aitools.workspace.domain always-skipped names"
  (it "recognizes the write protocol's temporary files by the .aitools-*.tmp glob"
    (expect (aitools-temporary-name-p ".aitools-op-123-0.tmp") :to-be-truthy)
    (expect (aitools-temporary-name-p ".aitools-.tmp") :to-be-truthy)
    (expect (aitools-temporary-name-p "aitools-op.tmp") :to-be-falsy))

  (it "treats .git case-insensitively"
    (expect (git-metadata-name-p ".git") :to-be-truthy)
    (expect (git-metadata-name-p ".GIT") :to-be-truthy)
    (expect (git-metadata-name-p ".gitignore") :to-be-falsy))

  (it "loads the builtin exclude list from data"
    (let ((list (builtin-ignore-list)))
      (expect (ignore-list-verdict list "node_modules" t) :to-be :ignored)
      (expect (ignore-list-verdict list "a/target" t) :to-be :ignored)
      (expect (ignore-list-verdict list "target" nil) :to-be-falsy)
      (expect (ignore-list-verdict list "result" nil) :to-be :ignored)
      (expect (ignore-list-verdict list "src" t) :to-be-falsy))))

(describe "aitools.workspace.domain glob filter"
  (it "matches base names without a slash and whole paths with one"
    (let ((filter (make-glob-filter '("*.lisp"))))
      (expect (glob-filter-accepts-p filter "a/b/c.lisp") :to-be-truthy)
      (expect (glob-filter-accepts-p filter "a/b/c.el") :to-be-falsy))
    (let ((filter (make-glob-filter '("src/**/*.lisp"))))
      (expect (glob-filter-accepts-p filter "src/x/y.lisp") :to-be-truthy)
      (expect (glob-filter-accepts-p filter "t/x/y.lisp") :to-be-falsy)))

  (it "excludes with a leading !"
    (let ((filter (make-glob-filter '("*.lisp" "!*-test.lisp"))))
      (expect (glob-filter-accepts-p filter "a.lisp") :to-be-truthy)
      (expect (glob-filter-accepts-p filter "a-test.lisp") :to-be-falsy)))

  (it "accepts everything without globs"
    (expect (make-glob-filter '()) :to-be-falsy)
    (expect (glob-filter-accepts-p nil "anything") :to-be-truthy)))
