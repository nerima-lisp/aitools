;;;; t/unit/vcs/flows-options-test.lisp
;;;;
;;;; The git group's flows' shared options and edges against the fake
;;;; GIT-PORT: --root, selectors, paths relative to the working directory,
;;;; git failures by kind, argument passing, and show/blame edge cases.
;;;; FAKE-PORT and the other helpers come from flows-test.lisp.
(in-package #:aitools.vcs.test)

(describe "aitools.vcs.application --root"
  (it "keeps the port without a root and rebinds it with one"
    (let* ((calls (list nil)) (port (fake-port :calls calls)))
      (expect (port-at-root port nil) :to-be port)
      (expect (car calls) :to-equal nil)
      (port-at-root port "../other")
      (expect (car calls) :to-equal '(("at" . "../other")))))
  (it "reports a missing --root directory as input.not-found"
    (multiple-value-bind (kind fields) (run-flow #'git-status/k (fake-port :probe :no-directory))
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "input.not-found")
      (expect (getf fields :message) :to-contain "/nowhere/"))))

(describe "aitools.vcs.application selectors"
  (it "blames only the lines --match selects"
    (multiple-value-bind (kind fields)
        (run-flow #'git-blame/k (fake-port :runs `(("blame" :ok ,(blame-output 30)))) "a.txt" :match "line 2")
      (expect kind :to-be :ok)
      (expect (mapcar (lambda (line) (json-alist-value line "n")) (field fields "lines"))
              :to-equal '(2 20 21 22 23 24 25 26 27 28 29))
      (expect (field fields "start_line") :to-equal 2)))

  (it "shows --match lines with their numbers, capped by --max-lines"
    (multiple-value-bind (kind fields)
        (run-flow #'git-show/k (fake-port :runs `(("cat-file" :ok ,(string-bytes (lines-text "a" "b" "a" "a")))))
                  "HEAD:x.txt" :match "a" :max-lines 2)
      (expect kind :to-be :partial)
      (expect (field fields "lines") :to-equal '("a" "a"))
      (expect (field fields "line_numbers") :to-equal '(1 3))
      (expect (field fields "next_commands") :to-equal '("aitools git show HEAD:x.txt --match a --max-lines 3"))))

  (it "rejects two selectors in one call"
    (multiple-value-bind (kind fields)
        (run-flow #'git-show/k (fake-port) "HEAD:x.txt" :range "1:2" :match "a")
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "argument.invalid")
      (expect (first-repair-command fields) :to-equal "aitools schema git.show")))

  (it "reports an ambiguous --between as selection.ambiguous with candidates"
    (multiple-value-bind (kind fields)
        (run-flow #'git-show/k (fake-port :runs `(("cat-file" :ok ,(string-bytes (lines-text "begin" "x" "end" "begin" "end")))))
                  "HEAD:x.txt" :between '("begin" "end"))
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "selection.ambiguous")
      (expect (length (getf fields :candidates)) :to-equal 2)
      (expect (first-repair-command fields) :to-equal "aitools git show HEAD:x.txt --range 1:3")))

  (it "repeats --root in suggested commands"
    (multiple-value-bind (kind fields)
        (run-flow #'git-blame/k (fake-port :runs `(("blame" :ok ,(blame-output 100)))) "a.txt" :root "../repo")
      (expect kind :to-be :partial)
      (expect (field fields "next_commands") :to-equal '("aitools --root ../repo git blame a.txt --range 81:100")))))

(describe "aitools.vcs.application paths relative to the working directory"
  (it "turns a path given from a subdirectory into a top-relative pathspec and output path"
    (let ((calls (list nil)))
      (multiple-value-bind (kind fields)
          (run-flow #'git-log/k
                    (fake-port :runs `(("rev-parse" :ok "abc") ("rev-list" :ok "1")
                                       ("log" :ok ,(log-output '("c1" "one"))))
                               :calls calls :directory "/repo/sub")
                    :path "../lib/./a.txt")
        (expect kind :to-be :ok)
        (expect (field fields "path") :to-equal "lib/a.txt")
        (let ((log-arguments (cdr (assoc "log" (car calls) :test #'string=))))
          (expect (last log-arguments 2) :to-equal '("--" "lib/a.txt")))
        (expect (assoc "at" (car calls) :test #'string=) :to-equal '("at" . "/repo")))))

  (it "names a summarized diff file relative to the working directory"
    (multiple-value-bind (kind fields)
        (run-flow #'git-diff/k
                  (fake-port :numstat (list (list :path "top.txt" :added 3 :deleted 0))
                             :runs `(("diff" :ok ,(lines-text "diff --git a/top.txt b/top.txt"
                                                              "--- a/top.txt" "+++ b/top.txt"
                                                              "@@ -0,0 +1,3 @@" "+1" "+2" "+3")))
                             :directory "/repo/sub")
                  :max-lines 1)
      (expect kind :to-be :partial)
      (expect (field fields "next_commands") :to-equal '("aitools git diff ../top.txt --max-lines 4"))))

  (it "reads git show objects relative to the working directory"
    (let ((calls (list nil)))
      (multiple-value-bind (kind fields)
          (run-flow #'git-show/k (fake-port :runs `(("cat-file" :ok ,(string-bytes (lines-text "x"))))
                                            :calls calls :directory "/repo/sub")
                    "HEAD~1:a.txt")
        (expect kind :to-be :ok)
        (expect (field fields "path") :to-equal "sub/a.txt")
        (expect (cdr (assoc "cat-file" (car calls) :test #'string=)) :to-equal '("blob" "HEAD~1:sub/a.txt")))))

  (it "rejects a path outside the repository"
    (multiple-value-bind (kind fields)
        (run-flow #'git-blame/k (fake-port :directory "/repo/sub") "../../etc/passwd")
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "argument.invalid"))))

(defun repair-commands (fields)
  (mapcar (lambda (repair) (getf repair :command)) (getf fields :repairs)))

(describe "aitools.vcs.application git failures by kind"
  (it-each ((:failed "environment.io" ("aitools git log"))
            (:missing "environment.unavailable" ("aitools sys tools git")))
      "maps a ~S failure of the HEAD check in git log to ~A with ~S"
      (kind code repairs)
    (multiple-value-bind (result fields)
        (run-flow #'git-log/k (fake-port :runs `(("rev-parse" :fail ,kind "git trouble"))))
      (expect result :to-be :error)
      (expect (getf fields :code) :to-equal code)
      (expect (getf fields :message) :to-equal "git trouble")
      (expect (repair-commands fields) :to-equal repairs))))

(describe "aitools.vcs.application git diff arguments"
  (it "passes an empty --ref on to git rather than reading it as an option"
    (let ((calls (list nil)))
      (multiple-value-bind (kind fields) (run-flow #'git-diff/k (fake-port :numstat '() :calls calls) :ref "" :output :stat)
        (declare (ignore fields))
        (expect kind :to-be :ok)
        (expect (car (last (cdr (assoc "numstat" (car calls) :test #'string=)))) :to-equal ""))))

  (it "repeats --staged and --ref in the rerun command"
    (multiple-value-bind (kind fields)
        (run-flow #'git-diff/k (fake-port :numstat (list (list :path "a" :added 1 :deleted 0))
                                          :runs `(("diff" :ok ,*flow-patch*)))
                  :staged t :ref "HEAD~1")
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "environment.io")
      (expect (first-repair-command fields) :to-equal "aitools git diff --staged --ref 'HEAD~1' --max-lines 400"))))

(describe "aitools.vcs.application git show and blame edge cases"
  (it "names <path> in the repair for an option-like object name"
    (multiple-value-bind (kind fields) (run-flow #'git-show/k (fake-port) "-x:y")
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "argument.invalid")
      (expect (first-repair-command fields) :to-equal "aitools git show 'HEAD:<path>'")))

  (it "asks git for a revision with an empty path as given, and repairs with the whole log"
    (let ((calls (list nil)))
      (multiple-value-bind (kind fields)
          (run-flow #'git-show/k (fake-port :calls calls :runs '(("cat-file" :fail :exit "fatal: not a blob"))) "HEAD:")
        (expect kind :to-be :error)
        (expect (getf fields :code) :to-equal "input.not-found")
        (expect (first-repair-command fields) :to-equal "aitools git log")
        (expect (cdr (assoc "cat-file" (car calls) :test #'string=)) :to-equal '("blob" "HEAD:")))))

  (it "repeats --invert in the next command of a cut --match"
    (multiple-value-bind (kind fields)
        (run-flow #'git-show/k (fake-port :runs `(("cat-file" :ok ,(string-bytes (lines-text "a" "b" "c" "b")))))
                  "HEAD:x.txt" :match "b" :invert t :max-lines 1)
      (expect kind :to-be :partial)
      (expect (field fields "line_numbers") :to-equal '(1))
      (expect (field fields "next_commands") :to-equal '("aitools git show HEAD:x.txt --match b --invert --max-lines 2"))))

  (it "shows nothing between adjacent --between lines with --exclusive"
    (multiple-value-bind (kind fields)
        (run-flow #'git-show/k (fake-port :runs `(("cat-file" :ok ,(string-bytes (lines-text "x" "begin" "end")))))
                  "HEAD:x.txt" :between '("begin" "end") :exclusive t)
      (expect kind :to-be :ok)
      (expect (field fields "start_line") :to-equal 3)
      (expect (field fields "lines") :to-equal '())))

  (it-each ((git-show/k "HEAD:x.txt" (:match "(") "input.syntax-error" "aitools git show HEAD:x.txt --range 1:80")
            (git-blame/k "a.txt" (:symbol "main") "input.unsupported-language" "aitools git blame a.txt --range 1:80"))
      "answers ~S of ~S with ~S's failure as ~A and a --range repair"
      (flow target selector code repair)
    (multiple-value-bind (kind fields)
        (apply #'run-flow (symbol-function flow)
               (fake-port :runs `(("cat-file" :ok ,(string-bytes (lines-text "a" "b")))
                                  ("blame" :ok ,(blame-output 2))))
               target selector)
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal code)
      (expect (first-repair-command fields) :to-equal repair))))
