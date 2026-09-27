;;;; t/unit/search/commands-test.lisp
;;;;
;;;; The search context's commands through cl-cli parsing and the
;;;; composition root's dispatch, against the in-memory filesystem: argv in,
;;;; one JSON envelope and an exit code out.
(in-package #:aitools.search.test)

(defun dispatch-search (ports &rest argv)
  "(VALUES EXIT-CODE STDOUT STDERR) for `aitools ARGV...`."
  (let ((registry (aitools.protocol.application:make-command-registry)))
    (aitools.search.presentation:register-search-commands registry ports)
    (let ((app (cl-cli:make-app :name "aitools" :version "0.0.0" :require-command t
                                :global-options (list (cl-cli:make-option :name "root" :kind :value))
                                :commands (aitools/cli:finalize-app-commands registry)))
          (stdout (make-string-output-stream))
          (stderr (make-string-output-stream)))
      (values (aitools/cli:dispatch app registry (cons "aitools" argv) :stdout stdout :stderr stderr)
              (get-output-stream-string stdout)
              (get-output-stream-string stderr)))))

(defun json-get (text &rest keys)
  (let ((value (json-kit:parse text)))
    (dolist (key keys value)
      (setf value (if (integerp key) (elt value key) (gethash key value))))))

(defparameter *command-files*
  (list (list "/w/src/a.lisp" (format nil "(defun hit ())~%(hit)~%"))
        (list "/w/b.txt" (format nil "hit~%"))))

(describe "aitools.search.presentation commands"
  (it "registers search, find, overview, and the code group with schemas"
    (let ((registry (aitools.protocol.application:make-command-registry)))
      (aitools.search.presentation:register-search-commands registry (make-fake-ports))
      (expect (mapcar #'aitools.protocol.domain:command-schema-name
                      (aitools.protocol.application:all-command-schemas registry))
              :to-equal '("search" "find" "overview" "code.outline" "code.defs" "code.refs"))))

  (it "treats the first positional as a path once --pattern supplies the pattern"
    (multiple-value-bind (code stdout)
        (dispatch-search (make-fake-ports :files *command-files*) "search" "--pattern" "hit" "src" "--output" "files")
      (expect code :to-be 0)
      (expect (json-get stdout "mode") :to-equal "files")
      (expect (coerce (json-get stdout "paths") 'list) :to-equal '("src/a.lisp"))))

  (it "accepts options after several paths"
    (multiple-value-bind (code stdout)
        (dispatch-search (make-fake-ports :files *command-files*) "search" "hit" "src" "b.txt" "--output" "count")
      (expect code :to-be 0)
      (expect (json-get stdout "mode") :to-equal "count")
      (expect (json-get stdout "total_matches") :to-be 3)))

  (it "exits 3 with status partial when --limit cuts the result"
    (multiple-value-bind (code stdout)
        (dispatch-search (make-fake-ports :files *command-files*) "search" "hit" "--limit" "1" "--context" "0")
      (expect code :to-be 3)
      (expect (json-get stdout "status") :to-equal "partial")
      (expect (json-get stdout "total_matches") :to-be 3)))

  (it "exits 1 with input.syntax-error on stderr for an invalid regex"
    (multiple-value-bind (code stdout stderr) (dispatch-search (make-fake-ports :files *command-files*) "search" "a(")
      (expect code :to-be 1)
      (expect stdout :to-equal "")
      (expect (json-get stderr "error" "code") :to-equal "input.syntax-error")
      (expect (json-get stderr "error" "repairs" 0 "command") :to-contain "--fixed")))

  (it "maps find's --type dir and --output tree"
    (multiple-value-bind (code stdout)
        (dispatch-search (make-fake-ports :files *command-files*) "find" "--type" "dir" "--output" "tree")
      (expect code :to-be 0)
      (expect (json-get stdout "mode") :to-equal "tree")
      (expect (json-get stdout "tree" "children" 0 "name") :to-equal "src")))

  (it "runs the code subcommands and overview"
    (let ((ports (make-fake-ports :files *command-files*)))
      (expect (json-get (nth-value 1 (dispatch-search ports "code" "outline" "src/a.lisp")) "symbols" 0 "name")
              :to-equal "hit")
      (expect (json-get (nth-value 1 (dispatch-search ports "code" "defs" "hit")) "defs" 0 "path") :to-equal "src/a.lisp")
      (expect (json-get (nth-value 1 (dispatch-search ports "code" "refs" "hit")) "total") :to-be 2)
      (expect (json-get (nth-value 1 (dispatch-search ports "overview")) "languages" 0 "lang") :to-equal "common-lisp")))

  (it "resolves the workspace from the global --root"
    (multiple-value-bind (code stdout)
        (dispatch-search (make-fake-ports :files *command-files* :cwd "/") "--root" "/w" "find" "b.txt")
      (expect code :to-be 0)
      (expect (json-get stdout "items" 0 "path") :to-equal "b.txt")))

  (it "reports a missing --root directory as input.not-found"
    (multiple-value-bind (code stdout stderr)
        (dispatch-search (make-fake-ports :files *command-files*) "--root" "/nope" "overview")
      (declare (ignore stdout))
      (expect code :to-be 1)
      (expect (json-get stderr "error" "code") :to-equal "input.not-found"))))

(describe "aitools.search.presentation commands pass their flags to the flows"
  (it "maps search's boolean flags, as the partial result's next command shows"
    (multiple-value-bind (code stdout)
        (dispatch-search (make-fake-ports :files (list (list "/w/a.txt" (format nil "a~%b~%c~%"))))
                         "search" "--pattern" "x" "--fixed" "--ignore-case" "--word" "--line-regexp" "--invert"
                         "--multiline" "--no-ignore" "--limit" "1" "--context" "0")
      (expect code :to-be 3)
      (expect (json-get stdout "next_commands" 0)
              :to-equal (format nil "aitools search --pattern x --fixed --ignore-case --word --line-regexp --invert ~
                                     --multiline --before 0 --after 0 --limit 3 --no-ignore"))))

  (it "reads the search pattern from standard input with --stdin"
    (multiple-value-bind (code stdout)
        (dispatch-search (make-fake-ports :files *command-files* :stdin (format nil "hit~%")) "search" "--stdin" "--output" "count")
      (expect code :to-be 0)
      (expect (json-get stdout "total_matches") :to-be 3)))

  (it "maps find's --executable, --empty, and --sizes"
    (let ((ports (make-fake-ports :files '(("/w/run.sh" "#!" :mode #o755) ("/w/b.txt" "b")) :directories '("/w/empty"))))
      (expect (json-get (nth-value 1 (dispatch-search ports "find" "--executable")) "items" 0 "path") :to-equal "run.sh")
      (multiple-value-bind (code stdout) (dispatch-search ports "find" "--empty" "--sizes" "--type" "dir")
        (expect code :to-be 0)
        (expect (json-get stdout "items" 0 "path") :to-equal "empty")
        (expect (json-get stdout "items" 0 "size") :to-be 0))))

  (it "maps code defs --prefix and overview --no-ignore"
    (let ((ports (make-fake-ports :files *command-files*)))
      (expect (json-get (nth-value 1 (dispatch-search ports "code" "defs" "hi" "--prefix")) "defs" 0 "name") :to-equal "hit")
      (expect (json-get (nth-value 1 (dispatch-search ports "overview" "--no-ignore")) "ignore_source") :to-equal "none"))))
