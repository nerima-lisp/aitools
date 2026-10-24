;;;; t/integration/vcs-git-test.lisp
;;;;
;;;; `git`: the production port and the dispatched CLI
;;;; against throwaway repositories built with the real git. Fixture git
;;;; runs ignore the user's and system's git configuration; the commands
;;;; under test do not, as in production, and their pinned flags are what
;;;; keeps the output parseable. Skipped, with that reason, when git is not
;;;; on PATH (the Nix build sandbox, for one).
(in-package #:cl-user)

(defpackage #:aitools.vcs.integration-test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:expect #:describe-run-if)
  (:import-from #:aitools.test.support #:json-alist-value))

(in-package #:aitools.vcs.integration-test)
(defun git-available-p ()
  (ignore-errors
   (zerop (sb-ext:process-exit-code
           (sb-ext:run-program "git" '("--version") :search t :output nil :error nil)))))

(defparameter *fixture-environment*
  '(("GIT_CONFIG_GLOBAL" . "/dev/null") ("GIT_CONFIG_NOSYSTEM" . "1")
    ("GIT_AUTHOR_NAME" . "Ada Lovelace") ("GIT_AUTHOR_EMAIL" . "ada@example.com")
    ("GIT_COMMITTER_NAME" . "Ada Lovelace") ("GIT_COMMITTER_EMAIL" . "ada@example.com")))

(defun git (directory &rest arguments)
  "Run fixture git in DIRECTORY; signal unless it exits 0."
  (vcs-kit:run-git/checked (vcs-kit:make-repository directory) (first arguments) (rest arguments)
                           :environment-update *fixture-environment*))

(defun commit (directory message date)
  (let ((*fixture-environment* (append (list (cons "GIT_AUTHOR_DATE" date) (cons "GIT_COMMITTER_DATE" date))
                                       *fixture-environment*)))
    (git directory "commit" "-q" "--no-verify" "-m" message)))

(defun write-file (directory name content)
  (with-open-file (out (merge-pathnames name directory) :direction :output :if-exists :supersede
                                                          :external-format :utf-8)
    (write-string content out)))

(defun numbered-lines (count)
  (format nil "~{line ~D~%~}" (loop for n from 1 to count collect n)))

(defun build-fixture (directory)
  "Two commits of a.txt (100 lines; line 2 changed in the second), then a
staged new file, an unstaged edit of a.txt, and an untracked file."
  (git directory "init" "-q")
  (git directory "symbolic-ref" "HEAD" "refs/heads/main")
  (write-file directory "a.txt" (numbered-lines 100))
  (write-file directory "b.txt" (format nil "keep~%"))
  (git directory "add" "a.txt" "b.txt")
  (commit directory "first commit" "2024-01-02T03:04:05+09:00")
  (write-file directory "a.txt" (concatenate 'string (format nil "line 1~%line two~%")
                                             (subseq (numbered-lines 100) (length (format nil "line 1~%line 2~%")))))
  (git directory "add" "a.txt")
  (commit directory "second commit" "2024-02-03T04:05:06+00:00")
  (write-file directory "staged.txt" (format nil "new~%"))
  (git directory "add" "staged.txt")
  (write-file directory "a.txt" (concatenate 'string (numbered-lines 100) (format nil "tail~%")))
  (write-file directory "untracked.txt" (format nil "u~%")))

(defun run-flow (flow directory &rest arguments)
  "Run FLOW with the production port from DIRECTORY as the working directory."
  (let ((result (uiop:with-current-directory (directory)
                  (aitools.protocol.application:call-with-command-result/k
                   (lambda (&rest continuations)
                     (apply flow (aitools.vcs.infrastructure:make-production-vcs-ports)
                            (append arguments continuations)))))))
    (values (aitools.protocol.application:command-result-kind result)
            (aitools.protocol.application:command-result-fields result))))

(defun field (fields name &optional default)
  (let ((pair (assoc name fields :test #'string=)))
    (if pair (cdr pair) default)))

(defun dispatch-in (directory &rest arguments)
  "Run `aitools ARGUMENTS...` through the composition root with DIRECTORY as
the working directory. Returns (VALUES EXIT-CODE ENVELOPE) where ENVELOPE is
the parsed stdout, or stderr for an error."
  (uiop:with-current-directory (directory)
    (multiple-value-bind (app registry) (aitools/cli:build-app)
      (let* ((stdout (make-string-output-stream))
             (stderr (make-string-output-stream))
             (code (aitools/cli:dispatch app registry (cons "aitools" arguments)
                                         :stdout stdout :stderr stderr))
             (text (get-output-stream-string (if (= code 1) stderr stdout)))
             (envelope (json-kit:parse text :object-type :hash-table)))
        (expect envelope :to-match-envelope (list))
        (values code envelope)))))

(describe-run-if (git-available-p) "aitools git commands against a real repository"
  (it "git status splits staged, unstaged, and untracked paths"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (build-fixture directory)
      (multiple-value-bind (kind fields) (run-flow #'aitools.vcs.application:git-status/k directory)
        (expect kind :to-be :ok)
        (expect (field fields "branch") :to-equal "main")
        (expect (mapcar (lambda (item) (json-alist-value item "path")) (field fields "staged")) :to-equal '("staged.txt"))
        (expect (json-alist-value (first (field fields "staged")) "status") :to-equal "A")
        (expect (mapcar (lambda (item) (json-alist-value item "path")) (field fields "unstaged")) :to-equal '("a.txt"))
        (expect (field fields "untracked") :to-equal '("untracked.txt"))))
    :prefix "aitools-process"))

  (it "git log returns commits newest first and is partial past --limit"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (build-fixture directory)
      (multiple-value-bind (kind fields) (run-flow #'aitools.vcs.application:git-log/k directory :limit 1)
        (expect kind :to-be :partial)
        (expect (field fields "total") :to-equal 2)
        (let ((item (first (field fields "items"))))
          (expect (json-alist-value item "subject") :to-equal "second commit")
          (expect (json-alist-value item "author") :to-equal "Ada Lovelace")
          (expect (json-alist-value item "date") :to-equal "2024-02-03T04:05:06+00:00")
          (expect (length (json-alist-value item "sha")) :to-equal 40)))
      (multiple-value-bind (kind fields) (run-flow #'aitools.vcs.application:git-log/k directory :path "b.txt")
        (expect kind :to-be :ok)
        (expect (mapcar (lambda (item) (json-alist-value item "subject")) (field fields "items"))
                :to-equal '("first commit"))))
    :prefix "aitools-process"))

  (it "git diff gives hunks, stat counts, and summary files past --max-lines"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (build-fixture directory)
      (multiple-value-bind (kind fields) (run-flow #'aitools.vcs.application:git-diff/k directory)
        (expect kind :to-be :ok)
        (let ((file (first (field fields "files"))))
          (expect (json-alist-value file "path") :to-equal "a.txt")
          (expect (json-alist-value file "added") :to-equal 2)
          (expect (json-alist-value file "deleted") :to-equal 1)
          (expect (json-alist-value (first (json-alist-value file "hunks")) "lines")
                  :to-equal '(" line 1" "-line two" "+line 2" " line 3" " line 4" " line 5"))))
      (multiple-value-bind (kind fields)
          (run-flow #'aitools.vcs.application:git-diff/k directory :staged t :output :stat)
        (expect kind :to-be :ok)
        (expect (mapcar (lambda (file) (list (json-alist-value file "path") (json-alist-value file "mode")
                                             (json-alist-value file "added")))
                        (field fields "files"))
                :to-equal '(("staged.txt" "stat" 1))))
      (multiple-value-bind (kind fields) (run-flow #'aitools.vcs.application:git-diff/k directory :max-lines 3)
        (expect kind :to-be :partial)
        (expect (json-alist-value (first (field fields "files")) "mode") :to-equal "summary")
        (expect (field fields "next_commands") :to-equal '("aitools git diff a.txt --max-lines 12")))
      (multiple-value-bind (kind fields)
          (run-flow #'aitools.vcs.application:git-diff/k directory :ref "HEAD~1..HEAD" :path "a.txt")
        (expect kind :to-be :ok)
        (expect (json-alist-value (first (field fields "files")) "added") :to-equal 1)))
    :prefix "aitools-process"))

  (it "git blame attributes lines to their commits within --range"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (build-fixture directory)
      (multiple-value-bind (kind fields)
          (run-flow #'aitools.vcs.application:git-blame/k directory "a.txt" :range "1:2")
        (expect kind :to-be :ok)
        (let ((lines (field fields "lines")))
          (expect (mapcar (lambda (line) (json-alist-value line "text")) lines) :to-equal '("line 1" "line 2"))
          (expect (json-alist-value (first lines) "date") :to-equal "2024-01-02T03:04:05+09:00")
          ;; Line 2 differs from HEAD, so git attributes it to the work tree.
          (expect (json-alist-value (second lines) "sha") :to-equal (make-string 40 :initial-element #\0))))
      (multiple-value-bind (kind fields) (run-flow #'aitools.vcs.application:git-blame/k directory "a.txt")
        (expect kind :to-be :partial)
        (expect (field fields "total_lines") :to-equal 101)
        (expect (field fields "next_commands") :to-equal '("aitools git blame a.txt --range 81:101"))))
    :prefix "aitools-process"))

  (it "git show reads a committed blob with --range"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (build-fixture directory)
      (multiple-value-bind (kind fields)
          (run-flow #'aitools.vcs.application:git-show/k directory "HEAD:a.txt" :range "2:3")
        (expect kind :to-be :ok)
        (expect (field fields "lines") :to-equal '("line two" "line 3"))
        (expect (field fields "total_lines") :to-equal 100))
      (multiple-value-bind (kind fields) (run-flow #'aitools.vcs.application:git-show/k directory "HEAD:nope.txt")
        (expect kind :to-be :error)
        (expect (getf fields :code) :to-equal "input.not-found")))
    :prefix "aitools-process"))

  (it "reports environment.unavailable outside a repository"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (multiple-value-bind (kind fields) (run-flow #'aitools.vcs.application:git-status/k directory)
        (expect kind :to-be :error)
        (expect (getf fields :code) :to-equal "environment.unavailable")))
    :prefix "aitools-process"))

  (it "dispatches `aitools git ...` with aitools exit codes"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (build-fixture directory)
      (multiple-value-bind (code envelope) (dispatch-in directory "git" "log" "--limit" "5")
        (expect code :to-equal 0)
        (expect (gethash "command" envelope) :to-equal "git log")
        (expect (length (gethash "items" envelope)) :to-equal 2))
      (multiple-value-bind (code envelope) (dispatch-in directory "git" "diff" "--max-lines" "3")
        (expect code :to-equal 3)
        (expect (gethash "status" envelope) :to-equal "partial"))
      (multiple-value-bind (code envelope) (dispatch-in directory "git" "show" "HEAD:a.txt" "--range" "1")
        (expect code :to-equal 0)
        (expect (coerce (gethash "lines" envelope) 'list) :to-equal '("line 1")))
      (multiple-value-bind (code envelope) (dispatch-in directory "git" "blame" "a.txt" "--match" "^line 10")
        (expect code :to-equal 0)
        (expect (map 'list (lambda (line) (gethash "n" line)) (gethash "lines" envelope))
                :to-equal '(10 100))))
    :prefix "aitools-process")
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (multiple-value-bind (code envelope) (dispatch-in directory "git" "status")
        (expect code :to-equal 1)
        (expect (gethash "code" (gethash "error" envelope)) :to-equal "environment.unavailable")))
    :prefix "aitools-process"))

  (it "resolves path arguments against the working directory, not the repository top"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (build-fixture directory)
      (let ((subdirectory (merge-pathnames "sub/" directory)))
        (ensure-directories-exist subdirectory)
        (multiple-value-bind (code envelope) (dispatch-in subdirectory "git" "log" "../b.txt")
          (expect code :to-equal 0)
          (expect (gethash "path" envelope) :to-equal "b.txt")
          (expect (gethash "total" envelope) :to-equal 1))
        (multiple-value-bind (code envelope) (dispatch-in subdirectory "git" "show" "HEAD:../b.txt")
          (expect code :to-equal 0)
          (expect (gethash "path" envelope) :to-equal "b.txt")
          (expect (coerce (gethash "lines" envelope) 'list) :to-equal '("keep")))
        (multiple-value-bind (code envelope) (dispatch-in subdirectory "git" "blame" "../a.txt" "--range" "1")
          (expect code :to-equal 0)
          (expect (gethash "path" envelope) :to-equal "a.txt"))))
    :prefix "aitools-process"))

  (it "runs git in the global --root directory"
    (aitools.test.support:call-with-temporary-directory (lambda (repository)
      (build-fixture repository)
      (aitools.test.support:call-with-temporary-directory (lambda (elsewhere)
        (multiple-value-bind (code envelope)
            (dispatch-in elsewhere "--root" (uiop:native-namestring repository) "git" "log")
          (expect code :to-equal 0)
          (expect (gethash "total" envelope) :to-equal 2))
        (multiple-value-bind (code envelope) (dispatch-in elsewhere "--root" "missing-dir" "git" "status")
          (expect code :to-equal 1)
          (expect (gethash "code" (gethash "error" envelope)) :to-equal "input.not-found")))
      :prefix "aitools-process"))
    :prefix "aitools-process")))

