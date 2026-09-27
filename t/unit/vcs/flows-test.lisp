;;;; t/unit/vcs/flows-test.lisp
;;;;
;;;; The git group's flows against a GIT-PORT built from closures over canned
;;;; git output, so every exit (ok, partial, each error code) is reachable
;;;; without a repository.
;;;; --root, selectors, relative paths, failure kinds, and argument edge
;;;; cases are in flows-options-test.lisp.
(in-package #:aitools.vcs.test)

(defun fake-port (&key (probe :repository) runs status numstat calls (top "/repo") (directory "/repo"))
  "A GIT-PORT answering from canned data. RUNS maps a subcommand to
(:OK output) or (:FAIL kind message). STATUS and NUMSTAT are the success
values of those slots, or (:FAIL kind message). CALLS, when given, is a
cons whose car collects (SUBCOMMAND . ARGUMENTS) for every RUN and NUMSTAT,
and (\"at\" . ROOT) for every AT, which returns the same port."
  (flet ((answer (response on-success on-failure)
           (if (and (consp response) (eq (first response) :fail))
               (funcall on-failure (second response) (third response))
               (funcall on-success response))))
    (let ((port nil))
      (setf port
            (make-git-port
             :probe (lambda (&key on-repository on-outside on-missing on-no-directory)
                      (ecase probe
                        (:repository (funcall on-repository top directory))
                        (:outside (funcall on-outside))
                        (:missing (funcall on-missing))
                        (:no-directory (funcall on-no-directory "/nowhere/"))))
             :at (lambda (root)
                   (when calls (push (cons "at" root) (car calls)))
                   port)
             :run (lambda (subcommand arguments &key octets on-success on-failure)
                    (declare (ignore octets))
                    (when calls (push (cons subcommand arguments) (car calls)))
                    (let ((response (cdr (assoc subcommand runs :test #'string=))))
                      (answer (if (eq (first response) :ok) (second response) response)
                              on-success on-failure)))
             :status (lambda (&key on-success on-failure) (answer status on-success on-failure))
             :numstat (lambda (arguments &key on-success on-failure)
                        (when calls (push (cons "numstat" arguments) (car calls)))
                        (answer numstat on-success on-failure)))))))

(defun run-flow (flow &rest arguments)
  "(VALUES KIND FIELDS) of the COMMAND-RESULT FLOW produces."
  (let ((result (aitools.protocol.application:call-with-command-result/k
                 (lambda (&rest continuations) (apply flow (append arguments continuations))))))
    (values (aitools.protocol.application:command-result-kind result)
            (aitools.protocol.application:command-result-fields result))))

(defun field (fields name &optional default)
  (let ((pair (assoc name fields :test #'string=)))
    (if pair (cdr pair) default)))

(defun first-repair-command (fields)
  (getf (first (getf fields :repairs)) :command))

(defun log-output (&rest commits)
  "`git log -z` text for COMMITS, each (SHA SUBJECT)."
  (apply #'nul-join (loop for (sha subject) in commits
                          append (list sha "Ada" "1704132245" "+0900" subject))))

(defun blame-output (line-count)
  (with-output-to-string (out)
    (loop for n from 1 to line-count
          do (format out "~A ~D ~D~@[ 1~]~%" (make-string 40 :initial-element #\a) n n (= n 1))
             (when (= n 1)
               (format out "author Ada~%author-time 1704132245~%author-tz +0900~%filename a.txt~%"))
             (format out "~Cline ~D~%" #\Tab n))))

(describe "aitools.vcs.application repository preconditions"
  (it "reports environment.unavailable outside a work tree, with a sys tools repair"
    (multiple-value-bind (kind fields) (run-flow #'git-status/k (fake-port :probe :outside))
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "environment.unavailable")
      (expect (first-repair-command fields) :to-equal "aitools sys tools git")))
  (it "reports environment.unavailable when git cannot start"
    (multiple-value-bind (kind fields) (run-flow #'git-log/k (fake-port :probe :missing))
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "environment.unavailable")))
  (it "reports a timeout or I/O failure as environment.io"
    (multiple-value-bind (kind fields) (run-flow #'git-status/k (fake-port :status '(:fail :failed "timed out")))
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "environment.io"))))

(describe "aitools.vcs.application git-status/k"
  (it "returns the status fields"
    (multiple-value-bind (kind fields)
        (run-flow #'git-status/k (fake-port :status (list :branch "main"
                                                          :entries (list (list :kind :untracked :path "n.txt")))))
      (expect kind :to-be :ok)
      (expect (field fields "branch") :to-equal "main")
      (expect (field fields "untracked") :to-equal '("n.txt"))
      (expect (field fields "redactions" :absent) :to-be :absent))))

(describe "aitools.vcs.application git-log/k"
  (it "returns no history on an unborn branch"
    (multiple-value-bind (kind fields)
        (run-flow #'git-log/k (fake-port :runs '(("rev-parse" :fail :exit "fatal: Needed a single revision"))))
      (expect kind :to-be :ok)
      (expect (field fields "items") :to-equal nil)
      (expect (field fields "total") :to-equal 0)
      (expect (json-kit:json-false-p (field fields "truncated")) :to-be-truthy)))

  (it "is partial with a larger --limit next command when more commits exist"
    (let ((calls (list nil)))
      (multiple-value-bind (kind fields)
          (run-flow #'git-log/k
                    (fake-port :runs `(("rev-parse" :ok "abc") ("rev-list" :ok ,(format nil "3~%"))
                                       ("log" :ok ,(log-output '("c3" "third") '("c2" "second"))))
                               :calls calls)
                    :path "a b" :limit 2)
        (expect kind :to-be :partial)
        (expect (field fields "path") :to-equal "a b")
        (expect (mapcar (lambda (item) (json-alist-value item "sha")) (field fields "items")) :to-equal '("c3" "c2"))
        (expect (field fields "total") :to-equal 3)
        (expect (field fields "truncated") :to-be t)
        (expect (field fields "next_commands") :to-equal '("aitools git log 'a b' --limit 3"))
        (let ((log-arguments (cdr (assoc "log" (car calls) :test #'string=))))
          (expect log-arguments :to-contain "-z")
          (expect (subseq log-arguments (- (length log-arguments) 4)) :to-equal '("-n" "2" "--" "a b"))))))

  (it "masks secrets in commit subjects and counts them"
    (multiple-value-bind (kind fields)
        (run-flow #'git-log/k
                  (fake-port :runs `(("rev-parse" :ok "abc") ("rev-list" :ok "1")
                                     ("log" :ok ,(log-output '("c1" "set password=hunter2"))))))
      (expect kind :to-be :ok)
      (expect (json-alist-value (first (field fields "items")) "subject") :to-equal "set password=[REDACTED_SECRET]")
      (expect (field fields "redactions") :to-equal 1))))

(defparameter *flow-patch*
  (lines-text "diff --git a/a b/a" "--- a/a" "+++ b/a" "@@ -0,0 +1 @@" "+1"
              "diff --git a/b b/b" "--- a/b" "+++ b/b" "@@ -0,0 +1,3 @@" "+1" "+2" "+3"))

(describe "aitools.vcs.application git-diff/k"
  (it "summarizes files past --max-lines and continues with a command sized for the first"
    (let ((calls (list nil)))
      (multiple-value-bind (kind fields)
          (run-flow #'git-diff/k
                    (fake-port :numstat (list (list :path "a" :added 1 :deleted 0) (list :path "b" :added 3 :deleted 0))
                               :runs `(("diff" :ok ,*flow-patch*))
                               :calls calls)
                    :staged t :max-lines 3)
        (expect kind :to-be :partial)
        (expect (field fields "mode") :to-equal "hunks")
        (expect (mapcar (lambda (file) (json-alist-value file "mode")) (field fields "files"))
                :to-equal '("hunks" "summary"))
        (expect (field fields "approx_tokens") :to-equal 1)
        (expect (field fields "next_commands") :to-equal '("aitools git diff b --staged --max-lines 4"))
        (expect (cdr (assoc "numstat" (car calls) :test #'string=)) :to-contain "--cached"))))

  (it "does not run the patch in stat mode"
    (let ((calls (list nil)))
      (multiple-value-bind (kind fields)
          (run-flow #'git-diff/k (fake-port :numstat (list (list :path "a" :added 1 :deleted 0)) :calls calls)
                    :output :stat :ref "HEAD~1..HEAD")
        (expect kind :to-be :ok)
        (expect (field fields "mode") :to-equal "stat")
        (expect (assoc "diff" (car calls) :test #'string=) :to-be nil)
        (expect (cdr (assoc "numstat" (car calls) :test #'string=)) :to-contain "HEAD~1..HEAD"))))

  (it "asks for a rerun when the numstat and patch file counts differ"
    (multiple-value-bind (kind fields)
        (run-flow #'git-diff/k (fake-port :numstat (list (list :path "a" :added 1 :deleted 0))
                                          :runs `(("diff" :ok ,*flow-patch*))))
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "environment.io")
      (expect (first-repair-command fields) :to-equal "aitools git diff --max-lines 400")))

  (it "rejects an option-like --ref"
    (multiple-value-bind (kind fields) (run-flow #'git-diff/k (fake-port) :ref "--output=/tmp/x")
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "argument.invalid")))

  (it "reports an unknown revision as input.not-found"
    (multiple-value-bind (kind fields)
        (run-flow #'git-diff/k (fake-port :numstat '(:fail :exit "fatal: bad revision 'nope'")) :ref "nope")
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "input.not-found")
      (expect (getf fields :message) :to-contain "nope"))))

(describe "aitools.vcs.application git-blame/k"
  (it "returns the first 80 lines by default and continues with --range"
    (multiple-value-bind (kind fields)
        (run-flow #'git-blame/k (fake-port :runs `(("blame" :ok ,(blame-output 100)))) "a.txt")
      (expect kind :to-be :partial)
      (expect (field fields "start_line") :to-equal 1)
      (expect (length (field fields "lines")) :to-equal 80)
      (expect (json-alist-value (first (field fields "lines")) "text") :to-equal "line 1")
      (expect (field fields "total_lines") :to-equal 100)
      (expect (field fields "next_commands") :to-equal '("aitools git blame a.txt --range 81:100"))))

  (it "returns an explicit range whole"
    (multiple-value-bind (kind fields)
        (run-flow #'git-blame/k (fake-port :runs `(("blame" :ok ,(blame-output 100)))) "a.txt" :range "90:")
      (expect kind :to-be :ok)
      (expect (mapcar (lambda (line) (json-alist-value line "n")) (field fields "lines"))
              :to-equal '(90 91 92 93 94 95 96 97 98 99 100))
      (expect (json-alist-value (first (field fields "lines")) "date") :to-equal "2024-01-02T03:04:05+09:00")))

  (it "reports a range past the end as selection.no-match"
    (multiple-value-bind (kind fields)
        (run-flow #'git-blame/k (fake-port :runs `(("blame" :ok ,(blame-output 3)))) "a.txt" :range "5:9")
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "selection.no-match")
      (expect (first-repair-command fields) :to-equal "aitools git blame a.txt --range 1:3")))

  (it "rejects a malformed --range before running git"
    (let ((calls (list nil)))
      (multiple-value-bind (kind fields)
          (run-flow #'git-blame/k (fake-port :calls calls) "a.txt" :range "9:2")
        (expect kind :to-be :error)
        (expect (getf fields :code) :to-equal "argument.invalid")
        (expect (car calls) :to-equal nil))))

  (it "reports an untracked path as input.not-found"
    (multiple-value-bind (kind fields)
        (run-flow #'git-blame/k (fake-port :runs '(("blame" :fail :exit "fatal: no such path 'n.txt' in HEAD")))
                  "n.txt")
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "input.not-found"))))

(describe "aitools.vcs.application git-show/k"
  (it "returns read's shape for a text blob, windowed by --range and --max-lines"
    (multiple-value-bind (kind fields)
        (run-flow #'git-show/k (fake-port :runs `(("cat-file" :ok ,(string-bytes (lines-text "a" "b" "c" "d" "e")))))
                  "HEAD:x.txt" :range "2:5" :max-lines 2)
      (expect kind :to-be :partial)
      (expect (field fields "rev") :to-equal "HEAD")
      (expect (field fields "path") :to-equal "x.txt")
      (expect (field fields "start_line") :to-equal 2)
      (expect (field fields "lines") :to-equal '("b" "c"))
      (expect (field fields "total_lines") :to-equal 5)
      (expect (field fields "hash")
              :to-equal (aitools.kernel.domain:content-hash (string-bytes (lines-text "a" "b" "c" "d" "e"))))
      (expect (field fields "approx_tokens") :to-equal 1)
      (expect (field fields "next_commands") :to-equal '("aitools git show HEAD:x.txt --range 4:5"))))

  (it "applies the default --max-lines when no range is given"
    (multiple-value-bind (kind fields)
        (run-flow #'git-show/k (fake-port :runs `(("cat-file" :ok ,(string-bytes (lines-text "a" "b")))))
                  "HEAD:x.txt")
      (expect kind :to-be :ok)
      (expect (json-kit:json-false-p (field fields "truncated")) :to-be-truthy)
      (expect (field fields "encoding_errors") :to-equal 0)))

  (it "describes a binary blob without lines"
    (multiple-value-bind (kind fields)
        (run-flow #'git-show/k (fake-port :runs `(("cat-file" :ok ,(octets 1 0 2)))) "HEAD:b.bin")
      (expect kind :to-be :ok)
      (expect (field fields "binary") :to-be t)
      (expect (field fields "size") :to-equal 3)
      (expect (field fields "mime") :to-equal "application/octet-stream")
      (expect (field fields "lines" :absent) :to-be :absent)))

  (it "rejects an object name without <rev>:<path>"
    (multiple-value-bind (kind fields) (run-flow #'git-show/k (fake-port) "README.md")
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "argument.invalid")
      (expect (first-repair-command fields) :to-equal "aitools git show HEAD:README.md")))

  (it "reports a missing revision or path as input.not-found with a log repair"
    (multiple-value-bind (kind fields)
        (run-flow #'git-show/k (fake-port :runs '(("cat-file" :fail :exit "fatal: path 'm' does not exist in 'HEAD'")))
                  "HEAD:m")
      (expect kind :to-be :error)
      (expect (getf fields :code) :to-equal "input.not-found")
      (expect (first-repair-command fields) :to-equal "aitools git log m"))))
