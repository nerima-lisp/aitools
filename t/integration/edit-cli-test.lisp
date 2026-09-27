;;;; t/integration/edit-cli-test.lisp
;;;;
;;;; The edit commands end to end through the composition root: cl-cli
;;;; parsing of the options built from the command table, the production
;;;; ports, the envelope, and exit codes. XDG_STATE_HOME points at a
;;;; temporary directory so the journal never lands in the user's state.
(in-package #:aitools.edit.test)

(defun run-aitools (&rest arguments)
  "(values exit-code envelope) of `aitools --root <workspace> ARGUMENTS...`."
  (multiple-value-bind (app registry) (aitools/cli:build-app)
    (let* ((out (make-string-output-stream))
           (err (make-string-output-stream))
           (code (aitools/cli:dispatch app registry (list* "aitools" "--root" *root* arguments) :stdout out :stderr err))
           (stdout (get-output-stream-string out))
           (stderr (get-output-stream-string err)))
      (values code (json-kit:parse (if (plusp (length stdout)) stdout stderr))))))

(defun envelope-value (object &rest keys)
  (reduce (lambda (value key) (and value (gethash key value))) keys :initial-value object))

(defun call-with-cli-workspace (function)
  (with-workspace ()
    (let ((previous (sb-posix:getenv "XDG_STATE_HOME")))
      (sb-posix:setenv "XDG_STATE_HOME" *home* 1)
      (unwind-protect (funcall function)
        (if previous
            (sb-posix:setenv "XDG_STATE_HOME" previous 1)
            (sb-posix:unsetenv "XDG_STATE_HOME"))))))

(describe "aitools edit commands through dispatch"
  (it "edits a file and answers with changes and op_id, exit 0"
    (call-with-cli-workspace
     (lambda ()
       (put "a.txt" (format nil "hello~%"))
       (multiple-value-bind (code envelope) (run-aitools "edit" "a.txt" "--old" "hello" "--new" "bye")
         (expect code :to-be 0)
         (expect (envelope-value envelope "command") :to-equal "edit")
         (expect (envelope-value envelope "strategy") :to-equal "exact")
         (expect (stringp (envelope-value envelope "op_id")) :to-be t))
       (expect (text "a.txt") :to-equal (format nil "bye~%")))))

  (it "parses group commands, repeated options and two-value options"
    (call-with-cli-workspace
     (lambda ()
       (put "c.json" "{\"a\": 1}")
       (put "l.txt" (format nil "s~%x~%e~%"))
       (expect (run-aitools "json" "set" "c.json" "/b" "[1]") :to-be 0)
       (expect (text "c.json") :to-equal "{\"a\":1,\"b\":[1]}")
       (expect (run-aitools "write" "w.txt" "--content" "a" "--content" "b" "--separator" "+") :to-be 0)
       (expect (text "w.txt") :to-equal "a+b")
       (expect (run-aitools "edit" "l.txt" "--between" "^s$" "^e$" "--exclusive" "--new" "y") :to-be 0)
       (expect (text "l.txt") :to-equal (format nil "s~%y~%e~%")))))

  (it "exits 1 for argument errors and 2 for failed preconditions, writing nothing"
    (call-with-cli-workspace
     (lambda ()
       (put "a.txt" (format nil "x~%x~%"))
       (multiple-value-bind (code envelope) (run-aitools "edit" "a.txt" "--range" "1" "--new" "y")
         (expect code :to-be 1)
         (expect (envelope-value envelope "error" "code") :to-equal "argument.invalid"))
       (multiple-value-bind (code envelope) (run-aitools "edit" "a.txt" "--old" "x" "--new" "y")
         (expect code :to-be 2)
         (expect (envelope-value envelope "error" "code") :to-equal "selection.ambiguous")
         (expect (length (envelope-value envelope "error" "candidates")) :to-be 2))
       (expect (text "a.txt") :to-equal (format nil "x~%x~%")))))

  (it "publishes each command's options in schema"
    (call-with-cli-workspace
     (lambda ()
       (multiple-value-bind (code envelope) (run-aitools "schema" "replace")
         (expect code :to-be 0)
         (expect (search "\"--literal-replacement\"" (json-kit:stringify envelope)) :to-be-truthy))))))

(describe "the symlink hash rule shared by info and --expect-hash"
  (it "accepts info's hash as --expect-hash for a symlink to a file, to a directory, and a dangling one"
    (call-with-cli-workspace
     (lambda ()
       (put "target.txt" "t")
       (put "other.txt" "o")
       (sb-posix:mkdir (disk "dir") #o755)
       (sb-posix:symlink "target.txt" (disk "to-file"))
       (sb-posix:symlink "dir" (disk "to-dir"))
       (sb-posix:symlink "missing.txt" (disk "dangling"))
       (loop for (link expected) in (list (list "to-file" (hash "target.txt"))
                                          (list "to-dir" (aitools.kernel.domain:content-hash (bytes "dir")))
                                          (list "dangling" (aitools.kernel.domain:content-hash (bytes "missing.txt"))))
             do (multiple-value-bind (code envelope) (run-aitools "info" (disk link) "--allow-missing")
                  (expect code :to-be 0)
                  (expect (envelope-value envelope "hash") :to-equal expected)
                  (multiple-value-bind (code written)
                      (run-aitools "link" "other.txt" link "--overwrite"
                                   "--expect-hash" (format nil "~A=~A" link (envelope-value envelope "hash")))
                    (expect (list link code) :to-equal (list link 0))
                    (expect (stringp (envelope-value written "op_id")) :to-be t)))
                (expect (sb-posix:readlink (disk link)) :to-equal "other.txt"))))))

(defun call-in-directory (directory function)
  (let ((previous (sb-posix:getcwd)))
    (sb-posix:chdir directory)
    (unwind-protect (funcall function)
      (sb-posix:chdir previous))))

(describe "relative paths resolve against the working directory for every command"
  (it "reads and then edits the same file from a subdirectory"
    (call-with-cli-workspace
     (lambda ()
       (put "hello.lisp" (format nil "(root)~%"))
       (put "sub/hello.lisp" (format nil "(sub)~%"))
       (call-in-directory
        (disk "sub")
        (lambda ()
          (multiple-value-bind (code envelope) (run-aitools "read" "hello.lisp")
            (expect code :to-be 0)
            (expect (coerce (envelope-value envelope "lines") 'list) :to-equal '("(sub)")))
          (multiple-value-bind (code envelope) (run-aitools "edit" "hello.lisp" "--old" "(sub)" "--new" "(edited)")
            (expect code :to-be 0)
            ;; Output paths stay workspace-root-relative.
            (expect (envelope-value (aref (envelope-value envelope "changes") 0) "path") :to-equal "sub/hello.lisp"))
          (multiple-value-bind (code envelope) (run-aitools "info" "hello.lisp")
            (expect code :to-be 0)
            (expect (envelope-value envelope "hash") :to-equal (hash "sub/hello.lisp")))))
       (expect (text "sub/hello.lisp") :to-equal (format nil "(edited)~%"))
       (expect (text "hello.lisp") :to-equal (format nil "(root)~%")))))

  (it "uses --root only to select the workspace, never as the base of a relative path"
    (call-with-cli-workspace
     (lambda ()
       (put "hello.lisp" (format nil "(root)~%"))
       (put "sub/x.txt" "x")
       (call-in-directory
        (disk "sub")
        (lambda ()
          (expect (run-aitools "write" "new.txt" "--content" "n") :to-be 0)
          (multiple-value-bind (code envelope) (run-aitools "read" "new.txt")
            (expect code :to-be 0)
            (expect (coerce (envelope-value envelope "lines") 'list) :to-equal '("n")))))
       (expect (text "sub/new.txt") :to-equal "n")
       (expect (kind "new.txt") :to-be :absent)
       ;; From a directory outside the workspace, hello.lisp names a file
       ;; there, not the root's, and writing it is refused by the workspace boundary.
       (call-in-directory
        (concatenate 'string *root* "/..")
        (lambda ()
          (multiple-value-bind (code envelope) (run-aitools "edit" "hello.lisp" "--old" "(root)" "--new" "x")
            (expect code :to-be 1)
            (expect (envelope-value envelope "error" "code") :to-equal "refusal.outside-workspace"))))
       (expect (text "hello.lisp") :to-equal (format nil "(root)~%"))))))

(describe "mktemp with a state home behind a symlink"
  (it "returns a real path and its hash that write --overwrite --expect-hash accepts, and journals workspace writes"
    (with-workspace ()
      (let ((link (concatenate 'string *home* "-link"))
            (previous (sb-posix:getenv "XDG_STATE_HOME")))
        (sb-posix:mkdir *home* #o755)
        (sb-posix:symlink *home* link)
        (sb-posix:setenv "XDG_STATE_HOME" link 1)
        (unwind-protect
             (multiple-value-bind (code created) (run-aitools "mktemp" "--suffix" ".txt")
               (expect code :to-be 0)
               (let ((path (envelope-value created "path"))
                     (hash (envelope-value created "hash")))
                 (expect (search *home* path) :to-be 0)
                 (expect hash :to-equal (aitools.kernel.domain:content-hash (bytes "")))
                 (expect (envelope-value (nth-value 1 (run-aitools "info" path)) "hash") :to-equal hash)
                 (multiple-value-bind (code written)
                     (run-aitools "write" path "--content" "data" "--overwrite" "--expect-hash" hash)
                   (expect (list code (envelope-value written "status")) :to-equal '(0 "ok")))
                 (with-open-file (in path) (expect (read-line in) :to-equal "data")))
               ;; The journal's own state directories live behind the same link.
               (multiple-value-bind (code written) (run-aitools "write" "w.txt" "--content" "w")
                 (expect (list code (envelope-value written "status")) :to-equal '(0 "ok")))
               (expect (text "w.txt") :to-equal "w"))
          (if previous
              (sb-posix:setenv "XDG_STATE_HOME" previous 1)
              (sb-posix:unsetenv "XDG_STATE_HOME")))))))

(describe "aitools edit commands through dispatch: omitted positionals"
  (it "refuses a write with no path through the CLI, exit 1"
    (call-with-cli-workspace
     (lambda ()
       (multiple-value-bind (code envelope) (run-aitools "write" "--content" "x")
         (expect code :to-be 1)
         (expect (envelope-value envelope "error" "code") :to-equal "argument.invalid")
         (expect (envelope-value envelope "error" "message") :to-equal "write takes exactly one path"))))))

(describe "aitools edit commands through dispatch: rest positionals"
  (it "passes replace's pattern, replacement and paths through as one list"
    (call-with-cli-workspace
     (lambda ()
       (put "a.txt" (format nil "foo~%"))
       (put "b.txt" (format nil "foo~%"))
       (multiple-value-bind (code envelope) (run-aitools "replace" "foo" "bar" "a.txt" "b.txt" "--expect-count" "2")
         (expect code :to-be 0)
         (expect (length (envelope-value envelope "changes")) :to-be 2))
       (expect (text "b.txt") :to-equal (format nil "bar~%"))))))

(defun dispatch-envelope (argv)
  "(values exit-code envelope) of dispatching the full ARGV, argv0 included."
  (multiple-value-bind (app registry) (aitools/cli:build-app)
    (let* ((out (make-string-output-stream))
           (err (make-string-output-stream))
           (code (aitools/cli:dispatch app registry argv :stdout out :stderr err))
           (stdout (get-output-stream-string out))
           (stderr (get-output-stream-string err)))
      (values code (json-kit:parse (if (plusp (length stdout)) stdout stderr))))))

(defun run-aitools-as (argv0 &rest arguments)
  "RUN-AITOOLS with ARGV0 as the program name, as a shell passes the
absolute path of the binary it ran."
  (dispatch-envelope (list* argv0 "--root" *root* arguments)))

(defun first-repair-command (envelope)
  (envelope-value (elt (envelope-value envelope "error" "repairs") 0) "command"))

(defun typed-command-line (&rest words)
  "The runnable line a repair should show for WORDS typed after the program
name: `aitools --root <workspace> WORDS...`."
  (aitools.protocol.domain:command-line (list* "aitools" "--root" *root* words)))

(describe "aitools --expect-count through dispatch"
  (it "counts a --match or replace selection under --dry-run without --expect-count, writing nothing"
    (call-with-cli-workspace
     (lambda ()
       (put "dup.txt" (format nil "a~%a~%b a~%"))
       (multiple-value-bind (code envelope) (run-aitools "replace" "a" "Q" "dup.txt" "--dry-run")
         (expect code :to-be 0)
         (expect (envelope-value envelope "dry_run") :to-be t)
         (expect (envelope-value envelope "expect_count") :to-be 3))
       (multiple-value-bind (code envelope) (run-aitools "edit" "dup.txt" "--match" "^a$" "--new" "x" "--dry-run")
         (expect code :to-be 0)
         (expect (envelope-value envelope "expect_count") :to-be 2))
       (expect (text "dup.txt") :to-equal (format nil "a~%a~%b a~%"))
       (expect (run-aitools "replace" "a" "Q" "dup.txt" "--expect-count" "3") :to-be 0)
       (expect (text "dup.txt") :to-equal (format nil "Q~%Q~%b Q~%")))))

  (it "repairs a real write missing --expect-count with one runnable --dry-run of the typed command"
    (call-with-cli-workspace
     (lambda ()
       (put "dup.txt" (format nil "a~%a~%"))
       (multiple-value-bind (code envelope) (run-aitools-as "/opt/bin/aitools" "replace" "a" "Q" "dup.txt")
         (expect code :to-be 1)
         (expect (envelope-value envelope "error" "code") :to-equal "argument.invalid")
         (expect (first-repair-command envelope)
                 :to-equal (typed-command-line "replace" "a" "Q" "dup.txt" "--dry-run")))
       (multiple-value-bind (code envelope) (run-aitools-as "/opt/bin/aitools" "edit" "dup.txt" "--match" "a" "--new" "x")
         (expect code :to-be 1)
         (expect (first-repair-command envelope)
                 :to-equal (typed-command-line "edit" "dup.txt" "--match" "a" "--new" "x" "--dry-run")))
       (multiple-value-bind (code envelope) (run-aitools "replace" "a" "Q" "dup.txt" "--dry-run")
         (expect code :to-be 0)
         (expect (envelope-value envelope "expect_count") :to-be 2))
       (expect (text "dup.txt") :to-equal (format nil "a~%a~%")))))

  (it "repairs a count mismatch with the command minus its --expect-count, --dry-run once"
    (call-with-cli-workspace
     (lambda ()
       (put "dup.txt" (format nil "a~%a~%"))
       (dolist (arguments '(("replace" "a" "Q" "dup.txt" "--expect-count" "9")
                            ("replace" "a" "Q" "dup.txt" "--expect-count=9" "--dry-run")))
         (multiple-value-bind (code envelope) (apply #'run-aitools-as "/opt/bin/aitools" arguments)
           (expect code :to-be 2)
           (expect (envelope-value envelope "error" "code") :to-equal "selection.count-mismatch")
           (expect (first-repair-command envelope)
                   :to-equal (typed-command-line "replace" "a" "Q" "dup.txt" "--dry-run"))))
       (expect (text "dup.txt") :to-equal (format nil "a~%a~%"))))))

(describe "aitools --root named through a symlink"
  (it "maps paths and the working directory, both real, into the workspace for search, overview and archive create"
    ;; macOS: `--root /tmp/x` from the working directory /private/tmp/x.
    (call-with-cli-workspace
     (lambda ()
       (put "a.txt" (format nil "hello a~%"))
       (put "sub/b.txt" (format nil "hello b~%"))
       (let ((link (concatenate 'string *root* "-link")))
         (sb-posix:symlink *root* link)
         (unwind-protect
              (progn
                (multiple-value-bind (code envelope)
                    (dispatch-envelope (list "aitools" "--root" link "search" "hello" "a.txt" "sub"))
                  (expect code :to-be 0)
                  (expect (envelope-value envelope "total_matches") :to-be 2))
                (multiple-value-bind (code envelope)
                    (dispatch-envelope (list "aitools" "--root" link "archive" "create" "out.tar" "a.txt" "sub"))
                  (expect code :to-be 0)
                  (expect (envelope-value envelope "command") :to-equal "archive create"))
                (expect (probe-file (disk "out.tar")) :to-be-truthy)
                (multiple-value-bind (code envelope)
                    (dispatch-envelope (list "aitools" "--root" link "overview" "sub"))
                  (expect code :to-be 0)
                  (expect (envelope-value envelope "path") :to-equal "sub"))
                ;; No path: the scan starts at the working directory, which
                ;; is inside the workspace though only through its real path.
                (sb-posix:chdir (disk "sub"))
                (multiple-value-bind (code envelope)
                    (dispatch-envelope (list "aitools" "--root" link "search" "hello"))
                  (expect code :to-be 0)
                  (expect (envelope-value envelope "total_matches") :to-be 1)))
           (sb-posix:chdir *root*)
           (sb-posix:unlink link)))))))
