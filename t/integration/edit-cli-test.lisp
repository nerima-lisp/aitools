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
  (with-edit-workspace ()
    (let ((previous (sb-posix:getenv "XDG_STATE_HOME")))
      (sb-posix:setenv "XDG_STATE_HOME" *home* 1)
      (unwind-protect (funcall function)
        (if previous
            (sb-posix:setenv "XDG_STATE_HOME" previous 1)
            (sb-posix:unsetenv "XDG_STATE_HOME"))))))

(describe "aitools edit commands through dispatch"
  (it "previews and removes a tree through rm's guarded CLI and restores it with undo"
    (call-with-cli-workspace
     (lambda ()
       (put "tree/data" (octet-vector 0 255 10))
       (let ((before (snapshot)))
         (multiple-value-bind (code envelope) (run-aitools "rm" "tree" "--recursive")
           (expect code :to-be 1)
           (expect (envelope-value envelope "error" "code") :to-equal "argument.invalid"))
         (multiple-value-bind (code envelope) (run-aitools "rm" "tree" "--recursive" "--dry-run")
           (expect code :to-be 0)
           (expect (envelope-value envelope "entries") :to-be 2)
           (expect (envelope-value envelope "expect_count") :to-be 2))
         (expect (snapshot) :to-equal before)
         (multiple-value-bind (code envelope)
             (run-aitools "rm" "tree" "--recursive" "--expect-count" "2" "--max-bytes" "3")
           (expect code :to-be 0)
           (expect (kind "tree") :to-be :absent)
           (expect (run-aitools "undo" (envelope-value envelope "op_id")) :to-be 0))
         (expect (snapshot) :to-equal before)))))

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

