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
  (call-with-edit-workspace (lambda () (let ((previous (sb-posix:getenv "XDG_STATE_HOME")))
      (sb-posix:setenv "XDG_STATE_HOME" *home* 1)
      (unwind-protect (funcall function)
        (if previous
            (sb-posix:setenv "XDG_STATE_HOME" previous 1)
            (sb-posix:unsetenv "XDG_STATE_HOME")))))))

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
       (expect (run-aitools "doc" "set" "c.json" "/b" "[1]") :to-be 0)
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

(describe "aitools doc writes across formats"
  (it "routes every write verb through JSON, YAML, and TOML"
    (call-with-edit-workspace
     (lambda ()
       (dolist (format (list "json" "yaml" "toml"))
         (let ((path (format nil "route.~A" format))
               (source
                 (cond ((string= format "json") "{\"key\":\"old\",\"drop\":true}")
                       ((string= format "yaml") (format nil "key: old~%drop: true~%"))
                       (t (format nil "key = \"old\"~%drop = true~%")))))
           (put path source)
           (expect (run "doc.set" (list path "/key" "\"updated\"") :format format)
                   :to-be :ok)
           (expect (run "doc.delete" (list path "/drop") :format format)
                   :to-be :ok)
           (setf *stdin* (bytes "{\"added\":true}"))
           (expect (run "doc.merge" (list path) :format format :stdin t)
                   :to-be :ok)
           (setf *stdin*
                 (bytes "[{\"op\":\"replace\",\"path\":\"/key\",\"value\":\"patched\"}]"))
           (expect (run "doc.patch" (list path) :format format :stdin t)
                   :to-be :ok)
           (expect (run "doc.fmt" (list path) :format format) :to-be :ok)
           (expect (text path) :to-contain "patched"))))))

  (it "preserves format repairs and rejects format-only options outside JSON"
    (call-with-edit-workspace
     (lambda ()
       (put "options.yaml" (format nil "key: value~%"))
       (put "options.toml" (format nil "key = true~%"))
       (dolist (format (list "yaml" "toml"))
         (let ((path (format nil "options.~A" format)))
           (dolist (extra (list (list :indent "2")
                                (list :minify t)
                                (list :sort-keys t)))
             (with-error (code)
                 (apply (function run) "doc.fmt" (list path)
                        (append (list :format format) extra))
               (expect code :to-equal "argument.invalid")))))
       (put "options.json" "{}")
       (with-error (code) (run "doc.fmt" (list "options.json") :indent "17")
         (expect code :to-equal "argument.invalid"))
       (with-error (code)
           (run "doc.fmt" (list "options.json") :indent "2" :minify t)
         (expect code :to-equal "argument.invalid"))
       (with-error (code message keys)
           (run "doc.merge" (list "extensionless") :tx "tx-id" :dry-run t :stdin t)
         (expect code :to-equal "argument.invalid")
         (let ((command (first (repair-commands keys))))
           (expect (search "aitools doc merge" command) :to-be-truthy)
           (expect (search "--tx" command) :to-be-truthy)
           (expect (search "--dry-run" command) :to-be-truthy)
           (expect (search "--stdin" command) :to-be-truthy)
           (expect (search "--format" command) :to-be-truthy)))
       (with-error (code message)
           (run "doc.set" (list) :format "json")
         (expect code :to-equal "argument.invalid")
         (expect (search "needs PATH" message) :to-be-truthy)))))
  (it "preserves format repairs around positional separators and propagates stdin errors"
    (call-with-edit-workspace
     (lambda ()
       (dolist (document-format (list "yaml" "toml"))
         (let ((path (format nil "invalid.~A" document-format)))
           (put path (if (string= document-format "yaml") "value: 1" "value = 1"))
           (let ((before (snapshot)))
             (with-error (code)
                 (run "doc.set" (list path) :format document-format :stdin-data "{nope")
               (expect code :to-equal "input.syntax-error"))
             (expect (snapshot) :to-equal before))))
       (with-error (code message keys)
           (run "doc.merge" (list "-extensionless") :tx "tx-id" :dry-run t :stdin t)
         (expect code :to-equal "argument.invalid")
         (let ((command (first (repair-commands keys))))
           (expect (search "--format" command) :to-be-truthy)
           (expect (search "--tx tx-id" command) :to-be-truthy)
           (expect (search "--dry-run" command) :to-be-truthy)
           (expect (search "--stdin -- -extensionless" command) :to-be-truthy)))
       (with-error (code message keys)
           (run "doc.merge" (list "-extensionless") :tx "tx-id" :dry-run t :stdin-data "{}")
         (expect code :to-equal "argument.invalid")
         (let ((command (first (repair-commands keys))))
           (expect (search "--format" command) :to-be-truthy)
           (expect (search "--tx tx-id" command) :to-be-truthy)
           (expect (search "--dry-run" command) :to-be-truthy)
           (expect (search "--stdin -- -extensionless" command) :to-be-truthy))))))
  (it "repairs missing JSON pointers and files with doc get"
    (call-with-cli-workspace
     (lambda ()
       (put "source.json" "{\"parent\":{},\"key\":1}")
       (dolist (test-case
                (list (list (list "doc" "delete" "source.json" "/parent/missing")
                            "aitools doc get source.json /parent")
                      (list (list "doc" "fmt" "missing.json")
                            "aitools doc get missing.json")
                      (list (list "doc" "fmt" "missing.yaml" "--format" "yaml")
                            "aitools doc get missing.yaml --format yaml")
                      (list (list "doc" "fmt" "missing.toml" "--format" "toml")
                            "aitools doc get missing.toml --format toml")))
         (multiple-value-bind (code envelope)
             (apply (function run-aitools) (first test-case))
           (expect code :to-be 1)
           (expect (envelope-value envelope "error" "code")
                   :to-equal "input.not-found")
           (expect (search (second test-case)
                           (envelope-value
                            (aref (envelope-value envelope "error" "repairs") 0)
                            "command"))
                   :to-be-truthy)))))))
