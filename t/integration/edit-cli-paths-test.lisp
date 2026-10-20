(in-package #:aitools.edit.test)

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
