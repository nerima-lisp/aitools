(in-package #:aitools.edit.test)

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
