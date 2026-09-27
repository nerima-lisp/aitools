;;;; t/integration/dispatch-invocation-test.lisp
;;;;
;;;; DISPATCH for invocations that name no runnable command, `--version` and
;;;; `--help`, and a standard output that fails. Helpers come from
;;;; dispatch-test.lisp.
(in-package #:aitools.integration.dispatch-test)

(describe "aitools dispatch: invocations that name no runnable command"
  (it-each ((())
            (("--root" "/tmp")))
      "answers argv ~S with no command given and the schema listing repair"
      (arguments)
    (multiple-value-bind (code envelope stream) (apply #'run-plain arguments)
      (expect (list code stream) :to-equal '(1 :stderr))
      (expect (value envelope "command") :to-equal "aitools")
      (expect (value envelope "error" "code") :to-equal "argument.invalid")
      (expect (value envelope "error" "message") :to-equal "no command given")
      (expect (repair-commands envelope) :to-equal '("aitools schema"))))

  (it "points a name that is neither a foreign command nor a group subcommand at the command listing"
    (multiple-value-bind (code envelope) (run-plain "frobnicate" "x")
      (expect code :to-be 1)
      (expect (value envelope "command") :to-equal "frobnicate")
      (expect (value envelope "error" "message") :to-equal "unknown command frobnicate")
      (expect (value envelope "error" "repairs" 0 "action") :to-equal "browse-commands")
      (expect (repair-commands envelope) :to-equal '("aitools schema"))))

  (it "names a top-level command, without a group, in its usage error"
    (multiple-value-bind (code envelope) (run-plain "read" "--nope" "a.txt")
      (expect code :to-be 1)
      (expect (value envelope "command") :to-equal "read")
      (expect (value envelope "error" "code") :to-equal "argument.invalid")
      (expect (search "--nope" (value envelope "error" "message")) :to-be-truthy)
      (expect (repair-commands envelope) :to-equal '("aitools schema read")))))

(describe "aitools dispatch: --version and --help answer with JSON envelopes"
  (it "answers --version with the app's name and version"
    (multiple-value-bind (app registry) (aitools/cli:build-app)
      (multiple-value-bind (code envelope stream) (dispatch-envelope app registry '("--version"))
        (expect (list code stream) :to-equal '(0 :stdout))
        (expect (value envelope "status") :to-equal "ok")
        (expect (value envelope "command") :to-equal "version")
        (expect (value envelope "name") :to-equal "aitools")
        (expect (stringp (cl-cli:app-version app)) :to-be t)
        (expect (value envelope "version") :to-equal (cl-cli:app-version app)))))

  (it "answers --help with the same command listing as schema"
    (multiple-value-bind (code help stream) (run-plain "--help")
      (expect (list code stream) :to-equal '(0 :stdout))
      (expect (value help "command") :to-equal "schema")
      (let ((names (map 'list (lambda (command) (value command "name")) (value help "commands"))))
        (expect (and (member "read" names :test #'string=) (member "batch" names :test #'string=) t) :to-be t)
        (expect names :to-equal (map 'list (lambda (command) (value command "name"))
                                     (value (nth-value 1 (run-plain "schema")) "commands"))))))

  (it "answers <command> --help with that command's schema detail"
    (multiple-value-bind (code help stream) (run-plain "read" "--help")
      (expect (list code stream) :to-equal '(0 :stdout))
      (expect (value help "name") :to-equal "read")
      (expect (json-kit:stringify help)
              :to-equal (json-kit:stringify (value (nth-value 1 (run-plain "schema" "read")) "commands" 0))))))

(describe "aitools dispatch: a standard output that fails"
  (it-each ((("--version") "aitools" "aitools schema")
            (("util" "uuid") "util uuid" "aitools schema util uuid"))
      "answers ~S with one internal.unexpected envelope on standard error"
      (arguments command repair)
    (with-dispatch-workspace ()
      (multiple-value-bind (app registry) (aitools/cli:build-app)
        (let ((out (make-string-output-stream))
              (err (make-string-output-stream)))
          (close out)
          (let* ((code (aitools/cli:dispatch app registry (list* +argv0+ "--root" *root* arguments)
                                             :stdout out :stderr err))
                 (envelope (json-kit:parse (get-output-stream-string err))))
            (expect code :to-be 1)
            (expect (value envelope "command") :to-equal command)
            (expect (value envelope "error" "code") :to-equal "internal.unexpected")
            (expect (repair-commands envelope) :to-equal (list repair))))))))
