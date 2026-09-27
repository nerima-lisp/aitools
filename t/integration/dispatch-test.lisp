;;;; t/integration/dispatch-test.lisp
;;;;
;;;; The composition root's DISPATCH (failure envelopes, exit codes, unknown names):
;;;; every failure, including a Lisp condition no command handled and a
;;;; recovery that cannot finish, answers with one JSON envelope naming the
;;;; command as typed and a repair that can be run as given. argv0 is an
;;;; absolute path, as it is for the installed binary, so a repair that
;;;; echoes it is caught.
;;;; Recovery that cannot finish is in dispatch-recovery-failure-test.lisp;
;;;; unrunnable invocations, --version/--help, and a failing standard output
;;;; are in dispatch-invocation-test.lisp.
(in-package #:cl-user)

(defpackage #:aitools.integration.dispatch-test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:expect)
  (:import-from #:aitools.store.test-support #:with-fault-at #:call-in-child-process #:kill-child))

(in-package #:aitools.integration.dispatch-test)

(defparameter +argv0+ "/nix/store/0000-aitools/bin/aitools")

(defvar *root* nil "The current test workspace's real root, no trailing slash.")

(defun call-with-dispatch-workspace (function)
  (let* ((base (sb-posix:mkdtemp (format nil "~A/aitools-dispatch-XXXXXX"
                                         (string-right-trim "/" (or (sb-posix:getenv "TMPDIR") "/tmp")))))
         (base (string-right-trim "/" (sb-ext:native-namestring (truename (concatenate 'string base "/")))))
         (previous (sb-posix:getenv "XDG_STATE_HOME")))
    (sb-posix:mkdir (concatenate 'string base "/work") #o755)
    (sb-posix:mkdir (concatenate 'string base "/state") #o755)
    (unwind-protect
         (let ((*root* (concatenate 'string base "/work")))
           (sb-posix:setenv "XDG_STATE_HOME" (concatenate 'string base "/state") 1)
           (funcall function))
      (if previous
          (sb-posix:setenv "XDG_STATE_HOME" previous 1)
          (sb-posix:unsetenv "XDG_STATE_HOME"))
      (uiop:delete-directory-tree (sb-ext:parse-native-namestring (concatenate 'string base "/"))
                                  :validate (lambda (path) (search "aitools-dispatch-" (namestring path)))
                                  :if-does-not-exist :ignore))))

(defmacro with-dispatch-workspace (() &body body)
  `(call-with-dispatch-workspace (lambda () ,@body)))

(defun disk (relative)
  (concatenate 'string *root* "/" relative))

(defun put (relative text)
  (with-open-file (out (sb-ext:parse-native-namestring (disk relative)) :direction :output :if-exists :supersede
                                                                        :external-format :utf-8)
    (write-string text out))
  relative)

(defun dispatch-envelope (app registry arguments)
  "(values exit-code envelope stream) of dispatching ARGUMENTS after argv0."
  (let* ((out (make-string-output-stream))
         (err (make-string-output-stream))
         (code (aitools/cli:dispatch app registry (cons +argv0+ arguments) :stdout out :stderr err))
         (stdout (get-output-stream-string out))
         (stderr (get-output-stream-string err)))
    (expect (zerop (length (if (plusp (length stdout)) stderr stdout))) :to-be t)
    (if (plusp (length stdout))
        (values code (json-kit:parse stdout) :stdout)
        (values code (json-kit:parse stderr) :stderr))))

(defun run-aitools (&rest arguments)
  "(values exit-code envelope stream) of `aitools --root <workspace> ARGUMENTS`."
  (multiple-value-bind (app registry) (aitools/cli:build-app)
    (dispatch-envelope app registry (list* "--root" *root* arguments))))

(defun run-plain (&rest arguments)
  "Like RUN-AITOOLS without the --root global."
  (multiple-value-bind (app registry) (aitools/cli:build-app)
    (dispatch-envelope app registry arguments)))

(defun value (object &rest keys)
  (reduce (lambda (value key)
            (cond ((null value) nil)
                  ((integerp key) (and (< key (length value)) (aref value key)))
                  (t (gethash key value))))
          keys :initial-value object))

(defun repair-commands (envelope)
  (map 'list (lambda (repair) (value repair "command")) (value envelope "error" "repairs")))

(defun run-repair (command)
  "Dispatch COMMAND, a repair's `aitools ...` line whose words need no
quoting, and return its exit code."
  (let ((words (uiop:split-string command :separator " ")))
    (expect (first words) :to-equal "aitools")
    (multiple-value-bind (app registry) (aitools/cli:build-app)
      (values (dispatch-envelope app registry (rest words))))))

(defun state-directory ()
  (aitools.store.application:store-state-directory (aitools.store.infrastructure:make-posix-store *root*)))

(defun crash-an-edit ()
  "Leave an unfinished write-protocol op behind: an edit that dies after its temp
file is prepared and before the commit point, which recovery discards."
  (put "a.txt" (format nil "one~%"))
  (expect (with-fault-at (:after-prepare) (run-aitools "edit" (disk "a.txt") "--old" "one" "--new" "two"))
          :to-be-truthy))

(defun faulting-app ()
  "An app whose commands fail with a Lisp condition no handler expects."
  (let ((globals (list (cl-cli:make-option :name "root" :kind :value)
                       (cl-cli:make-option :name "lock-timeout" :kind :value))))
    (values (cl-cli:make-app
             :name "aitools" :require-command t :global-options globals
             :commands (list (cl-cli:make-command :name "boom" :handler (lambda (invocation)
                                                                         (declare (ignore invocation))
                                                                         (error "kaput")))
                             (cl-cli:make-command :name "hog" :handler (lambda (invocation)
                                                                        (declare (ignore invocation))
                                                                        (error 'storage-condition)))
                             (cl-cli:make-command
                              :name "grp"
                              :subcommands (list (cl-cli:make-command :name "boom"
                                                                      :handler (lambda (invocation)
                                                                                 (declare (ignore invocation))
                                                                                 (error "kaput")))))))
            (aitools.protocol.application:make-command-registry))))

(describe "aitools dispatch: conditions no command handled"
  (it "names the failing command, as typed, in internal.unexpected and its schema repair"
    (with-dispatch-workspace ()
      (multiple-value-bind (app registry) (faulting-app)
        (multiple-value-bind (code envelope stream) (dispatch-envelope app registry (list "--root" *root* "boom"))
          (expect (list code stream) :to-equal '(1 :stderr))
          (expect (value envelope "command") :to-equal "boom")
          (expect (value envelope "error" "code") :to-equal "internal.unexpected")
          (expect (repair-commands envelope) :to-equal '("aitools schema boom")))
        (multiple-value-bind (code envelope) (dispatch-envelope app registry (list "--root" *root* "grp" "boom"))
          (expect code :to-be 1)
          (expect (value envelope "command") :to-equal "grp boom")
          (expect (repair-commands envelope) :to-equal '("aitools schema grp boom"))))))

  (it "answers heap or stack exhaustion with one environment envelope"
    (with-dispatch-workspace ()
      (multiple-value-bind (app registry) (faulting-app)
        (multiple-value-bind (code envelope stream) (dispatch-envelope app registry (list "--root" *root* "hog"))
          (expect (list code stream) :to-equal '(1 :stderr))
          (expect (value envelope "command") :to-equal "hog")
          (expect (value envelope "error" "code") :to-equal "environment.unavailable")
          (expect (repair-commands envelope) :to-equal '("aitools schema hog")))))))

(describe "aitools dispatch: store recovery before every command"
  (it "reports what it recovered on an error envelope too"
    (with-dispatch-workspace ()
      (crash-an-edit)
      (multiple-value-bind (code envelope) (run-aitools "read" (disk "missing.txt"))
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "input.not-found")
        (expect (length (value envelope "recovered")) :to-be 1)
        (expect (value envelope "recovered" 0 "action") :to-equal "discarded"))))

  (it "answers environment.io naming the blocking path when recovery itself fails"
    (with-dispatch-workspace ()
      (crash-an-edit)
      (let* ((commit (concatenate 'string (state-directory) "/commit"))
             (blocker (concatenate 'string commit "/00000000T000000Z-blocker.json")))
        ;; A record recovery cannot read: a directory where a file must be.
        (sb-posix:mkdir blocker #o755)
        (dotimes (attempt 2)
          (multiple-value-bind (code envelope stream) (run-aitools "read" (disk "a.txt"))
            (expect (list code stream) :to-equal '(1 :stderr))
            (expect (value envelope "command") :to-equal "read")
            (expect (value envelope "error" "code") :to-equal "environment.io")
            (expect (search blocker (value envelope "error" "message")) :to-be-truthy)
            (expect (repair-commands envelope) :to-equal (list (format nil "aitools info ~A" blocker))))))))

  (it "answers environment.busy with the same command, without argv0, when another process holds the lock"
    (with-dispatch-workspace ()
      (crash-an-edit)
      (let* ((store (aitools.store.infrastructure:make-posix-store *root*))
             (marker (disk "locked"))
             (pid (call-in-child-process
                   (lambda ()
                     (aitools.store.application:call-with-workspace-lock/k
                      store 1000
                      :on-acquired (lambda () (put "locked" "1") (sleep 30))
                      :on-timeout (lambda () (error "child could not take the lock")))))))
        (unwind-protect
             (progn
               (loop repeat 1000 until (probe-file marker) do (sleep 0.01))
               (expect (probe-file marker) :to-be-truthy)
               (multiple-value-bind (code envelope stream)
                   (run-aitools "--lock-timeout" "50ms" "read" (disk "a.txt"))
                 (expect (list code stream) :to-equal '(1 :stderr))
                 (expect (value envelope "command") :to-equal "read")
                 (expect (value envelope "error" "code") :to-equal "environment.busy")
                 (let ((repair (first (repair-commands envelope))))
                   (expect repair :to-equal (format nil "aitools --root ~A --lock-timeout 50ms read ~A"
                                                    *root* (disk "a.txt")))
                   (kill-child pid)
                   (setf pid nil)
                   (expect (run-repair repair) :to-be 0))))
          (when pid (kill-child pid)))))))

(describe "aitools dispatch: names and repairs as typed"
  (it "keeps the group in a subcommand's usage error"
    (with-dispatch-workspace ()
      (multiple-value-bind (code envelope) (run-aitools "bg" "logs")
        (expect code :to-be 1)
        (expect (value envelope "command") :to-equal "bg logs")
        (expect (repair-commands envelope) :to-equal '("aitools schema bg logs")))))

  (it "gives a group without its subcommand a repair that lists the group's subcommands"
    (with-dispatch-workspace ()
      (multiple-value-bind (code envelope) (run-aitools "json")
        (expect code :to-be 1)
        (expect (repair-commands envelope) :to-equal '("aitools schema json"))
        (multiple-value-bind (code listing) (run-plain "schema" "json")
          (expect code :to-be 0)
          (let ((names (map 'list (lambda (command) (value command "name")) (value listing "commands"))))
            (expect (and (member "json get" names :test #'string=) (member "json set" names :test #'string=)
                         t)
                    :to-be t)
            (expect (every (lambda (name) (eql 0 (search "json " name))) names) :to-be t))))))

  (it "lists schema's candidates in typed form"
    (multiple-value-bind (code envelope) (run-plain "schema" "nope")
      (expect code :to-be 1)
      (let ((candidates (coerce (value envelope "error" "candidates") 'list)))
        (expect (member "json get" candidates :test #'string=) :to-be-truthy)
        (expect (find #\. (format nil "~{~A~}" candidates)) :to-be nil))))

  (it "points a bare group subcommand name at every group form, keeping the operands"
    (with-dispatch-workspace ()
      (multiple-value-bind (code envelope) (run-aitools "get" "/a" "f.json")
        (expect code :to-be 1)
        (expect (value envelope "error" "code") :to-equal "argument.invalid")
        (expect (repair-commands envelope) :to-equal (list (format nil "aitools --root ~A json get /a f.json" *root*))))
      (multiple-value-bind (code envelope) (run-plain "status")
        (expect code :to-be 1)
        (expect (repair-commands envelope) :to-equal '("aitools bg status" "aitools git status" "aitools tx status")))
      (multiple-value-bind (code envelope) (run-plain "uuid")
        (expect code :to-be 1)
        (expect (repair-commands envelope) :to-equal '("aitools util uuid")))))

  ;; The correspondence table is a static hint (`cat` -> `aitools read`), so
  ;; the repair is the table's command verbatim, not the file the agent named;
  ;; the e2e meta suite pins each foreign name to the table this way.
  (it-each (("cat" ("f.txt") ("aitools read"))
            ("grep" ("x" "f.txt") ("aitools search")))
      "repairs the foreign name ~A with its correspondence command, without operands"
      (name args repairs)
    (multiple-value-bind (code envelope) (apply #'run-plain name args)
      (expect code :to-be 1)
      (expect (repair-commands envelope) :to-equal repairs)))

  (it "carries a bare group subcommand's operands over to its grouped form"
    ;; `get` is no foreign name, so its repair is the grouped form of the exact
    ;; command the agent meant, operands included.
    (multiple-value-bind (code envelope) (run-plain "get" "/a" "f.json")
      (expect code :to-be 1)
      (expect (member "aitools json get /a f.json" (repair-commands envelope) :test #'string=)
              :to-be-truthy))))

(describe "aitools dispatch: an envelope past the JSON writer's output limit"
  (it "writes nothing to standard output and answers refusal.too-large on standard error"
    (with-dispatch-workspace ()
      (let* ((huge (make-string (+ 16777216 10) :initial-element #\a))
             (app (cl-cli:make-app
                   :name "aitools" :require-command t
                   :global-options (list (cl-cli:make-option :name "root" :kind :value)
                                         (cl-cli:make-option :name "lock-timeout" :kind :value))
                   :commands (list (cl-cli:make-command
                                    :name "flood"
                                    :handler (lambda (invocation)
                                               (declare (ignore invocation))
                                               (aitools.protocol.application:call-with-command-result/k
                                                (lambda (&key on-ok on-partial on-error)
                                                  (declare (ignore on-partial on-error))
                                                  (funcall on-ok (list (cons "text" huge)))))))))))
        (multiple-value-bind (code envelope stream)
            (dispatch-envelope app (aitools.protocol.application:make-command-registry)
                               (list "--root" *root* "flood"))
          (expect (list code stream) :to-equal '(1 :stderr))
          (expect (value envelope "command") :to-equal "flood")
          (expect (value envelope "error" "code") :to-equal "refusal.too-large"))))))
