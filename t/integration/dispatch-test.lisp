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
  (:import-from #:cl-weave #:it #:it-each #:expect #:fail)
  (:import-from #:aitools.store.test-support #:call-with-fault-at #:call-in-child-process #:kill-child))

(in-package #:aitools.integration.dispatch-test)

(defparameter +argv0+ "/nix/store/0000-aitools/bin/aitools")

(defvar *root* nil "The current test workspace's real root, no trailing slash.")

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
  (expect (call-with-fault-at :after-prepare (lambda () (run-aitools "edit" (disk "a.txt") "--old" "one" "--new" "two")))
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
    (aitools.test.support:with-workspace
      (*root* state-home)
      (declare (ignore state-home))
      (multiple-value-bind (app registry) (faulting-app)
        (multiple-value-bind (code envelope stream) (dispatch-envelope app registry (list "--root" *root* "boom"))
          (expect (list code stream) :to-equal '(1 :stderr))
          (expect envelope :to-match-envelope "command" "boom" "status" "error")
          (expect (value envelope "error" "code") :to-equal "internal.unexpected")
          (expect (repair-commands envelope) :to-equal '("aitools schema boom")))
        (multiple-value-bind (code envelope) (dispatch-envelope app registry (list "--root" *root* "grp" "boom"))
          (expect code :to-be 1)
          (expect envelope :to-match-envelope "command" "grp boom" "status" "error")
          (expect (repair-commands envelope) :to-equal '("aitools schema grp boom"))))))

  (it "answers heap or stack exhaustion with one environment envelope"
    (aitools.test.support:with-workspace
      (*root* state-home)
      (declare (ignore state-home))
      (multiple-value-bind (app registry) (faulting-app)
        (multiple-value-bind (code envelope stream) (dispatch-envelope app registry (list "--root" *root* "hog"))
          (expect (list code stream) :to-equal '(1 :stderr))
          (expect envelope :to-match-envelope "command" "hog" "status" "error")
          (expect (value envelope "error" "code") :to-equal "environment.unavailable")
          (expect (repair-commands envelope) :to-equal '("aitools schema hog")))))))

(describe "aitools dispatch: names and repairs as typed"
  (it "keeps the group in a subcommand's usage error"
    (aitools.test.support:with-workspace
      (*root* state-home)
      (declare (ignore state-home))
      (multiple-value-bind (code envelope) (run-aitools "bg" "logs")
        (expect code :to-be 1)
        (expect envelope :to-match-envelope "command" "bg logs" "status" "error")
        (expect (repair-commands envelope) :to-equal '("aitools schema bg logs")))))

  (it "gives a group without its subcommand a repair that lists the group's subcommands"
    (aitools.test.support:with-workspace
      (*root* state-home)
      (declare (ignore state-home))
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
    (aitools.test.support:with-workspace
      (*root* state-home)
      (declare (ignore state-home))
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
          (expect envelope :to-match-envelope "command" "flood" "status" "error")
          (expect (value envelope "error" "code") :to-equal "refusal.too-large"))))))
(describe "aitools schema --all"
  (it "returns full details for every registered command"
    (aitools.test.support:with-workspace
      (*root* state-home)
      (declare (ignore state-home))
      (multiple-value-bind (summary-code summary summary-stream)
          (run-aitools "schema")
        (expect (list summary-code summary-stream) :to-equal (list 0 :stdout))
        (let ((summary-names
                (map 'list (lambda (command) (value command "name"))
                     (value summary "commands"))))
          (multiple-value-bind (all-code all all-stream)
              (run-aitools "schema" "--all")
            (expect (list all-code all-stream) :to-equal (list 0 :stdout))
            (let* ((commands (value all "commands"))
                   (all-names
                     (map 'list (lambda (command) (value command "name")) commands)))
              (expect (some (lambda (name) (string= "read" name)) all-names)
                      :to-be-truthy)
              (expect all-names :to-equal summary-names)
              (expect (every (lambda (command)
                               (and (stringp (value command "name"))
                                    (stringp (value command "summary"))
                                    (stringp (value command "description"))
                                    (vectorp (value command "args"))
                                    (vectorp (value command "output_fields"))
                                    (vectorp (value command "error_codes"))))
                             commands)
                      :to-be t))))))))
