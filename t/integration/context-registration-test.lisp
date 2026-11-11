(in-package #:aitools/test)

(defun %feature-presentation-context (system-name)
  (when (stringp system-name)
    (let* ((prefix "aitools/feature/")
           (suffix "/presentation")
           (suffix-start (- (length system-name) (length suffix))))
      (when (and (>= suffix-start (length prefix))
                 (string= prefix system-name :end2 (length prefix))
                 (string= suffix system-name :start2 suffix-start))
        (intern (string-upcase (subseq system-name (length prefix) suffix-start))
                :keyword)))))

(defun %declared-feature-presentation-contexts ()
  (loop for system-name in (asdf:system-depends-on
                            (asdf:find-system "aitools/cli"))
        for context = (%feature-presentation-context system-name)
        when context
          collect context))

(describe "Context test source attribution"
  (it "matches edit and environment tests loaded from a different root"
    (let* ((source-files (%context-test-source-files))
           (paths '("t/unit/edit/domain-test.lisp"
                    "t/integration/env-sysctl-test.lisp")))
      (aitools.test.support:call-with-temporary-directory
       (lambda (directory)
         (dolist (path paths)
           (let* ((context (if (search "edit/" path) :edit :env))
                  (source-file
                    (cdr (find-if (lambda (entry)
                                    (and (eq (car entry) context)
                                         (search path (cdr entry) :from-end t)))
                                  source-files))))
             (expect source-file :to-be-truthy)
             (when source-file
               (let* ((marker (search "/t/" source-file :from-end t))
                      (relative (subseq source-file (+ marker 3)))
                      (alternate (merge-pathnames
                                  (format nil "nix-store/source/t/~A" relative)
                                  directory)))
                 (aitools.test.support:write-text (namestring alternate) "")
                 (expect (%test-context-from-source-file
                          (namestring alternate) source-files)
                         :to-equal context))))))
       :prefix "aitools-context-source-alias"))))

(describe "Feature command registration"
  (it "registers every feature presentation in CLI dependency order"
    (let ((expected (list :search :inspect :edit :journal :process :org :env :md :util :net))
          (declared (%declared-feature-presentation-contexts))
          (actual
           (mapcar (function first)
                   (aitools/cli::context-command-registrations))))
      (expect declared :to-equal expected)
      (expect actual :to-equal expected)))
  (it "registers commands from each context dependency constructor"
    (let* ((registry (aitools.protocol.application:make-command-registry))
           (schemas
             (progn
               (aitools/cli:register-all-context-commands registry)
               (aitools.protocol.application:all-command-schemas registry))))
      (expect schemas :to-be-truthy)
      (let ((aitools/cli::*context-command-registrations* nil))
        (eval '(aitools/cli::define-context-command-registration
                :coverage-fixture #'identity #'identity))
        (expect (mapcar #'first (aitools/cli::context-command-registrations))
                :to-equal '(:coverage-fixture))
        (eval '(aitools/cli::define-context-command-registration
                :coverage-fixture #'car #'cdr))
        (let ((registration
                (first (aitools/cli::context-command-registrations))))
          (expect (first registration) :to-equal :coverage-fixture)
          (expect (eq (second registration) #'car) :to-be t)
          (expect (eq (third registration) #'cdr) :to-be t)))
      (let* ((with-span
               (getf (aitools/cli::%context-dependency-arguments) :with-span))
             (arguments (list 0 :payload)))
        (let ((aitools/cli::*trace-session* nil))
          (expect (multiple-value-list
                   (funcall with-span :io "coverage" #'values arguments))
                  :to-equal '(0 :payload)))
        (let ((aitools/cli::*trace-session* (aitools/cli::%make-trace-session)))
          (expect (multiple-value-list
                   (funcall with-span :io "coverage" #'values arguments))
                  :to-equal '(0 :payload)))))))
(describe "CLI composition coverage"
  (it "covers transaction insertion and non-object member lookup"
    (multiple-value-bind (app registry) (aitools/cli:build-app)
      (declare (ignore registry))
      (expect
       (aitools/cli::%with-tx
        app (list "--root=/tmp" "tx" "status") "tx-coverage")
       :to-equal
       (list "--root=/tmp" "tx" "status" "--tx" "tx-coverage"))
      (expect
       (aitools/cli::%with-tx
        app (list "unknown-command" "item") "tx-coverage")
       :to-equal (list "unknown-command" "item")))
    (expect (aitools/cli::%member "scalar" "field") :to-be nil))

  (it "formats a grouped repair command from parsed argv"
    (expect
     (aitools/cli::%group-repair-command
      (list "aitools" "--root" "/tmp" "get" "/a")
      "get" "org.todo")
     :to-equal "aitools --root /tmp org todo get /a"))

  (it "keeps deep command groups nested when finalizing a registry"
    (let ((registry (aitools.protocol.application:make-command-registry)))
      (aitools.protocol.application:register-command
       registry
       :name "a.b.c.leaf"
       :group "a.b.c"
       :cli-command (cl-cli:make-command :name "leaf")
       :schema nil)
      (let* ((root (first (aitools/cli:finalize-app-commands registry)))
             (child (first (cl-cli:command-subcommands root)))
             (grandchild (first (cl-cli:command-subcommands child)))
             (leaf (first (cl-cli:command-subcommands grandchild))))
        (expect (list (cl-cli:command-name root)
                      (cl-cli:command-name child)
                      (cl-cli:command-name grandchild)
                      (cl-cli:command-name leaf))
                :to-equal (list "a" "b" "c" "leaf")))))

  (it "handles an app invocation without a command through the app handler path"
    (let* ((app (cl-cli:make-app :name "aitools" :require-command nil))
           (registry (aitools.protocol.application:make-command-registry))
           (stdout (make-string-output-stream))
           (stderr (make-string-output-stream))
           (code (aitools/cli:dispatch app registry (list "aitools")
                                      :stdout stdout :stderr stderr))
           (envelope (json-kit:parse (get-output-stream-string stderr)))
           (error (gethash "error" envelope)))
      (expect code :to-be 1)
      (expect (gethash "command" envelope) :to-equal "aitools")
      (expect (search "requires a subcommand" (gethash "message" error))
              :to-be-truthy)))

  (it "writes a generic error envelope when no command name is available"
    (let ((stderr (make-string-output-stream)))
      (expect
       (aitools/cli::%write-error nil "argument.invalid" "invalid input" stderr)
       :to-be 1)
      (let* ((envelope (json-kit:parse (get-output-stream-string stderr)))
             (error (gethash "error" envelope)))
        (expect (gethash "command" envelope) :to-equal "aitools")
        (expect (gethash "code" error) :to-equal "argument.invalid")
        (expect (gethash "repairs" error) :to-be-truthy))))

  (it "evaluates entry point defaults without exiting the test process"
    (let ((aitools/cli::*application* nil)
          (*default-pathname-defaults* #p"/"))
      (let ((condition
              (handler-case
                  (progn (aitools/cli:image-entry-point) nil)
                (error (condition) condition))))
        (expect condition :to-be-truthy))))
  (it "passes argv, application, registry, and exit code through main"
    (let* ((app (cl-cli:make-app :name "coverage"))
           (registry (aitools.protocol.application:make-command-registry))
           (argv '("aitools" "version"))
           (dispatch-arguments nil)
           (quit-code nil)
           (aitools/cli::*application* (list app registry)))
      (expect
       (aitools/cli:main
        :argv argv
        :dispatch-function
        (lambda (actual-app actual-registry actual-argv)
          (setf dispatch-arguments
                (list actual-app actual-registry actual-argv))
          23)
        :quit-function
        (lambda (code)
          (setf quit-code code)
          :quit))
       :to-be :quit)
      (expect dispatch-arguments :to-equal (list app registry argv))
      (expect quit-code :to-equal 23)))

  (it "covers child process and owner error trace paths"
    (expect (aitools/cli::%trace-kind-name :child_process)
            :to-equal "child_process")
    (let* ((session (aitools/cli::%make-trace-session))
           (callback-condition nil))
      (let ((aitools/cli::*trace-session* session))
        (handler-case
            (aitools/cli::%call-with-trace-span
             :io "read"
             (lambda () (error "trace failure"))
             :condition-outcome-function
             (lambda (condition)
               (setf callback-condition condition)
               :timeout))
          (simple-error () nil)))
      (expect callback-condition :to-be-truthy)
      (let* ((record (first (aitools/cli::%trace-session-records session)))
             (span-id (observability-kit:span-record-span-id record)))
        (expect (observability-kit:span-record-status record) :to-be :error)
        (expect (gethash span-id (aitools/cli::%trace-session-outcomes session))
                :to-be :timeout)))
    (let ((session (aitools/cli::%make-trace-session)))
      (let ((aitools/cli::*trace-session* session)
            (aitools/cli::*trace-envelope-owner-p* t))
        (aitools/cli::%call-with-trace-span
         :command "root"
         (lambda ()
           (aitools/cli::%trace-envelope-meta :error)
           :ok)))
      (expect
       (observability-kit:span-record-status
        (first (aitools/cli::%trace-session-records session)))
       :to-be :error))
    (let ((session (aitools/cli::%make-trace-session)))
      (let ((aitools/cli::*trace-session* session))
        (aitools/cli::%call-with-trace-span :command "root" (lambda () :ok)))
      (let ((aitools/cli::*trace-session* session)
            (aitools/cli::*trace-envelope-owner-p* t))
        (expect (aitools/cli::%trace-envelope-meta :timeout)
                :to-be-truthy)))))
