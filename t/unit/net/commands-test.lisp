(in-package #:cl-user)

(progn
  (defun net-commands-test-boundaries ()
    (let ((boundaries (cl-boundary-kit:make-boundary-context)))
      (setf boundaries
            (cl-boundary-kit:boundary-context-with
             boundaries :net-http-send
             (lambda (&rest ignored)
               (declare (ignore ignored))
               (error "HTTP requester is not used by command tests."))))
      (setf boundaries
            (cl-boundary-kit:boundary-context-with
             boundaries :net-resolve
             (lambda (&rest ignored)
               (declare (ignore ignored))
               (error "Resolver is not used by command tests."))))
      boundaries))
  (defun net-commands-boundaries-p (boundaries)
    (and (functionp (cl-boundary-kit:boundary-context-require
                     boundaries :net-http-send))
         (functionp (cl-boundary-kit:boundary-context-require
                     boundaries :net-resolve))))
  (defun net-commands-dispatch (&rest argv)
    (let* ((registry (aitools.protocol.application:make-command-registry))
           (boundaries (net-commands-test-boundaries)))
      (aitools.net.presentation:register-net-commands registry boundaries)
      (let ((app (cl-cli:make-app
                  :name "aitools"
                  :version "0.0.0"
                  :require-command t
                  :commands (aitools/cli:finalize-app-commands registry)))
            (stdout (make-string-output-stream))
            (stderr (make-string-output-stream)))
        (values (aitools/cli:dispatch app registry (cons "aitools" argv)
                                      :stdout stdout :stderr stderr)
                (get-output-stream-string stdout)
                (get-output-stream-string stderr))))))

(defun net-commands-json-get (text &rest keys)
  (let ((value (json-kit:parse text)))
    (dolist (key keys value)
      (setf value (if (integerp key)
                      (elt value key)
                      (gethash key value))))))

(defun net-commands-object (&rest pairs)
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(cl-weave:describe "aitools.net.presentation commands"
  (cl-weave:it "registers the three schema-backed net commands"
    (let ((registry (aitools.protocol.application:make-command-registry)))
      (aitools.net.presentation:register-net-commands registry (net-commands-test-boundaries))
      (let ((schemas (aitools.protocol.application:all-command-schemas registry)))
        (cl-weave:expect
         (mapcar #'aitools.protocol.domain:command-schema-name schemas)
         :to-equal '("net.get" "net.head" "net.resolve"))
        (cl-weave:expect
         (mapcar (lambda (arg) (getf arg :name))
                 (aitools.protocol.domain:command-schema-args (first schemas)))
         :to-equal '("url" "--timeout" "--max-lines" "--range" "--line-offset")))))
  (cl-weave:it "fails clearly when a command schema entry is missing"
    (cl-weave:expect
     (handler-case
         (let ((aitools.data:*net-command-schemas* nil))
           (aitools.net.presentation::%schema "net.get")
           :missing-schema-was-accepted)
       (error () :missing-schema-rejected))
     :to-be :missing-schema-rejected))
  (cl-weave:it "passes get defaults and reports partial output"
    (let ((seen nil))
      (cl-weave:with-mocked-functions
          (((symbol-function 'aitools.net.application:get/k)
            (lambda (boundaries url &key timeout max-lines range line-offset on-ok on-partial on-error)
              (setf seen
                    (list (net-commands-boundaries-p boundaries) url timeout max-lines range line-offset
                          (every #'functionp (list on-ok on-partial on-error))))
              (funcall on-partial
                       (list (cons "start_line" 1)
                             (cons "lines" #("first"))
                             (cons "total_lines" 3)
                             (cons "truncated" t)
                             (cons "encoding_errors" 0)
                             (cons "next_commands"
                                   #("aitools net get https://fixture.invalid/ --range 2:")))))))
        (multiple-value-bind (code stdout stderr)
            (net-commands-dispatch "net" "get" "https://fixture.invalid/")
          (cl-weave:expect code :to-be 3)
          (cl-weave:expect stderr :to-equal "")
          (cl-weave:expect (net-commands-json-get stdout "status") :to-equal "partial")
          (cl-weave:expect (net-commands-json-get stdout "next_commands" 0)
                           :to-equal "aitools net get https://fixture.invalid/ --range 2:")
          (cl-weave:expect seen
                           :to-equal
                           '(t "https://fixture.invalid/" "30s" 80 nil nil t))))))
  (cl-weave:it "passes get range and line limits to the flow"
    (let ((seen nil))
      (cl-weave:with-mocked-functions
          (((symbol-function 'aitools.net.application:get/k)
            (lambda (boundaries url &key timeout max-lines range line-offset on-ok on-partial on-error)
              (declare (ignore on-partial on-error))
              (setf seen (list (net-commands-boundaries-p boundaries) url timeout max-lines range line-offset))
              (funcall on-ok
                       (list (cons "lines" #("second" "third"))
                             (cons "headers" (net-commands-object "Content-Type" "text/plain")))))))
        (multiple-value-bind (code stdout stderr)
            (net-commands-dispatch "net" "get" "https://fixture.invalid/"
                                   "--timeout" "4s" "--max-lines" "12" "--range" "2:3" "--line-offset" "7")
          (cl-weave:expect code :to-be 0)
          (cl-weave:expect stderr :to-equal "")
          (cl-weave:expect (net-commands-json-get stdout "lines" 1) :to-equal "third")
          (cl-weave:expect seen
                           :to-equal
                           '(t "https://fixture.invalid/" "4s" 12 "2:3" 7))))))
  (cl-weave:it "routes get failures through on-error"
    (cl-weave:with-mocked-functions
        (((symbol-function 'aitools.net.application:get/k)
          (lambda (boundaries url &key on-error &allow-other-keys)
            (declare (ignore boundaries url))
            (funcall on-error "argument.invalid" "invalid fixture URL"))))
      (multiple-value-bind (code stdout stderr)
          (net-commands-dispatch "net" "get" "not a URL")
        (cl-weave:expect code :to-be 1)
        (cl-weave:expect stdout :to-equal "")
        (cl-weave:expect (net-commands-json-get stderr "error" "code")
                         :to-equal "argument.invalid"))))
  (cl-weave:it "keeps head header-only and passes its timeout"
    (let ((seen nil))
      (cl-weave:with-mocked-functions
          (((symbol-function 'aitools.net.application:head/k)
            (lambda (boundaries url &key (timeout "30s") on-ok on-error)
              (declare (ignore on-error))
              (setf seen (append seen (list (list (net-commands-boundaries-p boundaries) url timeout))))
              (funcall on-ok
                       (list (cons "headers" (net-commands-object "Content-Type" "text/plain")))))))
        (multiple-value-bind (code stdout stderr)
            (net-commands-dispatch "net" "head" "https://fixture.invalid/")
          (cl-weave:expect code :to-be 0)
          (cl-weave:expect stderr :to-equal "")
          (cl-weave:expect (net-commands-json-get stdout "lines") :to-be nil)
          (cl-weave:expect (net-commands-json-get stdout "headers" "Content-Type")
                           :to-equal "text/plain"))
        (multiple-value-bind (code stdout stderr)
            (net-commands-dispatch "net" "head" "https://fixture.invalid/" "--timeout" "5s")
          (cl-weave:expect code :to-be 0)
          (cl-weave:expect stderr :to-equal ""))
        (cl-weave:expect
         seen
         :to-equal
         '((t "https://fixture.invalid/" "30s") (t "https://fixture.invalid/" "5s"))))))
  (cl-weave:it "passes resolve host and timeout and returns address records"
    (let ((seen nil))
      (cl-weave:with-mocked-functions
          (((symbol-function 'aitools.net.application:resolve/k)
            (lambda (boundaries host &key (timeout "30s") on-ok on-error)
              (declare (ignore on-error))
              (setf seen (append seen (list (list (net-commands-boundaries-p boundaries) host timeout))))
              (funcall on-ok
                       (list (cons "records"
                                   (vector (net-commands-object
                                            "name" host
                                            "type" "A"
                                            "value" "192.0.2.7"))))))))
        (multiple-value-bind (code stdout stderr)
            (net-commands-dispatch "net" "resolve" "localhost")
          (cl-weave:expect code :to-be 0)
          (cl-weave:expect stderr :to-equal "")
          (cl-weave:expect (net-commands-json-get stdout "records" 0 "type")
                           :to-equal "A"))
        (multiple-value-bind (code stdout stderr)
            (net-commands-dispatch "net" "resolve" "127.0.0.1" "--timeout" "2s")
          (cl-weave:expect code :to-be 0)
          (cl-weave:expect stderr :to-equal ""))
        (cl-weave:expect
         seen
         :to-equal
         '((t "localhost" "30s") (t "127.0.0.1" "2s"))))))
  (cl-weave:it "routes resolver failures through on-error"
    (cl-weave:with-mocked-functions
        (((symbol-function 'aitools.net.application:resolve/k)
          (lambda (boundaries host &key on-error &allow-other-keys)
            (declare (ignore boundaries host))
            (funcall on-error "environment.timeout" "resolver deadline expired"))))
      (multiple-value-bind (code stdout stderr)
          (net-commands-dispatch "net" "resolve" "fixture.invalid")
        (cl-weave:expect code :to-be 1)
        (cl-weave:expect stdout :to-equal "")
        (cl-weave:expect (net-commands-json-get stderr "error" "code")
                         :to-equal "environment.timeout"))))
  (cl-weave:it "rejects invalid limits and authentication header options"
    (let ((called nil))
      (cl-weave:with-mocked-functions
          (((symbol-function 'aitools.net.application:get/k)
            (lambda (boundaries url &rest arguments)
              (declare (ignore boundaries url))
              (setf called t)
              (funcall (getf arguments :on-ok) nil))))
        (cl-weave:expect
         (net-commands-dispatch "net" "get" "https://fixture.invalid/" "--max-lines" "0")
         :to-be 1)
        (cl-weave:expect
         (net-commands-dispatch "net" "get" "https://fixture.invalid/"
                                "--line-offset" "-1")
         :to-be 1)
        (cl-weave:expect
         (net-commands-dispatch "net" "get" "https://fixture.invalid/"
                                "--auth-header" "unsupported")
         :to-be 1))
      (cl-weave:expect called :to-be nil))))
