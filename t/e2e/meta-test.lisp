;;;;
;;;; Coverage and integrity of the e2e suite itself. Loaded after every case
;;;; file, so its specs run after the cases and see the complete registry.
(in-package #:aitools.e2e.test)

(defparameter +literal-repair-names+ (list "json.get" "json.select" "json.diff" "json.set" "json.delete" "json.merge" "json.patch" "json.fmt" "yaml.get" "yaml.select" "yaml.diff" "yaml.set" "yaml.delete" "yaml.merge" "yaml.patch" "yaml.fmt" "toml.get" "toml.select" "toml.diff" "toml.set" "toml.delete" "toml.merge" "toml.patch" "toml.fmt"))
(defparameter +not-a-shell-command+ (append (list "uuid" "delete") +literal-repair-names+) "Foreign names in the correspondence data that are not shell commands. The retired dotted document names are covered by literal repair cases.")

(defvar *command-words* nil)

(defun aitools-command-words (workspace)
  "The first word of every command `aitools schema` lists."
  (or *command-words*
      (setf *command-words*
            (remove-duplicates
             (mapcar (lambda (command)
                       (let ((name (jget command "name")))
                         (subseq name 0 (or (position #\Space name) (length name)))))
                     (jlist (jget (run-ok workspace '("schema")) "commands")))
             :test #'string=))))

(describe "aitools e2e meta: correspondence-table coverage"
  (it "has at least one e2e case for every table row and none for a row that does not exist"
    (let ((rows (length *correspondence-rows*)))
      (expect rows :to-be 45)
      (expect (loop for row from 1 to rows
                    unless (find row *row-cases* :key #'first)
                      collect row)
              :to-equal '())
      (expect (remove-if (lambda (row) (<= 1 row rows)) (mapcar #'first *row-cases*)) :to-equal '()))
    (format t "~&~{  ~{row ~2,'0D ~A: ~(~A~) ~A~}~%~}" (reverse *oracle-log*)))

  (it "registers every case under a distinct name"
    (expect *duplicate-case-names* :to-equal '())
    (expect (length (remove-duplicates (mapcar #'second *row-cases*) :test #'string=))
            :to-be (length *row-cases*)))

  (it "exercises every shell command name in the correspondence data"
    (let ((named (loop for (nil nil foreign) in *row-cases* append foreign)))
      (expect (loop for entry in aitools.data:*correspondence-table*
                    append (loop for name in (getf entry :foreign-names)
                                 unless (or (member name named :test #'string=)
                                            (member name +not-a-shell-command+ :test #'string=))
                                   collect name))
              :to-equal '())))

  (it "treats uuid as a group subcommand name, not a shell command"
    (let ((entry (find '("uuid") aitools.data:*correspondence-table*
                       :key (lambda (entry) (getf entry :foreign-names))
                       :test #'equal)))
      (expect (search "group subcommand" (getf (first (getf entry :repairs)) :detail)) :to-be-truthy)))

  (it "leaves no workspace of this run in the real state directory"
    (let ((roots (remove nil (list (let ((xdg (uiop:getenv "XDG_STATE_HOME")))
                                     (and xdg (plusp (length xdg))
                                          (merge-pathnames "aitools/" (uiop:ensure-directory-pathname xdg))))
                                   (merge-pathnames ".local/state/aitools/" (user-homedir-pathname))))))
      (expect (loop for root in roots
                    when (probe-file root)
                      append (remove-if-not (lambda (path) (search *run-token* (namestring path)))
                                            (uiop:subdirectories root)))
              :to-equal '()))))

(defun expect-unknown-name-repairs (workspace name entry)
  "`aitools NAME` for a name that is not an aitools command fails with argument.invalid and ENTRY repairs, in order. A NAME that is also an aitools command must dispatch to it instead."
  (let ((result (aitools workspace (list name "x"))))
    (if (member name (aitools-command-words workspace) :test #'string=)
        (expect-not (jget (aitools-envelope result) "error" "message")
                    :to-equal (format nil "unknown command ~A" name))
        (progn
          (expect (aitools-exit-code result) :to-be 1)
          (expect (aitools-stream result) :to-be :stderr)
          (expect (aitools-envelope result) :to-match-envelope "status" "error" "command" name)
          (expect (jget (aitools-envelope result) "error" "code")
                  :to-equal "argument.invalid")
          (expect (mapcar (lambda (repair) (jget repair "command"))
                          (jlist (jget (aitools-envelope result) "error" "repairs")))
                  :to-equal (mapcar (lambda (repair) (getf repair :command))
                                    (getf entry :repairs)))))))

(describe "aitools e2e meta: unknown names repair from the correspondence data"
  (dolist (entry aitools.data:*correspondence-table*)
    (dolist (name (getf entry :foreign-names))
      (unless (member name +literal-repair-names+ :test (function string=))
        (let ((entry entry) (name name))
          (it (format nil "aitools ~A repairs from the table unless it is an aitools command" name)
            (with-progress ((format nil "repair ~A" name))
              (let ((*current-case* (list 0 name)))
                (aitools-binary)
                (call-with-workspace
                 (lambda (ws) (expect-unknown-name-repairs ws name entry)))))))))))

(describe "aitools e2e meta: retired command repair"
  (it "repairs delete to aitools rm"
    (aitools-binary)
    (call-with-workspace
     (lambda (ws)
       (let* ((result (aitools ws (list "delete" "x")))
              (envelope (aitools-envelope result))
              (repairs (jlist (jget envelope "error" "repairs"))))
         (expect (member "delete" (aitools-command-words ws) :test #'string=)
                 :to-be nil)
         (expect (aitools-exit-code result) :to-be 1)
         (expect (aitools-stream result) :to-be :stderr)
         (expect envelope :to-match-envelope "status" "error" "command" "delete")
         (expect (jget envelope "error" "code") :to-equal "argument.invalid")
         (expect (mapcar (lambda (repair) (jget repair "command")) repairs)
                 :to-equal (list "aitools rm")))))))

(describe "aitools e2e meta: byte-capture invocation inventory"
  (it "keeps no-trace invocation IDs unique when capture is enabled"
    (when (let ((directory (uiop:getenv "AITOOLS_E2E_BYTE_CAPTURE_DIR")))
            (and directory (plusp (length directory))))
      (expect *byte-capture-identifiers* :to-be-truthy)
      (expect (length (remove-duplicates (copy-list *byte-capture-identifiers*) :test #'equal))
              :to-be (length *byte-capture-identifiers*)))))
(describe "aitools e2e meta: retired structured document repairs"
  (dolist (case
           (quote (("json" "get" "aitools doc get")
                   ("json" "select" "aitools doc select")
                   ("json" "diff" "aitools doc diff")
                   ("json" "set" "aitools doc set")
                   ("json" "delete" "aitools doc delete")
                   ("json" "merge" "aitools doc merge")
                   ("json" "patch" "aitools doc patch")
                   ("json" "fmt" "aitools doc fmt")
                   ("yaml" "get" "aitools doc get")
                   ("yaml" "select" "aitools doc select")
                   ("yaml" "diff" "aitools doc diff")
                   ("yaml" "set" "aitools doc set")
                   ("yaml" "delete" "aitools doc delete")
                   ("yaml" "merge" "aitools doc merge")
                   ("yaml" "patch" "aitools doc patch")
                   ("yaml" "fmt" "aitools doc fmt")
                   ("toml" "get" "aitools doc get")
                   ("toml" "select" "aitools doc select")
                   ("toml" "diff" "aitools doc diff")
                   ("toml" "set" "aitools doc set")
                   ("toml" "delete" "aitools doc delete")
                   ("toml" "merge" "aitools doc merge")
                   ("toml" "patch" "aitools doc patch")
                   ("toml" "fmt" "aitools doc fmt"))))
    (let ((group (first case))
          (verb (second case))
          (expected-repair (third case)))
      (it (format nil "repairs retired ~A ~A with its literal doc command" group verb)
        (let ((*current-case* (list 0 (format nil "~A ~A" group verb))))
          (aitools-binary)
          (call-with-workspace
           (lambda (ws)
             (let* ((result (aitools ws (list group verb)))
                    (envelope (aitools-envelope result))
                    (error (jget envelope "error")))
               (expect (aitools-exit-code result) :to-be 1)
               (expect (aitools-stream result) :to-be :stderr)
               (expect envelope :to-match-envelope "status" "error")
               (expect (jget envelope "schema_version") :to-be 2)
               (expect (jget error "code") :to-equal "argument.invalid")
               (expect (jget error "exit_code") :to-be 1)
               (expect (mapcar (lambda (repair) (jget repair "command"))
                               (jlist (jget error "repairs")))
                       :to-equal (list expected-repair))))))))))
