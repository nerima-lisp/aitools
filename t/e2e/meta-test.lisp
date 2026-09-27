;;;; t/e2e/meta-test.lisp
;;;;
;;;; Coverage and integrity of the e2e suite itself. Loaded after every case
;;;; file, so its specs run after the cases and see the complete registry.
(in-package #:aitools.e2e.test)

(defparameter +not-a-shell-command+ '("uuid")
  "Foreign names in the correspondence data that name no shell command the
table replaces: `uuid` is there so that the bare group subcommand gets a
repair. Each is pinned below by its own spec.")

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
  "`aitools NAME` for a name that is not an aitools command
fails with argument.invalid and ENTRY's repairs, in order. A NAME that is
also an aitools command (find, diff, git, ...) must dispatch to it instead."
  (let ((result (aitools workspace (list name "x"))))
    (if (member name (aitools-command-words workspace) :test #'string=)
        (expect-not (jget (aitools-envelope result) "error" "message")
                    :to-equal (format nil "unknown command ~A" name))
        (progn
          (expect (aitools-exit-code result) :to-be 1)
          (expect (aitools-stream result) :to-be :stderr)
          (expect (jget (aitools-envelope result) "error" "code") :to-equal "argument.invalid")
          (expect (mapcar (lambda (repair) (jget repair "command"))
                          (jlist (jget (aitools-envelope result) "error" "repairs")))
                  :to-equal (mapcar (lambda (repair) (getf repair :command)) (getf entry :repairs)))))))

(describe "aitools e2e meta: unknown names repair from the correspondence data"
  (dolist (entry aitools.data:*correspondence-table*)
    (dolist (name (getf entry :foreign-names))
      (let ((entry entry) (name name))
        (it (format nil "aitools ~A repairs from the table unless it is an aitools command" name)
          (with-progress ((format nil "repair ~A" name))
            (let ((*current-case* (list 0 name)))
              (aitools-binary)
              (call-with-workspace (lambda (ws) (expect-unknown-name-repairs ws name entry))))))))))
