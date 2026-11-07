;;;;
;;;; Case-side harness: per-case workspaces, the checked aitools invocation,
;;;; JSON helpers, shell oracles, and define-row-case registration.
;;;;
;;;; Expected values come from the shell command the table row replaces,
;;;; run through /bin/sh in a sibling copy of the same fixture. When a tool
;;;; the command needs is not on PATH the case takes a counted skip naming
;;;; the missing tool; when the tool is present its output is compared and,
;;;; where the case carries a fixed expected value, that constant is
;;;; cross-checked against the tool's own output so it stays honest.
(in-package #:aitools.e2e.test)

;;; Workspaces

(defstruct workspace root state)

(defun git-environment ()
  '(("GIT_CONFIG_GLOBAL" . "/dev/null")
    ("GIT_CONFIG_NOSYSTEM" . "1")
    ("GIT_AUTHOR_NAME" . "e2e") ("GIT_AUTHOR_EMAIL" . "e2e@example.invalid")
    ("GIT_COMMITTER_NAME" . "e2e") ("GIT_COMMITTER_EMAIL" . "e2e@example.invalid")))

(defun workspace-environment (workspace &optional extra)
  (environment-with
   (append extra
           `(("XDG_STATE_HOME" . ,(native (workspace-state workspace)))
             ("LC_ALL" . "C")
             ("AITOOLS_E2E_BINARY" . nil))
           (git-environment))))

(defun call-with-workspace (function &key git)
  "Call FUNCTION with a fresh WORKSPACE (a `git init` repository when GIT),
removing it and its state directory afterwards."
  (let* ((n (incf *invocation-counter*))
         (root (scratch-path (format nil "~A-ws-~D/" *run-token* n)))
         (state (scratch-path (format nil "state-~D/" n))))
    (ensure-directories-exist root)
    (ensure-directories-exist state)
    (let ((workspace (make-workspace :root (truename root) :state (truename state))))
      (unwind-protect
           (progn
             (when git
               (let ((result (run-process "git" '("init" "-q" "-b" "main" ".")
                                          :directory (workspace-root workspace)
                                          :environment (workspace-environment workspace))))
                 (unless (eql (result-exit-code result) 0)
                   (fail "~A" (format nil "git init failed: ~A" (utf8 (result-stderr result)))))))
             (funcall function workspace))
        (dolist (path (list root state))
          (uiop:delete-directory-tree path :validate (lambda (p) (search *run-token* (namestring p)))
                                           :if-does-not-exist :ignore))))))

(defun ws-path (workspace relative)
  (merge-pathnames relative (workspace-root workspace)))

(defun put (workspace relative content &key mode)
  "Write CONTENT (a string, written as UTF-8, or an octet vector) to RELATIVE."
  (let ((path (ws-path workspace relative)))
    (write-octets path (if (stringp content) (octets content) content))
    (when mode
      (sb-posix:chmod (native path) mode))
    path))

(defun file-octets (workspace relative)
  (read-octets (ws-path workspace relative)))

(defun file-text (workspace relative)
  (utf8 (file-octets workspace relative)))

(defun file-mode (workspace relative)
  (logand #o7777 (sb-posix:stat-mode (sb-posix:lstat (native (ws-path workspace relative))))))

;;; aitools

(defstruct (aitools-result (:conc-name aitools-))
  exit-code envelope stream stdout stderr argv)

(defun jget (value &rest keys)
  "Walk VALUE by string member names and integer indexes; NIL when absent."
  (reduce (lambda (current key)
            (etypecase key
              (string (and (hash-table-p current) (values (gethash key current))))
              (integer (and (vectorp current) (not (stringp current))
                            (< key (length current)) (aref current key)))))
          keys :initial-value value))

(defun ok (result)
  (let* ((envelope (aitools-envelope result))
         (output (if (eq (aitools-stream result) :stdout) (aitools-stdout result) (aitools-stderr result))))
    (unless (equal (jget envelope "status") "ok") (fail "~A" output))
    (expect envelope :to-match-envelope "status" "ok")
    (unless (eql (aitools-exit-code result) 0)
      (fail "~A"
            (format nil "aitools ~{~A~^ ~} exited ~A: ~A" (aitools-argv result)
                    (aitools-exit-code result) output)))
    envelope))

(defun run-ok (workspace arguments &rest options) (ok (apply #'aitools workspace arguments options)))

(defun jlist (value)
  (coerce value 'list))

(defun lines-text (lines)
  "LINES (a JSON array of strings) as newline-terminated text."
  (format nil "~{~A~%~}" (jlist lines)))

(defun text-lines (text)
  "TEXT split on newlines, without the empty string after a final newline."
  (let ((lines (uiop:split-string text :separator '(#\Newline))))
    (if (and lines (string= (car (last lines)) ""))
        (butlast lines)
        lines)))

(defun json= (a b)
  "Structural JSON equality: objects by member set, arrays in order, numbers
by value."
  (cond ((and (hash-table-p a) (hash-table-p b))
         (and (= (hash-table-count a) (hash-table-count b))
              (loop for key being the hash-keys of a using (hash-value value)
                    always (multiple-value-bind (other present) (gethash key b)
                             (and present (json= value other))))))
        ((and (vectorp a) (vectorp b) (not (stringp a)) (not (stringp b)))
         (and (= (length a) (length b)) (every #'json= a b)))
        ((and (numberp a) (numberp b)) (= a b))
        ((and (stringp a) (stringp b)) (string= a b))
        (t (eq a b))))

;;; Oracles

(defun note-oracle (kind detail)
  (push (list (first *current-case*) (second *current-case*) kind detail) *oracle-log*))

(defun missing-tools (tools)
  (remove-if #'tool-path tools))

(defun shell (workspace script &key stdin (expect-status 0) env octets)
  "Run SCRIPT with /bin/sh -c in WORKSPACE; its stdout as a string (octets
when OCTETS). A status other than EXPECT-STATUS (a list, or one integer, or
:ANY) fails the case, so a broken oracle never yields an empty expected value."
  (let ((result (run-process "/bin/sh" (list "-c" script)
                             :directory (workspace-root workspace)
                             :environment (workspace-environment workspace env)
                             :input (and stdin (if (stringp stdin) (octets stdin) stdin)))))
    (unless (or (eq expect-status :any)
                (member (result-exit-code result) (uiop:ensure-list expect-status)))
      (fail "~A" (format nil "oracle `~A` exited ~A: ~A"
                         script (result-exit-code result) (utf8 (result-stderr result)))))
    (values (if octets (result-stdout result) (utf8 (result-stdout result)))
            (result-exit-code result))))

(defun oracle (workspace tools script fixed &rest options &key probe &allow-other-keys)
  "The expected value for a case: the stdout of SCRIPT, run when every name in
TOOLS is on PATH and PROBE (a script checking that the installed variant
supports the options SCRIPT uses) exits 0. FIXED, when non-NIL, is the value
this case's author expects; it is cross-checked against SCRIPT's actual
output, so the constant is verified against the real tool wherever the tool
runs. When a tool or the probed variant is absent the case takes a COUNTED
SKIP naming what was missing, rather than silently asserting against FIXED
without ever running the tool it claims to mirror."
  (let ((missing (or (missing-tools tools)
                     (and probe
                          (/= 0 (nth-value 1 (shell workspace probe :expect-status :any)))
                          (list (format nil "a variant supporting `~A`" probe)))))
        (options (loop for (key value) on options by #'cddr
                       unless (eq key :probe) append (list key value))))
    (cond (missing
           (note-oracle :skip missing)
           (skip (format nil "oracle unavailable: ~{~A~^, ~} not on PATH" missing)))
          (t
           (let ((output (apply #'shell workspace script options)))
             (note-oracle :shell script)
             (when fixed
               (unless (equalp output fixed)
                 (fail "~A" (format nil "fixed expected value ~S disagrees with `~A` output ~S"
                                    fixed script output))))
             output)))))

;;; Case registration

(defvar *row-cases* '()
  "One (ROW NAME FOREIGN-NAMES) per registered case, newest first.")

(defvar *duplicate-case-names* '()
  "Case names registered more than once; a repeated name would replace the
earlier cl-weave registration and silently drop a case.")

(defun emit-progress (event label)
  "Write one immediately-flushed line to *ERROR-OUTPUT* as a case is entered
or left. cl-weave buffers its own report until the whole module finishes, so
a run killed by the sandbox timeout leaves no trace of where it stopped; these
lines do, the last unmatched `start` naming the case that was still running."
  (format *error-output* "~&[e2e ~A] ~A~%" event label)
  (finish-output *error-output*))

(defmacro with-progress ((label) &body body)
  "Run BODY, emitting a start line before and an end line after (even on a
non-local exit such as SKIP or FAIL); an external kill leaves only the start."
  (let ((label-var (gensym "LABEL")))
    `(let ((,label-var ,label))
       (emit-progress "start" ,label-var)
       (unwind-protect (progn ,@body)
         (emit-progress "end" ,label-var)))))

(defun note-row-case (row name foreign)
  (when (find name *row-cases* :key #'second :test #'string=)
    (pushnew name *duplicate-case-names* :test #'string=))
  (setf *row-cases* (cons (list row name foreign)
                          (remove name *row-cases* :key #'second :test #'string=))))

(defmacro define-row-case ((row name &key foreign git) (workspace) &body body)
  "Register an e2e case for correspondence-table row ROW: FOREIGN lists the
shell command names it exercises (cross-checked with the correspondence
table data), GIT makes WORKSPACE a fresh repository."
  (let ((title (format nil "row ~2,'0D: ~A" row name)))
    `(progn
       (note-row-case ,row ,title ',foreign)
       (describe ,(format nil "aitools e2e row ~2,'0D" row)
         (it ,title
           (with-progress (,title)
             (let ((*current-case* (list ,row ,title)))
               (aitools-binary)
               (call-with-workspace (lambda (,workspace) (declare (ignorable ,workspace)) ,@body)
                                    :git ,git))))))))

(defmacro define-org-case ((name workspace) &body body)
  `(it ,name
     (with-progress (,name)
       (let ((*current-case* (list 0 ,name)))
         (aitools-binary)
         (call-with-workspace (lambda (,workspace) ,@body))))))

(defun sorted (strings)
  (sort (copy-list strings) #'string<))

(defun strip-dot-slash (path)
  (if (and (>= (length path) 2) (string= "./" path :end2 2)) (subseq path 2) path))

(defun oracle-lines (workspace tools script fixed)
  "The oracle's output lines with a leading ./ removed, sorted."
  (sorted (mapcar #'strip-dot-slash (text-lines (oracle workspace tools script fixed)))))

(defun trimmed (text)
  (string-trim '(#\Newline #\Space) text))

(defun tree-snapshot (workspace directory)
  "Every entry under DIRECTORY as (relative kind mode content-or-target),
sorted: an observer independent of both aitools and the shell tools."
  (let ((root (ws-path workspace directory)))
    (labels ((walk (path prefix)
               (loop for entry in (sorted (mapcar (lambda (p)
                                                     (let ((name (if (uiop:directory-pathname-p p)
                                                                     (car (last (pathname-directory p)))
                                                                     (file-namestring p))))
                                                       name))
                                                   (append (uiop:subdirectories path)
                                                           (uiop:directory-files path))))
                     for relative = (if prefix (format nil "~A/~A" prefix entry) entry)
                     for native-path = (format nil "~A~A" (native path) entry)
                     for stat = (sb-posix:lstat native-path)
                     for mode = (sb-posix:stat-mode stat)
                     for kind = (cond ((= (logand mode #o170000) #o120000) :symlink)
                                      ((= (logand mode #o170000) #o040000) :dir)
                                      (t :file))
                     collect (list relative kind (logand mode #o7777)
                                   (case kind
                                     (:symlink (sb-posix:readlink native-path))
                                     (:file (read-octets native-path))))
                     when (eq kind :dir)
                       append (walk (uiop:ensure-directory-pathname native-path) relative))))
      (if (probe-file root) (walk root nil) :absent))))

(defun put-diff-pair (workspace)
  (put workspace "x.txt" (format nil "a~%b~%c~%e~%"))
  (put workspace "y.txt" (format nil "a~%B~%c~%d~%e~%")))

(defparameter +json-fixture+
  (format nil "{\"b\":{\"c\":\"hi\\n\",\"d\":[1,2]},\"a\":[1,2,3],\"name\":\"x\"}~%"))

(defun parse-json-text (text)
  (json-kit:parse text :false-value nil :null-value nil))

(defun jq (workspace filter file fixed &rest flags)
  "jq -c FILTER FILE's single output value, parsed."
  (parse-json-text (oracle workspace '("jq") (format nil "jq -c~{ ~A~} '~A' ~A" flags filter file) fixed)))

(defun put-archive-sources (workspace)
  (put workspace "src/a.txt" (format nil "hello~%"))
  (put workspace "src/sub/b.txt" (format nil "world~%second~%")))

(defun make-archives (workspace)
  "Build t.tar, t.tgz, z.zip, and a.gz from src/ with the real tools."
  (put-archive-sources workspace)
  (shell workspace "tar -cf t.tar src && tar -czf t.tgz src && zip -qr z.zip src && gzip -c src/a.txt > a.gz"))

(defun expect-same-json-edit (workspace filter arguments &key stdin)
  "Apply jq FILTER to o.json and aitools ARGUMENTS to a.json (both
+JSON-FIXTURE+) and compare the documents structurally."
  (put workspace "a.json" +json-fixture+)
  (put workspace "o.json" +json-fixture+)
  (let ((expected (jq workspace filter "o.json" nil)))
    (run-ok workspace arguments :stdin stdin)
    (expect (json= (parse-json-text (file-text workspace "a.json")) expected) :to-be t)))

(defun expect-child-process-span (envelope name outcome)
  (let* ((spans (jlist (jget envelope "meta" "trace" "spans")))
         (command (find-if (lambda (span)
                             (and (string= (jget span "kind") "command")
                                  (null (jget span "parent_span_id"))))
                           spans))
         (child (find name spans :key (lambda (span) (jget span "name")) :test #'string=)))
    (expect command :to-be-truthy)
    (expect child :to-be-truthy)
    (multiple-value-bind (parent present-p) (gethash "parent_span_id" command)
      (expect present-p :to-be t)
      (expect parent :to-be nil))
    (expect (jget child "kind") :to-equal "child_process")
    (expect (jget child "outcome") :to-equal outcome)
    (expect (jget child "parent_span_id") :to-be (jget command "span_id"))))
