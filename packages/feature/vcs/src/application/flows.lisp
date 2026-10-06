;;;; packages/feature/vcs/src/application/flows.lisp
;;;;
;;;; The `git status`, `git log`, and `git diff` use cases and the helpers
;;;; `git blame` and `git show` (blame-show-flows.lisp) share. Each flow follows the command
;;;; handler contract: it calls exactly one of ON-OK, ON-PARTIAL, ON-ERROR with an
;;;; envelope-ordered alist (or an error code, message, and repairs) and
;;;; never writes output or picks an exit code itself. ROOT is the global
;;;; `--root` value: it only chooses the repository (git discovers it from
;;;; there), and every suggested command repeats it. User paths are relative
;;;; to the process's working directory; git runs at the repository top with
;;;; top-relative paths, and output `path` fields are top-relative.
(in-package #:aitools.vcs.application)

(defparameter *blame-default-lines* 80
  "Lines `git blame` returns when no selector is given.")

(defparameter *diff-arguments*
  '("--no-color" "--no-ext-diff" "--no-textconv" "--no-relative")
  "Pinned so user configuration (color, external drivers, textconv,
diff.relative) cannot change what the parsers read.")

(defun %git-command (root &rest words)
  "`aitools [--root ROOT] git WORDS...` as one shell command line; NIL
words are dropped."
  (apply #'aitools.vcs.domain:command-line "aitools" (when root "--root") root "git" words))

(defun %git-unavailable-repairs ()
  (list (aitools.protocol.domain:repair
         "check-tool" "Check whether git is installed and on PATH." "aitools sys tools git")))

(defun %finish (continuation fields &optional next-commands)
  "Mask FIELDS' secret values, append `redactions` when anything was
masked and `next_commands` when given, and pass the result to CONTINUATION."
  (let ((redactions 0) (masked nil))
    (dolist (field fields)
      (multiple-value-bind (value count) (aitools.protocol.application:redact-json-value (cdr field))
        (incf redactions count)
        (push (cons (car field) value) masked)))
    (funcall continuation
             (append (nreverse masked)
                     (when (plusp redactions) (list (cons "redactions" redactions)))
                     (when next-commands (list (cons "next_commands" next-commands)))))))

(defun %fail (on-error kind message &key (not-found-repairs nil))
  "Report a failed git run. :EXIT means git rejected the request (an unknown
revision or path), which is reported as input.not-found."
  (ecase kind
    (:exit (funcall on-error "input.not-found" message :repairs not-found-repairs))
    (:missing (funcall on-error "environment.unavailable" message :repairs (%git-unavailable-repairs)))
    (:failed (funcall on-error "environment.io" message :repairs not-found-repairs))))

(defun %call-in-repository (port on-error root path continuation)
  "Probe PORT's directory and call CONTINUATION with (REPOSITORY-PORT
REPOSITORY-PATH TOP DIRECTORY): REPOSITORY-PORT runs git at the repository
top TOP, REPOSITORY-PATH is PATH (relative to the process's working
DIRECTORY) made relative to TOP, or NIL without PATH. A PATH outside the
repository is argument.invalid."
  (funcall (git-port-probe port)
           :on-repository
           (lambda (top directory)
             (let ((repository-path (and path (aitools.vcs.domain:repository-relative-path path directory top))))
               (if (and path (null repository-path))
                   (funcall on-error "argument.invalid"
                            (format nil "~A is outside the repository ~A" path top)
                            :repairs (list (aitools.protocol.domain:repair
                                            "list-changes" "List the paths this repository tracks changes for."
                                            (%git-command root "status"))))
                   (funcall continuation (port-at-root port top) repository-path top directory))))
           :on-no-directory (lambda (directory)
                              (funcall on-error "input.not-found"
                                       (format nil "--root directory does not exist: ~A" directory)
                                       :repairs (list (aitools.protocol.domain:repair
                                                       "use-working-directory"
                                                       "Run from the working directory instead."
                                                       (%git-command nil "status")))))
           :on-outside (lambda ()
                         (funcall on-error "environment.unavailable"
                                  "the working directory is not inside a git work tree"
                                  :repairs (%git-unavailable-repairs)))
           :on-missing (lambda ()
                         (funcall on-error "environment.unavailable" "git could not be started"
                                  :repairs (%git-unavailable-repairs)))))

(defun %run-git (port subcommand arguments on-error on-success &key octets not-found-repairs)
  (funcall (git-port-run port) subcommand arguments
           :octets octets
           :on-success on-success
           :on-failure (lambda (kind message)
                         (%fail on-error kind message :not-found-repairs not-found-repairs))))

(defun %option-like-p (text)
  (and (plusp (length text)) (char= (char text 0) #\-)))

(defun %pathspec (path)
  (when path (list "--" path)))

;;; ------------------------------------------------------------ git status

(defun git-status/k (port &key root on-ok on-partial on-error)
  (declare (ignore on-partial))
  (let ((port (port-at-root port root)))
    (%call-in-repository
     port on-error root nil
     (lambda (port repository-path top directory)
       (declare (ignore repository-path top directory))
       (funcall (git-port-status port)
                :on-success (lambda (snapshot) (%finish on-ok (aitools.vcs.domain:status-fields snapshot)))
                :on-failure (lambda (kind message)
                              (%fail on-error kind message
                                     :not-found-repairs (list (aitools.protocol.domain:repair "retry" "Run the status again."
                                                                       (%git-command root "status"))))))))))

;;; --------------------------------------------------------------- git log

(defun git-log/k (port &key root path (limit 20) on-ok on-partial on-error)
  "The newest LIMIT commits touching PATH (all commits when NIL),
with the total count; PARTIAL when more commits exist than LIMIT."
  (let ((port (port-at-root port root))
        (repairs (list (aitools.protocol.domain:repair "list-commits" "List the newest commits of the repository."
                                (%git-command root "log")))))
    (flet ((respond (repository-path items total)
             (let ((fields (append (when repository-path (list (cons "path" repository-path)))
                                   (list (cons "items" items)
                                         (cons "total" total)
                                         (cons "truncated" (aitools.protocol.domain:json-boolean (> total limit)))))))
               (if (> total limit)
                   (%finish on-partial fields
                            (list (%git-command root "log" path "--limit"
                                                (princ-to-string (min total (* 2 limit))))))
                   (%finish on-ok fields)))))
      (%call-in-repository
       port on-error root path
       (lambda (port repository-path top directory)
         (declare (ignore top directory))
         (funcall (git-port-run port) "rev-parse" '("-q" "--verify" "HEAD")
                  ;; An unborn branch has no HEAD commit and so no history.
                  :on-failure (lambda (kind message)
                                (if (eq kind :exit)
                                    (respond repository-path nil 0)
                                    (%fail on-error kind message :not-found-repairs repairs)))
                  :on-success
                  (lambda (head)
                    (declare (ignore head))
                    (%run-git
                     port "rev-list" (append '("--count" "HEAD") (%pathspec repository-path)) on-error
                     (lambda (count-text)
                       (let ((total (parse-integer count-text :junk-allowed t)))
                         (%run-git
                          port "log"
                          (append (list "-z" "--no-color" "--no-show-signature" "--date=format:%z"
                                        (concatenate 'string "--format=" aitools.vcs.domain:*log-format*)
                                        "-n" (princ-to-string limit))
                                  (%pathspec repository-path))
                          on-error
                          (lambda (text)
                            (let ((items nil))
                              (flet ((collect (sha author date subject)
                                       (push (aitools.vcs.domain:log-item sha author date subject) items)))
                                (declare (dynamic-extent #'collect))
                                (aitools.vcs.domain:map-log-records text #'collect))
                              (respond repository-path (nreverse items) total)))
                          :not-found-repairs repairs)))
                     :not-found-repairs repairs))))))))

;;; -------------------------------------------------------------- git diff

(defun %diff-command (root path staged ref max-lines)
  "The hunks-mode `git diff` command line; stat mode never truncates or reruns
the patch, so no suggested command carries `--output stat`."
  (%git-command root "diff" path (when staged "--staged") (when ref "--ref") ref
                "--max-lines" (princ-to-string max-lines)))

(defun git-diff/k (port &key root path staged ref (output :hunks) (max-lines 400) on-ok on-partial on-error)
  "Changed files with line counts, and with OUTPUT :HUNKS their hunks
until MAX-LINES rendered lines; later files come back as mode \"summary\"
and the result is PARTIAL."
  (if (and ref (%option-like-p ref))
      (funcall on-error "argument.invalid" (format nil "--ref must name a revision, not an option: ~A" ref)
               :repairs (list (aitools.protocol.domain:repair "diff-range" "Compare two revisions."
                                       (%git-command root "diff" "--ref" "HEAD~1..HEAD"))))
      (let ((port (port-at-root port root))
            (repairs (list (aitools.protocol.domain:repair "list-commits" "Find a revision to compare against." (%git-command root "log"))))
            (mode (if (eq output :stat) "stat" "hunks")))
        (%call-in-repository
         port on-error root path
         (lambda (port repository-path top directory)
           (let ((arguments (append *diff-arguments* (when staged '("--cached")) (when ref (list ref))
                                    (%pathspec repository-path))))
             (labels ((respond (continuation files characters next-commands)
                        (%finish continuation
                                 (append (when repository-path (list (cons "path" repository-path)))
                                         (list (cons "mode" mode)
                                               (cons "files" files)
                                               (cons "truncated" (aitools.protocol.domain:json-boolean next-commands))
                                               (cons "approx_tokens"
                                                     (aitools.kernel.domain:approx-token-count characters))))
                                 next-commands))
                      (shape (records patches)
                        (aitools.vcs.domain:diff-files/k
                         records patches :output output :max-lines max-lines
                         :on-complete (lambda (files characters) (respond on-ok files characters nil))
                         :on-truncated (lambda (files characters summary-path summary-lines)
                                         (respond on-partial files characters
                                                  (list (%diff-command
                                                         root (aitools.vcs.domain:path-from-directory
                                                               summary-path top directory)
                                                         staged ref (max max-lines summary-lines))))))))
               (funcall (git-port-numstat port) arguments
                        :on-failure (lambda (kind message)
                                      (%fail on-error kind message :not-found-repairs repairs))
                        :on-success
                        (lambda (records)
                          (if (eq output :stat)
                              (shape records nil)
                              (%run-git
                               port "diff" (append '("--src-prefix=a/" "--dst-prefix=b/" "-U3") arguments)
                               on-error
                               (lambda (text)
                                 (let ((patches (aitools.vcs.domain:split-patch-by-file text)))
                                   ;; Two git runs: a work-tree change between them
                                   ;; can leave the lists out of step.
                                   (if (= (length patches) (length records))
                                       (shape records patches)
                                       (funcall on-error "environment.io"
                                                "the diff changed between git runs; run the command again"
                                                :repairs (list (aitools.protocol.domain:repair "retry" "Run the same diff again."
                                                                        (%diff-command root path staged ref max-lines)))))))
                               :not-found-repairs repairs)))))))))))
