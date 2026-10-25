;;;;
;;;; The `git status`, `git log`, and `git diff` flows are defined here.
;;;; `git blame` is in `blame-flows.lisp`, and `git show` is in
;;;; `show-flow.lisp`. A flow calls ON-OK, ON-PARTIAL, or ON-ERROR with an
;;;; envelope-ordered alist, or with an error code, message, and repairs.
;;;; Flows do not write output or choose an exit code. ROOT is the global
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
  (apply #'aitools.protocol.domain:command-line "aitools" (when root "--root") root "git" words))

(defun %git-unavailable-repairs ()
  (list (repair
         "check-tool" "Check whether git is installed and on PATH." "aitools sys tools git")))

(defun %finish-fields (fields &optional next-commands)
  (let ((redactions 0) (masked nil))
    (dolist (field fields)
      (multiple-value-bind (value count) (aitools.protocol.application:redact-json-value (cdr field))
        (incf redactions count)
        (push (cons (car field) value) masked)))
    (append (nreverse masked)
            (when (plusp redactions) (list (cons "redactions" redactions)))
            (when next-commands (list (cons "next_commands" next-commands))))))

(aitools.kernel.domain:define-flow
    (%fail kind message &key (not-found-repairs nil) on-error)
  "Report a failed git run. :EXIT means git rejected the request (an unknown
revision or path), which is reported as input.not-found."
  (ecase kind
    (:exit
     (aitools.kernel.domain:flow-continue
      :on-error "input.not-found" message :repairs not-found-repairs))
    (:missing
     (aitools.kernel.domain:flow-continue
      :on-error "environment.unavailable" message :repairs (%git-unavailable-repairs)))
    (:failed
     (aitools.kernel.domain:flow-continue
      :on-error "environment.io" message :repairs not-found-repairs))))

(aitools.kernel.domain:define-flow
    (%call-in-repository port root path &key on-repository on-error)
  "Probe PORT's directory and continue with the repository port, repository
path, top, and working directory. A PATH outside the repository is invalid."
  (funcall (git-port-probe (port-at-root port root))
           :on-repository
           (lambda (top directory)
             (let ((repository-path (and path (aitools.vcs.domain:repository-relative-path path directory top))))
               (if (and path (null repository-path))
                   (aitools.kernel.domain:flow-continue
                    :on-error
                    "argument.invalid"
                    (format nil "~A is outside the repository ~A" path top)
                    :repairs (list (repair
                                    "list-changes" "List the paths this repository tracks changes for."
                                    (%git-command root "status"))))
                   (aitools.kernel.domain:flow-continue
                    :on-repository (port-at-root port top) repository-path top directory))))
           :on-no-directory
           (lambda (directory)
             (aitools.kernel.domain:flow-continue
              :on-error
              "input.not-found"
              (format nil "--root directory does not exist: ~A" directory)
              :repairs (list (repair
                              "use-working-directory"
                              "Run from the working directory instead."
                              (%git-command nil "status")))))
           :on-outside
           (lambda ()
             (aitools.kernel.domain:flow-continue
              :on-error
              "environment.unavailable"
              "the working directory is not inside a git work tree"
              :repairs (%git-unavailable-repairs)))
           :on-missing
           (lambda ()
             (aitools.kernel.domain:flow-continue
              :on-error
              "environment.unavailable"
              "git could not be started"
              :repairs (%git-unavailable-repairs)))
           :on-failure
           (lambda (kind message)
             (declare (ignore kind))
             (aitools.kernel.domain:flow-continue
              :on-error "environment.io" message))))


(aitools.kernel.domain:define-flow
    (%run-git port subcommand arguments &key octets not-found-repairs on-success on-error)
  (funcall (git-port-run port) subcommand arguments
           :octets octets
           :on-success
           (lambda (output)
             (aitools.kernel.domain:flow-continue :on-success output))
           :on-failure
           (lambda (kind message)
             (aitools.kernel.domain:flow-call
                 (%fail kind message :not-found-repairs not-found-repairs)
               (:on-error (code text &key repairs)
                 (aitools.kernel.domain:flow-continue
                  :on-error code text :repairs repairs))))))

(defun %option-like-p (text)
  (and (plusp (length text)) (char= (char text 0) #\-)))

(defun %pathspec (path)
  (when path (list "--" path)))

(aitools.kernel.domain:define-flow
    (git-status/k port &key root on-ok on-partial on-error)
  (declare (ignore on-partial))
  (aitools.kernel.domain:flow-call
      (%call-in-repository port root nil)
    (:on-repository (port repository-path top directory)
      (declare (ignore repository-path top directory))
      (funcall (git-port-status port)
               :on-success
               (lambda (snapshot)
                 (aitools.kernel.domain:flow-continue
                  :on-ok (%finish-fields
                          (aitools.vcs.domain:status-fields snapshot))))
               :on-failure
               (lambda (kind message)
                 (aitools.kernel.domain:flow-call
                     (%fail kind message
                            :not-found-repairs
                            (list (repair "retry" "Run the status again."
                                          (%git-command root "status"))))
                   (:on-error (code text &key repairs)
                     (aitools.kernel.domain:flow-continue
                      :on-error code text :repairs repairs))))))
    (:on-error (code message &key repairs)
      (aitools.kernel.domain:flow-continue
       :on-error code message :repairs repairs))))

(aitools.kernel.domain:define-flow
    (git-log/k port &key root path (limit 20) on-ok on-partial on-error)
  "The newest LIMIT commits touching PATH (all commits when NIL),
with the total count; PARTIAL when more commits exist than LIMIT."
  (let ((port (port-at-root port root))
        (repairs (list (repair "list-commits" "List the newest commits of the repository."
                               (%git-command root "log")))))
    (labels ((respond (repository-path items total)
               (let ((fields
                       (append (when repository-path
                                 (list (cons "path" repository-path)))
                               (list (cons "items" items)
                                     (cons "total" total)
                                     (cons "truncated"
                                           (aitools.protocol.domain:json-boolean
                                            (> total limit)))))))
                 (if (> total limit)
                     (aitools.kernel.domain:flow-continue
                      :on-partial
                      (%finish-fields
                       fields
                       (list (%git-command root "log" path "--limit"
                                           (princ-to-string (min total (* 2 limit)))))))
                     (aitools.kernel.domain:flow-continue
                      :on-ok (%finish-fields fields))))))
      (aitools.kernel.domain:flow-call
          (%call-in-repository port root path)
        (:on-repository (port repository-path top directory)
          (declare (ignore top directory))
          (funcall (git-port-run port) "rev-parse" '("-q" "--verify" "HEAD")
                   :on-failure
                   (lambda (kind message)
                     ;; An unborn branch has no HEAD commit and so no history.
                     (if (eq kind :exit)
                         (respond repository-path nil 0)
                         (aitools.kernel.domain:flow-call
                             (%fail kind message :not-found-repairs repairs)
                           (:on-error (code text &key repairs)
                             (aitools.kernel.domain:flow-continue
                              :on-error code text :repairs repairs)))))
                   :on-success
                   (lambda (head)
                     (declare (ignore head))
                     (aitools.kernel.domain:flow-call
                         (%run-git port "rev-list"
                                   (append '("--count" "HEAD") (%pathspec repository-path))
                                   :not-found-repairs repairs)
                       (:on-success (count-text)
                         (let ((total (parse-integer count-text :junk-allowed t)))
                           (aitools.kernel.domain:flow-call
                               (%run-git
                                port "log"
                                (append (list "-z" "--no-color" "--no-show-signature"
                                              "--date=format:%z"
                                              (concatenate 'string "--format="
                                                           aitools.vcs.domain:*log-format*)
                                              "-n" (princ-to-string limit))
                                        (%pathspec repository-path))
                                :not-found-repairs repairs)
                             (:on-success (text)
                               (let ((items nil))
                                 (flet ((collect (sha author date subject)
                                          (push (aitools.vcs.domain:log-item
                                                 sha author date subject)
                                                items)))
                                   (declare (dynamic-extent #'collect))
                                   (aitools.vcs.domain:map-log-records text #'collect))
                                 (respond repository-path (nreverse items) total)))
                             (:on-error (code message &key repairs)
                               (aitools.kernel.domain:flow-continue
                                :on-error code message :repairs repairs)))))
                       (:on-error (code message &key repairs)
                         (aitools.kernel.domain:flow-continue
                          :on-error code message :repairs repairs))))))
        (:on-error (code message &key repairs)
          (aitools.kernel.domain:flow-continue
           :on-error code message :repairs repairs))))))
