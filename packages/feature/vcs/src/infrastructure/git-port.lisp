;;;; packages/feature/vcs/src/infrastructure/git-port.lisp
;;;;
;;;; The production GIT-PORT over cl-vcs-kit. Every git condition is turned
;;;; into a failure continuation here, and the continuation is called only
;;;; after HANDLER-CASE has returned, so a condition raised by a flow's own
;;;; continuation is never mistaken for a git failure.
(in-package #:aitools.vcs.infrastructure)

(defparameter *git-timeout-seconds* 120)

(defparameter *git-output-limit* (* 256 1024 1024)
  "Characters (or octets) of stdout kept per git run. cl-process-kit's own
default of 1 MiB would cut an ordinary large diff short.")

(defun %repository (directory)
  (vcs-kit:make-repository (or directory (uiop:getcwd)) :default-timeout *git-timeout-seconds*))

(defun %text (output)
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (if (stringp output)
                   output
                   (sb-ext:octets-to-string output :external-format '(:utf-8 :replacement #\?)))))

(defun %failure (condition)
  "(VALUES KIND MESSAGE) for a cl-vcs-kit condition; see
AITOOLS.VCS.APPLICATION:GIT-PORT for the kinds."
  (typecase condition
    (vcs-kit:git-exit-error
     (let* ((result (vcs-kit:git-error-result condition))
            (stderr (and result (%text (vcs-kit:process-result-stderr result)))))
       (values :exit (if (plusp (length stderr)) stderr (princ-to-string condition)))))
    (vcs-kit:git-launch-error (values :missing (princ-to-string condition)))
    (t (values :failed (princ-to-string condition)))))

(defmacro %with-git-outcome ((value-var form &key on-failure) &body on-success)
  "Evaluate FORM; on a VCS-ERROR call ON-FAILURE (a function of KIND and
MESSAGE), otherwise run ON-SUCCESS with VALUE-VAR bound to FORM's value.
Both run outside the handler."
  (let ((kind (gensym "KIND")) (payload (gensym "PAYLOAD")))
    `(multiple-value-bind (,kind ,payload)
         (handler-case (values :ok ,form)
           (vcs-kit:vcs-error (condition) (%failure condition)))
       (if (eq ,kind :ok)
           (let ((,value-var ,payload)) ,@on-success)
           (funcall ,on-failure ,kind ,payload)))))

(defun %run (directory subcommand arguments &key octets on-success on-failure)
  (%with-git-outcome (result (vcs-kit:run-git/checked (%repository directory) subcommand arguments
                                                      :result-type (if octets :octets :string)
                                                      :max-output-characters *git-output-limit*)
                      :on-failure on-failure)
    (if (vcs-kit:process-result-stdout-truncated-p result)
        (funcall on-failure :failed
                 (format nil "git ~A printed more than ~D characters" subcommand *git-output-limit*))
        (funcall on-success (vcs-kit:process-result-stdout result)))))

(defun %probe (directory &key on-repository on-outside on-missing on-no-directory)
  (if (and directory (not (uiop:directory-exists-p directory)))
      (funcall on-no-directory (uiop:native-namestring directory))
      (%probe-git directory :on-repository on-repository :on-outside on-outside :on-missing on-missing)))

(defun %probe-git (directory &key on-repository on-outside on-missing)
  (%with-git-outcome (result (vcs-kit:run-git/checked (%repository directory) "rev-parse"
                                                      '("--is-inside-work-tree" "--show-toplevel"))
                      :on-failure (lambda (kind message)
                                    (declare (ignore message))
                                    (if (eq kind :missing) (funcall on-missing) (funcall on-outside))))
    (let* ((output (%text (vcs-kit:process-result-stdout result)))
           (newline (position #\Newline output)))
      (if (and newline (string= (subseq output 0 newline) "true"))
          (funcall on-repository (subseq output (1+ newline)) (%working-directory))
          (funcall on-outside)))))

(defun %working-directory ()
  "The process's working directory with symlinks resolved, as git resolves
the `--show-toplevel` path it prints."
  (string-right-trim "/" (uiop:native-namestring (truename (uiop:getcwd)))))

(defun %status-plist (snapshot)
  (list :branch (vcs-kit:status-snapshot-branch-head snapshot)
        :upstream (vcs-kit:status-snapshot-branch-upstream snapshot)
        :ahead (vcs-kit:status-snapshot-ahead snapshot)
        :behind (vcs-kit:status-snapshot-behind snapshot)
        :entries (mapcar (lambda (entry)
                           (list :kind (vcs-kit:status-entry-kind entry)
                                 :index (vcs-kit:status-entry-index-status entry)
                                 :worktree (vcs-kit:status-entry-worktree-status entry)
                                 :path (vcs-kit:status-entry-path entry)
                                 :original-path (vcs-kit:status-entry-original-path entry)))
                         (vcs-kit:status-snapshot-entries snapshot))))

(defun %status (directory &key on-success on-failure)
  (%with-git-outcome (snapshot (vcs-kit:git-status (%repository directory)
                                                   :execution-options (list :max-output-characters
                                                                            *git-output-limit*))
                      :on-failure on-failure)
    (funcall on-success (%status-plist snapshot))))

(defun %numstat (directory arguments &key on-success on-failure)
  (%with-git-outcome (entries (apply #'vcs-kit:git-diff-numstat (%repository directory)
                                     (append arguments
                                             (list :execution-options
                                                   (list :max-output-characters *git-output-limit*))))
                      :on-failure on-failure)
    (funcall on-success
             (mapcar (lambda (entry)
                       (list :path (vcs-kit:numstat-entry-path entry)
                             :original-path (vcs-kit:numstat-entry-original-path entry)
                             :added (vcs-kit:numstat-entry-additions entry)
                             :deleted (vcs-kit:numstat-entry-deletions entry)
                             :binary (vcs-kit:numstat-entry-binary-p entry)))
                     entries))))

(defun %root-directory (root)
  "ROOT (a `--root` string) as an absolute directory pathname, relative to
the working directory at the time of the call."
  (uiop:merge-pathnames* (uiop:parse-native-namestring root :ensure-directory t) (uiop:getcwd)))

(defun make-production-vcs-ports (&key directory &allow-other-keys)
  "The production AITOOLS.VCS.APPLICATION:GIT-PORT. Git runs in DIRECTORY,
or in the process's working directory at the time of each call when
DIRECTORY is NIL. Constructing the port performs no I/O."
  (aitools.vcs.application:make-git-port
   :probe (lambda (&rest keys) (apply #'%probe directory keys))
   :run (lambda (subcommand arguments &rest keys) (apply #'%run directory subcommand arguments keys))
   :status (lambda (&rest keys) (apply #'%status directory keys))
   :numstat (lambda (arguments &rest keys) (apply #'%numstat directory arguments keys))
   :at (lambda (root) (make-production-vcs-ports :directory (%root-directory root)))))
