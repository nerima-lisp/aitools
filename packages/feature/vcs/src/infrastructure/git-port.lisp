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

(defun %run-vcs-operation (operation)
  (handler-case (values :ok (funcall operation))
    (vcs-kit:vcs-error (condition)
      (multiple-value-bind (kind payload) (%failure condition)
        (values :on-failure (list kind payload))))))

(defun %run (directory subcommand arguments &key octets)
  (multiple-value-bind (callback-key result)
      (%run-vcs-operation
       (lambda ()
         (vcs-kit:run-git/checked
          (%repository directory) subcommand arguments
          :result-type (if octets :octets :string)
          :max-output-characters *git-output-limit*)))
    (if (eq callback-key :ok)
        (if (vcs-kit:process-result-stdout-truncated-p result)
            (values :on-failure
                    (list :failed
                          (format nil "git ~A printed more than ~D characters"
                                  subcommand *git-output-limit*)))
            (values :on-success
                    (list (vcs-kit:process-result-stdout result))))
        (values callback-key result))))

(defun %probe-git (directory)
  (multiple-value-bind (callback-key payload)
      (%run-vcs-operation
       (lambda ()
         (vcs-kit:run-git/checked
          (%repository directory) "rev-parse"
          '("--is-inside-work-tree" "--show-toplevel"))))
    (if (eq callback-key :ok)
        (let* ((output (%text (vcs-kit:process-result-stdout payload)))
               (newline (position #\Newline output)))
          (if (and newline (string= (subseq output 0 newline) "true"))
              (values :on-repository
                      (list (subseq output (1+ newline)) (%working-directory)))
              (values :on-outside nil)))
        (cond
          ((eq (first payload) :missing)
           (values :on-missing nil))
          ((and (eq (first payload) :exit)
                (search "not a git repository" (second payload)))
           (values :on-outside nil))
          (t (values :on-failure payload))))))


(defun %probe (directory)
  (if (and directory (not (uiop:directory-exists-p directory)))
      (values :on-no-directory (list (uiop:native-namestring directory)))
      (%probe-git directory)))

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

(defun %status (directory)
  (multiple-value-bind (callback-key payload)
      (%run-vcs-operation
       (lambda ()
         (vcs-kit:git-status
          (%repository directory)
          :execution-options (list :max-output-characters *git-output-limit*))))
    (if (eq callback-key :ok)
        (values :on-success (list (%status-plist payload)))
        (values callback-key payload))))

(defun %numstat (directory arguments)
  (multiple-value-bind (callback-key payload)
      (%run-vcs-operation
       (lambda ()
         (apply #'vcs-kit:git-diff-numstat
                (%repository directory)
                (append
                 arguments
                 (list :execution-options
                       (list :max-output-characters *git-output-limit*))))))
    (if (eq callback-key :ok)
        (values
         :on-success
         (list
          (mapcar (lambda (entry)
                    (list :path (vcs-kit:numstat-entry-path entry)
                          :original-path (vcs-kit:numstat-entry-original-path entry)
                          :added (vcs-kit:numstat-entry-additions entry)
                          :deleted (vcs-kit:numstat-entry-deletions entry)
                          :binary (vcs-kit:numstat-entry-binary-p entry)))
                  payload)))
        (values callback-key payload))))

(defun %root-directory (root)
  "ROOT (a `--root` string) as an absolute directory pathname, relative to
the working directory at the time of the call."
  (uiop:merge-pathnames* (uiop:parse-native-namestring root :ensure-directory t) (uiop:getcwd)))

(defun make-production-vcs-ports (&key directory &allow-other-keys)
  "The production AITOOLS.VCS.APPLICATION:GIT-PORT. Git runs in DIRECTORY,
or in the process's working directory at the time of each call when
DIRECTORY is NIL. Constructing the port performs no I/O."
  (flet ((deliver-callback (callback-key arguments callbacks)
           (apply (getf callbacks callback-key) arguments)))
    (aitools.vcs.application:make-git-port
     :probe (lambda (&rest callbacks)
              (multiple-value-bind (callback-key arguments) (%probe directory)
                (deliver-callback callback-key arguments callbacks)))
     :run (lambda (subcommand arguments &rest callbacks)
            (multiple-value-bind (callback-key callback-arguments)
                (%run directory subcommand arguments :octets (getf callbacks :octets))
              (deliver-callback callback-key callback-arguments callbacks)))
     :status (lambda (&rest callbacks)
               (multiple-value-bind (callback-key arguments) (%status directory)
                 (deliver-callback callback-key arguments callbacks)))
     :numstat (lambda (arguments &rest callbacks)
                (multiple-value-bind (callback-key callback-arguments)
                    (%numstat directory arguments)
                  (deliver-callback callback-key callback-arguments callbacks)))
     :at (lambda (root)
           (make-production-vcs-ports :directory (%root-directory root))))))
