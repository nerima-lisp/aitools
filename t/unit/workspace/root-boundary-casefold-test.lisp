(in-package #:aitools.workspace.test)
(defun %git-metadata-alias-verdict (name target)
  (let* ((root-path (format nil "/bare/~A" name))
         (host (make-fake-workspace-boundaries))
         (root (aitools.workspace.domain:make-workspace-root
                :path root-path
                :real root-path
                :source :option
                :repository
                (aitools.workspace.domain:make-git-repository
                 :top root-path
                 :git-dir "/bare/worktrees/wt"
                 :common-dir "/bare"))))
    (boundary-verdict host root target)))
(describe "aitools.workspace.application Unicode Git metadata aliases"
  (it "refuses the long-s spelling of hooks"
    (expect (%git-metadata-alias-verdict "hookſ" "pre-commit")
            :to-be :git-directory))
  (it "refuses the long-s spelling of refs"
    (expect (%git-metadata-alias-verdict "refſ" "heads/main")
            :to-be :git-directory))
  (it "refuses the ligature spelling of lost-found"
    (expect (%git-metadata-alias-verdict "loﬆ-found" "file")
            :to-be :git-directory))
  (it "refuses a real-filesystem metadata alias by device and inode"
    (aitools.test.support:call-with-temporary-directory
     (lambda (directory)
       (let* ((base (namestring directory))
              (common (concatenate 'string base "common"))
              (canonical-directory (concatenate 'string common "/lost-found"))
              (alias-directory (concatenate 'string common "/loﬆ-found"))
              (canonical-file (concatenate 'string canonical-directory "/probe"))
              (alias-file (concatenate 'string alias-directory "/probe"))
              (git-dir (concatenate 'string common "/worktrees/wt"))
              (host
               (aitools.workspace.infrastructure:make-host-workspace-boundaries
                :getenv (constantly nil)
                :home-directory (constantly base)
                :current-directory (constantly base)))
              (root
               (aitools.workspace.domain:make-workspace-root
                :path alias-directory
                :real alias-directory
                :source :option
                :repository
                (aitools.workspace.domain:make-git-repository
                 :top alias-directory :git-dir git-dir :common-dir common))))
         (sb-posix:mkdir common #o700)
         (sb-posix:mkdir canonical-directory #o700)
         (sb-posix:mkdir (concatenate 'string common "/worktrees") #o700)
         (sb-posix:mkdir git-dir #o700)
         (with-open-file (stream (sb-ext:parse-native-namestring canonical-file)
                                 :direction :output
                                 :if-exists :error)
           (write-string "probe" stream))
         (let* ((canonical-stat (sb-posix:stat canonical-file))
                (alias-stat
                 (handler-case (sb-posix:stat alias-file)
                   (sb-posix:syscall-error (condition)
                     (if (= (sb-posix:syscall-errno condition)
                            sb-posix:enoent)
                         nil
                         (error condition))))))
           (unless alias-stat
             (cl-weave:skip
              (concatenate 'string
                           "case-sensitive filesystem: property cannot be observed on a case-sensitive filesystem; "
                           "fake-host name-check specs cover the folding rule on every platform")))
           (expect (and (= (sb-posix:stat-dev canonical-stat)
                           (sb-posix:stat-dev alias-stat))
                        (= (sb-posix:stat-ino canonical-stat)
                           (sb-posix:stat-ino alias-stat)))
                   :to-be t)
           (expect (boundary-verdict host root "pre-commit")
                   :to-be :git-directory)))))))
