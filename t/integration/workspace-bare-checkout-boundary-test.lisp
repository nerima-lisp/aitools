;;;;
;;;; Checkouts below a bare repository on the real filesystem: linked
;;;; worktrees that git itself registers, with absolute and relative paths.
;;;; Without git on PATH the tests skip.
(in-package #:aitools.workspace.integration-test)

(defun %bare-checkout-host (scratch directory)
  (let ((environment (%environment scratch)))
    (aitools.workspace.infrastructure:make-host-workspace-boundaries
     :getenv (lambda (name) (cdr (assoc name environment :test (function string=))))
     :home-directory (lambda () (cdr (assoc "HOME" environment :test (function string=))))
     :current-directory (lambda () directory))))

(defun %bare-checkout-root (host &optional root)
  (aitools.workspace.application:call-with-resolved-root/k
   host
   :root root
   :on-resolved (function identity)
   :on-error (lambda (reason path) (error "root ~A ~A" reason path))))

(defun %bare-checkout-verdict (host root target)
  (aitools.workspace.application:call-with-workspace-boundary/k
   host root target
   :on-inside (lambda (path verdict) (declare (ignore path)) verdict)
   :on-outside (lambda (verdict lexical real) (declare (ignore lexical real)) verdict)))

(defun %build-bare-fixture (scratch)
  "A bare clone at SCRATCH/r.git, as ghq leaves it. Returns the bare directory."
  (let ((source (concatenate 'string scratch "/src"))
        (bare (concatenate 'string scratch "/r.git")))
    (git scratch scratch "init" "-q" source)
    (write-file (concatenate 'string source "/a.txt") "a")
    (git scratch source "add" "a.txt")
    (git scratch source "commit" "-q" "-m" "init")
    (git scratch scratch "clone" "-q" "--bare" source bare)
    bare))

(defun %expect-bare-metadata-refused (host root bare)
  (dolist (name '("HEAD" "config" "refs/x" "objects/x"))
    (expect (%bare-checkout-verdict host root (concatenate 'string bare "/" name))
            :to-be :outside-root)))

(describe "aitools workspace checkouts below a bare repository"
  (it "allows registered linked worktrees with absolute and relative paths"
    (unless (program-path "git") (skip "git is not on PATH; linked worktrees not created"))
    (call-with-scratch-directory
     (lambda (scratch)
       (let ((bare (%build-bare-fixture scratch)))
         (git scratch bare "worktree" "add" "-q" "-b" "gw" ".worktrees/gw" "HEAD")
         (git scratch bare "worktree" "add" "-q" "--relative-paths" "-b" "gwrel"
              ".worktrees/gwrel" "HEAD")
         (expect (sb-ext:octets-to-string
                  (read-file (concatenate 'string bare "/worktrees/gwrel/gitdir"))
                  :external-format :utf-8)
                 :to-equal (format nil "../../.worktrees/gwrel/.git~%"))
         (dolist (name '("gw" "gwrel"))
           (let* ((worktree (format nil "~A/.worktrees/~A" bare name))
                  (host (%bare-checkout-host scratch worktree))
                  (root (%bare-checkout-root host))
                  (option-root (%bare-checkout-root host worktree)))
             (expect (aitools.workspace.application:workspace-root-path root)
                     :to-equal worktree)
             (dolist (value (list root option-root))
               (expect (%bare-checkout-verdict host value "new.txt") :to-be :inside)
               (expect (%bare-checkout-verdict host value ".git") :to-be :git-directory)
               (expect (%bare-checkout-verdict
                        host value (format nil "../../worktrees/~A/index.lock" name))
                       :to-be :outside-root)
               (%expect-bare-metadata-refused host value bare))
             (make-symlink (format nil "../../worktrees/~A" name)
                           (concatenate 'string worktree "/meta"))
             (expect (%bare-checkout-verdict host root "meta/x") :to-be :symlink-escape)))))))
  (it "refuses a worktree whose registration was removed"
    (unless (program-path "git") (skip "git is not on PATH; linked worktrees not created"))
    (call-with-scratch-directory
     (lambda (scratch)
       (let* ((bare (%build-bare-fixture scratch))
              (worktree (concatenate 'string bare "/.worktrees/gone")))
         (git scratch bare "worktree" "add" "-q" "-b" "gone" ".worktrees/gone" "HEAD")
         (uiop:delete-directory-tree
          (sb-ext:parse-native-namestring (concatenate 'string bare "/worktrees/gone/"))
          :validate t)
         (let* ((host (%bare-checkout-host scratch worktree))
                (root (%bare-checkout-root host))
                (option-root (%bare-checkout-root host worktree)))
           (expect (aitools.workspace.application:workspace-root-path root) :to-equal bare)
           (expect (%bare-checkout-verdict host root ".worktrees/gone/new.txt")
                   :to-be :git-directory)
           (expect (%bare-checkout-verdict host option-root "new.txt")
                   :to-be :git-directory)))))))
