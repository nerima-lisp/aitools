(in-package #:aitools.store.test)
(describe "Store write and filesystem edge coverage"
  (it "returns mtimes for files and resolves absolute symlinks before path checks"
    (call-with-temp-store
     (lambda (store)
       (put-file store "file.txt" "contents")
       (sb-posix:mkdir (disk-path store "directory") #o755)
       (expect (workspace-mtime store "file.txt") :to-be-truthy)
       (expect (workspace-mtime store "directory") :to-be nil)
       (expect (entry-state-mtime (aitools.store.application::%state-with-mtime store "file.txt")) :to-be-truthy)
       (let ((state (aitools.store.application::%state-with-mtime store "directory")))
         (expect (entry-state-kind state) :to-be :directory)
         (expect (entry-state-mtime state) :to-be nil))
       (let ((root-link (disk-path store "root-link"))
             (git-link (disk-path store "git-link")))
         (sb-posix:symlink (store-root store) root-link)
         (expect (aitools.store.application::%real-path store root-link) :to-equal (store-root store))
         (sb-posix:symlink (join-path (store-root store) ".git") git-link)
         (expect (aitools.store.application::%path-violation store "git-link/config") :to-equal "is inside a .git directory")))
     :io nil))
  (it "cleans temporary files and directories on return and unwind"
    (call-with-temp-store
     (lambda (store)
       (let* ((state (store-state-directory store))
              (state-home (subseq state 0 (position #\/ state :from-end t)))
              (file (disk-path store "temp-file"))
              (failed-file (disk-path store "failed-temp-file"))
              (unlink (store-operation (store-boundaries store) :unlink))
              (boundaries
                (cl-boundary-kit:boundary-context-with
                 (store-boundaries store) :unlink
                 (lambda (path)
                   (if (string= path failed-file)
                       (error 'store-io-error :operation "unlink" :path path :detail "denied")
                       (funcall unlink path)))))
              (broken (make-store boundaries (store-root store) state-home)))
         (expect (aitools.store.application::call-with-temp-file
                  store file (bytes "contents")
                  (lambda (path)
                    (expect (octets-string (funcall (store-operation (store-boundaries store) :read-file) path))
                            :to-equal "contents")
                    :returned))
                 :to-be :returned)
         (expect (entry-state-kind (workspace-state store "temp-file")) :to-be :absent)
         (signals error (aitools.store.application::with-temp-file (path store file (bytes "temporary"))
                          (error "callback failure")))
         (expect (entry-state-kind (workspace-state store "temp-file")) :to-be :absent)
         (let ((condition
                 (handler-case
                     (progn (aitools.store.application::with-temp-file (path broken failed-file (bytes "left"))
                              (error "original callback failure")) nil)
                   (error (condition) condition))))
           (expect (typep condition 'simple-error) :to-be t)
           (expect (search "original callback failure" (princ-to-string condition)) :to-be-truthy))
         (expect (entry-state-kind (workspace-state store "failed-temp-file")) :to-be :file)
         (let ((directory (disk-path store "temp-directory")))
           (expect
            (aitools.store.application::call-with-temp-dir
             store directory
             (lambda (path)
               (funcall (store-operation (store-boundaries store) :mkdir) (join-path path "child") :mode #o700)
               :returned))
            :to-be :returned)
           (expect (entry-state-kind (workspace-state store "temp-directory")) :to-be :absent)
           (aitools.store.application::with-temp-dir (path store directory)
             (funcall (store-operation (store-boundaries store) :create-file) (join-path path "file") (bytes "inside")))
           (expect (entry-state-kind (workspace-state store "temp-directory")) :to-be :absent))))
     :io nil))
  (it "preserves every rejection field and returns busy for a held zero-timeout lock"
    (call-with-temp-store
     (lambda (store)
       (multiple-value-bind (status code message fields)
           (commit-changes/k
            store '("edit")
            (lambda (commit reject)
              (declare (ignore commit))
              (funcall reject "selection.no-match" "no match" :candidates '("a.txt")
                       :diagnostics nil :conflicts '("b.txt")
                       :repairs '("repair") :path "a.txt"))
            :lock-timeout-ms 0
            :on-committed (lambda (&rest args) (declare (ignore args)) :committed)
            :on-rejected
            (lambda (code message &key (candidates nil candidates-p) (diagnostics nil diagnostics-p)
                     (conflicts nil conflicts-p) (repairs nil repairs-p) (path nil path-p))
              (values :rejected code message (list :candidates (list candidates-p candidates)
                    :diagnostics (list diagnostics-p diagnostics) :conflicts (list conflicts-p conflicts)
                    :repairs (list repairs-p repairs) :path (list path-p path))))
            :on-busy (lambda () :busy))
         (expect status :to-be :rejected)
         (expect code :to-equal "selection.no-match")
         (expect message :to-equal "no match")
         (expect fields :to-equal
                 (list :candidates (list t '("a.txt")) :diagnostics (list t nil)
                       :conflicts (list t '("b.txt")) :repairs (list t '("repair"))
                       :path (list t "a.txt"))))
       (let* ((try-lock (store-operation (store-boundaries store) :try-lock))
              (unlock (store-operation (store-boundaries store) :unlock))
              (handle (funcall try-lock (lock-file-path (store-state-directory store)) :create t))
              (validated nil))
         (expect handle :to-be-truthy)
         (unwind-protect
              (progn
                (expect (commit-changes/k store '("edit")
                         (lambda (commit reject) (declare (ignore reject))
                           (setf validated t) (funcall commit '()))
                         :lock-timeout-ms 0
                         :on-committed (lambda (&rest args) (declare (ignore args)) :committed)
                         :on-rejected (lambda (&rest args) (declare (ignore args)) :rejected)
                         :on-busy (lambda () :busy))
                       :to-be :busy)
                (expect validated :to-be nil))
           (funcall unlock handle))))
     :io nil))
  (it "turns a preflight store I/O failure into an environment.io rejection"
    (call-with-temp-store
     (lambda (store)
       (let ((code nil))
         (expect
          (with-workspace-lock (store 1000 :on-timeout :busy)
            (aitools.kernel.domain:flow-call
                (aitools.store.application::%plan-and-commit/k store '("test") nil
                 :preflight-check (lambda ()
                                    (error 'store-io-error :operation "lstat" :path (disk-path store "target"))))
              (:on-committed (op-id results) (declare (ignore op-id results)) :committed)
              (:on-rejected (rejection-code message)
                (declare (ignore message)) (setf code rejection-code) :rejected)))
          :to-be :rejected)
         (expect code :to-equal "environment.io") (expect (intent-files store) :to-equal '())))
     :io nil)))

(describe "Store symlink and path validation coverage"
  (it "resolves relative symlinks and distinguishes .git paths"
    (call-with-temp-store
     (lambda (store)
       (put-file store "target.txt" "contents")
       (sb-posix:symlink "target.txt" (disk-path store "relative-link"))
       (expect (aitools.store.application::%real-path
                store (disk-path store "relative-link"))
               :to-equal (disk-path store "target.txt"))
       (expect (aitools.store.application::%path-violation store "safe.txt")
               :to-be nil)
       (expect (aitools.store.application::%path-violation store ".git/config")
               :to-equal "is inside a .git directory"))
     :io nil)))
