(in-package #:aitools.store.test)

(describe "Store application I/O final coverage"
  (it "normalizes roots, dispatches I/O boundaries, and calls fault hooks"
    (expect (aitools.store.application::%normalize-root "/workspace/") :to-equal "/workspace")
    (expect (aitools.store.application::%normalize-root "/") :to-equal "/")
    (signals error (aitools.store.application::%normalize-root "workspace"))
    (let ((observed nil))
      (let ((aitools.store.application::*fault-hook* nil))
        (expect (aitools.store.application::fault-point :test) :to-be nil))
      (let ((aitools.store.application::*fault-hook*
              (lambda (&rest event) (setf observed event))))
        (aitools.store.application::fault-point :after-intent "op-1"))
      (expect observed :to-equal (list :after-intent "op-1")))
    (call-with-temp-store
     (lambda (store)
       (expect (entry-state-kind (workspace-state store "missing")) :to-be :absent)
       (expect (workspace-mtime store "missing") :to-be nil))
     :io nil))

  (it "resolves absolute symlinks and checks a symlinked dot-git target"
    (call-with-temp-store
     (lambda (store)
       (put-file store "file.txt" "contents")
       (sb-posix:mkdir (disk-path store "directory") #o755)
       (expect (workspace-mtime store "file.txt") :to-be-truthy)
       (expect (workspace-mtime store "directory") :to-be nil)
       (expect (entry-state-mtime (aitools.store.application::%state-with-mtime store "file.txt"))
               :to-be-truthy)
       (expect (entry-state-mtime (aitools.store.application::%state-with-mtime store "directory"))
               :to-be nil)
       (let ((root-link (disk-path store "root-link"))
             (git-link (disk-path store "git-link")))
         (sb-posix:symlink (store-root store) root-link)
         (expect (aitools.store.application::%real-path store root-link) :to-equal (store-root store))
         (sb-posix:symlink (join-path (store-root store) ".git") git-link)
         (expect (aitools.store.application::%path-violation store "git-link/config")
                 :to-equal "is inside a .git directory")))
     :io nil))

  (it "runs temporary file and directory helpers and the preparation macro"
    (call-with-temp-store
     (lambda (store)
       (let ((file (disk-path store "call-temp"))
             (macro-file (disk-path store "macro-temp"))
             (directory (disk-path store "call-directory"))
             (macro-directory (disk-path store "macro-directory")))
         (expect (aitools.store.application::call-with-temp-file
                  store file (bytes "call")
                  (lambda (path)
                    (expect (entry-state-kind (workspace-state store "call-temp")) :to-be :file)
                    (expect (octets-string (funcall (store-operation (store-boundaries store) :read-file) path))
                            :to-equal "call")
                    :created))
                 :to-be :created)
         (expect (entry-state-kind (workspace-state store "call-temp")) :to-be :absent)
         (aitools.store.application::with-temp-file (path store macro-file (bytes "macro"))
           (expect (entry-state-kind (workspace-state store "macro-temp")) :to-be :file))
         (expect (entry-state-kind (workspace-state store "macro-temp")) :to-be :absent)
         (expect (aitools.store.application::call-with-temp-dir
                  store directory
                  (lambda (path)
                    (funcall (store-operation (store-boundaries store) :create-file)
                             (join-path path "child") (bytes "inside"))
                    :created))
                 :to-be :created)
         (expect (entry-state-kind (workspace-state store "call-directory")) :to-be :absent)
         (aitools.store.application::with-temp-dir (path store macro-directory)
           (funcall (store-operation (store-boundaries store) :create-file)
                    (join-path path "child") (bytes "inside")))
         (expect (entry-state-kind (workspace-state store "macro-directory")) :to-be :absent)
         (expect (aitools.store.application::with-prepared-intent (store nil nil) :prepared)
                 :to-be :prepared)))
     :io nil))

  (it "uses default rejection keywords and the default workspace lock timeout"
    (call-with-temp-store
     (lambda (store)
       (multiple-value-bind (status fields)
           (commit-changes/k
            store '("test")
            (lambda (commit reject)
              (declare (ignore commit))
              (funcall reject "selection.no-match" "no match"))
            :on-committed (lambda (&rest args) (declare (ignore args)) :committed)
            :on-rejected
            (lambda (code message &key (candidates nil candidates-p) (diagnostics nil diagnostics-p)
                     (conflicts nil conflicts-p) (repairs nil repairs-p) (path nil path-p))
              (declare (ignore code message))
              (values :rejected
                      (list (list candidates-p candidates) (list diagnostics-p diagnostics)
                            (list conflicts-p conflicts) (list repairs-p repairs) (list path-p path))))
            :on-busy (lambda () :busy))
         (expect status :to-be :rejected)
         (expect fields :to-equal '((nil nil) (nil nil) (nil nil) (nil nil) (nil nil)))))
     :io nil))

  (it "tightens an existing workspace state directory before taking the lock"
    (call-with-temp-store
     (lambda (store)
       (expect (commit store '()) :to-be :committed)
       (let* ((state (store-state-directory store))
              (state-root (workspace-state-root state)))
         (sb-posix:chmod state-root #o755)
         (sb-posix:chmod state #o755)
         (expect (commit store '()) :to-be :committed)
         (expect (logand #o777 (sb-posix:stat-mode (sb-posix:lstat state-root))) :to-be #o700)
         (expect (logand #o777 (sb-posix:stat-mode (sb-posix:lstat state))) :to-be #o700)))
     :io nil))

  (it "accepts identity-free legacy deletes and skips utime after the file vanished"
    (call-with-temp-store
     (lambda (store)
       (put-file store "legacy-delete" "contents")
       (let ((step (make-intent-step :op :unlink :path "legacy-delete"
                                     :before (workspace-state store "legacy-delete"))))
         (expect (aitools.store.application::%refuse-changed-deletion store step) :to-be nil))
       (let ((step (make-intent-step :op :utime :path "vanished" :mode #o644 :mtime 1000000000)))
         (aitools.store.application::%apply-step store step (make-hash-table :test 'equal))
         (expect (entry-state-kind (workspace-state store "vanished")) :to-be :absent)))
     :io nil))

  (it "keeps a committed mkdir intent when lstat cannot provide its identity"
    (call-with-temp-store
     (lambda (plain)
       (let* ((base (store-boundaries plain))
              (real-lstat (store-operation base :lstat))
              (real-mkdir (store-operation base :mkdir))
              (target (disk-path plain "new-directory"))
              (state (store-state-directory plain))
              (state-home (subseq state 0 (position #\/ state :from-end t)))
              (created nil)
              (boundaries
                (cl-boundary-kit:boundary-context-with
                 base
                 :mkdir (lambda (path &rest arguments)
                          (prog1 (apply real-mkdir path arguments)
                            (when (string= path target) (setf created t))))
                 :lstat (lambda (path)
                          (multiple-value-bind (kind mode symlink mtime links identity)
                              (funcall real-lstat path)
                            (if (and created (string= path target))
                                (values :directory mode symlink mtime links nil)
                                (values kind mode symlink mtime links identity))))))
              (broken (make-store boundaries (store-root plain) state-home))
              (condition
                (handler-case
                    (progn (commit broken (list (mkdir-request "new-directory" :mode #o755))) nil)
                  (store-committed-error (condition) condition))))
         (expect (store-io-error-operation condition) :to-equal "mkdir")
         (expect (store-io-error-path condition) :to-equal target)
         (expect (entry-state-kind (workspace-state plain "new-directory")) :to-be :directory)
         (expect (length (intent-files plain)) :to-be 1)))
     :io nil))

  (it "collects an orphan blob through the direct garbage collector"
    (call-with-temp-store
     (lambda (store)
       (put-file store "a.txt" "before")
       (expect (commit store (list (write-file-request "a.txt" (bytes "after")))) :to-be :committed)
       (let* ((directory (blobs-directory (store-state-directory store)))
              (kept (aitools.kernel.domain:content-hash (bytes "before")))
              (orphan (aitools.kernel.domain:content-hash (bytes "orphan"))))
         (funcall (store-operation (store-boundaries store) :create-file)
                  (join-path directory orphan) (bytes "orphan"))
         (collect-garbage store)
         (let ((names (funcall (store-operation (store-boundaries store) :list-directory) directory)))
           (expect (member kept names :test #'string=) :to-be-truthy)
           (expect (member orphan names :test #'string=) :to-be nil))))
     :io nil)))
