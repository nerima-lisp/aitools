(in-package #:aitools.store.test)
(describe "aitools.store path hashes"
  (it-each ((:file "file" "payload" "payload")
            (:link "link" "target" "payload")
            (:dangling "dangling" "missing-target" "missing-target")
            (:directory-link "directory-link" "directory" "directory")
            (:nested-dangling "nested/dangling" "missing" "missing"))
      "hashes the disk view for ~A"
      (kind path target content)
    (call-with-temp-store
     (lambda (store)
       (let ((host (aitools.workspace.infrastructure:make-host-workspace-host)))
         (case kind
           (:file (put-file store path target))
           (:link
            (put-file store target content)
            (sb-posix:symlink target (disk-path store path)))
           (:dangling (sb-posix:symlink target (disk-path store path)))
           (:directory-link
            (put-file store "directory/seed" "seed")
            (sb-posix:symlink target (disk-path store path)))
           (:nested-dangling
            (put-file store "nested/seed" "seed")
            (sb-posix:symlink target (disk-path store path))))
         (expect (path-hash host (store-root store) (disk-view store)
                            (disk-path store path))
                 :to-equal (aitools.kernel.domain:content-hash (bytes content)))))
     :io nil))

  (it-each ((:directory "directory") (:absent "missing"))
      "returns no hash for a ~A path"
      (kind path)
    (call-with-temp-store
     (lambda (store)
       (when (eq kind :directory)
         (put-file store "directory/seed" "seed"))
       (let ((host (aitools.workspace.infrastructure:make-host-workspace-host)))
         (expect (path-hash host (store-root store) (disk-view store)
                            (disk-path store path))
                 :to-be nil)))
     :io nil))

  (it "returns no hash for the workspace root"
    (call-with-temp-store
     (lambda (store)
       (let ((host (aitools.workspace.infrastructure:make-host-workspace-host)))
         (expect (path-hash host (store-root store) (disk-view store)
                            (store-root store))
                 :to-be nil)))
     :io nil))
  (it "resolves the filesystem root"
    (call-with-temp-store
     (lambda (store)
       (expect (aitools.store.application::%real-path store "/")
               :to-equal "/"))
     :io nil))

  (it "uses the host for paths outside a view and returns nil when bytes cannot be read"
    (call-with-temp-store
     (lambda (store)
       (put-file store "file" "payload")
       (put-file store "../outside" "outside")
       (let* ((host (aitools.workspace.infrastructure:make-host-workspace-host))
              (unreadable-host
                (aitools.workspace.application:make-workspace-host
                 :list-directory (lambda (path)
                                   (aitools.workspace.application:host-list-directory host path))
                 :stat (lambda (path) (aitools.workspace.application:host-stat host path))
                 :read-link (lambda (path) (aitools.workspace.application:host-read-link host path))
                 :read-octets (lambda (path) (declare (ignore path)) nil)
                 :getenv (lambda (name) (aitools.workspace.application:host-getenv host name))
                 :home-directory (lambda () (aitools.workspace.application:host-home-directory host))
                 :current-directory (lambda () (aitools.workspace.application:host-current-directory host))))
              (outside (disk-path store "../outside")))
         (expect (path-hash host (store-root store) nil (disk-path store "file"))
                 :to-equal (aitools.kernel.domain:content-hash (bytes "payload")))
         (expect (path-hash host (store-root store) nil outside)
                 :to-equal (aitools.kernel.domain:content-hash (bytes "outside")))
         (expect (path-hash unreadable-host (store-root store) nil (disk-path store "file"))
                 :to-be nil)))
     :io nil))

  (it "hashes the target text when a symlink loop cannot be resolved"
    (call-with-temp-store
     (lambda (store)
       (sb-posix:symlink "loop-b" (disk-path store "loop-a"))
       (sb-posix:symlink "loop-a" (disk-path store "loop-b"))
       (let ((host (aitools.workspace.infrastructure:make-host-workspace-host)))
         (expect (path-hash host (store-root store) (disk-view store)
                            (disk-path store "loop-a"))
                 :to-equal (aitools.kernel.domain:content-hash (bytes "loop-b")))))
     :io nil))
  (it "rejects a write path whose parent symlink loops"
    (call-with-temp-store
     (lambda (store)
       (sb-posix:symlink "loop-b" (disk-path store "loop-a"))
       (sb-posix:symlink "loop-a" (disk-path store "loop-b"))
       (expect (aitools.store.application::%path-violation store "loop-a/pwn.txt")
               :to-equal "cannot be resolved (symlink loop)"))
     :io nil))
  (it "rejects an invalid relative path before resolving a write target"
    (call-with-temp-store
     (lambda (store)
       (expect (aitools.store.application::%path-violation store "../outside/pwn.txt")
               :to-equal "is not a workspace-relative path"))
     :io nil))
  (it "rejects a symlink into .git when the store root is /"
    (call-with-temp-store
     (lambda (store)
       (let* ((git-dir (disk-path store ".git"))
              (link-dir (disk-path store "linked"))
              (relative (concatenate 'string (subseq link-dir 1) "/config"))
              (root-store (make-store (store-boundaries store) "/" (disk-path store "state"))))
         (sb-posix:mkdir git-dir #o755)
         (sb-posix:symlink git-dir link-dir)
         (expect (store-root root-store) :to-equal "/")
         (expect (aitools.store.application::%path-violation root-store relative)
                 :to-equal "is inside a .git directory")))
     :io nil))

  (it "hashes staged file contents through a symlink in the tx view"
    (call-with-temp-store
     (lambda (store)
       (let ((tx (begin store))
             (host (aitools.workspace.infrastructure:make-host-workspace-host)))
         (stage store tx (list (write-file-request "staged" (bytes "staged"))))
         (sb-posix:symlink "staged" (disk-path store "staged-link"))
         (call-with-tx-view/k
          store tx
          :on-view
          (lambda (view)
            (expect (path-hash host (store-root store) view (disk-path store "staged"))
                    :to-equal (aitools.kernel.domain:content-hash (bytes "staged")))
            (expect (path-hash host (store-root store) view (disk-path store "staged-link"))
                    :to-equal (aitools.kernel.domain:content-hash (bytes "staged"))))
          :on-not-found (lambda () (expect :not-found :to-be nil)))))
     :io nil))
  (it "hashes a dangling symlink outside the view through the host"
    (call-with-temp-store
     (lambda (store)
       (sb-posix:symlink "missing-target" (disk-path store "../outside-link"))
       (let ((host (aitools.workspace.infrastructure:make-host-workspace-host)))
         (expect (path-hash host (store-root store) nil (disk-path store "../outside-link"))
                 :to-equal (aitools.kernel.domain:content-hash (bytes "missing-target")))))
     :io nil)))

