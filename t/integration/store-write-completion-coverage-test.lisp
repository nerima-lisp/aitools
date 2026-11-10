(in-package #:aitools.store.test)

(describe "Store write completion and recovery edge coverage"
  (it "keeps preparation cleanup errors from replacing the original callback error"
    (call-with-temp-store
     (lambda (store)
       (put-file store "seed.txt" "before")
       (expect (commit store (list (write-file-request "seed.txt" (bytes "after")))) :to-be :committed)
       (let* ((state (store-state-directory store))
              (state-home (subseq state 0 (position #\/ state :from-end t)))
              (temp-path (disk-path store ".aitools-prepared.tmp"))
              (intent-path (join-path state "commit" "prepared.json"))
              (unlink (store-operation (store-boundaries store) :unlink))
              (boundaries
                (cl-boundary-kit:boundary-context-with
                 (store-boundaries store) :unlink
                 (lambda (path)
                   (if (string= path intent-path)
                       (error 'store-io-error :operation "unlink" :path path :detail "denied")
                       (funcall unlink path)))))
              (broken (make-store boundaries (store-root store) state-home))
              (step (make-intent-step :op :replace :path "target" :temp ".aitools-prepared.tmp"))
              (condition nil))
         (funcall (store-operation (store-boundaries store) :create-file) temp-path (bytes "temp"))
         (funcall (store-operation (store-boundaries store) :create-file) intent-path (bytes "pending"))
         (setf condition
               (handler-case
                   (progn
                     (aitools.store.application::with-prepared-intent (broken intent-path (list step))
                       (error "original callback failure"))
                     nil)
                 (error (condition) condition)))
         (expect (typep condition 'simple-error) :to-be t)
         (expect (search "original callback failure" (princ-to-string condition)) :to-be-truthy)
         (expect (entry-state-kind (workspace-state store ".aitools-prepared.tmp")) :to-be :absent)
         (expect (octets-string (funcall (store-operation (store-boundaries store) :read-file) intent-path))
                 :to-equal "pending")))
     :io nil))

  (it "reports a created directory without an identity as a committed I/O failure"
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

  (it "finishes a committed write when garbage collection reports store I/O"
    (call-with-temp-store
     (lambda (plain)
       (put-file plain "a.txt" "before")
       (let* ((base (store-boundaries plain))
              (list-directory (store-operation base :list-directory))
              (blobs (blobs-directory (store-state-directory plain)))
              (state (store-state-directory plain))
              (state-home (subseq state 0 (position #\/ state :from-end t)))
              (failed nil)
              (boundaries
                (cl-boundary-kit:boundary-context-with
                 base :list-directory
                 (lambda (path)
                   (if (string= path blobs)
                       (progn
                         (setf failed t)
                         (error 'store-io-error :operation "list-directory" :path path :detail "denied"))
                       (funcall list-directory path)))))
              (broken (make-store boundaries (store-root plain) state-home)))
         (expect (commit broken (list (write-file-request "a.txt" (bytes "after")))) :to-be :committed)
         (expect failed :to-be t)
         (expect (disk-text plain "a.txt") :to-equal "after")
         (expect (intent-files plain) :to-equal '())))
     :io nil))

  (it "collects an orphan blob through the direct garbage-collection operation"
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
     :io nil))

  (it "ignores short and non-json intent names while recovering a torn json record"
    (call-with-temp-store
     (lambda (store)
       (expect (commit store (list (write-file-request "seed.txt" (bytes "seed")))) :to-be :committed)
       (let ((directory (commit-directory (store-state-directory store))))
         (dolist (name '("x" "notes.txt" "torn.json"))
           (funcall (store-operation (store-boundaries store) :create-file)
                    (join-path directory name) (bytes "unfinished")))
         (multiple-value-bind (status entries) (recover store)
           (expect status :to-be :recovered)
           (expect entries :to-equal (list (cons "torn" "discarded"))))
         (expect (sort (copy-list (intent-files store)) #'string<)
                 :to-equal '("notes.txt" "x"))
         (expect (recover store) :to-be :none)))
     :io nil)))
