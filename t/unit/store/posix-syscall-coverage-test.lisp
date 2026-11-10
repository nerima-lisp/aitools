(in-package #:aitools.store.test)

(defun capture-posix-store-error (thunk)
  (handler-case (progn
                  (funcall thunk)
                  :accepted)
    (store-io-error (condition)
      (list :error
            (store-io-error-operation condition)
            (store-io-error-path condition)
            (store-io-error-errno condition)
            (store-io-error-detail condition)))))

(describe "Store POSIX syscall boundary"
          (it "converts syscall failures and preserves successful calls"
              (call-with-temp-store
               (lambda (store)
                 (let* ((path (disk-path store "present"))
                        (missing (disk-path store "missing"))
                        (fd nil))
                   (put-file store "present" "data")
                   (setf fd (aitools.store.infrastructure::%syscall
                             ("open" path)
                             (sb-posix:open path sb-posix:o-rdonly)))
                   (expect (integerp fd) :to-be t)
                   (sb-posix:close fd)
                   (let ((failure
                          (capture-posix-store-error
                           (lambda ()
                             (aitools.store.infrastructure::%syscall
                              ("open" missing)
                              (sb-posix:open missing sb-posix:o-rdonly))))))
                     (expect (subseq failure 0 4)
                             :to-equal
                             (list :error "open" missing sb-posix:enoent)))))
               :io
               nil))
          (it "maps stream errors and preserves the no-error result"
              (let* ((path "/stream-target")
                     (success
                      (aitools.store.infrastructure::%stream-errors
                       ("write" path)
                       :written))
                     (failure
                      (capture-posix-store-error
                       (lambda ()
                         (aitools.store.infrastructure::%stream-errors
                          ("write" path)
                          (error 'stream-error :stream *standard-output*))))))
                (expect success :to-be :written)
                (expect (subseq failure 0 3)
                        :to-equal
                        (list :error "write" path))
                (expect (fifth failure) :to-be-truthy)))
          (it "opens a search root and maps open failures"
              (call-with-temp-store
               (lambda (store)
                 (let* ((root (store-root store))
                        (fd
                         (aitools.store.infrastructure::%open-search-root root)))
                   (expect (integerp fd) :to-be t)
                   (sb-posix:close fd)
                   (let* ((missing (disk-path store "missing-root"))
                          (failure
                           (capture-posix-store-error
                            (lambda ()
                              (aitools.store.infrastructure::%open-search-root
                               missing)))))
                     (expect (subseq failure 0 4)
                             :to-equal
                             (list :error "open" missing sb-posix:enoent)))))
               :io
               nil))
          (it "controls whether open-directory-at follows symlinks"
              (call-with-temp-store
               (lambda (store)
                 (let* ((root (store-root store))
                        (path (disk-path store "linked"))
                        (root-fd nil))
                   (sb-posix:mkdir (disk-path store "real") #o755)
                   (sb-posix:symlink "real" path)
                   (unwind-protect
                       (progn
                         (setf root-fd (aitools.store.infrastructure::%open-search-root
                                        root))
                         (let ((followed-fd
                                (aitools.store.infrastructure::%open-directory-at
                                 root-fd
                                 "linked"
                                 path
                                 :follow
                                 t)))
                           (expect (integerp followed-fd) :to-be t)
                           (sb-posix:close followed-fd))
                         (let ((failure
                                (capture-posix-store-error
                                 (lambda ()
                                   (aitools.store.infrastructure::%open-directory-at
                                    root-fd
                                    "linked"
                                    path)))))
                           (expect (subseq failure 0 3)
                                   :to-equal
                                   (list :error "open" path)))
                         (let ((directory-fd
                                (aitools.store.infrastructure::%open-directory-at
                                 root-fd
                                 "real"
                                 path)))
                           (sb-posix:close directory-fd)))
                     (when root-fd
                       (sb-posix:close root-fd)))))
               :io
               nil))
          (it
           "pins parents, accepts the root itself, and refuses invalid roots"
           (call-with-temp-store
            (lambda (store)
              (let* ((root (store-root store))
                     (path (disk-path store "nested/file")))
                (sb-posix:mkdir (disk-path store "nested") #o755)
                (expect
                 (aitools.store.infrastructure::%call-with-parent root
                                                                  path
                                                                  (lambda
                                                                      (fd name)
                                                                    (list name
                                                                          (=
                                                                           (logand
                                                                            (sb-posix:stat-mode
                                                                             (sb-posix:fstat
                                                                              fd))
                                                                            sb-posix:s-ifmt)
                                                                           sb-posix:s-ifdir))))
                 :to-equal
                 '("file" t))
                (expect
                 (aitools.store.infrastructure::%call-with-parent root
                                                                  root
                                                                  (lambda
                                                                      (fd name)
                                                                    (list name
                                                                          (integerp
                                                                           fd))))
                 :to-equal
                 '("." t))
                (dolist
                    (invalid-root-and-path
                     (list (list nil path)
                           (list "/" path)
                           (let ((missing (disk-path store "missing-root")))
                             (list missing missing))))
                  (let ((failure
                         (capture-posix-store-error
                          (lambda ()
                            (aitools.store.infrastructure::%call-with-parent
                             (first invalid-root-and-path)
                             path
                             (lambda (fd name)
                               (declare (ignore fd name))
                               :called))))))
                    (expect (subseq failure 0 3)
                            :to-equal
                            (list :error "open" (second invalid-root-and-path)))))))
            :io
            nil))
          #+linux
  (it "classifies Linux search entries and maps absent and unrelated errors"
    (call-with-temp-store
     (lambda (store)
       (let* ((root (store-root store))
              (root-fd nil)
              (directory-fd nil)
              (open-entry #'aitools.store.infrastructure::%open-entry-linux))
         (sb-posix:mkdir (disk-path store "directory") #o755)
         (put-file store "file" "data")
         (sb-posix:symlink "file" (disk-path store "link"))
         (unwind-protect
              (progn
                (setf root-fd (aitools.store.infrastructure::%open-search-root root))
                (multiple-value-bind (kind fd identity)
                    (funcall open-entry root-fd "directory" (disk-path store "directory"))
                  (setf directory-fd fd)
                  (expect kind :to-be :directory)
                  (expect (integerp fd) :to-be t)
                  (expect identity :to-be-truthy)
                  (sb-posix:close fd)
                  (setf directory-fd nil))
                (dolist (name-and-kind `(("file" :file) ("link" :symlink)))
                  (multiple-value-bind (kind fd identity)
                      (funcall open-entry root-fd (first name-and-kind)
                               (disk-path store (first name-and-kind)))
                    (expect kind :to-be (second name-and-kind))
                    (expect fd :to-be nil)
                    (expect identity :to-be nil)))
                (dolist (name (list "absent" "file/child"))
                  (multiple-value-bind (kind fd identity)
                      (funcall open-entry root-fd name (disk-path store name))
                    (expect (list kind fd identity) :to-equal '(:absent nil nil))))
                (let ((failure
                        (capture-posix-store-error
                         (lambda ()
                           (funcall open-entry -1 "file" (disk-path store "file"))))))
                  (expect (subseq failure 0 4)
                          :to-equal (list :error "open" (disk-path store "file") sb-posix:ebadf))))
           (when directory-fd (sb-posix:close directory-fd))
           (when root-fd (sb-posix:close root-fd)))))
     :io nil)))
