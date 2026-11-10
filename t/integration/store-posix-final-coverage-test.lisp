(in-package #:aitools.store.test)

(defun posix-final-fd-closed-p (fd)
  (handler-case (progn
                  (sb-posix:fstat fd)
                  nil)
    (sb-posix:syscall-error ()
      t)))

(describe "Store final POSIX coverage"
          (it "executes the POSIX syscall macro expanders"
              (let ((syscall
                     (macroexpand-1
                      '(aitools.store.infrastructure::%syscall
                        ("open" "/unused")
                        :ok)))
                    (alien
                     (macroexpand-1
                      '(aitools.store.infrastructure::%alien-call
                        ("open" "/unused")
                        "open"
                        (function sb-alien:int))))
                    (stream
                     (macroexpand-1
                      '(aitools.store.infrastructure::%stream-errors
                        ("write" "/unused")
                        :ok))))
                (expect (first syscall) :to-be 'handler-case)
                (expect (first alien) :to-be 'let)
                (expect (first stream) :to-be 'handler-case)))
          (it "walks multiple pinned parents and rejects malformed paths"
              (call-with-temp-store
               (lambda (store)
                 (let* ((root (store-root store))
                        (path (disk-path store "nested/deep/file")))
                   (sb-posix:mkdir (disk-path store "nested") #o755)
                   (sb-posix:mkdir (disk-path store "nested/deep") #o755)
                   (expect
                    (aitools.store.infrastructure::%call-with-parent root
                                                                     path
                                                                     (lambda
                                                                         (fd
                                                                          name)
                                                                       (list
                                                                        name
                                                                        (=
                                                                         (logand
                                                                          (sb-posix:stat-mode
                                                                           (sb-posix:fstat
                                                                            fd))
                                                                          sb-posix:s-ifmt)
                                                                         sb-posix:s-ifdir))))
                    :to-equal
                    '("file" t))
                   (dolist
                       (bad-path
                        (list (concatenate 'string root "//")
                              (concatenate 'string root "/../outside")))
                     (let ((failure
                            (capture-posix-store-error
                             (lambda ()
                               (aitools.store.infrastructure::%call-with-parent
                                root
                                bad-path
                                (lambda (fd name)
                                  (declare (ignore fd name))
                                  :called))))))
                       (expect (fifth failure)
                               :to-equal
                               "path is not beneath the workspace root")))
                   (dolist
                       (root-and-path
                        (list (list "relative-root" path)
                              (list root nil)
                              (list root "relative/path")))
                     (let ((failure
                            (capture-posix-store-error
                             (lambda ()
                               (aitools.store.infrastructure::%call-with-parent
                                (first root-and-path)
                                (second root-and-path)
                                (lambda (fd name)
                                  (declare (ignore fd name))
                                  :called))))))
                       (expect (subseq failure 0 3)
                               :to-equal
                               (list :error
                                     "open"
                                     (or (second root-and-path) ""))))))
                 :io
                 nil)))
          (it
           "closes the search root after descriptor metadata reports a non-directory"
           (call-with-temp-store
            (lambda (store)
              (let ((opened-fd nil)
                    (root (store-root store)))
                (let ((failure
                       (cl-weave:with-mocked-functions
                        (((symbol-function
                           'aitools.store.infrastructure::%descriptor-info)
                          (lambda (fd path)
                            (declare (ignore path))
                            (setf opened-fd fd)
                            (values :file "test-identity"))))
                        (capture-posix-store-error
                         (lambda ()
                           (aitools.store.infrastructure::%open-search-root
                            root))))))
                  (expect (subseq failure 0 3)
                          :to-equal
                          (list :error "open" root))
                  (expect (fifth failure)
                          :to-equal
                          "workspace root is not a directory")
                  (expect (posix-final-fd-closed-p opened-fd) :to-be t))))
            :io
            nil))
          #+linux
(it "closes a Linux search entry when descriptor inspection fails"
  (call-with-temp-store
   (lambda (store)
     (let* ((root (store-root store))
            (path (disk-path store "entry"))
            (root-fd (aitools.store.infrastructure::%open-search-root root))
            (entry-fd nil)
            (failure nil))
       (put-file store "entry" "data")
       (unwind-protect
            (progn
              (setf failure
                    (handler-case
                        (cl-weave:with-mocked-functions
                          (((symbol-function 'aitools.store.infrastructure::%descriptor-info)
                            (lambda (fd path)
                              (declare (ignorable path))
                              (setf entry-fd fd)
                              (error 'store-io-error :operation "fstat" :path path
                                                     :errno sb-posix:eio))))
                          (aitools.store.infrastructure::%open-entry-linux root-fd "entry" path))
                      (store-io-error (condition) condition)))
              (expect (typep failure 'store-io-error) :to-be t)
              (expect (posix-final-fd-closed-p entry-fd) :to-be t))
         (sb-posix:close root-fd))))
   :io nil))
          (it
           "uses the default directory mode and maps exhausted retries to no result"
           (call-with-temp-store
            (lambda (store)
              (let* ((boundaries (make-posix-store-io :root (store-root store)))
                     (mkdir (store-operation boundaries :mkdir))
                     (path (disk-path store "default-mode")))
                (funcall mkdir path)
                (expect
                 (=
                  (logand (sb-posix:stat-mode (sb-posix:stat path))
                          sb-posix:s-ifmt)
                  sb-posix:s-ifdir)
                 :to-be
                 t)))
            :io
            nil)
           (let* ((boundaries (make-posix-store-io))
                  (call-with-retry
                   (store-operation boundaries :call-with-retry)))
             (expect
              (cl-weave:with-mocked-functions
               (((symbol-function 'resilience-kit:call-with-retry)
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (error 'resilience-kit:retry-exhausted))))
               (funcall call-with-retry
                        nil
                        (lambda ()
                          :unused)
                        nil
                        nil))
              :to-be
              nil))))
