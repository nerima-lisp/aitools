(in-package #:aitools.store.test)

(defun capture-posix-boundary-error (thunk)
  (handler-case (progn
                  (funcall thunk)
                  :accepted)
    (store-io-error (condition)
      (list :error
            (store-io-error-operation condition)
            (store-io-error-path condition)
            (store-io-error-errno condition)
            (store-io-error-detail condition)))))

(describe "Store POSIX file boundary"
          (it
           "creates files with exact modes and refuses duplicate and rootless writes"
           (call-with-temp-store
            (lambda (store)
              (let* ((root (store-root store))
                     (boundaries
                      (make-posix-store-io :root (concatenate 'string root "/")))
                     (mkdir (store-operation boundaries :mkdir))
                     (create (store-operation boundaries :create-file))
                     (directory (disk-path store "nested"))
                     (explicit (disk-path store "nested/explicit"))
                     (default (disk-path store "nested/default")))
                (funcall mkdir directory :mode #o755)
                (funcall create explicit (bytes "explicit") :mode #o640 :sync t)
                (funcall create default (bytes "default"))
                (expect (disk-text store "nested/explicit")
                        :to-equal
                        "explicit")
                (expect (disk-mode store "nested/explicit") :to-be #o640)
                (expect (disk-text store "nested/default") :to-equal "default")
                (expect (disk-mode store "nested/default") :to-be #o600)
                (let ((failure
                       (capture-posix-boundary-error
                        (lambda ()
                          (funcall create explicit (bytes "replace"))))))
                  (expect (subseq failure 0 4)
                          :to-equal
                          (list :error "create" explicit sb-posix:eexist)))
                (let* ((unrooted (make-posix-store-io))
                       (unrooted-create (store-operation unrooted :create-file))
                       (failure
                        (capture-posix-boundary-error
                         (lambda ()
                           (funcall unrooted-create explicit (bytes "refused"))))))
                  (expect (subseq failure 0 3)
                          :to-equal
                          (list :error "open" explicit)))))
            :io
            nil))
          (it "changes a directory only when its identity matches"
              (call-with-temp-store
               (lambda (store)
                 (let* ((root (store-root store))
                        (boundaries (make-posix-store-io :root root))
                        (mkdir (store-operation boundaries :mkdir))
                        (lstat (store-operation boundaries :lstat))
                        (chmod
                         (store-operation boundaries :chmod-directory-identity))
                        (path (disk-path store "directory")))
                   (funcall mkdir path :mode #o755)
                   (multiple-value-bind (kind mode target mtime links identity)
                       (funcall lstat path)
                     (declare (ignore mode target mtime links))
                     (expect kind :to-be :directory)
                     (expect identity :to-be-truthy)
                     (funcall chmod path #o700 identity))
                   (expect
                    (logand (sb-posix:stat-mode (sb-posix:stat path)) #o7777)
                    :to-be
                    #o700)
                   (let ((failure
                          (capture-posix-boundary-error
                           (lambda ()
                             (funcall chmod path #o750 "different-identity")))))
                     (expect (subseq failure 0 3)
                             :to-equal
                             (list :error "chmod" path))
                     (expect (fifth failure)
                             :to-equal
                             "directory identity changed"))
                   (expect
                    (logand (sb-posix:stat-mode (sb-posix:stat path)) #o7777)
                    :to-be
                    #o700)))
               :io
               nil))
          (it "creates, contends for, releases, and reopens a lock"
              (call-with-temp-store
               (lambda (store)
                 (let* ((root (store-root store))
                        (boundaries (make-posix-store-io :root root))
                        (mkdir (store-operation boundaries :mkdir))
                        (try-lock (store-operation boundaries :try-lock))
                        (unlock (store-operation boundaries :unlock))
                        (path (disk-path store "nested/lock"))
                        (missing (disk-path store "nested/missing-lock"))
                        (first nil))
                   (funcall mkdir (disk-path store "nested"))
                   (unwind-protect
                       (progn
                         (setf first (funcall try-lock path :create t))
                         (expect (integerp first) :to-be t)
                         (expect
                          (logand (sb-posix:stat-mode (sb-posix:stat path))
                                  #o7777)
                          :to-be
                          #o600)
                         (expect (funcall try-lock path) :to-be nil)
                         (funcall unlock first)
                         (setf first nil)
                         (let ((again (funcall try-lock path)))
                           (expect (integerp again) :to-be t)
                           (funcall unlock again))
                         (let ((failure
                                (capture-posix-boundary-error
                                 (lambda ()
                                   (funcall try-lock missing)))))
                           (expect (subseq failure 0 3)
                                   :to-equal
                                   (list :error "open" missing)))
                         (let ((failure
                                (cl-weave:with-mocked-functions
                                 (((symbol-function
                                    'aitools.store.infrastructure::%flock)
                                   (lambda (fd operation)
                                     (declare (ignore fd operation))
                                     (sb-alien:set-errno sb-posix:ebadf)
                                     -1)))
                                 (capture-posix-boundary-error
                                  (lambda ()
                                    (funcall try-lock path))))))
                           (expect (subseq failure 0 4)
                                   :to-equal
                                   (list :error "flock" path sb-posix:ebadf))))
                     (when first
                       (funcall unlock first)))))
               :io
               nil))
          (it "maps an fchmod failure from an invalid descriptor"
              (call-with-temp-store
               (lambda (store)
                 (let* ((path (disk-path store "chmod-failure"))
                        (real-set-file-mode
                          (symbol-function
                           'aitools.store.infrastructure::%set-created-file-mode))
                        (failure
                         (cl-weave:with-mocked-functions
                          (((symbol-function
                             'aitools.store.infrastructure::%set-created-file-mode)
                            (lambda (fd mode target)
                              (declare (ignore fd))
                              (funcall real-set-file-mode -1 mode target))))
                          (capture-posix-boundary-error
                           (lambda ()
                             (aitools.store.infrastructure::%create-file
                              (store-root store) path (bytes "contents") :mode #o600))))))
                   (expect (subseq failure 0 4)
                           :to-equal (list :error "fchmod" path sb-posix:ebadf))))
               :io nil)))
