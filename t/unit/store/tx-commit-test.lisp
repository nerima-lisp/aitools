(in-package #:aitools.store.test)

(describe "aitools.store application tx symlink recheck"
          (it
           "closes commit-time recheck descriptors on safe completion and refusal"
           (call-with-temp-store
            (lambda (store)
              (sb-posix:mkdir (disk-path store "safe") #o755)
              (sb-posix:mkdir (disk-path store "safe/nested") #o755)
              (let* ((io (store-io-port store))
                     (opened 0)
                     (closed 0)
                     (tracked-io
                      (copy-store-io io
                                     :open-search-root
                                     (lambda (path)
                                       (multiple-value-bind (fd identity)
                                           (funcall
                                            (store-io-open-search-root io)
                                            path)
                                         (when fd
                                           (incf opened))
                                         (values fd identity)))
                                     :open-search-entry
                                     (lambda (dirfd name path)
                                       (multiple-value-bind (kind fd identity)
                                           (funcall
                                            (store-io-open-search-entry io)
                                            dirfd
                                            name
                                            path)
                                         (when fd
                                           (incf opened))
                                         (values kind fd identity)))
                                     :close-search-directory
                                     (lambda (fd)
                                       (incf closed)
                                       (funcall
                                        (store-io-close-search-directory io)
                                        fd))))
                     (tracked-store
                      (make-store tracked-io (store-root store) "/tmp")))
                (flet ((symlink-index (target &optional (path "link"))
                         (let ((index (make-tx-index)))
                           (tx-index-put index
                                         path
                                         (absent-state)
                                         (symlink-state target))
                           index)))
                  (dolist
                      (entry
                       (list (list "safe/child" "link")
                             (list "safe/./child" "link")
                             (list "safe//child" "link")
                             (list "safe/../child" "link")
                             (list "child" "safe/nested/link")))
                    (let ((opened-before opened)
                          (closed-before closed))
                      (aitools.store.application::%recheck-tx-symlink-targets
                       tracked-store
                       (symlink-index (first entry) (second entry)))
                      (expect (> opened opened-before) :to-be t)
                      (expect (- opened opened-before)
                              :to-be
                              (- closed closed-before))))
                  (dolist
                      (target
                       (list nil
                             ""
                             "/outside"
                             (format nil "bad~Ctarget" (code-char 92))
                             (format nil "bad~Ctarget" (code-char 0))
                             "C:outside"
                             "safe/../../outside"))
                    (let ((opened-before opened)
                          (closed-before closed)
                          (refusal nil))
                      (handler-case (aitools.store.application::%recheck-tx-symlink-targets
                                     tracked-store
                                     (symlink-index target))
                        (aitools.store.application::store-refusal (condition)
                          (setf refusal condition)))
                      (expect refusal :to-be-truthy)
                      (expect
                       (aitools.store.application::store-refusal-code refusal)
                       :to-equal
                       "refusal.outside-workspace")
                      (expect (- opened opened-before)
                              :to-be
                              (- closed closed-before))))))))))

(describe "aitools.store transaction commit requests"
          (it "plans every staged entry kind before lexically earlier deletions"
              (call-with-temp-store
               (lambda (store)
                 (let* ((base-bytes
                         (sb-ext:string-to-octets "base"
                                                  :external-format
                                                  :utf-8))
                        (changed-bytes
                         (sb-ext:string-to-octets "changed"
                                                  :external-format
                                                  :utf-8))
                        (base-hash (with-workspace-lock
                           (store +default-lock-timeout-ms+
                                  :on-timeout
                                  (error "test store workspace lock timed out"))
                         (write-blob store base-bytes)))
                        (changed-hash (with-workspace-lock
                           (store +default-lock-timeout-ms+
                                  :on-timeout
                                  (error "test store workspace lock timed out"))
                         (write-blob store changed-bytes)))
                        (index (make-tx-index)))
                   (tx-index-put index
                                 "a-delete"
                                 (file-state base-hash #o644)
                                 (absent-state))
                   (tx-index-put index
                                 "b-create"
                                 (absent-state)
                                 (file-state changed-hash #o600))
                   (tx-index-put index
                                 "c-replace"
                                 (file-state base-hash #o644)
                                 (file-state changed-hash #o644))
                   (tx-index-put index
                                 "d-mtime"
                                 (file-state base-hash #o644)
                                 (file-state base-hash #o644 1234))
                   (tx-index-put index
                                 "e-chmod"
                                 (file-state base-hash #o644)
                                 (file-state base-hash #o600))
                   (tx-index-put index
                                 "f-mkdir"
                                 (absent-state)
                                 (directory-state #o755))
                   (tx-index-put index
                                 "g-dir-chmod"
                                 (directory-state #o755)
                                 (directory-state #o700))
                   (tx-index-put index
                                 "h-symlink"
                                 (absent-state)
                                 (symlink-state "target"))
                   (tx-index-put index
                                 "z-unchanged"
                                 (file-state base-hash #o644)
                                 (file-state base-hash #o644))
                   (let ((requests
                          (aitools.store.application::%tx-commit-requests store
                                                                          index)))
                     (expect (mapcar (function change-request-op) requests)
                             :to-equal
                             (list :write
                                   :write
                                   :mtime
                                   :chmod
                                   :mkdir
                                   :chmod
                                   :symlink
                                   :delete))
                     (expect (mapcar (function change-request-path) requests)
                             :to-equal
                             (list "b-create"
                                   "c-replace"
                                   "d-mtime"
                                   "e-chmod"
                                   "f-mkdir"
                                   "g-dir-chmod"
                                   "h-symlink"
                                   "a-delete"))
                     (expect (change-request-content (first requests))
                             :to-equalp
                             changed-bytes)
                     (expect (change-request-content (second requests))
                             :to-equalp
                             changed-bytes)
                     (expect (change-request-mode (first requests))
                             :to-be
                             #o600)
                     (expect (change-request-mtime (third requests))
                             :to-be
                             1234)
                     (expect (change-request-mode (third requests))
                             :to-be
                             #o644)
                     (expect (change-request-mode (fourth requests))
                             :to-be
                             #o600)
                     (expect (change-request-mode (sixth requests))
                             :to-be
                             #o700)
                     (expect (change-request-target (seventh requests))
                             :to-equal
                             "target")))))))
