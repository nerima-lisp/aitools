(in-package #:aitools.store.test)

(describe "aitools.store archive symlink prefix checker"
          (it "checks disk and staged links, replacements, path boundaries, and descriptor cleanup"
              (call-with-temp-store
               (lambda (store)
                 (sb-posix:mkdir (disk-path store "base") #o755)
                 (sb-posix:mkdir (disk-path store "base/nested") #o755)
                 (progn
                   (sb-posix:symlink "target" (disk-path store "base/link"))
                   (with-open-file (stream (disk-path store "base/file") :direction :output)
                     (write-string "file" stream)))
                 (let* ((io (store-io-port store))
                        (opened 0)
                        (closed 0)
                        (injected-fault nil)
                        (tracked-io
                         (copy-store-io
                          io
                          :open-search-root
                          (lambda (path)
                            (multiple-value-bind (fd identity)
                                (funcall (store-io-open-search-root io) path)
                              (incf opened)
                              (values fd identity)))
                          :open-search-entry
                          (lambda (dirfd name path)
                            (multiple-value-bind (kind fd identity)
                                (funcall (store-io-open-search-entry io) dirfd name path)
                              (cond ((and (eq injected-fault :parent) (string= name "..")) (setf identity "mismatched"))
                                      ((and (eq injected-fault :identity) (string= name ".") (string= path ".")) (setf identity "mismatched"))
                                      ((and (eq injected-fault :boundary) (string= name "boom"))
                                       (error (quote aitools.store.application:store-io-error)
                                              :operation "open" :path path :detail "injected boundary failure")))
                              (when fd (incf opened))
                              (values kind fd identity)))
                          :close-search-directory
                          (lambda (fd)
                            (incf closed)
                            (funcall (store-io-close-search-directory io) fd))))
                        (tracked-store (make-store tracked-io (store-root store) "/tmp"))
                        (checker (let ((index (make-tx-index)))
                                   (tx-index-put index
                                                 (format nil "base/Stra~Ce" (code-char #xDF))
                                                 (absent-state)
                                                 (symlink-state "target"))
                                   (tx-index-put index
                                                 (format nil "base/caf~C" (code-char #xE9))
                                                 (absent-state)
                                                 (symlink-state "target"))
                                   (make-archive-symlink-prefix-checker
                                    (aitools.store.application::%make-store-view tracked-store index)
                                    "base"
                                    nil))))
                   (unwind-protect
                       (progn
                         (let ((cursor (funcall checker :start)))
                           (unwind-protect
                               (progn
                                 (multiple-value-bind (next result)
                                     (funcall checker :enter cursor "nested")
                                   (setf cursor next)
                                   (expect result :to-be :clear))
                                 (multiple-value-bind (next result)
                                     (funcall checker :parent cursor)
                                   (setf cursor next)
                                   (expect result :to-be :clear))
                                 (multiple-value-bind (next result)
                                     (funcall checker :parent cursor)
                                   (setf cursor next)
                                   (expect result :to-be :clear))
                                 (multiple-value-bind (next result)
                                     (funcall checker :parent cursor)
                                   (setf cursor next)
                                   (expect result :to-be :outside)))
                             (funcall checker :finish cursor)
                             (funcall checker :finish cursor)))
                         (let ((cursor (funcall checker :start)))
                           (unwind-protect
                               (progn
                                 (multiple-value-bind (next result)
                                     (funcall checker :enter cursor "nested")
                                   (setf cursor next)
                                   (expect result :to-be :clear))
                                 (setf injected-fault :parent)
                                 (multiple-value-bind (next result)
                                     (funcall checker :parent cursor)
                                   (setf cursor next)
                                   (expect result :to-be :outside)))
                             (setf injected-fault nil)
                             (funcall checker :finish cursor)
                             (funcall checker :finish cursor)))
                         (let ((path-results
                                (list (list "link" :symlink)
                                      (list "file" :clear)
                                      (list "STRASSE" :symlink)
                                      (list (format nil "cafe~C" (code-char #x301)) :symlink))))
                           (dolist (path-result path-results)
                             (let ((cursor (funcall checker :start)))
                               (unwind-protect
                                   (multiple-value-bind (next result)
                                       (funcall checker :enter cursor (first path-result))
                                     (setf cursor next)
                                     (expect result :to-be (second path-result))
                                     (when (eq (second path-result) :symlink)
                                       (multiple-value-bind (parent parent-result)
                                           (funcall checker :parent cursor)
                                         (setf cursor parent)
                                         (expect parent-result :to-be :outside))))
                                 (funcall checker :finish cursor)
                                 (funcall checker :finish cursor)))))
                         (let* ((index (make-tx-index))
                                (replacement-checker
                                 (progn
                                   (tx-index-put index "base/link"
                                                 (symlink-state "target")
                                                 (file-state (make-string 64 :initial-element (code-char 97)) #o644))
                                   (make-archive-symlink-prefix-checker
                                    (aitools.store.application::%make-store-view tracked-store index)
                                    "base"
                                    nil
                                    :deletions-visible t))))
                           (let ((cursor (funcall replacement-checker :start)))
                             (unwind-protect
                                 (multiple-value-bind (next result)
                                     (funcall replacement-checker :enter cursor "link")
                                   (setf cursor next)
                                   (expect result :to-be :clear))
                               (funcall replacement-checker :finish cursor)
                               (funcall replacement-checker :finish cursor)))
                           (funcall replacement-checker :finish)
                           (funcall replacement-checker :finish))
                         (funcall checker :finish)
                         (funcall checker :finish)
                         (expect opened :to-be closed)
                         (progn
                            (let ((index (make-tx-index)))
                              (tx-index-put index "base/link"
                                            (symlink-state "target")
                                            (absent-state))
                              (dolist (deletions-visible (list nil t))
                                (let* ((deletion-checker
                                         (make-archive-symlink-prefix-checker
                                          (aitools.store.application::%make-store-view
                                           tracked-store index)
                                          "base" nil
                                          :deletions-visible deletions-visible))
                                       (cursor
                                         (funcall deletion-checker :start)))
                                  (unwind-protect
                                       (multiple-value-bind (next result)
                                           (funcall deletion-checker :enter
                                                    cursor "link")
                                         (setf cursor next)
                                         (expect result :to-be
                                                 (if deletions-visible
                                                     :symlink
                                                     :clear)))
                                    (funcall deletion-checker :finish cursor)
                                    (funcall deletion-checker :finish cursor)))))
                            (flet ((expect-refusal (open-delta thunk)
                                     (let ((before-open opened) (before-close closed) (condition nil))
                                       (handler-case (funcall thunk)
                                         (aitools.store.application:store-io-error (caught) (setf condition caught)))
                                       (expect condition :to-be-truthy)
                                       (expect (aitools.store.application:store-io-error-operation condition) :to-equal "open")
                                       (expect (- opened before-open) :to-be open-delta)
                                       (expect (- closed before-close) :to-be open-delta))))
                              (expect-refusal 0
                                              (lambda ()
                                                (make-archive-symlink-prefix-checker
                                                 (aitools.store.application::%make-store-view
                                                  (make-store io (disk-path store "base/file") "/tmp") nil)
                                                 "" nil)))
                              (let* ((missing-io
                                       (aitools.store.application:make-store-io
                                        :lstat (store-io-lstat io) :open-search-root nil :open-search-entry nil
                                        :close-search-directory nil :read-file (store-io-read-file io)
                                        :create-file (store-io-create-file io) :append-file (store-io-append-file io)
                                        :rename (store-io-rename io) :unlink (store-io-unlink io)
                                        :rmdir (store-io-rmdir io) :mkdir (store-io-mkdir io)
                                        :chmod (store-io-chmod io) :symlink (store-io-symlink io)
                                        :set-mtime (store-io-set-mtime io) :list-directory (store-io-list-directory io)
                                        :try-lock (store-io-try-lock io) :unlock (store-io-unlock io)
                                        :call-with-retry (aitools.store.application::store-io-call-with-retry io)
                                        :sleep (store-io-sleep io) :monotonic-ms (store-io-monotonic-ms io)
                                        :now (store-io-now io) :random-hex (store-io-random-hex io)))
                                     (missing-store (make-store missing-io (store-root store) "/tmp")))
                                (expect-refusal 0
                                                (lambda ()
                                                  (make-archive-symlink-prefix-checker
                                                   (aitools.store.application::%make-store-view missing-store nil)
                                                   "" nil))))
                              (setf injected-fault :identity)
                              (unwind-protect
                                   (expect-refusal 2
                                                   (lambda ()
                                                     (make-archive-symlink-prefix-checker
                                                      (aitools.store.application::%make-store-view tracked-store nil)
                                                      "base" nil)))
                                (setf injected-fault nil))
                              (let* ((before-open opened)
                                     (before-close closed)
                                     (start-checker
                                       (make-archive-symlink-prefix-checker
                                        (aitools.store.application::%make-store-view tracked-store nil)
                                        "" nil)))
                                (setf injected-fault :identity)
                                (unwind-protect
                                     (expect-refusal 1 (lambda () (funcall start-checker :start)))
                                  (setf injected-fault nil)
                                  (funcall start-checker :finish))
                                (expect (- opened before-open) :to-be 3)
                                (expect (- closed before-close) :to-be 3))
                              (setf injected-fault :boundary)
                              (unwind-protect
                                   (expect-refusal 3
                                                   (lambda ()
                                                     (make-archive-symlink-prefix-checker
                                                      (aitools.store.application::%make-store-view tracked-store nil)
                                                      "base/boom" nil)))
                                (setf injected-fault nil)))))
                     (funcall checker :finish))))))
          (it "does not treat lib64 as a prefix of staged lib"
              (call-with-temp-store
               (lambda (store)
                 (let* ((index (make-tx-index))
                        (checker
                         (progn
                           (tx-index-put index
                                         "base/lib"
                                         (absent-state)
                                         (symlink-state "target"))
                           (make-archive-symlink-prefix-checker
                            (aitools.store.application::%make-store-view store
                                                                         index)
                            "base"
                            nil))))
                   (unwind-protect
                       (let ((cursor (funcall checker :start)))
                         (unwind-protect
                             (multiple-value-bind (next result)
                                 (funcall checker :enter cursor "lib64")
                               (setf cursor next)
                               (expect result :to-be :clear))
                           (funcall checker :finish cursor)))
                     (funcall checker :finish)))))))

