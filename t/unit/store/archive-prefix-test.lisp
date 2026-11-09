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
                        (parent-mismatch nil)
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
                              (when (and parent-mismatch (string= name ".."))
                                (setf identity "mismatched"))
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
                                 (setf parent-mismatch t)
                                 (multiple-value-bind (next result)
                                     (funcall checker :parent cursor)
                                   (setf cursor next)
                                   (expect result :to-be :outside)))
                             (setf parent-mismatch nil)
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
                         (expect opened :to-be closed))
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

