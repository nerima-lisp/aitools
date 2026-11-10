(in-package #:aitools.store.test)

(describe "archive prefix parent descriptor validation"
  (it "rejects parent results without a matching directory handle"
    (call-with-temp-store
     (lambda (store)
       (sb-posix:mkdir (disk-path store "base") #o755)
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
                     (when fd (incf opened))
                     (values fd identity)))
                 :open-search-entry
                 (lambda (dirfd name path)
                   (multiple-value-bind (kind fd identity)
                       (funcall (store-io-open-search-entry io) dirfd name path)
                     (when fd (incf opened))
                     (when (string= name "..")
                       (case injected-fault
                         (:not-directory
                          (setf kind :file))
                         (:no-descriptor
                          (when fd
                            (incf closed)
                            (funcall (store-io-close-search-directory io) fd))
                          (setf fd nil))))
                     (values kind fd identity)))
                 :close-search-directory
                 (lambda (fd)
                   (incf closed)
                   (funcall (store-io-close-search-directory io) fd))))
              (tracked-store (make-store tracked-io (store-root store) "/tmp"))
              (checker
                (aitools.store.application:make-archive-symlink-prefix-checker
                 (aitools.store.application::%make-store-view tracked-store nil)
                 "" nil)))
         (unwind-protect
              (dolist (fault (list :not-directory :no-descriptor))
                (let ((cursor (funcall checker :start)))
                  (unwind-protect
                       (progn
                         (multiple-value-bind (next result)
                             (funcall checker :enter cursor "base")
                           (setf cursor next)
                           (expect result :to-be :clear))
                         (setf injected-fault fault)
                         (multiple-value-bind (next result)
                             (funcall checker :parent cursor)
                           (setf cursor next)
                           (expect result :to-be :outside)))
                    (setf injected-fault nil)
                    (funcall checker :finish cursor))))
           (funcall checker :finish))
         (expect opened :to-be closed)))
     :io nil)))
