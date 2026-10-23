(in-package #:aitools/cli)
(defun %trace-store-condition-outcome (condition)
  (if (typep condition 'aitools.store.application:store-io-error)
      :unavailable
    :error))
(defun %make-trace-store-operation (operation name)
  (lambda (&rest arguments)
    (%call-with-trace-span
     :io name
     (lambda () (apply operation arguments))
     :condition-outcome-function #'%trace-store-condition-outcome)))
(defun %trace-store-io (io)
  (if (null *trace-session*)
      io
    (let ((overrides
           (loop for (slot reader name) in
                 '((:lstat aitools.store.application:store-io-lstat "store.lstat")
                   (:read-file aitools.store.application:store-io-read-file "store.read-file")
                   (:create-file aitools.store.application:store-io-create-file "store.create-file")
                   (:append-file aitools.store.application:store-io-append-file "store.append-file")
                   (:rename aitools.store.application:store-io-rename "store.rename")
                   (:unlink aitools.store.application:store-io-unlink "store.unlink")
                   (:rmdir aitools.store.application:store-io-rmdir "store.rmdir")
                   (:mkdir aitools.store.application:store-io-mkdir "store.mkdir")
                   (:chmod aitools.store.application:store-io-chmod "store.chmod")
                   (:chmod-directory-identity
                    aitools.store.application:store-io-chmod-directory-identity
                    "store.chmod-directory-identity")
                   (:sync-directory aitools.store.application:store-io-sync-directory
                                    "store.sync-directory")
                   (:symlink aitools.store.application:store-io-symlink "store.symlink")
                   (:set-mtime aitools.store.application:store-io-set-mtime "store.set-mtime")
                   (:list-directory aitools.store.application:store-io-list-directory
                                    "store.list-directory")
                   (:try-lock aitools.store.application:store-io-try-lock "store.try-lock")
                   (:unlock aitools.store.application:store-io-unlock "store.unlock"))
                 for operation = (funcall reader io)
                 when operation
                 append (list slot (%make-trace-store-operation operation name)))))
      (apply #'aitools.store.application:copy-store-io io overrides))))
