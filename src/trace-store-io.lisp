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
(defun %trace-store-io (boundaries)
  (if (null *trace-session*)
      boundaries
      (let ((overrides
              (loop for (key name) in
                    (quote ((:lstat "store.lstat")
                            (:read-file "store.read-file")
                            (:create-file "store.create-file")
                            (:append-file "store.append-file")
                            (:rename "store.rename")
                            (:unlink "store.unlink")
                            (:rmdir "store.rmdir")
                            (:mkdir "store.mkdir")
                            (:chmod "store.chmod")
                            (:chmod-directory-identity "store.chmod-directory-identity")
                            (:sync-directory "store.sync-directory")
                            (:symlink "store.symlink")
                            (:set-mtime "store.set-mtime")
                            (:list-directory "store.list-directory")
                            (:try-lock "store.try-lock")
                            (:unlock "store.unlock")))
                    for operation = (cl-boundary-kit:boundary-context-get boundaries key)
                    when operation
                      append (list key (%make-trace-store-operation operation name)))))
        (apply (function cl-boundary-kit:boundary-context-with) boundaries overrides))))
