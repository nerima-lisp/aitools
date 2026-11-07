(in-package #:aitools/test)
(defun %trace-test-store-io (operation &key retry random)
  (aitools.store.application:make-store-io
   :lstat operation
   :read-file operation
   :create-file operation
   :append-file operation
   :rename operation
   :unlink operation
   :rmdir operation
   :mkdir operation
   :chmod operation
   :chmod-directory-identity operation
   :sync-directory operation
   :symlink operation
   :set-mtime operation
   :list-directory operation
   :try-lock operation
   :unlock operation
   :call-with-retry (or retry operation)
   :random-hex (or random operation)))
(describe "aitools CLI store IO trace decorators"
  (it "returns the identical IO when tracing is disabled"
    (let* ((operation (lambda (&rest arguments)
                        (declare (ignore arguments))
                        :ok))
           (io (%trace-test-store-io operation))
           (aitools/cli::*trace-session* nil))
      (expect (eq (aitools/cli::%trace-store-io io) io) :to-be t)))

  (it-each ((:lstat "store.lstat" ("path"))
            (:read-file "store.read-file" ("path"))
            (:create-file "store.create-file" ("path" #()))
            (:append-file "store.append-file" ("path" #()))
            (:rename "store.rename" ("from" "to"))
            (:unlink "store.unlink" ("path"))
            (:rmdir "store.rmdir" ("path"))
            (:mkdir "store.mkdir" ("path"))
            (:chmod "store.chmod" ("path" 0))
            (:chmod-directory-identity "store.chmod-directory-identity" ("path" 0 nil))
            (:sync-directory "store.sync-directory" ("path"))
            (:symlink "store.symlink" ("target" "path"))
            (:set-mtime "store.set-mtime" ("path" 0))
            (:list-directory "store.list-directory" ("path"))
            (:try-lock "store.try-lock" ("path"))
            (:unlock "store.unlock" (nil)))
    "wraps the ~S slot as ~A and preserves multiple values"
    (slot-name span-name arguments)
    (let* ((operation (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (values :first :second)))
           (io (%trace-test-store-io operation))
           (session (aitools/cli::%make-trace-session))
           (wrapped nil))
      (let ((aitools/cli::*trace-session* session))
        (setf wrapped (aitools/cli::%trace-store-io io))
        (expect
         (multiple-value-list
          (apply
           (ecase slot-name
             (:lstat #'aitools.store.application:store-io-lstat)
             (:read-file #'aitools.store.application:store-io-read-file)
             (:create-file #'aitools.store.application:store-io-create-file)
             (:append-file #'aitools.store.application:store-io-append-file)
             (:rename #'aitools.store.application:store-io-rename)
             (:unlink #'aitools.store.application:store-io-unlink)
             (:rmdir #'aitools.store.application:store-io-rmdir)
             (:mkdir #'aitools.store.application:store-io-mkdir)
             (:chmod #'aitools.store.application:store-io-chmod)
             (:chmod-directory-identity #'aitools.store.application:store-io-chmod-directory-identity)
             (:sync-directory #'aitools.store.application:store-io-sync-directory)
             (:symlink #'aitools.store.application:store-io-symlink)
             (:set-mtime #'aitools.store.application:store-io-set-mtime)
             (:list-directory #'aitools.store.application:store-io-list-directory)
             (:try-lock #'aitools.store.application:store-io-try-lock)
             (:unlock #'aitools.store.application:store-io-unlock))
           wrapped arguments))
         :to-equal '(:first :second))
        (expect
         (mapcar #'observability-kit:span-record-name
                 (aitools/cli::%trace-session-records session))
         :to-equal (list span-name)))))

  (it-each ((:store-io-error :unavailable)
            (:simple-error :error))
    "preserves the exact ~S condition and maps store IO failures to ~S"
    (condition-kind outcome)
    (let* ((condition
             (ecase condition-kind
               (:store-io-error
                (make-condition 'aitools.store.application:store-io-error
                                :operation "read-file" :path "private-path-marker"))
               (:simple-error
                (make-condition 'simple-error
                                :format-control "private-condition-marker"
                                :format-arguments nil))))
           (operation (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (error condition)))
           (io (%trace-test-store-io operation))
           (session (aitools/cli::%make-trace-session))
           (wrapped nil)
           (caught nil))
      (let ((aitools/cli::*trace-session* session))
        (setf wrapped (aitools/cli::%trace-store-io io))
        (setf caught
              (handler-case
                  (funcall (aitools.store.application:store-io-read-file wrapped)
                           "private-path-marker")
                (error (raised) raised))))
      (expect (eq caught condition) :to-be t)
      (let* ((record (first (aitools/cli::%trace-session-records session)))
             (span-id (observability-kit:span-record-span-id record)))
        (expect (observability-kit:span-record-status-message record)
                :to-be-falsy)
        (expect (observability-kit:span-record-attributes record)
                :to-be-falsy)
        (expect (gethash span-id (aitools/cli::%trace-session-outcomes session))
                :to-be outcome))))

  (it "retains retry and random functions without tracing them"
    (let* ((operation (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (values :retained :secondary)))
           (retry operation)
           (random operation)
           (io (%trace-test-store-io operation :retry retry :random random))
           (session (aitools/cli::%make-trace-session))
           (wrapped nil))
      (let ((aitools/cli::*trace-session* session))
        (setf wrapped (aitools/cli::%trace-store-io io))
        (expect (eq (aitools.store.application::store-io-call-with-retry wrapped) retry)
                :to-be t)
        (expect (eq (aitools.store.application:store-io-random-hex wrapped) random)
                :to-be t)
        (funcall (aitools.store.application::store-io-call-with-retry wrapped)
                 nil operation nil nil)
        (funcall (aitools.store.application:store-io-random-hex wrapped) 4))
      (expect (aitools/cli::%trace-session-records session) :to-be nil))))
