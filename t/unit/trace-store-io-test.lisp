(in-package #:aitools/test)
(defun %trace-test-store-io (operation &key retry sleeper monotonic now random)
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
   :sleep (or sleeper operation)
   :monotonic-ms (or monotonic operation)
   :now (or now operation)
   :random-hex (or random operation)))
(describe "aitools CLI store IO trace decorators"
  (it "returns the identical IO when tracing is disabled"
    (let* ((operation (lambda (&rest arguments)
                        (declare (ignore arguments))
                        :ok))
           (io (%trace-test-store-io operation))
           (aitools/cli::*trace-session* nil))
      (expect (eq (aitools/cli::%trace-store-io io) io) :to-be t)))

  (it "wraps every filesystem and lock slot with its fixed name and preserves multiple values"
    (let* ((operation (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (values :first :second)))
           (io (%trace-test-store-io operation))
           (session (aitools/cli::%make-trace-session))
           (wrapped nil)
           (slots
             '((aitools.store.application:store-io-lstat "store.lstat" ("path"))
               (aitools.store.application:store-io-read-file "store.read-file" ("path"))
               (aitools.store.application:store-io-create-file "store.create-file" ("path" #()))
               (aitools.store.application:store-io-append-file "store.append-file" ("path" #()))
               (aitools.store.application:store-io-rename "store.rename" ("from" "to"))
               (aitools.store.application:store-io-unlink "store.unlink" ("path"))
               (aitools.store.application:store-io-rmdir "store.rmdir" ("path"))
               (aitools.store.application:store-io-mkdir "store.mkdir" ("path"))
               (aitools.store.application:store-io-chmod "store.chmod" ("path" 0))
               (aitools.store.application:store-io-chmod-directory-identity
                "store.chmod-directory-identity" ("path" 0 nil))
               (aitools.store.application:store-io-sync-directory "store.sync-directory" ("path"))
               (aitools.store.application:store-io-symlink "store.symlink" ("target" "path"))
               (aitools.store.application:store-io-set-mtime "store.set-mtime" ("path" 0))
               (aitools.store.application:store-io-list-directory "store.list-directory" ("path"))
               (aitools.store.application:store-io-try-lock "store.try-lock" ("path"))
               (aitools.store.application:store-io-unlock "store.unlock" (nil))))
           (returned nil))
      (let ((aitools/cli::*trace-session* session))
        (setf wrapped (aitools/cli::%trace-store-io io))
        (dolist (slot slots)
          (push (multiple-value-list
                 (apply (funcall (symbol-function (first slot)) wrapped)
                        (third slot)))
                returned)))
      (expect (nreverse returned)
              :to-equal (make-list (length slots) :initial-element '(:first :second)))
      (expect (mapcar #'observability-kit:span-record-name
                      (aitools/cli::%trace-session-records session))
              :to-equal (mapcar #'second slots))))

  (it "preserves the exact condition and maps store IO failures to unavailable"
    (dolist (case
             (list (list (make-condition 'aitools.store.application:store-io-error
                                         :operation "read-file" :path "private-path-marker")
                         :unavailable)
                   (list (make-condition 'simple-error
                                         :format-control "private-condition-marker"
                                         :format-arguments nil)
                         :error)))
      (let* ((condition (first case))
             (outcome (second case))
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
                    :to-be outcome)))))

  (it "retains sleep, clock, random, and retry functions without tracing them"
    (let* ((operation (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (values :retained :secondary)))
           (retry operation)
           (sleeper operation)
           (monotonic operation)
           (now operation)
           (random operation)
           (io (%trace-test-store-io operation :retry retry :sleeper sleeper
                                     :monotonic monotonic :now now :random random))
           (session (aitools/cli::%make-trace-session))
           (wrapped nil))
      (let ((aitools/cli::*trace-session* session))
        (setf wrapped (aitools/cli::%trace-store-io io))
        (expect (eq (aitools.store.application::store-io-call-with-retry wrapped) retry)
                :to-be t)
        (expect (eq (aitools.store.application:store-io-sleep wrapped) sleeper) :to-be t)
        (expect (eq (aitools.store.application:store-io-monotonic-ms wrapped) monotonic)
                :to-be t)
        (expect (eq (aitools.store.application:store-io-now wrapped) now) :to-be t)
        (expect (eq (aitools.store.application:store-io-random-hex wrapped) random)
                :to-be t)
        (funcall (aitools.store.application::store-io-call-with-retry wrapped)
                 nil operation operation operation)
        (funcall (aitools.store.application:store-io-sleep wrapped) 0)
        (funcall (aitools.store.application:store-io-monotonic-ms wrapped))
        (funcall (aitools.store.application:store-io-now wrapped))
        (funcall (aitools.store.application:store-io-random-hex wrapped) 4))
      (expect (aitools/cli::%trace-session-records session) :to-be nil))))
