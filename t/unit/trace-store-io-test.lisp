(in-package #:aitools/test)
(defun %trace-test-store-io (operation &key retry random
                                       (clock (cl-boundary-kit:make-clock))
                                       (sleeper (cl-boundary-kit:make-sleeper)))
  (cl-boundary-kit:make-boundary-context
   :clock clock
   :sleeper sleeper
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
           (cl-boundary-kit:boundary-context-require wrapped slot-name)
           arguments))
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
                  (funcall (cl-boundary-kit:boundary-context-require wrapped :read-file)
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

  (it "retains clock and sleeper objects plus retry and random functions without tracing them"
    (let* ((operation (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (values :retained :secondary)))
           (clock (cl-boundary-kit:make-fake-clock :start 0 :monotonic-start 0))
           (sleeper (cl-boundary-kit:make-recording-sleeper))
           (retry operation)
           (random operation)
           (io (%trace-test-store-io operation :retry retry :random random
                                     :clock clock :sleeper sleeper))
           (session (aitools/cli::%make-trace-session))
           (wrapped nil))
      (let ((aitools/cli::*trace-session* session))
        (setf wrapped (aitools/cli::%trace-store-io io))
        (expect (eq (cl-boundary-kit:boundary-context-require wrapped :clock) clock)
                :to-be t)
        (expect (eq (cl-boundary-kit:boundary-context-require wrapped :sleeper) sleeper)
                :to-be t)
        (expect (eq (cl-boundary-kit:boundary-context-require wrapped :call-with-retry) retry)
                :to-be t)
        (expect (eq (cl-boundary-kit:boundary-context-require wrapped :random-hex) random)
                :to-be t)
        (cl-boundary-kit:clock-now (cl-boundary-kit:boundary-context-require wrapped :clock))
        (cl-boundary-kit:clock-monotonic
         (cl-boundary-kit:boundary-context-require wrapped :clock))
        (cl-boundary-kit:sleeper-sleep
         (cl-boundary-kit:boundary-context-require wrapped :sleeper) 0.01d0)
        (funcall (cl-boundary-kit:boundary-context-require wrapped :call-with-retry)
                 nil operation nil nil)
        (funcall (cl-boundary-kit:boundary-context-require wrapped :random-hex) 4))
      (expect (cl-boundary-kit:recording-sleeper-calls sleeper)
              :to-equal '((:operation :sleep :arguments (0.01d0) :result 0.01d0)))
      (expect (aitools/cli::%trace-session-records session) :to-be nil))))
