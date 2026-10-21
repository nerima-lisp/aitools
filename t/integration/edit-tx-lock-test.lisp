(in-package #:aitools.edit.test)

(describe "aitools edit lock contention"
  (it "reports environment.busy when a tx write cannot take the workspace lock"
    (with-workspace ()
      (put "a.txt" "a")
      (let* ((tx (begin-tx))
             (result nil)
             (store (open-store *root*))
             (io (aitools.store.application:store-io-port store))
             (path (aitools.store.domain:lock-file-path (aitools.store.application:store-state-directory store))))
        ;; Hold the file lock directly so the edit path contends as it would
        ;; when another process holds the lock.
        (let ((handle (funcall (aitools.store.application:store-io-try-lock io) path :create t)))
          (unwind-protect
              (run-edit-command *ports* "edit" (list "a.txt") (list :old "a" :new "b" :tx tx)
                                :root *root* :lock-timeout "20ms"
                                :on-ok (lambda (fields) (setf result fields))
                                :on-error (lambda (code message &rest keys)
                                            (declare (ignore message keys))
                                            (setf result code)))
            (funcall (aitools.store.application:store-io-unlock io) handle)))
        (expect result :to-equal "environment.busy")
        (expect (text "a.txt") :to-equal "a")
        (expect (run-in tx "edit" (list "a.txt") :old "a" :new "b") :to-be :ok)
        (expect (commit-tx tx) :to-be :committed)
        (expect (text "a.txt") :to-equal "b"))))
  (it "reports environment.busy with the same command as repair when the workspace lock is held"
    (with-workspace ()
      (put "a.txt" "a")
      (let ((result nil))
        (let* ((store (open-store *root*))
               (io (aitools.store.application:store-io-port store))
               (path (aitools.store.domain:lock-file-path (aitools.store.application:store-state-directory store))))
          ;; Create the lock path through the normal API, then hold its file
          ;; lock through the port to simulate a competing process.
          (aitools.store.application:call-with-workspace-lock/k store 1000 :on-acquired (lambda () nil)
                                                                          :on-timeout (lambda () nil))
          (let ((handle (funcall (aitools.store.application:store-io-try-lock io) path :create t)))
            (unwind-protect
                 (run-edit-command *ports* "edit" (list "a.txt") (list :old "a" :new "b")
                                   :root *root* :lock-timeout "20ms"
                                   :display-argv (list "edit" "a.txt" "--old" "a" "--new" "b")
                                   :on-ok (lambda (fields) (setf result fields))
                                   :on-error (lambda (code message &key repairs &allow-other-keys)
                                               (declare (ignore message))
                                               (setf result (list code (getf (first repairs) :command)))))
              (funcall (aitools.store.application:store-io-unlock io) handle))))
        (expect result :to-equal (list "environment.busy" "aitools edit a.txt --old a --new b"))
        (expect (text "a.txt") :to-equal "a")))))
