(in-package #:aitools.store.test)
(progn
  (describe "aitools.store transaction continuation coverage"
            (it "routes missing transactions through their not-found continuations"
                (call-with-temp-store
                 (lambda (store)
                   (let ((tx (begin store)))
                     (expect (tx-abort/k store tx
                                         :on-aborted #'identity
                                         :on-not-found (lambda () :not-found)
                                         :on-busy (lambda () :busy))
                             :to-equal '())
                     (expect (tx-record-read/k store tx "a.txt"
                                               :on-recorded #'identity
                                               :on-not-found (lambda () :not-found)
                                               :on-busy (lambda () :busy))
                             :to-be :not-found)
                     (expect (tx-stage/k store tx '("stage")
                                         (lambda (&rest arguments)
                                           (declare (ignore arguments))
                                           (error "validate should not run for a missing tx"))
                                         :on-staged (lambda (&rest arguments)
                                                      (declare (ignore arguments))
                                                      :staged)
                                         :on-rejected (lambda (&rest arguments)
                                                        (declare (ignore arguments))
                                                        :rejected)
                                         :on-not-found (lambda () :not-found)
                                         :on-busy (lambda () :busy))
                             :to-be :not-found)
                     (expect (tx-drop/k store tx 1
                                        :on-dropped #'identity
                                        :on-not-found (lambda () :not-found)
                                        :on-busy (lambda () :busy))
                             :to-be :not-found)
                     (expect (tx-rebase/k store tx #'replay-append
                                          :on-rebased #'identity
                                          :on-conflict #'identity
                                          :on-not-found (lambda () :not-found)
                                          :on-busy (lambda () :busy))
                             :to-be :not-found)
                     (expect (tx-commit/k store tx '("tx" "commit")
                                          :on-committed (lambda (&rest arguments)
                                                          (declare (ignore arguments))
                                                          :committed)
                                          :on-rejected (lambda (&rest arguments)
                                                         (declare (ignore arguments))
                                                         :rejected)
                                          :on-not-found (lambda () :not-found)
                                          :on-busy (lambda () :busy))
                             :to-be :not-found)
                     (expect (tx-abort/k store tx
                                         :on-aborted #'identity
                                         :on-not-found (lambda () :not-found)
                                         :on-busy (lambda () :busy))
                             :to-be :not-found)))
                 :io nil))

            (it "forwards every supported rejection field from the stage validator"
                (call-with-temp-store
                 (lambda (store)
                   (let* ((tx (begin store))
                          (outcome
                           (tx-stage/k store tx '("write" "a.txt")
                                       (lambda (view commit reject)
                                         (declare (ignore view commit))
                                         (funcall reject "refusal.target-changed" "external edit"
                                                  :candidates '("a.txt")
                                                  :diagnostics '("base changed")
                                                  :conflicts nil
                                                  :repairs '("rebase")
                                                  :path "a.txt"))
                                       :on-staged (lambda (&rest arguments)
                                                    (declare (ignore arguments))
                                                    :staged)
                                       :on-rejected (lambda (code message &rest keys)
                                                      (list code message keys))
                                       :on-not-found (lambda () :not-found)
                                       :on-busy (lambda () :busy))))
                     (expect (first outcome) :to-equal "refusal.target-changed")
                     (expect (second outcome) :to-equal "external edit")
                     (expect (getf (third outcome) :candidates) :to-equal '("a.txt"))
                     (expect (getf (third outcome) :diagnostics) :to-equal '("base changed"))
                     (expect (getf (third outcome) :repairs) :to-equal '("rebase"))
                     (expect (getf (third outcome) :path) :to-equal "a.txt")))
                 :io nil))
            (it "routes transaction lock contention through each busy continuation"
                (call-with-temp-store
                 (lambda (plain)
                   (let* ((tx (begin plain))
                          (boundaries
                           (cl-boundary-kit:boundary-context-with
                            (store-boundaries plain)
                            :try-lock (lambda (path &key create)
                                        (declare (ignore path create))
                                        nil)))
                          (store (aitools.store.application::%make-store
                                  boundaries (store-clock plain) (aitools.store.application::store-sleeper plain)
                                  (store-root plain) (store-state-directory plain) nil)))
                     (expect (tx-begin/k store :lock-timeout-ms 0 :on-begun #'list
                                         :on-busy (constantly :busy)) :to-be :busy)
                     (expect (tx-record-read/k store tx "a.txt" :lock-timeout-ms 0
                                               :on-recorded #'identity :on-not-found (constantly :not-found)
                                               :on-busy (constantly :busy)) :to-be :busy)
                     (expect (tx-stage/k store tx '("write" "a.txt")
                                         (lambda (&rest arguments)
                                           (declare (ignore arguments))
                                           (error "validator must not run while the lock is busy"))
                                         :lock-timeout-ms 0 :on-staged #'list :on-rejected #'list
                                         :on-not-found (constantly :not-found) :on-busy (constantly :busy))
                             :to-be :busy)
                     (expect (tx-drop/k store tx 1 :lock-timeout-ms 0 :on-dropped #'identity
                                        :on-not-found (constantly :not-found) :on-busy (constantly :busy))
                             :to-be :busy)
                     (expect (tx-rebase/k store tx #'replay-append :lock-timeout-ms 0
                                          :on-rebased #'identity :on-conflict #'identity
                                          :on-not-found (constantly :not-found) :on-busy (constantly :busy))
                             :to-be :busy)
                     (expect (tx-commit/k store tx '("tx" "commit") :lock-timeout-ms 0
                                          :on-committed #'list :on-rejected #'list
                                          :on-not-found (constantly :not-found) :on-busy (constantly :busy))
                             :to-be :busy)
                     (expect (tx-abort/k store tx :lock-timeout-ms 0 :on-aborted #'identity
                                         :on-not-found (constantly :not-found) :on-busy (constantly :busy))
                             :to-be :busy)))
                 :io nil))
            (it "reports a same-content permission change as a mode change in tx diff"
  (call-with-temp-store
   (lambda (store)
     (put-file store "mode.txt" "x")
     (let ((tx (begin store)))
       (stage store tx (list (chmod-request "mode.txt" #o600)))
       (tx-diff/k store tx
                  :on-diff (lambda (results)
                             (expect (mapcar #'change-result-action results)
                                     :to-equal '(:mode-changed)))
                  :on-not-found (lambda () (error "transaction disappeared")))
       (expect (disk-mode store "mode.txt") :to-be #o644)))
   :io nil))

            (it "keeps clean earlier ops when a drifted position-based op conflicts"
  (call-with-temp-store
   (lambda (store)
     (put-file store "a.txt" "base-a")
     (put-file store "b.txt" "base-b")
     (let ((tx (begin store)))
       (stage-append store tx "b.txt" "+clean")
       (stage-append store tx "a.txt" "+position" :replayable nil)
       (put-file store "a.txt" "external-a")
       (let ((outcome
               (tx-rebase/k store tx #'replay-append
                            :on-rebased (lambda (paths) (list :rebased paths))
                            :on-conflict (lambda (conflicts) (cons :conflict conflicts))
                            :on-not-found (lambda () :not-found)
                            :on-busy (lambda () :busy))))
         (expect (first outcome) :to-be :conflict)
         (expect (mapcar #'conflict-path (rest outcome)) :to-equal '("a.txt"))
         (expect (tx-view-text store tx "a.txt") :to-equal "base-a+position")
         (expect (tx-view-text store tx "b.txt") :to-equal "base-b+clean"))))
   :io nil)))

  (describe "aitools.store tx lock coverage"
    (it "rechecks a transaction that vanishes after its lock is acquired"
      (call-with-temp-store
       (lambda (store)
         (let* ((tx (begin store))
                (index-path (join-path (tx-directory (store-state-directory store) tx)
                                       "index.json"))
                (lock-path (join-path (tx-directory (store-state-directory store) tx)
                                      "lock"))
                (base-boundaries (store-boundaries store))
                (base-lstat (store-operation base-boundaries :lstat))
                (index-checks 0)
                (unlocked nil)
                (boundaries
                  (cl-boundary-kit:boundary-context-with
                   base-boundaries
                   :lstat (lambda (path)
                            (if (string= path index-path)
                                (if (= (incf index-checks) 1)
                                    (values :file #o600)
                                    (values :absent nil))
                                (funcall base-lstat path)))
                   :try-lock (lambda (path &key create)
                               (declare (ignore create))
                               (expect path :to-equal lock-path)
                               :mock-handle)
                   :unlock (lambda (handle) (setf unlocked handle))))
                (test-store
                  (aitools.store.application::%make-store
                   boundaries (store-clock store) (aitools.store.application::store-sleeper store)
                   (store-root store) (store-state-directory store) nil)))
           (expect (call-with-tx-lock/k test-store tx 0
                                       :on-acquired (lambda () :acquired)
                                       :on-timeout (lambda () :timeout)
                                       :on-not-found (lambda () :not-found))
                   :to-be :not-found)
           (expect index-checks :to-be 2)
           (expect unlocked :to-be :mock-handle)))
       :io nil))

    (it "handles root-level directory paths without probing a parent"
      (call-with-temp-store
       (lambda (store)
         (let* ((path "/store-root-level")
                (calls nil)
                (boundaries
                  (cl-boundary-kit:boundary-context-with
                   (store-boundaries store)
                   :lstat (lambda (candidate)
                            (push (list :lstat candidate) calls)
                            (values :absent nil))
                   :mkdir (lambda (candidate &key mode)
                            (push (list :mkdir candidate mode) calls))))
                (test-store
                  (aitools.store.application::%make-store
                   boundaries (store-clock store) (aitools.store.application::store-sleeper store)
                   (store-root store) (store-state-directory store) nil)))
           (aitools.store.application::%ensure-directory test-store path)
           (expect (nreverse calls)
                   :to-equal `((:lstat ,path) (:mkdir ,path #o700)))))
       :io nil))

    (it "tightens an existing directory that has permissive mode bits"
      (call-with-temp-store
       (lambda (store)
         (let* ((path "/state/aitools/workspace")
                (calls nil)
                (boundaries
                  (cl-boundary-kit:boundary-context-with
                   (store-boundaries store)
                   :lstat (lambda (candidate)
                            (push (list :lstat candidate) calls)
                            (values :directory #o755))
                   :chmod (lambda (candidate mode)
                            (push (list :chmod candidate mode) calls))))
                (test-store
                  (aitools.store.application::%make-store
                   boundaries (store-clock store) (aitools.store.application::store-sleeper store)
                   (store-root store) (store-state-directory store) nil)))
           (aitools.store.application::%tighten-directory test-store path)
           (expect (nreverse calls)
                   :to-equal `((:lstat ,path) (:chmod ,path #o700)))))
       :io nil))))

