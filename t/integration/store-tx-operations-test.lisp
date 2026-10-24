(in-package #:aitools.store.test)

(describe "aitools.store tx rebase and drop"
  (it "replays content-based ops onto the new base so the tx commits"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+tx")
        (put-file store "a.txt" "external")
        (expect (tx-rebase/k store tx #'replay-append
                             :on-rebased #'identity
                             :on-conflict (lambda (conflicts) (list :conflict conflicts))
                             :on-not-found (lambda () :not-found) :on-busy (lambda () :busy))
                :to-equal '("a.txt"))
        (expect (tx-view-text store tx "a.txt") :to-equal "external+tx")
        (expect (commit-tx store tx) :to-be :committed)
        (expect (disk-text store "a.txt") :to-equal "external+tx"))) :io nil))
  (it "stops a replayer after its commit callback"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (let ((tx (begin store))
            (continued nil))
        (stage-append store tx "a.txt" "+tx")
        (put-file store "a.txt" "external")
        (expect (tx-rebase/k store tx
                              (lambda (record view commit reject)
                                (declare (ignore record view reject))
                                (funcall commit (list (write-file-request "a.txt" (bytes "rebased"))))
                                (setf continued t))
                              :on-rebased (function identity)
                              :on-conflict (lambda (conflicts) (declare (ignore conflicts)) :conflict)
                              :on-not-found (lambda () :not-found)
                              :on-busy (lambda () :busy))
                  :to-equal (list "a.txt"))
          (expect continued :to-be nil)
          (expect (tx-view-text store tx "a.txt") :to-equal "rebased"))) :io nil))

  (it "refuses to rebase a position-based op and leaves the tx unchanged"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+tx" :replayable nil)
        (put-file store "a.txt" "external")
        (let ((outcome (tx-rebase/k store tx #'replay-append
                                    :on-rebased (lambda (paths) (list :rebased paths))
                                    :on-conflict (lambda (conflicts) (list :conflict conflicts))
                                    :on-not-found (lambda () :not-found) :on-busy (lambda () :busy))))
          (expect (first outcome) :to-be :conflict)
          (expect (mapcar #'conflict-path (second outcome)) :to-equal '("a.txt")))
        (expect (tx-view-text store tx "a.txt") :to-equal "a+tx")
        (expect (tx-status-drift (status-of store tx)) :to-equal '("a.txt")))) :io nil))

  (it "drops the named op and every later one, restoring the earlier state"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (stage-append store tx "a.txt" "+2")
        (stage store tx (list (write-file-request "b.txt" (bytes "b"))))
        (expect (tx-drop/k store tx 2 :on-dropped #'identity :on-not-found (lambda () :not-found)
                                      :on-busy (lambda () :busy))
                :to-equal '(2 3))
        (expect (tx-view-text store tx "a.txt") :to-equal "a+1")
        (expect (tx-view-text store tx "b.txt") :to-be :absent)
        (expect (mapcar #'tx-op-record-tx-op (tx-status-ops (status-of store tx))) :to-equal '(1))
        (expect (tx-drop/k store tx 5 :on-dropped #'identity :on-not-found (lambda () :not-found)
                                      :on-busy (lambda () :busy))
                :to-be :not-found)
        (expect (stage-append store tx "a.txt" "+3") :to-be 2)
        (expect (tx-view-text store tx "a.txt") :to-equal "a+1+3"))) :io nil)))

(describe "aitools.store tx abort and interruption"
  (it "removes the tx and its blobs without touching the working tree"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+unique-content")
        (expect (member (aitools.kernel.domain:content-hash (bytes "a+unique-content")) (blob-names store)
                        :test #'string=)
                :to-be-truthy)
        (expect (tx-abort/k store tx :on-aborted #'identity :on-not-found (lambda () :not-found)
                                     :on-busy (lambda () :busy))
                :to-equal '("a.txt"))
        (expect (status-of store tx) :to-be :not-found)
        (expect (tx-list store) :to-equal '())
        (expect (blob-names store) :to-equal '())
        (expect (disk-text store "a.txt") :to-equal "a"))) :io nil))

  (it "stays at the previous op's state when a tx op is interrupted"
    (dolist (point '(:tx-after-blobs :tx-after-ops))
      (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
        (let ((tx (begin store)))
          (stage-append store tx "a.txt" "+1")
          (expect (call-with-fault-at point (lambda () (stage-append store tx "a.txt" "+2"))) :to-be-truthy)
          (expect (tx-view-text store tx "a.txt") :to-equal "a+1")
          (expect (mapcar #'tx-op-record-tx-op (tx-status-ops (status-of store tx))) :to-equal '(1))
          (expect (stage-append store tx "a.txt" "+3") :to-be 2)
          (expect (tx-view-text store tx "a.txt") :to-equal "a+1+3"))) :io nil)))

  (it "serialises two processes writing into one tx"
    (call-with-temp-store (lambda (store) (put-file store "log.txt" "")
      (let* ((tx (begin store))
             (pids (loop for n from 0 below 2
                         collect (let ((n n))
                                   (call-in-child-process
                                    (lambda ()
                                      (loop repeat 5
                                            do (unless (integerp (stage-append store tx "log.txt"
                                                                               (format nil "~D" n)))
                                                 (error "stage failed")))))))))
        (dolist (pid pids)
          (expect (multiple-value-list (wait-for-child pid)) :to-equal '(:exited 0)))
        (expect (mapcar #'tx-op-record-tx-op (tx-status-ops (status-of store tx)))
                :to-equal '(1 2 3 4 5 6 7 8 9 10))
        (expect (length (tx-view-text store tx "log.txt")) :to-be 10))) :io nil)))

(defun replay-chmod (record view commit reject)
  "Re-run a recorded (\"chmod\" path mode) op against VIEW."
  (declare (ignore view reject))
  (destructuring-bind (command path mode) (tx-op-record-argv record)
    (declare (ignore command))
    (funcall commit (list (chmod-request path (parse-integer mode :radix 8))))))

(defun stage-chmod (store tx-id path mode)
  (tx-stage/k store tx-id (list "chmod" path (format nil "~O" mode))
              (lambda (view commit reject)
                (declare (ignore view reject))
                (funcall commit (list (chmod-request path mode))))
              :replayable t
              :on-staged (lambda (tx-op results) (list tx-op (actions results)))
              :on-rejected (lambda (code &rest rest) (declare (ignore rest)) code)
              :on-not-found (lambda () :not-found)
              :on-busy (lambda () :busy)))

(defun tx-view-state (store tx-id path)
  (call-with-tx-view/k store tx-id
                       :on-view (lambda (view)
                                  (let ((state (view-path-state view path)))
                                    (list (entry-state-kind state) (entry-state-hash state) (entry-state-mode state))))
                       :on-not-found (lambda () :not-found)))

(describe "aitools.store mode-only changes inside a tx"
  (it "stages a chmod keeping the content hash, and commits and undoes it as one op"
    (call-with-temp-store (lambda (store) (put-file store "cfg.txt" "secret" :mode #o644)
      (let ((tx (begin store))
            (hash (aitools.kernel.domain:content-hash (bytes "secret"))))
        (expect (stage-chmod store tx "cfg.txt" #o600) :to-equal '(1 (("cfg.txt" :mode-changed))))
        (expect (disk-mode store "cfg.txt") :to-be #o644)
        (expect (tx-view-state store tx "cfg.txt") :to-equal (list :file hash #o600))
        (expect (tx-view-text store tx "cfg.txt") :to-equal "secret")
        (multiple-value-bind (status op-id results) (commit-tx store tx)
          (expect status :to-be :committed)
          (expect (actions results) :to-equal '(("cfg.txt" :mode-changed)))
          (expect (disk-mode store "cfg.txt") :to-be #o600)
          (expect (disk-text store "cfg.txt") :to-equal "secret")
          (expect (status-of store tx) :to-be :not-found)
          (expect (undo store op-id) :to-be :committed)
          (expect (disk-mode store "cfg.txt") :to-be #o644)
          (expect (disk-text store "cfg.txt") :to-equal "secret")))) :io nil))

  (it "stages a chmod on content the tx already rewrote"
    (call-with-temp-store (lambda (store) (put-file store "cfg.txt" "old" :mode #o644)
      (let ((tx (begin store)))
        (stage store tx (list (write-file-request "cfg.txt" (bytes "new"))))
        (expect (stage-chmod store tx "cfg.txt" #o600) :to-equal '(2 (("cfg.txt" :mode-changed))))
        (expect (tx-view-state store tx "cfg.txt")
                :to-equal (list :file (aitools.kernel.domain:content-hash (bytes "new")) #o600))
        (expect (commit-tx store tx) :to-be :committed)
        (expect (disk-text store "cfg.txt") :to-equal "new")
        (expect (disk-mode store "cfg.txt") :to-be #o600))) :io nil))

  (it "drops a staged chmod back to the disk mode"
    (call-with-temp-store (lambda (store) (put-file store "cfg.txt" "secret" :mode #o644)
      (let ((tx (begin store)))
        (stage-chmod store tx "cfg.txt" #o600)
        (expect (tx-drop/k store tx 1 :on-dropped #'identity :on-not-found (lambda () :not-found)
                                      :on-busy (lambda () :busy))
                :to-equal '(1))
        (expect (tx-view-state store tx "cfg.txt")
                :to-equal (list :file (aitools.kernel.domain:content-hash (bytes "secret")) #o644))
        (expect (commit-tx store tx) :to-be :committed)
        (expect (disk-mode store "cfg.txt") :to-be #o644))) :io nil))

  (it "replays a staged chmod onto drifted content at rebase"
    (call-with-temp-store (lambda (store) (put-file store "cfg.txt" "v1" :mode #o644)
      (let ((tx (begin store)))
        (stage-chmod store tx "cfg.txt" #o600)
        (put-file store "cfg.txt" "v2" :mode #o644)
        (expect (tx-rebase/k store tx #'replay-chmod
                             :on-rebased #'identity
                             :on-conflict (lambda (conflicts) (list :conflict conflicts))
                             :on-not-found (lambda () :not-found) :on-busy (lambda () :busy))
                :to-equal '("cfg.txt"))
        (expect (tx-view-state store tx "cfg.txt")
                :to-equal (list :file (aitools.kernel.domain:content-hash (bytes "v2")) #o600))
        (expect (commit-tx store tx) :to-be :committed)
        (expect (disk-text store "cfg.txt") :to-equal "v2")
        (expect (disk-mode store "cfg.txt") :to-be #o600))) :io nil)))

