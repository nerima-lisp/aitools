(in-package #:aitools.store.test)

(describe "aitools.store failures after the commit point"
  (flet ((failing-apply-store (store)
           ;; Every rename onto a workspace path fails: the first one is the
           ;; apply step after the commit point.
           (let* ((io (store-io-port store))
                  (rename (store-io-rename io))
                  (root (concatenate 'string (store-root store) "/")))
             (make-store (aitools.store.application::copy-store-io io :rename (lambda (from to)
                                                     (if (eql 0 (search root to))
                                                         (error 'store-io-error :operation "rename" :path to
                                                                                :detail "injected")
                                                         (funcall rename from to))))
                         (store-root store) (state-home-of store)))))
    (it "signals store-committed-error naming the op and path, and keeps the intent for recovery"
      (with-temp-store (store)
        (seed-state store)
        (let* ((broken (failing-apply-store store))
               (condition (handler-case (progn (commit broken (list (write-file-request "a.txt" (bytes "a")))) nil)
                            (store-committed-error (condition) condition))))
          (expect condition :to-be-truthy)
          (expect (store-io-error-path condition) :to-equal (disk-path store "a.txt"))
          (expect (store-io-error-operation condition) :to-equal "rename")
          (expect (intent-files store)
                  :to-equal (list (concatenate 'string (store-committed-error-op-id condition) ".json")))
          (expect (recover store) :to-be :recovered)
          (expect (disk-text store "a.txt") :to-equal "a"))))

    (it "reports a record recovery cannot complete through ON-FAILED instead of wedging"
      (with-temp-store (store)
        (seed-state store)
        (let ((broken (failing-apply-store store)))
          (handler-case (commit broken (list (write-file-request "a.txt" (bytes "a"))))
            (store-committed-error () nil))
          (let ((reported (recover/k broken
                                     :on-rolled-forward (lambda (op-id) (declare (ignore op-id)))
                                     :on-discarded (lambda (op-id) (declare (ignore op-id)))
                                     :on-none (lambda () :none)
                                     :on-busy (lambda () :busy)
                                     :on-failed (lambda (condition entries)
                                                  (list :failed (store-committed-error-op-id condition)
                                                        (store-io-error-path condition) entries)))))
            (expect (first reported) :to-be :failed)
            (expect (third reported) :to-equal (disk-path store "a.txt"))
            (expect (fourth reported) :to-equal '()))
          (signals store-committed-error
            (recover/k broken
                       :on-rolled-forward (lambda (op-id) (declare (ignore op-id)))
                       :on-discarded (lambda (op-id) (declare (ignore op-id)))
                       :on-none (lambda () :none)
                       :on-busy (lambda () :busy)))
          (expect (length (intent-files store)) :to-be 1))
        (expect (recover store) :to-be :recovered)
        (expect (disk-text store "a.txt") :to-equal "a")))))

(describe "aitools.store stream errors"
  (it "reports a failed write as store-io-error, not a stream error"
    (with-temp-store (store)
      (put-file store "ro.txt" "r")
      (let* ((path (disk-path store "ro.txt"))
             (fd (sb-posix:open path sb-posix:o-rdonly)))
        (signals store-io-error
          (aitools.store.infrastructure::%write-and-close path fd (bytes "data") nil))))))

(describe "aitools.store tx blobs and rebase atomicity"
  (it "keeps a blob only an earlier tx op references when another write collects garbage"
    (with-temp-store (store)
      (let ((tx (begin store)))
        (expect (stage store tx (list (write-file-request "a.txt" (bytes "X")))) :to-be 1)
        (expect (stage store tx (list (write-file-request "a.txt" (bytes "Y")))) :to-be 2)
        (expect (commit store (list (write-file-request "other.txt" (bytes "o")))) :to-be :committed)
        (expect (tx-drop/k store tx 2 :on-dropped #'identity :on-not-found (lambda () :not-found)
                                      :on-busy (lambda () :busy))
                :to-equal '(2))
        (expect (tx-view-text store tx "a.txt") :to-equal "X"))))

  (it "leaves ops and index on one timeline when a rebase is interrupted between its writes"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+tx")
        (put-file store "a.txt" "external")
        (expect (with-fault-at (:tx-after-ops)
                  (tx-rebase/k store tx #'replay-append
                               :on-rebased #'identity :on-conflict #'identity
                               :on-not-found (lambda () :not-found) :on-busy (lambda () :busy)))
                :to-be-truthy)
        (let* ((status (status-of store tx))
               (after (cdr (assoc "a.txt" (tx-op-record-after (first (tx-status-ops status))) :test #'string=)))
               (staged (tx-path-staged (find "a.txt" (tx-status-paths status) :key #'tx-path-path :test #'string=))))
          (expect (entry-state-hash after) :to-equal (entry-state-hash staged)))
        (expect (tx-rebase/k store tx #'replay-append
                             :on-rebased #'identity :on-conflict #'identity
                             :on-not-found (lambda () :not-found) :on-busy (lambda () :busy))
                :to-equal '("a.txt"))
        (expect (tx-view-text store tx "a.txt") :to-equal "external+tx")))))

(describe "aitools.store writes into the mktemp area"
  (it "neither journals them nor creates a second state directory"
    (with-temp-store (plain)
      (let* ((state-home (concatenate 'string (base-directory plain) "/state"))
             (workspace (posix-store-under (store-root plain) state-home))
             (tmp (tmp-directory (store-state-directory workspace))))
        (seed-state workspace)
        (sb-posix:mkdir tmp #o700)
        (let ((temporary (posix-store-under tmp state-home)))
          (expect (commit temporary (list (write-file-request "scratch.txt" (bytes "one")))) :to-be :committed)
          (expect (commit temporary (list (write-file-request "scratch.txt" (bytes "two")))) :to-be :committed))
        (expect (octets-string (funcall (store-io-read-file (store-io-port plain)) (join-path tmp "scratch.txt")))
                :to-equal "two")
        (expect (funcall (store-io-list-directory (store-io-port plain)) (concatenate 'string state-home "/aitools"))
                :to-equal (list (subseq (store-state-directory workspace)
                                        (1+ (position #\/ (store-state-directory workspace) :from-end t)))))
        (expect (length (read-journal workspace)) :to-be 1)))))

(describe "aitools.store rename failures name the blocking destination"
  (it "reports the destination of a failed posix rename, not its source"
    (with-temp-store (store)
      (put-file store "src.txt" "s")
      (put-file store "blocker/inside.txt" "i")
      (let ((condition (handler-case (progn (funcall (store-io-rename (store-io-port store))
                                                     (disk-path store "src.txt") (disk-path store "blocker"))
                                            nil)
                         (store-io-error (condition) condition))))
        (expect (store-io-error-operation condition) :to-equal "rename")
        (expect (store-io-error-path condition) :to-equal (disk-path store "blocker"))
        (expect (search (disk-path store "src.txt") (store-io-error-detail condition)) :to-be-truthy))
      (expect (disk-text store "src.txt") :to-equal "s")))

  (it "points recovery's failed journal rewrite at journal/ops.jsonl, not the vanished temp"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let ((journal (journal-file-path (store-state-directory store))))
        (expect (with-fault-at (:after-apply) (commit store (list (write-file-request "a.txt" (bytes "b")))))
                :to-be-truthy)
        (when (exists-p journal)
          (sb-posix:unlink journal))
        (ensure-directories-exist (sb-ext:parse-native-namestring (concatenate 'string journal "/keep/")))
        (let ((reported (recover/k store
                                   :on-rolled-forward (lambda (op-id) (declare (ignore op-id)))
                                   :on-discarded (lambda (op-id) (declare (ignore op-id)))
                                   :on-none (lambda () :none)
                                   :on-busy (lambda () :busy)
                                   :on-failed (lambda (condition entries)
                                                (list (store-io-error-operation condition)
                                                      (store-io-error-path condition)
                                                      entries)))))
          (expect reported :to-equal (list "rename" journal '())))
        (expect (length (intent-files store)) :to-be 1)))))

(describe "aitools.store commit-changes/k continuations"
  (it "reports a dry-run planner refusal without writing"
    (with-temp-store (store)
      (expect (multiple-value-list (commit store (list (delete-request "none.txt")) :dry-run t))
              :to-equal '(:rejected "input.not-found" "none.txt does not exist" ()))))

  (it "signals when VALIDATE calls neither continuation"
    (with-temp-store (store)
      (expect (handler-case (commit-changes/k store '("x") (lambda (commit reject) (declare (ignore commit reject)))
                                              :on-committed #'list :on-rejected #'list :on-busy #'list)
                (error (condition) (princ-to-string condition)))
              :to-equal "commit-changes/k: VALIDATE returned without calling COMMIT or REJECT")))

  (it "prints a store refusal as its message"
    (expect (princ-to-string (make-condition 'aitools.store.application::store-refusal
                                             :code "refusal.not-a-file" :message "a.txt has 2 hard links"))
            :to-equal "a.txt has 2 hard links"))

  (it "creates the state directories only under a directory, reporting the file in the way"
    (with-temp-store (plain)
      (let* ((blocker (concatenate 'string (base-directory plain) "/blocker"))
             (store (make-store (make-posix-store-io) (store-root plain) (concatenate 'string blocker "/state"))))
        (with-open-file (out (sb-ext:parse-native-namestring blocker) :direction :output) (write-string "f" out))
        (let ((condition (handler-case (progn (commit store (list (write-file-request "a.txt" (bytes "a")))) nil)
                           (store-io-error (condition) condition))))
          (expect (list (store-io-error-operation condition) (store-io-error-path condition))
                  :to-equal (list "mkdir" blocker)))
        (expect (disk-text plain "a.txt") :to-be :absent)))))

