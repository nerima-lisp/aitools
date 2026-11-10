(in-package #:aitools.store.test)

(describe "aitools.store failures after the commit point"
  (flet ((call-with-failing-apply-store (store function)
  (let* ((base-boundaries (store-boundaries store))
         (root (concatenate 'string (store-root store) "/"))
         (boundaries
           (cl-boundary-kit:boundary-context-with
            base-boundaries
            :rename
            (lambda (from to)
              (if (eql 0 (search root to))
                  (error 'store-io-error :operation "rename" :path to :detail "injected")
                  (funcall (store-operation base-boundaries :rename) from to)))))
         (state-directory (store-state-directory store))
         (state-home (subseq state-directory 0 (position #\/ state-directory :from-end t)))
         (broken (make-store boundaries (store-root store) state-home)))
    (funcall function broken))))
    (it "signals store-committed-error naming the op and path, and keeps the intent for recovery"
      (call-with-temp-store (lambda (store) (seed-state store)
        (call-with-failing-apply-store
         store
         (lambda (broken)
           (let* ((condition (handler-case
                                 (progn (commit broken (list (write-file-request "a.txt" (bytes "a")))) nil)
                               (store-committed-error (condition) condition))))
             (expect condition :to-be-truthy)
             (expect (store-io-error-path condition) :to-equal (disk-path store "a.txt"))
             (expect (store-io-error-operation condition) :to-equal "rename")
             (expect (intent-files store)
                     :to-equal (list (concatenate 'string (store-committed-error-op-id condition) ".json")))
             (expect (recover store) :to-be :recovered)
             (expect (disk-text store "a.txt") :to-equal "a"))))) :io nil))
    (it "reports a record recovery cannot complete through ON-FAILED instead of wedging"
      (call-with-temp-store (lambda (store) (seed-state store)
        (call-with-failing-apply-store
         store
         (lambda (broken)
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
           (expect (length (intent-files store)) :to-be 1)))
        (expect (recover store) :to-be :recovered)
        (expect (disk-text store "a.txt") :to-equal "a")) :io nil))))


(describe "aitools.store stream errors"
  (it "reports a failed write as store-io-error, not a stream error"
    (call-with-temp-store (lambda (store) (put-file store "ro.txt" "r")
      (let* ((path (disk-path store "ro.txt"))
             (fd (sb-posix:open path sb-posix:o-rdonly)))
        (signals store-io-error
          (aitools.store.infrastructure::%write-and-close path fd (bytes "data") nil)))) :io nil)))

(describe "aitools.store tx blobs and rebase atomicity"
  (it "keeps a blob only an earlier tx op references when another write collects garbage"
    (call-with-temp-store (lambda (store) (let ((tx (begin store)))
        (expect (stage store tx (list (write-file-request "a.txt" (bytes "X")))) :to-be 1)
        (expect (stage store tx (list (write-file-request "a.txt" (bytes "Y")))) :to-be 2)
        (expect (commit store (list (write-file-request "other.txt" (bytes "o")))) :to-be :committed)
        (expect (tx-drop/k store tx 2 :on-dropped #'identity :on-not-found (lambda () :not-found)
                                      :on-busy (lambda () :busy))
                :to-equal '(2))
        (expect (tx-view-text store tx "a.txt") :to-equal "X"))) :io nil))

  (it "leaves ops and index on one timeline when a rebase is interrupted between its writes"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+tx")
        (put-file store "a.txt" "external")
        (expect (call-with-fault-at :tx-after-ops (lambda () (tx-rebase/k store tx #'replay-append
                               :on-rebased #'identity :on-conflict #'identity
                               :on-not-found (lambda () :not-found) :on-busy (lambda () :busy))))
                :to-be-truthy)
        (let* ((status (status-of store tx))
               (after (cdr (assoc "a.txt" (tx-op-record-after (first (tx-status-ops status))) :test #'string=)))
               (staged (tx-path-staged (find "a.txt" (tx-status-paths status) :key #'tx-path-path :test #'string=))))
          (expect (entry-state-hash after) :to-equal (entry-state-hash staged)))
        (expect (tx-rebase/k store tx #'replay-append
                             :on-rebased #'identity :on-conflict #'identity
                             :on-not-found (lambda () :not-found) :on-busy (lambda () :busy))
                :to-equal '("a.txt"))
        (expect (tx-view-text store tx "a.txt") :to-equal "external+tx"))) :io nil)))

(describe "aitools.store writes into the mktemp area"
  (it "neither journals them nor creates a second state directory"
    (call-with-temp-store (lambda (plain) (let* ((state-home (concatenate 'string (base-directory plain) "/state"))
             (workspace (posix-store-under (store-root plain) state-home))
             (tmp (tmp-directory (store-state-directory workspace))))
        (seed-state workspace)
        (sb-posix:mkdir tmp #o700)
        (let ((temporary (posix-store-under tmp state-home)))
          (expect (commit temporary (list (write-file-request "scratch.txt" (bytes "one")))) :to-be :committed)
          (expect (commit temporary (list (write-file-request "scratch.txt" (bytes "two")))) :to-be :committed))
        (expect (octets-string (funcall (store-operation (store-boundaries plain) :read-file) (join-path tmp "scratch.txt")))
                :to-equal "two")
        (expect (funcall (store-operation (store-boundaries plain) :list-directory) (concatenate 'string state-home "/aitools"))
                :to-equal (list (subseq (store-state-directory workspace)
                                        (1+ (position #\/ (store-state-directory workspace) :from-end t)))))
        (expect (length (read-journal workspace)) :to-be 1))) :io nil)))

(describe "aitools.store rename failures name the blocking destination"
  (it "reports the destination of a failed posix rename, not its source"
    (call-with-temp-store (lambda (store) (put-file store "src.txt" "s")
      (put-file store "blocker/inside.txt" "i")
      (let ((condition (handler-case (progn (funcall (store-operation (store-boundaries store) :rename)
                                                     (disk-path store "src.txt") (disk-path store "blocker"))
                                            nil)
                         (store-io-error (condition) condition))))
        (expect (store-io-error-operation condition) :to-equal "rename")
        (expect (store-io-error-path condition) :to-equal (disk-path store "blocker"))
        (expect (search (disk-path store "src.txt") (store-io-error-detail condition)) :to-be-truthy))
      (expect (disk-text store "src.txt") :to-equal "s")) :io nil))

  (it "points recovery's failed journal rewrite at journal/ops.jsonl, not the vanished temp"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (let ((journal (journal-file-path (store-state-directory store))))
        (expect (call-with-fault-at :after-apply (lambda () (commit store (list (write-file-request "a.txt" (bytes "b"))))))
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
        (expect (length (intent-files store)) :to-be 1))) :io nil)))

(describe "aitools.store commit-changes/k continuations"
  (it "reports a dry-run planner refusal without writing"
    (call-with-temp-store (lambda (store) (expect (multiple-value-list (commit store (list (delete-request "none.txt")) :dry-run t))
              :to-equal '(:rejected "input.not-found" "none.txt does not exist" ()))) :io nil))

  (it "signals when VALIDATE calls neither continuation"
    (call-with-temp-store (lambda (store) (expect (handler-case (commit-changes/k store '("x") (lambda (commit reject) (declare (ignore commit reject)))
                                              :on-committed #'list :on-rejected #'list :on-busy #'list)
                (error (condition) (princ-to-string condition)))
              :to-equal "commit-changes/k: VALIDATE returned without calling COMMIT or REJECT")) :io nil))

  (it "prints a store refusal as its message"
    (expect (princ-to-string (make-condition 'aitools.store.application::store-refusal
                                             :code "refusal.not-a-file" :message "a.txt has 2 hard links"))
            :to-equal "a.txt has 2 hard links"))

  (it "creates the state directories only under a directory, reporting the file in the way"
    (call-with-temp-store (lambda (plain) (let* ((blocker (concatenate 'string (base-directory plain) "/blocker"))
             (store (make-store (make-posix-store-io :root (store-root plain)) (store-root plain) (concatenate 'string blocker "/state"))))
        (with-open-file (out (sb-ext:parse-native-namestring blocker) :direction :output) (write-string "f" out))
        (let ((condition (handler-case (progn (commit store (list (write-file-request "a.txt" (bytes "a")))) nil)
                           (store-io-error (condition) condition))))
          (expect (list (store-io-error-operation condition) (store-io-error-path condition))
                  :to-equal (list "mkdir" blocker)))
        (expect (disk-text plain "a.txt") :to-be :absent))) :io nil)))
(describe "aitools.store required write boundaries"
  (it-each ((:sync-directory "checkpoint" "mkdir" "directory identity unavailable" :unrecoverable)
            (:chmod-directory-identity "finalize-directory" nil nil :recoverable))
      "keeps the committed mkdir intent when ~A is unavailable"
      (capability operation recovery-operation recovery-detail recovery-status)
    (call-with-temp-store
     (lambda (plain)
       (seed-state plain)
       (let* ((state-directory (store-state-directory plain))
              (state-home (subseq state-directory 0 (position #\/ state-directory :from-end t)))
              (boundaries (cl-boundary-kit:boundary-context-with
                           (store-boundaries plain) capability nil))
              (broken (make-store boundaries (store-root plain) state-home))
              (condition (handler-case
                             (progn
                               (commit broken (list (mkdir-request "private" :mode #o500)))
                               nil)
                           (store-committed-error (condition) condition))))
         (expect condition :to-be-truthy)
         (expect (store-io-error-operation condition) :to-equal operation)
         (expect (length (intent-files plain)) :to-be 1)
         (let ((recovery (handler-case
                             (list :recovered (recover plain))
                           (store-committed-error (condition)
                             (list :failed condition)))))
           (if (eq recovery-status :recoverable)
               (progn
                 (expect (first recovery) :to-be :recovered)
                 (expect (second recovery) :to-be :recovered)
                 (expect (disk-mode plain "private") :to-be #o500)
                 (expect (intent-files plain) :to-equal nil))
               (progn
                 (expect (first recovery) :to-be :failed)
                 (expect (store-io-error-operation (second recovery)) :to-equal recovery-operation)
                 (expect (store-io-error-detail (second recovery)) :to-equal recovery-detail)
                 (expect (disk-mode plain "private") :to-be #o700)
                 (expect (length (intent-files plain)) :to-be 1))))))
     :io nil)))

(describe "aitools.store directory modes in the mktemp area"
  (it "finalizes restrictive parent modes after nested writes"
    (call-with-temp-store
     (lambda (plain)
       (let* ((state-home (concatenate 'string (base-directory plain) "/state"))
              (workspace (posix-store-under (store-root plain) state-home))
              (tmp (tmp-directory (store-state-directory workspace))))
         (seed-state workspace)
         (sb-posix:mkdir tmp #o700)
         (let ((temporary (posix-store-under tmp state-home)))
           (unwind-protect
                (progn
                  (expect
                   (commit temporary
                           (list (mkdir-request "private" :mode #o400)
                                 (mkdir-request "private/nested" :mode #o750)
                                 (write-file-request "private/nested/file" (bytes "data"))))
                   :to-be :committed)
                  (expect (disk-mode temporary "private") :to-be #o400)
                  (sb-posix:chmod (disk-path temporary "private") #o700)
                  (expect (disk-mode temporary "private/nested") :to-be #o750)
                  (expect (disk-text temporary "private/nested/file") :to-equal "data"))
             (when (probe-file (disk-path temporary "private"))
               (sb-posix:chmod (disk-path temporary "private") #o700))))))
     :io nil)))
