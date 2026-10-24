;;;;
;;;; Transaction overlay reads and commit behavior in a temporary workspace.
(in-package #:aitools.store.test)

(defun begin (store &optional name)
  (tx-begin/k store :name name :on-begun (lambda (tx-id name created) (declare (ignore name created)) tx-id)
                    :on-busy (lambda () :busy)))

(defun append-requests (view argv)
  "The requests for the test's content-based op (\"append\" path text):
append TEXT to PATH as the view shows it."
  (destructuring-bind (command path text) argv
    (declare (ignore command))
    (let ((current (view-read-file view path)))
      (list (write-file-request path (bytes (concatenate 'string (if current (octets-string current) "") text)))))))

(defun stage-append (store tx-id path text &key (replayable t))
  (let ((argv (list "append" path text)))
    (tx-stage/k store tx-id argv
                (lambda (view commit reject)
                  (declare (ignore reject))
                  (funcall commit (append-requests view argv)))
                :replayable replayable
                :on-staged (lambda (tx-op results) (declare (ignore results)) tx-op)
                :on-rejected (lambda (code &rest rest) (declare (ignore rest)) code)
                :on-not-found (lambda () :not-found)
                :on-busy (lambda () :busy))))

(defun stage (store tx-id requests)
  (tx-stage/k store tx-id '("test")
              (lambda (view commit reject)
                (declare (ignore view reject))
                (funcall commit requests))
              :on-staged (lambda (tx-op results) (declare (ignore results)) tx-op)
              :on-rejected (lambda (code &rest rest) (declare (ignore rest)) code)
              :on-not-found (lambda () :not-found)
              :on-busy (lambda () :busy)))

(defun tx-view-text (store tx-id path)
  (call-with-tx-view/k store tx-id
                       :on-view (lambda (view)
                                  (let ((octets (view-read-file view path)))
                                    (if octets (octets-string octets) (entry-state-kind (view-path-state view path)))))
                       :on-not-found (lambda () :not-found)))

(defun status-of (store tx-id)
  (tx-status/k store tx-id :on-status #'identity :on-not-found (lambda () :not-found)))

(defun commit-tx (store tx-id &key ignore-stale-reads)
  (tx-commit/k store tx-id '("tx" "commit")
               :ignore-stale-reads ignore-stale-reads
               :on-committed (lambda (op-id results) (values :committed op-id results))
               :on-rejected (lambda (code message &rest keys) (values :rejected code message keys))
               :on-not-found (lambda () :not-found)
               :on-busy (lambda () :busy)))

(defun replay-append (record view commit reject)
  (declare (ignore reject))
  (funcall commit (append-requests view (tx-op-record-argv record))))

(defun blob-names (store)
  (funcall (store-io-list-directory (store-io-port store)) (blobs-directory (store-state-directory store))))

(describe "aitools.store tx overlay"
  (it "stages writes without touching the working tree and reads them back through the tx"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (put-file store "gone.txt" "g")
      (let ((tx (begin store "demo")))
        (expect (valid-tx-id-p tx) :to-be-truthy)
        (expect (stage-append store tx "a.txt" "+1") :to-be 1)
        (expect (stage store tx (list (delete-request "gone.txt") (write-file-request "dir/new.txt" (bytes "n"))))
                :to-be 2)
        (expect (disk-text store "a.txt") :to-equal "a")
        (expect (disk-text store "gone.txt") :to-equal "g")
        (expect (disk-text store "dir") :to-be :absent)
        (expect (tx-view-text store tx "a.txt") :to-equal "a+1")
        (expect (tx-view-text store tx "gone.txt") :to-be :absent)
        (expect (tx-view-text store tx "dir/new.txt") :to-equal "n")
        (call-with-tx-view/k store tx
                             :on-view (lambda (view)
                                        (expect (view-directory-entries view "")
                                                :to-equal '(("a.txt" . :file) ("dir" . :directory)))
                                        ;; --expect-hash inside a tx compares with the tx state.
                                        (expect (entry-state-hash (view-path-state view "a.txt"))
                                                :to-equal (aitools.kernel.domain:content-hash (bytes "a+1"))))
                             :on-not-found (lambda () (expect :not-found :to-be nil)))
        (let ((status (status-of store tx)))
          (expect (tx-status-name status) :to-equal "demo")
          (expect (mapcar #'tx-op-record-tx-op (tx-status-ops status)) :to-equal '(1 2))
          (expect (mapcar #'tx-path-path (tx-status-paths status)) :to-equal '("a.txt" "dir" "dir/new.txt" "gone.txt"))
          (expect (tx-status-drift status) :to-equal '()))
        (expect (mapcar #'tx-status-id (tx-list store)) :to-equal (list tx)))) :io nil))

  (it "diffs base to staged with both contents"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (stage store tx (list (write-file-request "b.txt" (bytes "b"))))
        (tx-diff/k store tx
                   :on-diff (lambda (results)
                              (expect (actions results) :to-equal '(("a.txt" :modified) ("b.txt" :created)))
                              (expect (octets-string (change-result-before-content (first results))) :to-equal "a")
                              (expect (octets-string (change-result-after-content (first results))) :to-equal "a+1"))
                   :on-not-found (lambda () (expect :not-found :to-be nil))))) :io nil))

  (it "answers not-found for an unknown or malformed tx id"
    (call-with-temp-store (lambda (store) (expect (stage-append store (format-tx-id 3964000000 "00000000") "a" "x") :to-be :not-found)
      (expect (stage-append store "../../x" "a" "x") :to-be :not-found)
      (expect (status-of store "tx-nope") :to-be :not-found)) :io nil)))

(describe "aitools.store tx commit"
  (it "preserves an explicit directory mode from the tx overlay"
    (call-with-temp-store (lambda (store) (let ((tx (begin store)))
        (expect (stage store tx (list (mkdir-request "private" :mode #o700)
                                      (write-file-request "private/file" (bytes "data"))))
                :to-be 1)
        (expect (disk-text store "private") :to-be :absent)
        (expect (commit-tx store tx) :to-be :committed)
        (expect (disk-mode store "private") :to-be #o700)
        (expect (disk-text store "private/file") :to-equal "data"))) :io nil))

  (it "applies the whole tx as one journal op, removes the tx, and undoes as a unit"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (put-file store "gone.txt" "g")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (stage-append store tx "a.txt" "+2")
        (stage store tx (list (delete-request "gone.txt") (write-file-request "dir/new.txt" (bytes "n"))))
        (multiple-value-bind (status op-id results) (commit-tx store tx)
          (expect status :to-be :committed)
          (expect (actions results)
                  :to-equal '(("a.txt" :modified) ("dir" :created) ("dir/new.txt" :created) ("gone.txt" :deleted)))
          (expect (length (read-journal store)) :to-be 1)
          (expect (disk-text store "a.txt") :to-equal "a+1+2")
          (expect (disk-text store "dir/new.txt") :to-equal "n")
          (expect (disk-text store "gone.txt") :to-be :absent)
          (expect (status-of store tx) :to-be :not-found)
          (expect (undo store op-id) :to-be :committed)
          (expect (disk-text store "a.txt") :to-equal "a")
          (expect (disk-text store "gone.txt") :to-equal "g")
          (expect (disk-text store "dir") :to-be :absent)))) :io nil))

  (it "refuses a write-set conflict with kind write and writes nothing"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (put-file store "b.txt" "b")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+tx")
        (stage-append store tx "b.txt" "+tx")
        (put-file store "a.txt" "external")
        (expect (tx-status-drift (status-of store tx)) :to-equal '("a.txt"))
        (multiple-value-bind (status code message keys) (commit-tx store tx)
          (declare (ignore message))
          (expect status :to-be :rejected)
          (expect code :to-equal "refusal.target-changed")
          (expect (mapcar (lambda (c) (list (conflict-path c) (conflict-kind c))) (getf keys :conflicts))
                  :to-equal '(("a.txt" :write))))
        (expect (disk-text store "a.txt") :to-equal "external")
        (expect (disk-text store "b.txt") :to-equal "b")
        (expect (read-journal store) :to-equal '()))) :io nil))

  (it "refuses a stale read with kind read, until re-read or ignored"
    (call-with-temp-store (lambda (store) (put-file store "config.txt" "v1")
      (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (flet ((record-read ()
                 (tx-record-read/k store tx "config.txt"
                                   :on-recorded (lambda (state) (declare (ignore state)) :recorded)
                                   :on-not-found (lambda () :not-found) :on-busy (lambda () :busy))))
          (expect (record-read) :to-be :recorded)
          (stage-append store tx "a.txt" "+tx")
          (put-file store "config.txt" "v2")
          (expect (tx-status-stale-reads (status-of store tx)) :to-equal '("config.txt"))
          (multiple-value-bind (status code message keys) (commit-tx store tx)
            (declare (ignore message))
            (expect status :to-be :rejected)
            (expect code :to-equal "refusal.target-changed")
            (expect (mapcar #'conflict-kind (getf keys :conflicts)) :to-equal '(:read)))
          (expect (record-read) :to-be :recorded)
          (expect (commit-tx store tx) :to-be :committed)
          (expect (disk-text store "a.txt") :to-equal "a+tx")))) :io nil))

  (it "commits over a stale read with --ignore-stale-reads"
    (call-with-temp-store (lambda (store) (put-file store "config.txt" "v1")
      (let ((tx (begin store)))
        (tx-record-read/k store tx "config.txt" :on-recorded #'identity
                                                :on-not-found (lambda ()) :on-busy (lambda ()))
        (stage store tx (list (write-file-request "out.txt" (bytes "o"))))
        (put-file store "config.txt" "v2")
        (expect (commit-tx store tx) :to-be :rejected)
        (expect (commit-tx store tx :ignore-stale-reads t) :to-be :committed))) :io nil))

  (it "rolls forward a commit interrupted after its commit point and removes the tx"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (stage store tx (list (write-file-request "b.txt" (bytes "b"))))
        (call-with-fault-at :after-apply-step (lambda () (commit-tx store tx)) :nth 1)
        (expect (mapcar #'cdr (nth-value 1 (recover store))) :to-equal '("rolled-forward"))
        (expect (disk-text store "a.txt") :to-equal "a+1")
        (expect (disk-text store "b.txt") :to-equal "b")
        (expect (status-of store tx) :to-be :not-found)
        (expect (length (read-journal store)) :to-be 1))) :io nil))

  (it "keeps the tx intact when a commit is interrupted before its commit point"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (call-with-fault-at :after-prepare (lambda () (commit-tx store tx)))
        (expect (mapcar #'cdr (nth-value 1 (recover store))) :to-equal '("discarded"))
        (expect (disk-text store "a.txt") :to-equal "a")
        (expect (commit-tx store tx) :to-be :committed)
        (expect (disk-text store "a.txt") :to-equal "a+1"))) :io nil)))

