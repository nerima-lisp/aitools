;;;; t/integration/journal-tx-test.lisp
;;;;
;;;; The `tx` group (docs/src/reference/transactions.md) through the
;;;; journal flows against a real temporary workspace and store. Writes into
;;;; the tx go through the store's TX-STAGE/K exactly as a `--tx` write
;;;; command does; "append" is this file's content-based test command, with
;;;; a replayer registered for `tx rebase`.
;;;; Store failures, lock contention, and their repairs are in
;;;; journal-tx-failure-test.lisp.
(in-package #:aitools.journal.test)

(defun append-requests (view argv)
  "Requests for (\"append\" path text): PATH as VIEW shows it, plus TEXT."
  (destructuring-bind (command path text) argv
    (declare (ignore command))
    (let ((current (aitools.store.application:view-read-file view path)))
      (list (aitools.store.domain:write-file-request
             path (bytes (concatenate 'string (if current (aitools.store.domain:octets-string current) "") text)))))))

(defun replay-append (argv view commit reject)
  (declare (ignore reject))
  (funcall commit (append-requests view argv)))

(defun call-with-append-replayer (thunk)
  (let ((aitools.journal.application::*tx-replayers* (make-hash-table :test 'equal)))
    (register-tx-replayer "append" #'replay-append)
    (funcall thunk)))

(defun stage-append (store tx path text &key (replayable t) (lock-timeout-ms 10000))
  "Stage (\"append\" PATH TEXT) into TX; returns the tx_op number."
  (let ((argv (list "append" path text)))
    (aitools.store.application:tx-stage/k
     store tx argv
     (lambda (view commit reject)
       (declare (ignore reject))
       (funcall commit (append-requests view argv)))
     :replayable replayable
     :lock-timeout-ms lock-timeout-ms
     :on-staged (lambda (tx-op results) (declare (ignore results)) tx-op)
     :on-rejected (lambda (code &rest rest) (error "stage rejected: ~A ~S" code rest))
     :on-not-found (lambda () (error "stage: no tx"))
     :on-busy (lambda () (error "stage: busy")))))

(defun record-read (store tx path)
  (aitools.store.application:tx-record-read/k
   store tx path
   :on-recorded #'identity
   :on-not-found (lambda () (error "no tx"))
   :on-busy (lambda () (error "busy"))))

(defun tx-text (store tx path)
  (aitools.store.application:call-with-tx-view/k
   store tx
   :on-view (lambda (view)
              (let ((octets (aitools.store.application:view-read-file view path)))
                (if octets (aitools.store.domain:octets-string octets) :absent)))
   :on-not-found (lambda () :no-tx)))

(defun begin (store &optional name)
  (field (nth-value 1 (run #'tx-begin-flow store :name name)) "tx"))

(describe "aitools.journal tx begin and status"
  (it "opens a tx and lists it with counts, drift and stale reads"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (multiple-value-bind (kind fields) (run #'tx-begin-flow store :name "demo")
        (expect kind :to-be :ok)
        (expect (mapcar #'car fields) :to-equal '("tx" "name"))
        (expect (field fields "name") :to-equal "demo")
        (let ((tx (field fields "tx")))
          (expect (aitools.store.domain:valid-tx-id-p tx) :to-be-truthy)
          (stage-append store tx "a.txt" "+1")
          (stage-append store tx "b.txt" "b")
          (put-file store "a.txt" "changed")
          (multiple-value-bind (kind fields) (run #'tx-status-flow store nil)
            (expect kind :to-be :ok)
            (expect (field fields "total") :to-be 1)
            (expect (json-alist (first (field fields "items")))
                    :to-match-object (list (cons "tx" tx) (cons "name" "demo") (cons "ops" 2) (cons "paths" 2)
                                           (cons "drift" '("a.txt")) (cons "stale_reads" '())))
            (expect (mapcar #'car (json-alist (first (field fields "items"))))
                    :to-equal '("tx" "name" "created" "ops" "paths" "drift" "stale_reads")))))))

  (it "details one tx: ops, paths with action/base/staged, drift, stale reads"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (put-file store "r.txt" "r")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (record-read store tx "r.txt")
        (put-file store "r.txt" "r2")
        (multiple-value-bind (kind fields) (run #'tx-status-flow store tx)
          (expect kind :to-be :ok)
          (expect (mapcar #'car fields) :to-equal '("tx" "name" "created" "ops" "paths" "drift" "stale_reads"))
          (expect (field fields "name") :to-be json-kit:+json-null+)
          (expect (mapcar #'json-alist (field fields "ops"))
                  :to-equal '((("tx_op" . 1) ("command" . "aitools append a.txt +1") ("paths" . ("a.txt")))))
          (expect (mapcar #'json-alist (field fields "paths"))
                  :to-equal (list (list (cons "path" "a.txt") (cons "action" "modified")
                                        (cons "base" (aitools.kernel.domain:content-hash (bytes "a")))
                                        (cons "staged" (aitools.kernel.domain:content-hash (bytes "a+1"))))))
          (expect (field fields "drift") :to-equal '())
          (expect (field fields "stale_reads") :to-equal '("r.txt"))))))

  (it "answers input.not-found with tx status as the repair for an unknown tx"
    (with-temp-store (store)
      (dolist (flow-call (list (list #'tx-status-flow "tx-20260101T000000Z-00000000")
                               (list #'tx-diff-flow "tx-20260101T000000Z-00000000")
                               (list #'tx-rebase-flow "../x")
                               (list #'tx-commit-flow "tx-20260101T000000Z-00000000")
                               (list #'tx-abort-flow "tx-20260101T000000Z-00000000")
                               (list #'tx-drop-flow "tx-20260101T000000Z-00000000" "1")))
        (multiple-value-bind (kind error) (apply #'run (first flow-call) store (rest flow-call))
          (expect kind :to-be :error)
          (expect (getf error :code) :to-equal "input.not-found")
          (expect (some (lambda (command) (search "tx status" command)) (repair-commands error)) :to-be-truthy))))))

(describe "aitools.journal tx diff and commit"
  (it "leaves the working tree alone until commit, then writes one op that undo reverts as a whole"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (stage-append store tx "new.txt" "n")
        (expect (disk-text store "a.txt") :to-equal "a")
        (expect (disk-text store "new.txt") :to-be :absent)
        (multiple-value-bind (kind fields) (run #'tx-diff-flow store tx)
          (expect kind :to-be :ok)
          (expect (mapcar #'car fields) :to-equal '("tx" "changes"))
          (expect (mapcar (lambda (change) (list (member-value change "path") (member-value change "action")))
                          (field fields "changes"))
                  :to-equal '(("a.txt" "modified") ("new.txt" "created"))))
        (multiple-value-bind (kind fields) (run #'tx-commit-flow store tx)
          (expect kind :to-be :ok)
          (expect (mapcar #'car fields) :to-equal '("changes" "op_id"))
          (expect (disk-text store "a.txt") :to-equal "a+1")
          (expect (disk-text store "new.txt") :to-equal "n")
          (expect (nth-value 0 (run #'tx-status-flow store tx)) :to-be :error)
          (let ((op (field fields "op_id")))
            (expect (member-value (first (field (nth-value 1 (run #'history-flow store)) "items")) "command")
                    :to-equal (format nil "aitools tx commit ~A" tx))
            (expect (run #'undo-flow store op) :to-be :ok)
            (expect (disk-text store "a.txt") :to-equal "a")
            (expect (disk-text store "new.txt") :to-be :absent))))))

  (it "cuts tx diff and points at tx diff with a limit that fits"
    (with-temp-store (store)
      (let ((tx (begin store)))
        (stage-append store tx "big.txt" (format nil "~{~D~%~}" (loop for i below 30 collect i)))
        (multiple-value-bind (kind fields) (run #'tx-diff-flow store tx :max-diff-lines 3)
          (expect kind :to-be :ok)
          (expect (member-value (first (field fields "changes")) "diff_truncated") :to-be t)
          (expect (field fields "next_commands")
                  :to-equal (list (format nil "aitools --root ~A tx diff ~A --max-diff-lines 31"
                                          (aitools.store.application:store-root store) tx)))))))

  (it "refuses a write conflict with kind write and tx rebase as the repair, writing nothing"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (put-file store "b.txt" "b")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (stage-append store tx "b.txt" "+1")
        (put-file store "a.txt" "external")
        (multiple-value-bind (kind error) (run #'tx-commit-flow store tx)
          (expect kind :to-be :error)
          (expect (getf error :code) :to-equal "refusal.target-changed")
          (expect (mapcar (lambda (c) (member-value c "kind")) (getf error :conflicts)) :to-equal '("write"))
          (expect (some (lambda (command) (search (format nil "tx rebase ~A" tx) command)) (repair-commands error))
                  :to-be-truthy))
        (expect (disk-text store "a.txt") :to-equal "external")
        (expect (disk-text store "b.txt") :to-equal "b"))))

  (it "refuses a stale read, then commits after a re-read or with --ignore-stale-reads"
    (with-temp-store (store)
      (put-file store "r.txt" "r")
      (let ((tx (begin store)))
        (record-read store tx "r.txt")
        (stage-append store tx "w.txt" "w")
        (put-file store "r.txt" "r2")
        (multiple-value-bind (kind error) (run #'tx-commit-flow store tx)
          (expect kind :to-be :error)
          (expect (mapcar (lambda (c) (member-value c "kind")) (getf error :conflicts)) :to-equal '("read"))
          (let ((commands (repair-commands error)))
            (expect (some (lambda (c) (search (format nil "read r.txt --tx ~A" tx) c)) commands) :to-be-truthy)
            (expect (some (lambda (c) (search "--ignore-stale-reads" c)) commands) :to-be-truthy)))
        (record-read store tx "r.txt")
        (expect (run #'tx-commit-flow store tx) :to-be :ok)
        (expect (disk-text store "w.txt") :to-equal "w")))
    (with-temp-store (store)
      (put-file store "r.txt" "r")
      (let ((tx (begin store)))
        (record-read store tx "r.txt")
        (stage-append store tx "w.txt" "w")
        (put-file store "r.txt" "r2")
        (expect (run #'tx-commit-flow store tx :ignore-stale-reads t) :to-be :ok)
        (expect (disk-text store "w.txt") :to-equal "w"))))

  (it "lets the first of two concurrent commits on one file win and the other conflict"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let* ((txs (list (begin store) (begin store))))
        (loop for tx in txs for n from 1 do (stage-append store tx "a.txt" (format nil "+~D" n)))
        (let ((pids (mapcar (lambda (tx)
                              (call-in-child-process
                               (lambda ()
                                 (unless (eq (run #'tx-commit-flow store tx) :ok)
                                   (error "conflict")))))
                            txs)))
          (expect (sort (mapcar (lambda (pid) (nth-value 1 (wait-for-child pid))) pids) #'<) :to-equal '(0 1))
          (expect (member (disk-text store "a.txt") '("a+1" "a+2") :test #'string=) :to-be-truthy)
          (expect (field (nth-value 1 (run #'tx-status-flow store nil)) "total") :to-be 1))))))

(describe "aitools.journal tx rebase"
  (it "re-applies content-based ops onto the changed file so the tx commits"
    (call-with-append-replayer
     (lambda ()
       (with-temp-store (store)
         (put-file store "a.txt" "a")
         (let ((tx (begin store)))
           (stage-append store tx "a.txt" "+1")
           (put-file store "a.txt" "external")
           (multiple-value-bind (kind fields) (run #'tx-rebase-flow store tx)
             (expect kind :to-be :ok)
             (expect fields :to-equal (list (cons "tx" tx) (cons "rebased" '("a.txt")))))
           (expect (tx-text store tx "a.txt") :to-equal "external+1")
           (expect (run #'tx-commit-flow store tx) :to-be :ok)
           (expect (disk-text store "a.txt") :to-equal "external+1"))))))

  (it "reports nothing rebased when nothing drifted"
    (with-temp-store (store)
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "x")
        (expect (field (nth-value 1 (run #'tx-rebase-flow store tx)) "rebased") :to-equal '()))))

  (it "conflicts with exit 2 and leaves the tx unchanged for a position-based op or a command without a replayer"
    (dolist (case '(:not-replayable :no-replayer))
      (call-with-append-replayer
       (lambda ()
         (with-temp-store (store)
           (put-file store "a.txt" "a")
           (let ((tx (begin store)))
             (stage-append store tx "a.txt" "+1" :replayable (eq case :no-replayer))
             (when (eq case :no-replayer)
               (clrhash aitools.journal.application::*tx-replayers*))
             (put-file store "a.txt" "external")
             (multiple-value-bind (kind error) (run #'tx-rebase-flow store tx)
               (expect kind :to-be :error)
               (expect (getf error :code) :to-equal "refusal.target-changed")
               (expect (mapcar (lambda (c) (member-value c "path")) (getf error :conflicts)) :to-equal '("a.txt"))
               (expect (repair-commands error) :not :to-equal '()))
             (expect (tx-text store tx "a.txt") :to-equal "a+1")
             (expect (field (nth-value 1 (run #'tx-status-flow store tx)) "drift") :to-equal '("a.txt")))))))))

(describe "aitools.journal tx drop and abort"
  (it "drops the given op and every later one, back to the state before it"
    (with-temp-store (store)
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "1")
        (stage-append store tx "a.txt" "2")
        (stage-append store tx "b.txt" "3")
        (multiple-value-bind (kind fields) (run #'tx-drop-flow store tx "2")
          (expect kind :to-be :ok)
          (expect fields :to-equal (list (cons "tx" tx) (cons "dropped" '(2 3)))))
        (expect (tx-text store tx "a.txt") :to-equal "1")
        (expect (tx-text store tx "b.txt") :to-be :absent)
        (expect (length (field (nth-value 1 (run #'tx-status-flow store tx)) "ops")) :to-be 1))))

  (it "rejects a tx_op that is not a number, and one the tx does not have"
    (with-temp-store (store)
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "1")
        (expect (getf (nth-value 1 (run #'tx-drop-flow store tx "x")) :code) :to-equal "argument.invalid")
        (expect (getf (nth-value 1 (run #'tx-drop-flow store tx "5")) :code) :to-equal "input.not-found"))))

  (it "aborts without touching the working tree and leaves no tx behind"
    (with-temp-store (store)
      (put-file store "a.txt" "a")
      (let ((tx (begin store)))
        (stage-append store tx "a.txt" "+1")
        (stage-append store tx "new.txt" "n")
        (multiple-value-bind (kind fields) (run #'tx-abort-flow store tx)
          (expect kind :to-be :ok)
          (expect fields :to-equal (list (cons "tx" tx) (cons "discarded" '("a.txt" "new.txt")))))
        (expect (disk-text store "a.txt") :to-equal "a")
        (expect (disk-text store "new.txt") :to-be :absent)
        (expect (tx-text store tx "a.txt") :to-be :no-tx)
        (expect (funcall (aitools.store.application:store-io-list-directory (aitools.store.application:store-io-port store))
                         (aitools.store.domain:tx-root-directory (aitools.store.application:store-state-directory store)))
                :to-equal '())))))

(describe "aitools.journal tx concurrency"
  (it "serialises two processes staging into one tx, so ops stay numbered and intact"
    (with-temp-store (store)
      (let* ((tx (begin store))
             (pids (loop for n below 2
                         collect (let ((n n))
                                   (call-in-child-process
                                    (lambda ()
                                      (loop repeat 4 do (stage-append store tx "log.txt" (princ-to-string n)))))))))
        (dolist (pid pids)
          (expect (multiple-value-list (wait-for-child pid)) :to-equal '(:exited 0)))
        (multiple-value-bind (kind fields) (run #'tx-status-flow store tx)
          (expect kind :to-be :ok)
          (expect (mapcar (lambda (op) (member-value op "tx_op")) (field fields "ops")) :to-equal '(1 2 3 4 5 6 7 8)))
        (expect (length (tx-text store tx "log.txt")) :to-be 8)))))
