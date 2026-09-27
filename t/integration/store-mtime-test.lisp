;;;; t/integration/store-mtime-test.lisp
;;;;
;;;; MTIME-REQUEST, the change `touch` makes to an existing file (undoing a
;;;; `touch` restores the original mtime): journaled with the old and new
;;;; times, undone in place, staged and committed through a tx, and
;;;; recovered like every other write-protocol step.
(in-package #:aitools.store.test)

(defconstant +old-mtime+ 1000000000)
(defconstant +new-mtime+ 1577934245)

(defun disk-mtime (store relative)
  (sb-posix:stat-mtime (sb-posix:lstat (disk-path store relative))))

(defun put-old-file (store relative string)
  (put-file store relative string)
  (sb-posix:utimes (disk-path store relative) +old-mtime+ +old-mtime+))

(defun touch-op (store)
  (commit store (list (mtime-request "a.txt" +new-mtime+))))

(describe "aitools.store mtime requests (touch of an existing file)"
  (it "sets the mtime, keeps content and mode, and journals both times"
    (with-temp-store (store)
      (put-old-file store "a.txt" "keep")
      (sb-posix:chmod (disk-path store "a.txt") #o600)
      (multiple-value-bind (status op results) (touch-op store)
        (expect status :to-be :committed)
        (expect op :to-be-truthy)
        (expect (actions results) :to-equal '(("a.txt" :modified)))
        (expect (change-diff (first results)) :to-equal ""))
      (expect (disk-mtime store "a.txt") :to-be +new-mtime+)
      (expect (disk-text store "a.txt") :to-equal "keep")
      (expect (disk-mode store "a.txt") :to-be #o600)
      (let ((change (first (journal-entry-changes (first (read-journal store))))))
        (expect (entry-state-mtime (change-result-before change)) :to-be +old-mtime+)
        (expect (entry-state-mtime (change-result-after change)) :to-be +new-mtime+))))

  (it "is undone by restoring the original mtime, and redone by undoing the undo"
    (with-temp-store (store)
      (put-old-file store "a.txt" "keep")
      (let* ((op (nth-value 1 (touch-op store)))
             (undo-op (nth-value 1 (undo store op))))
        (expect undo-op :to-be-truthy)
        (expect (disk-mtime store "a.txt") :to-be +old-mtime+)
        (expect (disk-text store "a.txt") :to-equal "keep")
        (expect (undo store undo-op) :to-be :committed)
        (expect (disk-mtime store "a.txt") :to-be +new-mtime+))))

  (it "refuses the undo when the mtime changed after the op"
    (with-temp-store (store)
      (put-old-file store "a.txt" "keep")
      (let ((op (nth-value 1 (touch-op store))))
        (sb-posix:utimes (disk-path store "a.txt") 1234567890 1234567890)
        (multiple-value-bind (status code message keys) (undo store op)
          (declare (ignore message))
          (expect (list status code) :to-equal '(:rejected "refusal.target-changed"))
          (expect (mapcar #'conflict-path (getf keys :conflicts)) :to-equal '("a.txt")))
        (expect (disk-mtime store "a.txt") :to-be 1234567890))))

  (it "refuses a missing path and a directory"
    (with-temp-store (store)
      (sb-posix:mkdir (disk-path store "d") #o755)
      (expect (nth-value 1 (commit store (list (mtime-request "none.txt" +new-mtime+)))) :to-equal "input.not-found")
      (expect (nth-value 1 (commit store (list (mtime-request "d" +new-mtime+)))) :to-equal "refusal.not-a-file")))

  (it "gives a newly written file the mtime its request names"
    (with-temp-store (store)
      (commit store (list (write-file-request "new.txt" (bytes "") :mtime +new-mtime+)))
      (expect (disk-mtime store "new.txt") :to-be +new-mtime+))))

(describe "aitools.store mtime requests inside a tx"
  (it "stages the mtime without touching the disk and applies it at commit, undoable as one op"
    (with-temp-store (store)
      (put-old-file store "a.txt" "keep")
      (let ((tx (begin store)))
        (expect (stage store tx (list (mtime-request "a.txt" +new-mtime+))) :to-be 1)
        (expect (disk-mtime store "a.txt") :to-be +old-mtime+)
        (multiple-value-bind (status op results) (commit-tx store tx)
          (expect status :to-be :committed)
          (expect (actions results) :to-equal '(("a.txt" :modified)))
          (expect (disk-mtime store "a.txt") :to-be +new-mtime+)
          (expect (undo store op) :to-be :committed)
          (expect (disk-mtime store "a.txt") :to-be +old-mtime+)))))

  (it "keeps a staged mtime when the tx also rewrites the content first"
    (with-temp-store (store)
      (put-old-file store "a.txt" "keep")
      (let ((tx (begin store)))
        (stage store tx (list (write-file-request "a.txt" (bytes "changed"))))
        (stage store tx (list (mtime-request "a.txt" +new-mtime+)))
        (expect (commit-tx store tx) :to-be :committed)
        (expect (disk-text store "a.txt") :to-equal "changed")
        (expect (disk-mtime store "a.txt") :to-be +new-mtime+)))))

(describe "aitools.store recovery of an mtime step"
  (it "discards an mtime op stopped before its commit point"
    (with-temp-store (store)
      (put-old-file store "a.txt" "keep")
      (expect (with-fault-at (:after-intent-header) (touch-op store)) :to-be-truthy)
      (multiple-value-bind (status entries) (recover store)
        (expect status :to-be :recovered)
        (expect (mapcar #'cdr entries) :to-equal '("discarded")))
      (expect (disk-mtime store "a.txt") :to-be +old-mtime+)
      (expect (read-journal store) :to-equal '())))

  (it "rolls an mtime op forward from every step after its commit point"
    (dolist (case '((:after-intent 1) (:after-apply-step 1) (:after-apply 1) (:after-journal 1)))
      (with-temp-store (store)
        (put-old-file store "a.txt" "keep")
        (expect (with-fault-at ((first case) :nth (second case)) (touch-op store)) :to-be-truthy)
        (multiple-value-bind (status entries) (recover store)
          (expect status :to-be :recovered)
          (expect (mapcar #'cdr entries) :to-equal '("rolled-forward")))
        (expect (disk-mtime store "a.txt") :to-be +new-mtime+)
        (expect (disk-text store "a.txt") :to-equal "keep")
        (expect (length (read-journal store)) :to-be 1)
        (expect (intent-files store) :to-equal '()))))

  (it "reaches the same result when the roll-forward is itself interrupted"
    (with-temp-store (store)
      (put-old-file store "a.txt" "keep")
      (with-fault-at (:after-intent) (touch-op store))
      (expect (with-fault-at (:after-apply-step) (recover store)) :to-be-truthy)
      (expect (mapcar #'cdr (nth-value 1 (recover store))) :to-equal '("rolled-forward"))
      (expect (disk-mtime store "a.txt") :to-be +new-mtime+)
      (expect (length (read-journal store)) :to-be 1)
      (expect (recover store) :to-be :none))))
