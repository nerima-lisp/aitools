(in-package #:aitools.store.test)

(defun two-file-write (store)
  "Modify a.txt and create b/new.txt in one op: three apply steps (mkdir b,
replace a.txt, replace b/new.txt) and two temp files."
  (commit store (list (write-file-request "a.txt" (bytes "a-new"))
                      (write-file-request "b/new.txt" (bytes "b-new")))))

(defun unchanged-p (store)
  (and (equal (disk-text store "a.txt") "a-old")
       (eq (disk-text store "b") :absent)
       (null (temp-files store))))

(defun all-applied-p (store)
  (and (equal (disk-text store "a.txt") "a-new")
       (equal (disk-text store "b/new.txt") "b-new")
       (null (temp-files store))
       (null (intent-files store))
       (= 1 (length (read-journal store)))))

(defun crash-then-recover (point &key (nth 1))
  "Crash the two-file write at POINT; return (values fired status entries
unchanged-p all-applied-p) after recovery."
  (call-with-temp-store (lambda (store) (put-file store "a.txt" "a-old")
    (let ((fired (call-with-fault-at point (lambda () (two-file-write store)) :nth nth)))
      (multiple-value-bind (status entries) (recover store)
        (values fired status (mapcar #'cdr entries) (unchanged-p store) (all-applied-p store))))) :io nil))

(describe "aitools.store recovery before the commit point"
  (it-each ((:after-lock) (:after-validate) (:after-blobs))
      "has nothing to recover when the op stopped at ~A, before its intent record existed"
      (point)
    (multiple-value-bind (fired status actions unchanged) (crash-then-recover point)
      (declare (ignore actions))
      (expect fired :to-be-truthy)
      (expect status :to-be :none)
      (expect unchanged :to-be-truthy)))

  (it-each ((:after-intent-header 1) (:after-temp 1) (:after-temp 2) (:after-prepare 1))
      "discards an op whose intent record is incomplete at ~A (nth ~A), removing its temp files"
      (point nth)
    (multiple-value-bind (fired status actions unchanged) (crash-then-recover point :nth nth)
      (expect fired :to-be-truthy)
      (expect status :to-be :recovered)
      (expect actions :to-equal '("discarded"))
      (expect unchanged :to-be-truthy)))

  (it "leaves temp files behind at the crash, which recovery then removes"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a-old")
      (call-with-fault-at :after-prepare (lambda () (two-file-write store)))
      (expect (length (temp-files store)) :to-be 2)
      (expect (length (intent-files store)) :to-be 1)
      (recover store)
      (expect (temp-files store) :to-equal '())
      (expect (intent-files store) :to-equal '())) :io nil))

  (it "cleans up at once when prepare fails with an error"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a-old")
      (expect (handler-case (call-with-fault-at :after-temp (lambda () (two-file-write store)) :mode :error)
                (injected-fault () :signalled))
              :to-be :signalled)
      (expect (unchanged-p store) :to-be-truthy)
      (expect (intent-files store) :to-equal '())
      (expect (recover store) :to-be :none)) :io nil)))

(describe "aitools.store recovery after the commit point"
  (it-each ((nil) (t))
      "recovers a finalized mode-zero directory with children: ~A"
      (children)
    (call-with-temp-store (lambda (store) (unwind-protect
           (progn
             (expect (call-with-fault-at :after-apply (lambda () (commit store
                               (append (list (mkdir-request "private" :mode 0))
                                       (when children
                                         (list (mkdir-request "private/nested" :mode 0)
                                               (write-file-request "private/nested/file"
                                                                   (bytes "data"))))))))
                     :to-be-truthy)
             (expect (recover store) :to-be :recovered)
             (expect (disk-mode store "private") :to-be 0)
             (expect (intent-files store) :to-equal '())
             (expect (length (read-journal store)) :to-be 1)
             (expect (recover store) :to-be :none)
             ;; Inspection and fixture cleanup must not rely on recovery bypassing permissions.
             (sb-posix:chmod (disk-path store "private") #o700)
             (when children
               (expect (disk-mode store "private/nested") :to-be 0)
               (sb-posix:chmod (disk-path store "private/nested") #o700)
               (expect (disk-text store "private/nested/file") :to-equal "data")))
        (when (probe-file (disk-path store "private"))
          (sb-posix:chmod (disk-path store "private") #o700))
        (when (probe-file (disk-path store "private/nested"))
          (sb-posix:chmod (disk-path store "private/nested") #o700)))) :io nil))

  (it-each ((:after-apply-step) (:after-apply))
      "restores explicit directory modes and children after interruption at ~A"
      (point)
    (call-with-temp-store (lambda (store) (unwind-protect
           (progn
             (expect (call-with-fault-at point (lambda () (commit store (list (mkdir-request "private" :mode #o500)
                                           (mkdir-request "private/nested" :mode #o500)
                                           (write-file-request "private/nested/file" (bytes "data"))))))
                     :to-be-truthy)
             (expect (recover store) :to-be :recovered)
             (expect (disk-mode store "private") :to-be #o500)
             (expect (disk-mode store "private/nested") :to-be #o500)
             (expect (disk-text store "private/nested/file") :to-equal "data")
             (expect (intent-files store) :to-equal '())
             (expect (recover store) :to-be :none))
        (when (probe-file (disk-path store "private"))
          (sb-posix:chmod (disk-path store "private") #o700))
        (when (probe-file (disk-path store "private/nested"))
          (sb-posix:chmod (disk-path store "private/nested") #o700)))) :io nil))

  (it-each ((:after-intent 1) (:after-apply-step 1) (:after-apply-step 2) (:after-apply-step 3)
            (:after-apply 1) (:after-journal 1))
      "rolls forward to the complete result from ~A (nth ~A)"
      (point nth)
    (multiple-value-bind (fired status actions unchanged applied) (crash-then-recover point :nth nth)
      (declare (ignore unchanged))
      (expect fired :to-be-truthy)
      (expect status :to-be :recovered)
      (expect actions :to-equal '("rolled-forward"))
      (expect applied :to-be-truthy)))

  (it "never leaves a multi-file write half applied"
    (dolist (n '(1 2 3))
      (multiple-value-bind (fired status actions unchanged applied) (crash-then-recover :after-apply-step :nth n)
        (declare (ignore fired status actions))
        (expect (or unchanged applied) :to-be-truthy)
        (expect applied :to-be-truthy))))

  (it "reaches the same result when recovery itself is interrupted (idempotence)"
    (dolist (second-crash '(1 2 3))
      (call-with-temp-store (lambda (store) (put-file store "a.txt" "a-old")
        (call-with-fault-at :after-apply-step (lambda () (two-file-write store)) :nth 2)
        (expect (call-with-fault-at :after-apply-step (lambda () (recover store)) :nth second-crash) :to-be-truthy)
        (expect (call-with-fault-at :after-journal (lambda () (recover store))) :to-be-truthy)
        (multiple-value-bind (status entries) (recover store)
          (expect status :to-be :recovered)
          (expect (mapcar #'cdr entries) :to-equal '("rolled-forward")))
        (expect (all-applied-p store) :to-be-truthy)
        (expect (recover store) :to-be :none)) :io nil)))

  (it "reaches the same result when a discard is interrupted"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a-old")
      (call-with-fault-at :after-prepare (lambda () (two-file-write store)))
      (call-with-fault-at :recovery-after-discard (lambda () (recover store)))
      (expect (length (temp-files store)) :to-be 1)
      (multiple-value-bind (status entries) (recover store)
        (expect status :to-be :recovered)
        (expect (mapcar #'cdr entries) :to-equal '("discarded")))
      (expect (unchanged-p store) :to-be-truthy)) :io nil))

  (it "does not move a path again once the same op has rewritten it"
    ;; Undoing an overwriting move is `move dst -> src` then `write dst`.
    ;; After both steps dst exists again; a roll-forward that re-ran the
    ;; move because its source exists would move the restored file away.
    (call-with-temp-store (lambda (store) (put-file store "src.txt" "S")
      (put-file store "dst.txt" "D")
      (let ((op (nth-value 1 (commit store (list (move-request "src.txt" "dst.txt"))))))
        (expect (call-with-fault-at :after-apply (lambda () (undo store op))) :to-be-truthy)
        (expect (mapcar #'cdr (nth-value 1 (recover store))) :to-equal '("rolled-forward"))
        (expect (list (disk-text store "src.txt") (disk-text store "dst.txt")) :to-equal '("S" "D")))) :io nil))

  (it "heals a torn final journal line left by a crash mid-append"
    ;; With per-op O_APPEND a crash can tear the journal append, leaving a
    ;; partial last line. The op's intent record is still present, so recovery
    ;; replays it; its rewrite drops the torn tail rather than appending after
    ;; it, which would merge the two into one corrupt line and fail every
    ;; later read.
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a-old")
      (call-with-fault-at :after-apply (lambda () (two-file-write store)))
      (let ((journal (journal-file-path (store-state-directory store))))
        (with-open-file (out (sb-ext:parse-native-namestring journal)
                             :direction :output :if-exists :append :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
          (write-sequence (bytes "{\"op_id\":\"op-2026") out)))
      (multiple-value-bind (status entries) (recover store)
        (expect status :to-be :recovered)
        (expect (mapcar #'cdr entries) :to-equal '("rolled-forward")))
      (expect (all-applied-p store) :to-be-truthy)
      (expect (recover store) :to-be :none)) :io nil))

  (it "rolls forward an undo interrupted mid-apply"
    (call-with-temp-store (lambda (store) (put-file store "a.txt" "a-old")
      (let ((op (nth-value 1 (two-file-write store))))
        (call-with-fault-at :after-apply-step (lambda () (undo store op)) :nth 1)
        (recover store)
        (expect (unchanged-p store) :to-be-truthy)
        (expect (journal-entry-undoes (first (last (read-journal store)))) :to-equal op))) :io nil)))

