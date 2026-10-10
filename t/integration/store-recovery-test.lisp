;;;; t/integration/store-recovery-test.lisp
;;;;
;;;; Interrupt the write protocol after each step and check what the
;;;; next start's recovery does. In-process crashes use the :throw mode of
;;;; the fault adapter; the last cases kill a forked child for real.
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
  (with-temp-store (store)
    (put-file store "a.txt" "a-old")
    (let ((fired (with-fault-at (point :nth nth) (two-file-write store))))
      (multiple-value-bind (status entries) (recover store)
        (values fired status (mapcar #'cdr entries) (unchanged-p store) (all-applied-p store))))))

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
    (with-temp-store (store)
      (put-file store "a.txt" "a-old")
      (with-fault-at (:after-prepare) (two-file-write store))
      (expect (length (temp-files store)) :to-be 2)
      (expect (length (intent-files store)) :to-be 1)
      (recover store)
      (expect (temp-files store) :to-equal '())
      (expect (intent-files store) :to-equal '())))

  (it "cleans up at once when prepare fails with an error"
    (with-temp-store (store)
      (put-file store "a.txt" "a-old")
      (expect (handler-case (with-fault-at (:after-temp :mode :error) (two-file-write store))
                (injected-fault () :signalled))
              :to-be :signalled)
      (expect (unchanged-p store) :to-be-truthy)
      (expect (intent-files store) :to-equal '())
      (expect (recover store) :to-be :none))))

(describe "aitools.store recovery after the commit point"
  (it-each ((nil) (t))
      "recovers a finalized mode-zero directory with children: ~A"
      (children)
    (with-temp-store (store)
      (unwind-protect
           (progn
             (expect (with-fault-at (:after-apply)
                       (commit store
                               (append (list (mkdir-request "private" :mode 0))
                                       (when children
                                         (list (mkdir-request "private/nested" :mode 0)
                                               (write-file-request "private/nested/file"
                                                                   (bytes "data")))))))
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
          (sb-posix:chmod (disk-path store "private/nested") #o700)))))

  (it-each ((:after-apply-step) (:after-apply))
      "restores explicit directory modes and children after interruption at ~A"
      (point)
    (with-temp-store (store)
      (unwind-protect
           (progn
             (expect (with-fault-at (point)
                       (commit store (list (mkdir-request "private" :mode #o500)
                                           (mkdir-request "private/nested" :mode #o500)
                                           (write-file-request "private/nested/file" (bytes "data")))))
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
          (sb-posix:chmod (disk-path store "private/nested") #o700)))))

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
      (with-temp-store (store)
        (put-file store "a.txt" "a-old")
        (with-fault-at (:after-apply-step :nth 2) (two-file-write store))
        (expect (with-fault-at (:after-apply-step :nth second-crash) (recover store)) :to-be-truthy)
        (expect (with-fault-at (:after-journal) (recover store)) :to-be-truthy)
        (multiple-value-bind (status entries) (recover store)
          (expect status :to-be :recovered)
          (expect (mapcar #'cdr entries) :to-equal '("rolled-forward")))
        (expect (all-applied-p store) :to-be-truthy)
        (expect (recover store) :to-be :none))))

  (it "reaches the same result when a discard is interrupted"
    (with-temp-store (store)
      (put-file store "a.txt" "a-old")
      (with-fault-at (:after-prepare) (two-file-write store))
      (with-fault-at (:recovery-after-discard) (recover store))
      (expect (length (temp-files store)) :to-be 1)
      (multiple-value-bind (status entries) (recover store)
        (expect status :to-be :recovered)
        (expect (mapcar #'cdr entries) :to-equal '("discarded")))
      (expect (unchanged-p store) :to-be-truthy)))

  (it "does not move a path again once the same op has rewritten it"
    ;; Undoing an overwriting move is `move dst -> src` then `write dst`.
    ;; After both steps dst exists again; a roll-forward that re-ran the
    ;; move because its source exists would move the restored file away.
    (with-temp-store (store)
      (put-file store "src.txt" "S")
      (put-file store "dst.txt" "D")
      (let ((op (nth-value 1 (commit store (list (move-request "src.txt" "dst.txt"))))))
        (expect (with-fault-at (:after-apply) (undo store op)) :to-be-truthy)
        (expect (mapcar #'cdr (nth-value 1 (recover store))) :to-equal '("rolled-forward"))
        (expect (list (disk-text store "src.txt") (disk-text store "dst.txt")) :to-equal '("S" "D")))))

  (it "heals a torn final journal line left by a crash mid-append"
    ;; With per-op O_APPEND a crash can tear the journal append, leaving a
    ;; partial last line. The op's intent record is still present, so recovery
    ;; replays it; its rewrite drops the torn tail rather than appending after
    ;; it, which would merge the two into one corrupt line and fail every
    ;; later read.
    (with-temp-store (store)
      (put-file store "a.txt" "a-old")
      (with-fault-at (:after-apply) (two-file-write store))
      (let ((journal (journal-file-path (store-state-directory store))))
        (with-open-file (out (sb-ext:parse-native-namestring journal)
                             :direction :output :if-exists :append :if-does-not-exist :create
                             :element-type '(unsigned-byte 8))
          (write-sequence (bytes "{\"op_id\":\"op-2026") out)))
      (multiple-value-bind (status entries) (recover store)
        (expect status :to-be :recovered)
        (expect (mapcar #'cdr entries) :to-equal '("rolled-forward")))
      (expect (all-applied-p store) :to-be-truthy)
      (expect (recover store) :to-be :none)))

  (it "rolls forward an undo interrupted mid-apply"
    (with-temp-store (store)
      (put-file store "a.txt" "a-old")
      (let ((op (nth-value 1 (two-file-write store))))
        (with-fault-at (:after-apply-step :nth 1) (undo store op))
        (recover store)
        (expect (unchanged-p store) :to-be-truthy)
        (expect (journal-entry-undoes (first (last (read-journal store)))) :to-equal op)))))

(describe "aitools.store directory recovery identity and durability"
  (it-each ((nil) (t))
      "replays an identityless mkdir only when its target is absent (existing: ~A)"
      (existing)
    (with-temp-store (store)
      (expect (with-fault-at (:after-intent)
                (commit store (list (mkdir-request "private" :mode #o500)))) :to-be-truthy)
      (when existing (sb-posix:mkdir (disk-path store "private") #o700))
      (unwind-protect
           (progn
             (expect (handler-case (recover store) (store-io-error () :refused))
                     :to-be (if existing :refused :recovered))
             (expect (disk-mode store "private") :to-be (if existing #o700 #o500))
             (expect (length (intent-files store)) :to-be (if existing 1 0))
             (expect (length (read-journal store)) :to-be (if existing 0 1)))
        (sb-posix:chmod (disk-path store "private") #o700))))

  (it "retries directory synchronization after the requested mode already took effect"
    (with-temp-store (plain)
      (let* ((io (store-io-port plain))
             (real-chmod (store-io-chmod-directory-identity io))
             (calls 0)
             (failing (copy-store-io
                       io :chmod-directory-identity
                       (lambda (path mode identity)
                         (incf calls)
                         (if (= calls 1)
                             (progn
                               ;; Model fchmod succeeding and the subsequent fsync failing.
                               (sb-posix:chmod path mode)
                               (error 'store-io-error :operation "fsync" :path path))
                             (funcall real-chmod path mode identity)))))
             (store (make-store failing (store-root plain)
                                (subseq (store-state-directory plain) 0
                                        (position #\/ (store-state-directory plain) :from-end t)))))
        (unwind-protect
             (progn
               (expect (handler-case (commit store (list (mkdir-request "private" :mode #o500)))
                         (store-committed-error () :pending)) :to-be :pending)
               (expect (disk-mode plain "private") :to-be #o500)
               (expect (length (intent-files plain)) :to-be 1)
               (expect (read-journal plain) :to-equal '())
               (expect (recover store) :to-be :recovered)
               (expect calls :to-be 2)
               (expect (intent-files plain) :to-equal '())
               (expect (length (read-journal plain)) :to-be 1)
               (expect (recover store) :to-be :none))
          (when (probe-file (disk-path plain "private"))
            (sb-posix:chmod (disk-path plain "private") #o700))))))

  (it-each ((nil) (t))
      "refuses a replaced directory after the initial mkdir apply step (symlink: ~A)"
      (symlink)
    (with-temp-store (store)
      (expect (with-fault-at (:after-apply-step)
                (commit store (list (mkdir-request "private" :mode #o500)))) :to-be-truthy)
      (sb-posix:rename (disk-path store "private") (disk-path store "saved"))
      (sb-posix:mkdir (disk-path store "replacement") #o700)
      (if symlink
          (sb-posix:symlink "replacement" (disk-path store "private"))
          (sb-posix:mkdir (disk-path store "private") #o700))
      (unwind-protect
           (progn
             (expect (handler-case (recover store) (store-io-error () :refused)) :to-be :refused)
             (expect (length (intent-files store)) :to-be 1)
             (expect (read-journal store) :to-equal '())
             (expect (disk-mode store "saved") :to-be #o700)
             (expect (disk-mode store (if symlink "replacement" "private")) :to-be #o700))
        (unless symlink (sb-posix:chmod (disk-path store "private") #o700))))))

(describe "aitools.store recovery after real process death"
  (it-each ((:after-intent-checkpoint 1) (:after-intent-checkpoint 2)
            (:after-intent-checkpoint 3) (:after-intent-checkpoint 4)
            (:after-intent-checkpoint 5))
      "preserves the full journal across restrictive directory checkpoint death at ~A (~A)"
      (point nth)
    (with-temp-store (store)
      (unwind-protect
           (progn
             (let ((pid (call-in-child-process
                         (lambda ()
                           (with-fault-at (point :nth nth :mode :exit)
                             (commit store (list (mkdir-request "private" :mode 0)
                                                 (mkdir-request "private/nested" :mode 0)
                                                 (write-file-request "private/nested/file" (bytes "data")))))))))
               (expect (multiple-value-list (wait-for-child pid)) :to-equal '(:exited 99)))
             (let* ((path (join-path (commit-directory (store-state-directory store))
                                     (first (intent-files store))))
                    (intent (nth-value 1 (decode-intent
                                         (aitools.store.application::octets-string
                                          (funcall (store-io-read-file (store-io-port store)) path)))))
                    (original (encode-journal (list (intent-journal-entry intent)))))
               (expect (length (journal-entry-changes (intent-journal-entry intent))) :to-be 3)
               (expect (recover store) :to-be :recovered)
               (expect (encode-journal (read-journal store)) :to-equal original)
               (expect (recover store) :to-be :none)
               (expect (length (read-journal store)) :to-be 1))
             (expect (intent-files store) :to-equal '())
             (expect (disk-mode store "private") :to-be 0)
             (sb-posix:chmod (disk-path store "private") #o700)
             (expect (disk-mode store "private/nested") :to-be 0)
             (sb-posix:chmod (disk-path store "private/nested") #o700)
             (expect (disk-text store "private/nested/file") :to-equal "data"))
        (when (probe-file (disk-path store "private"))
          (sb-posix:chmod (disk-path store "private") #o700))
        (when (probe-file (disk-path store "private/nested"))
          (sb-posix:chmod (disk-path store "private/nested") #o700)))))

  (it-each ((:after-directory-chmod 1) (:after-directory-chmod 2)
            (:after-directory-mode 1) (:after-directory-mode 2))
      "retains an inaccessible unsynchronized checkpoint after death at ~A (~A)"
      (point nth)
    (with-temp-store (store)
      (unwind-protect
           (progn
             (let ((pid (call-in-child-process
                         (lambda ()
                           (with-fault-at (point :nth nth :mode :exit)
                             (commit store (list (mkdir-request "private" :mode 0)
                                                 (mkdir-request "private/nested" :mode 0)
                                                 (write-file-request "private/nested/file" (bytes "data")))))))))
               (expect (multiple-value-list (wait-for-child pid)) :to-equal '(:exited 99)))
             (dotimes (attempt 2)
               (declare (ignore attempt))
               (expect (handler-case (recover store) (store-io-error () :refused)) :to-be :refused)
               (expect (length (intent-files store)) :to-be 1)
               (expect (read-journal store) :to-equal '())
               (expect (disk-mode store "private") :to-be (if (= nth 2) 0 #o700)))
             (sb-posix:chmod (disk-path store "private") #o700)
             (expect (disk-mode store "private/nested") :to-be 0))
        (when (probe-file (disk-path store "private"))
          (sb-posix:chmod (disk-path store "private") #o700))
        (when (probe-file (disk-path store "private/nested"))
          (sb-posix:chmod (disk-path store "private/nested") #o700)))))

  (it-each ((nil) (t))
      "retains a checkpoint without chmod of a replacement target (symlink: ~A)"
      (symlink)
    (with-temp-store (store)
      (expect (with-fault-at (:after-intent-checkpoint)
                (commit store (list (mkdir-request "private" :mode 0)))) :to-be-truthy)
      (sb-posix:rename (disk-path store "private") (disk-path store "saved"))
      (sb-posix:mkdir (disk-path store "replacement") #o700)
      (if symlink
          (sb-posix:symlink "replacement" (disk-path store "private"))
          (sb-posix:mkdir (disk-path store "private") #o700))
      (expect (handler-case (recover store) (store-io-error () :refused)) :to-be :refused)
      (expect (length (intent-files store)) :to-be 1)
      (expect (read-journal store) :to-equal '())
      (expect (disk-mode store "saved") :to-be #o700)
      (expect (disk-mode store (if symlink "replacement" "private")) :to-be #o700)))

  (it "refuses undo of an inaccessible tree without reopening its permissions"
    (with-temp-store (store)
      (unwind-protect
           (let ((op (nth-value 1 (commit store (list (mkdir-request "private" :mode 0)
                                                      (mkdir-request "private/nested" :mode 0)
                                                      (write-file-request "private/nested/file" (bytes "data")))))))
             (expect (handler-case (undo store op) (store-io-error () :refused)) :to-be :refused)
             (expect (disk-mode store "private") :to-be 0)
             (expect (length (read-journal store)) :to-be 1)
             (expect (intent-files store) :to-equal '()))
        (when (probe-file (disk-path store "private"))
          (sb-posix:chmod (disk-path store "private") #o700))
        (when (probe-file (disk-path store "private/nested"))
          (sb-posix:chmod (disk-path store "private/nested") #o700)))))

  (it "discards an op whose process died before the commit point"
    (with-temp-store (store)
      (put-file store "a.txt" "a-old")
      (let ((pid (call-in-child-process
                  (lambda () (with-fault-at (:after-temp :nth 2 :mode :exit) (two-file-write store))))))
        (expect (multiple-value-list (wait-for-child pid)) :to-equal '(:exited 99)))
      (expect (length (temp-files store)) :to-be 2)
      (multiple-value-bind (status entries) (recover store)
        (expect status :to-be :recovered)
        (expect (mapcar #'cdr entries) :to-equal '("discarded")))
      (expect (unchanged-p store) :to-be-truthy)))

  (it "rolls forward an op whose process died after the commit point"
    (with-temp-store (store)
      (put-file store "a.txt" "a-old")
      (let ((pid (call-in-child-process
                  (lambda () (with-fault-at (:after-apply-step :nth 2 :mode :exit) (two-file-write store))))))
        (expect (multiple-value-list (wait-for-child pid)) :to-equal '(:exited 99)))
      (expect (disk-text store "a.txt") :to-equal "a-new")
      (expect (disk-text store "b/new.txt") :to-be :absent)
      (multiple-value-bind (status entries) (recover store)
        (expect status :to-be :recovered)
        (expect (mapcar #'cdr entries) :to-equal '("rolled-forward")))
      (expect (all-applied-p store) :to-be-truthy))))

(defun write-intent-record (store op-id octets)
  (let ((path (intent-file-path (store-state-directory store) op-id)))
    (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output :element-type '(unsigned-byte 8))
      (write-sequence octets out))
    path))

(describe "aitools.store recovery of damaged records"
  (it "discards a record torn inside a multi-byte character, naming the op by its file"
    (with-temp-store (store)
      (commit store (list (write-file-request "seed.txt" (bytes "s"))))
      (let ((op-id (format-op-id 3964000000 "0000beef")))
        (write-intent-record store op-id (coerce (list 123 34 227 129) '(simple-array (unsigned-byte 8) (*))))
        (multiple-value-bind (status entries) (recover store)
          (expect status :to-be :recovered)
          (expect entries :to-equal (list (cons op-id "discarded"))))
        (expect (intent-files store) :to-equal '()))))

  (it "never deletes a named temp the store could not have written"
    (with-temp-store (store)
      (commit store (list (write-file-request "seed.txt" (bytes "s"))))
      (let* ((op-id (format-op-id 3964000000 "0000cafe"))
             (temp (concatenate 'string ".git/" (temp-file-name op-id 1)))
             (header (encode-intent-header
                      (make-intent :op-id op-id
                                   :steps (list (make-intent-step :op :replace :path "a.txt" :temp temp
                                                                  :kind :file :mode #o644))
                                   :journal-entry (make-journal-entry :op-id op-id :argv '("write") :time "t"
                                                                      :changes '())))))
        (put-file store temp "not ours")
        (write-intent-record store op-id (bytes (format nil "~A~%" header)))
        (expect (mapcar #'cdr (nth-value 1 (recover store))) :to-equal '("discarded"))
        (expect (disk-text store temp) :to-equal "not ours")
        (expect (intent-files store) :to-equal '())))))
