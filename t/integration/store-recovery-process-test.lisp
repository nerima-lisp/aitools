(in-package #:aitools.store.test)

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
             (failing (aitools.store.application::copy-store-io
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
