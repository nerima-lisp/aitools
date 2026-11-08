(in-package #:aitools.store.test)

(describe "aitools.store.domain tx model"
  (it "restores previous staged states newest first on drop"
    (let ((index (make-tx-index :last-tx-op 2))
          (a0 (file-state (make-string 64 :initial-element #\a) #o644))
          (a1 (file-state (make-string 64 :initial-element #\b) #o644))
          (a2 (file-state (make-string 64 :initial-element #\c) #o644)))
      (tx-index-put index "a" a0 a2)
      (tx-index-put index "b" (absent-state) (directory-state))
      (let* ((ops (list (make-tx-op-record :tx-op 1 :argv '() :paths '("a") :previous (list (cons "a" nil))
                                           :after (list (cons "a" a1)) :time "t")
                        (make-tx-op-record :tx-op 2 :argv '() :paths '("a" "b")
                                           :previous (list (cons "a" a1) (cons "b" nil))
                                           :after (list (cons "a" a2) (cons "b" (directory-state))) :time "t")))
             (dropped-2 (tx-drop-index index ops 2))
             (dropped-1 (tx-drop-index index ops 1)))
        (expect (entry-state-equal (tx-path-staged (tx-index-find dropped-2 "a")) a1) :to-be-truthy)
        (expect (tx-index-find dropped-2 "b") :to-be nil)
        (expect (tx-index-last-tx-op dropped-2) :to-be 1)
        (expect (hash-table-count (tx-index-paths dropped-1)) :to-be 0)
        (expect (entry-state-equal (tx-path-staged (tx-index-find index "a")) a2) :to-be-truthy))))

  (it "reports write conflicts before read conflicts, once per path"
    (let ((index (make-tx-index))
          (base (file-state (make-string 64 :initial-element #\a) #o644))
          (moved (file-state (make-string 64 :initial-element #\f) #o644)))
      (tx-index-put index "w" base (absent-state))
      (let ((conflicts (tx-commit-conflicts index (list (cons "w" base) (cons "r" base))
                                            (lambda (path) (declare (ignore path)) moved))))
        (expect (mapcar (lambda (c) (list (conflict-path c) (conflict-kind c))) conflicts)
                :to-equal '(("w" :write) ("r" :read))))
      (expect (length (tx-commit-conflicts index (list (cons "r" base))
                                           (lambda (path) (declare (ignore path)) moved)
                                           :ignore-stale-reads t))
              :to-be 1)))

  (it-property "tx record JSON round trips preserve generated strings"
      ((name (gen-string :min-length 0 :max-length 40 :alphabet "abcXYZ0123 /é✓"))
       (path (gen-string :min-length 1 :max-length 30 :alphabet "abcXYZ0123_-")))
    (let* ((path (format nil "file-~A" path))
           (state (file-state (make-string 64 :initial-element (char "a" 0)) #o644))
           (meta (make-tx-meta :id "tx-20261020T120000Z-00000000"
                               :name name
                               :created "2026-10-20T12:00:00Z"))
           (index (let ((index (make-tx-index :last-tx-op 1 :ops-generation 2)))
                    (tx-index-put index path state (absent-state))
                    index))
           (op (make-tx-op-record :tx-op 1 :argv (list name) :paths (list path)
                                  :previous (list (cons path state))
                                  :after (list (cons path (absent-state)))
                                  :time "2026-10-20T12:00:00Z"))
           (index-text (encode-tx-index index))
           (ops-text (encode-tx-ops (list op)))
           (reads-text (encode-tx-reads (list (cons path state)))))
      (expect (encode-tx-meta (decode-tx-meta (encode-tx-meta meta)))
              :to-equal (encode-tx-meta meta))
      (expect (encode-tx-index (decode-tx-index index-text)) :to-equal index-text)
      (expect (encode-tx-ops (decode-tx-ops ops-text 1)) :to-equal ops-text)
      (expect (encode-tx-reads (decode-tx-reads reads-text)) :to-equal reads-text)))
  (it "collects base and staged file hashes while ignoring other state kinds"
    (let ((index (make-tx-index))
          (base-hash (make-string 64 :initial-element #\a))
          (staged-hash (make-string 64 :initial-element #\b)))
      (tx-index-put index "file.txt"
                    (file-state base-hash #o644)
                    (file-state staged-hash #o600))
      (tx-index-put index "directory-to-absent"
                    (directory-state) (absent-state))
      (tx-index-put index "absent-to-directory"
                    (absent-state) (directory-state))
      (let ((hashes (tx-index-referenced-blobs index)))
        (expect (length hashes) :to-be 2)
        (expect (member base-hash hashes :test (function string=)) :to-be-truthy)
        (expect (member staged-hash hashes :test (function string=)) :to-be-truthy)))))
(describe "aitools.store archive symlink prefix checker"
  (it "checks disk links, parent boundaries, and idempotent descriptor cleanup"
    (call-with-temp-store
     (lambda (store)
       (sb-posix:mkdir (disk-path store "base") #o755)
       (sb-posix:mkdir (disk-path store "base/nested") #o755)
       (sb-posix:symlink "target" (disk-path store "base/link"))
       (let* ((io (store-io-port store))
              (opened 0)
              (closed 0)
              (tracked-io
                (copy-store-io
                 io
                 :open-search-root
                 (lambda (path)
                   (multiple-value-bind (fd identity)
                       (funcall (store-io-open-search-root io) path)
                     (incf opened)
                     (values fd identity)))
                 :open-search-entry
                 (lambda (dirfd name path)
                   (multiple-value-bind (kind fd identity)
                       (funcall (store-io-open-search-entry io) dirfd name path)
                     (when fd (incf opened))
                     (values kind fd identity)))
                 :close-search-directory
                 (lambda (fd)
                   (incf closed)
                   (funcall (store-io-close-search-directory io) fd))))
              (tracked-store (make-store tracked-io (store-root store) "/tmp"))
              (checker (make-archive-symlink-prefix-checker
                        (disk-view tracked-store) "base" nil)))
         (unwind-protect
              (progn
                (let ((cursor (funcall checker :start)))
                  (unwind-protect
                       (progn
                         (multiple-value-bind (next result)
                             (funcall checker :enter cursor "nested")
                           (setf cursor next)
                           (expect result :to-be :clear))
                         (multiple-value-bind (next result)
                             (funcall checker :parent cursor)
                           (setf cursor next)
                           (expect result :to-be :clear))
                         (multiple-value-bind (next result)
                             (funcall checker :parent cursor)
                           (setf cursor next)
                           (expect result :to-be :clear))
                         (multiple-value-bind (next result)
                             (funcall checker :parent cursor)
                           (setf cursor next)
                           (expect result :to-be :outside)))
                    (funcall checker :finish cursor)
                    (funcall checker :finish cursor)))
                (let ((cursor (funcall checker :start)))
                  (unwind-protect
                       (multiple-value-bind (next result)
                           (funcall checker :enter cursor "link")
                         (setf cursor next)
                         (expect result :to-be :symlink))
                    (funcall checker :finish cursor)
                    (funcall checker :finish cursor)))
                (funcall checker :finish)
                (funcall checker :finish)
                (expect opened :to-be closed))
           (funcall checker :finish)))))))
