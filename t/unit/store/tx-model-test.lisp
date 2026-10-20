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

  (it-property "tx metadata JSON round trip preserves generated names"
      ((name (gen-string :min-length 0 :max-length 40 :alphabet "abcXYZ0123 /é✓")))
    (let* ((meta (make-tx-meta :id "tx-20261020T120000Z-00000000"
                               :name name
                               :created "2026-10-20T12:00:00Z"))
           (decoded (decode-tx-meta (encode-tx-meta meta))))
      (expect (tx-meta-id decoded) :to-equal (tx-meta-id meta))
      (expect (tx-meta-name decoded) :to-equal name)
      (expect (tx-meta-created decoded) :to-equal (tx-meta-created meta))))
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
