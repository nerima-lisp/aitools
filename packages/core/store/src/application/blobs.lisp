;;;; packages/core/store/src/application/blobs.lisp
;;;;
;;;; The content-addressed blobs, shared by the journal and every tx:
;;;; `blobs/<content-hash>`. A blob is written to a dot-prefixed temp name,
;;;; fsynced, and renamed, so a blob that exists under its hash is complete.
;;;;
;;;; Every blob writer and COLLECT-GARBAGE run under the workspace lock, so
;;;; garbage collection can never delete a blob another process has written
;;;; but not yet referenced.
(in-package #:aitools.store.application)

(defun write-blob (store octets)
  "Store OCTETS as a blob and return its content hash."
  (let* ((hash (aitools.kernel.domain:content-hash octets))
         (path (blob-file-path (store-state-directory store) hash)))
    (unless (eq (%kind-at store path) :file)
      (let ((temp-path (join-path (blobs-directory (store-state-directory store))
                                  (format nil ".~A.~A.tmp" hash (%io store random-hex 8)))))
        (with-temp-file (temp store temp-path octets :sync t)
          (%io store rename temp path))))
    hash))

(defun read-blob (store hash)
  "The bytes of blob HASH. A referenced blob that is missing or whose bytes
no longer hash to its name means the state directory was damaged, which is
reported rather than papered over."
  (let* ((path (blob-file-path (store-state-directory store) hash))
         (octets (%read-file-if-exists store path)))
    (unless (and octets (string= (aitools.kernel.domain:content-hash octets) hash))
      (error 'store-format-error :detail (format nil "blob ~A is missing or damaged" hash)))
    octets))

(defun %tx-referenced-blobs (store)
  "Blob hashes referenced by every live tx, in its index and in its ops
records, or :UNKNOWN when some tx cannot be read (then nothing may be
collected)."
  (let ((root (tx-root-directory (store-state-directory store)))
        (hashes '()))
    (dolist (name (%io store list-directory root) hashes)
      (when (valid-tx-id-p name)
        (let ((text (%read-text-if-exists store (join-path root name "index.json"))))
          (when text
            (handler-case
                (let* ((index (decode-tx-index text))
                       (ops (%read-text-if-exists
                             store (join-path root name (tx-ops-file-name (tx-index-ops-generation index))))))
                  (setf hashes (nconc (tx-index-referenced-blobs index)
                                      (tx-ops-referenced-blobs (decode-tx-ops (or ops "") (tx-index-last-tx-op index)))
                                      hashes)))
              (store-format-error () (return :unknown)))))))))

(defun collect-garbage (store &key (journal :reread))
  "Delete every blob no journal entry or tx references, blob temp
files left by a crashed writer, and tx directories a crash left half
deleted. Call with the workspace lock held. Skipped entirely when a record
cannot be read, since an unreadable record's references are unknown.

JOURNAL defaults to re-reading the retained journal; a caller that already
decoded it (the write path, through %REAP-JOURNAL) passes the retained
entries, or :UNKNOWN when a record could not be read."
  (let ((tx-hashes (%tx-referenced-blobs store))
        (journal (if (eq journal :reread)
                     (handler-case (read-journal store)
                       (store-format-error () :unknown))
                     journal)))
    (unless (or (eq tx-hashes :unknown) (eq journal :unknown))
      (let ((referenced (make-hash-table :test 'equal))
            (blobs (blobs-directory (store-state-directory store))))
        (dolist (hash (append tx-hashes (journal-referenced-blobs journal)))
          (setf (gethash hash referenced) t))
        (dolist (name (%io store list-directory blobs))
          (when (or (char= (char name 0) #\.)
                    (and (valid-blob-hash-p name) (not (gethash name referenced))))
            (%delete-tree store (join-path blobs name))))))
    (let ((root (tx-root-directory (store-state-directory store))))
      (dolist (name (%io store list-directory root))
        (when (char= (char name 0) #\.)
          (%delete-tree store (join-path root name)))))))

(defun %reap-journal (store)
  "Run after a write, under the workspace lock: compact the journal once the
file has grown well past its retained size (so appends stay O(1) while reads
and collection stay bounded), then collect garbage. Decodes the journal once
and hands the retained entries to COLLECT-GARBAGE so it need not re-decode."
  (let ((raw (handler-case (%journal-entries store)
               (store-format-error () :unknown))))
    (if (eq raw :unknown)
        (collect-garbage store :journal :unknown)
        (let ((retained (%retained-entries raw)))
          (when (> (length raw) (* 2 (max 1 (length retained))))
            (%replace-file store (journal-file-path (store-state-directory store))
                           (encode-journal retained)))
          (collect-garbage store :journal retained)))))
