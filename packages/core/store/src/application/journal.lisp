;;;; packages/core/store/src/application/journal.lisp
;;;;
;;;; Reading and appending the journal (`journal/ops.jsonl`, one JSON
;;;; object per line). A normal write appends a single line with O_APPEND, so
;;;; its cost does not grow with the journal's length. Entries outside
;;;; generation-based retention remain until compaction; an interrupted append
;;;; can leave an unterminated final line.
;;;; READ-JOURNAL returns the retained view callers expect: DECODE-JOURNAL
;;;; ignores an unterminated tail, while malformed complete lines signal
;;;; STORE-FORMAT-ERROR. RETENTION-REMOVALS drops operations outside the retained
;;;; generations for every path they touch. %REAP-JOURNAL (in blobs.lisp)
;;;; compacts when raw entries exceed twice the retained count (with one as the
;;;; minimum), keeping reads and garbage collection bounded.
;;;;
;;;; Appends follow the intent record's checksum line being fsynced, so recovery
;;;; may replay an operation after a crash.
(in-package #:aitools.store.application)

(defun %journal-entries (store)
  "Decoded journal entries, oldest first."
  (let ((text (%read-text-if-exists store (journal-file-path (store-state-directory store)))))
    (if text (decode-journal text) '())))

(defun %retained-entries (entries)
  (let ((removals (retention-removals entries)))
    (if removals
        (remove-if (lambda (entry)
                     (member (journal-entry-op-id entry) removals :test #'string=))
                   entries)
        entries)))

(defun read-journal (store)
  "Every retained journal entry, oldest first."
  (%retained-entries (%journal-entries store)))

(defun %append-journal-entry (store entry &key recovered)
  "Record ENTRY. A normal write (RECOVERED NIL) appends one line with O_APPEND,
O(1) in the journal's length -- ENTRY's op id is freshly minted, so it cannot
already be present. Recovery (RECOVERED T) may replay an op after a crash
between the append and the intent removal, so it rewrites the journal from the
lines that parse instead: idempotent by op id, and the rewrite discards any
torn tail the crash left rather than appending after it."
  (if recovered
      (%rewrite-journal store entry)
      (%append-journal-line store (string-octets (encode-journal (list entry))))))

(defun %append-journal-line (store octets)
  (let ((path (journal-file-path (store-state-directory store))))
    (if (eq (%kind-at store path) :file)
        ;; Not fsynced: fsync is limited to temp files, blobs, and intent
        ;; records, and %REPLACE-FILE kept the journal unsynced too.
        (%io store append-file path octets)
        ;; The first entry creates the file (0600, like %REPLACE-FILE's temp).
        (%io store create-file path octets))))

(defun %rewrite-journal (store entry)
  (let* ((entries (%journal-entries store))
         (present (find (journal-entry-op-id entry) entries
                        :key #'journal-entry-op-id :test #'string=))
         (all (if present entries (append entries (list entry)))))
    (%replace-file store (journal-file-path (store-state-directory store))
                   (encode-journal (%retained-entries all)))))

(defun map-journal-entries (store emit &key path)
  "Call EMIT with each JOURNAL-ENTRY, newest first (`log` order), until it
returns :STOP. With PATH, only entries that touched PATH or something
inside it (a move's source counts)."
  (declare (type function emit))
  (dolist (entry (reverse (read-journal store)))
    (when (or (null path)
              (some (lambda (touched) (path-under-p path touched)) (journal-entry-paths entry)))
      (when (eq (funcall emit entry) :stop)
        (return))))
  (values))

(defun find-journal-entry (store op-id)
  "The entry for OP-ID, or NIL (including when OP-ID is not an op id at all)."
  (and (valid-op-id-p op-id)
       (find op-id (read-journal store) :key #'journal-entry-op-id :test #'string=)))
