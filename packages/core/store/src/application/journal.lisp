;;;; packages/core/store/src/application/journal.lisp
;;;;
;;;; Reading and appending the journal (`journal/ops.jsonl`, one JSON
;;;; object per line). A normal write appends a single line with O_APPEND, so
;;;; its cost does not grow with the journal's length. The file therefore keeps
;;;; the whole append history, including ops retention has since aged out and,
;;;; after a crash mid-append, a torn final line; READ-JOURNAL is what turns
;;;; that back into the retained view every caller expects: DECODE-JOURNAL
;;;; keeps only the lines that parse (dropping a torn tail) and
;;;; RETENTION-REMOVALS drops the aged-out ops. %REAP-JOURNAL (in blobs.lisp)
;;;; compacts the file down to its retained lines once it has grown well past
;;;; that size, so reads and garbage collection stay bounded.
;;;;
;;;; Appends happen after an op's commit point, so recovery may replay the same
;;;; op. Its replay path (:RECOVERED) rewrites the file rather than appending:
;;;; the rewrite is idempotent by op id (a re-run does not double-append) and
;;;; discards any torn tail the crash left, so it cannot merge with a new line.
(in-package #:aitools.store.application)

(defun %journal-entries (store)
  "The entries physically present in the journal file, oldest first.
DECODE-JOURNAL drops a torn final line, so this is exactly the lines that
parse."
  (let ((text (%read-text-if-exists store (journal-file-path (store-state-directory store)))))
    (if text (decode-journal text) '())))

(defun %retained-entries (entries)
  "ENTRIES with the ops retention has aged out removed, order preserved."
  (let ((removals (retention-removals entries)))
    (if removals
        (remove-if (lambda (entry)
                     (member (journal-entry-op-id entry) removals :test #'string=))
                   entries)
        entries)))

(defun read-journal (store)
  "Every retained journal entry, oldest first: the file's parsable lines with
retention applied. The file is appended to per op and compacted only
occasionally, so it can hold aged-out ops and a torn tail that this filters
away."
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
