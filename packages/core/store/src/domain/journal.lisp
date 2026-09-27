;;;; packages/core/store/src/domain/journal.lisp
;;;;
;;;; Journal entries (`journal/ops.jsonl`, one JSON object per line)
;;;; and the retention rule: keep the newest +RETAINED-GENERATIONS+ ops per
;;;; path, delete an op once every one of its paths has that many newer ops.
;;;; Only `before` file states reference blobs: `after` content is either on
;;;; disk or some later op's `before`.
(in-package #:aitools.store.domain)

(defconstant +retained-generations+ 20)

(defstruct (journal-entry (:copier nil))
  (op-id nil :type string :read-only t)
  (argv nil :type list :read-only t)
  (time nil :type string :read-only t)
  ;; CHANGE-RESULTs without their BEFORE-CONTENT/AFTER-CONTENT bytes.
  (changes nil :type list :read-only t)
  (undoes nil :type (or null string) :read-only t))

(defun journal-entry-paths (entry)
  "Every path the entry touched, a move's source included, deduplicated in
first-seen order."
  (let ((paths '()))
    (dolist (change (journal-entry-changes entry) (nreverse paths))
      (pushnew (change-result-path change) paths :test #'string=)
      (when (change-result-from change)
        (pushnew (change-result-from change) paths :test #'string=)))))

(defun %change->json (change)
  (apply #'json-object
         (append (list "path" (change-result-path change)
                       "action" (action-name (change-result-action change)))
                 (when (change-result-from change)
                   (list "from" (change-result-from change)))
                 (list "before" (entry-state->json (change-result-before change))
                       "after" (entry-state->json (change-result-after change)))
                 (when (change-result-source-before change)
                   (list "source_before" (entry-state->json (change-result-source-before change)))))))

(defun %json->change (object)
  (let ((path (%json-field object "path" :type 'string))
        (from (%json-field object "from" :type 'string :required nil))
        (source (%json-field object "source_before" :type 'hash-table :required nil)))
    (unless (and (valid-relative-path-p path) (or (null from) (valid-relative-path-p from)))
      (%format-error "journal change path has the wrong shape"))
    (make-change-result :path path
                        :action (parse-action-name (%json-field object "action" :type 'string))
                        :from from
                        :before (json->entry-state (%json-field object "before" :type 'hash-table))
                        :after (json->entry-state (%json-field object "after" :type 'hash-table))
                        :source-before (and source (json->entry-state source)))))

(defun journal-entry->json (entry)
  (json-object "op_id" (journal-entry-op-id entry)
                "argv" (journal-entry-argv entry)
                "time" (journal-entry-time entry)
                "changes" (mapcar #'%change->json (journal-entry-changes entry))
                "undoes" (%json-null-or (journal-entry-undoes entry))))

(defun json->journal-entry (object)
  (let ((op-id (%json-field object "op_id" :type 'string))
        (argv (%json-list object "argv"))
        (undoes (%json-field object "undoes" :type 'string :required nil)))
    (unless (valid-op-id-p op-id)
      (%format-error "journal op_id has the wrong shape"))
    (unless (every #'stringp argv)
      (%format-error "journal argv holds a non-string"))
    (when (and undoes (not (valid-op-id-p undoes)))
      (%format-error "journal undoes has the wrong shape"))
    (make-journal-entry :op-id op-id
                        :argv argv
                        :time (%json-field object "time" :type 'string)
                        :changes (mapcar (lambda (change)
                                           (unless (hash-table-p change)
                                             (%format-error "journal change is not an object"))
                                           (%json->change change))
                                         (%json-list object "changes"))
                        :undoes undoes)))

(defun encode-journal (entries)
  (with-output-to-string (out)
    (dolist (entry entries)
      (write-string (json-kit:stringify (journal-entry->json entry)) out)
      (write-char #\Newline out))))

(defun decode-journal (text)
  "Parse ops.jsonl TEXT, oldest entry first. The store replaces the file
with rename, so a line without its terminating newline can only come from
an external edit; it is dropped rather than trusted. Any complete line that
is not a valid entry signals STORE-FORMAT-ERROR."
  (loop with start = 0
        for newline = (position #\Newline text :start start)
        while newline
        unless (= start newline)
          collect (let ((object (%parse-json (subseq text start newline))))
                    (unless (hash-table-p object)
                      (%format-error "journal line is not an object"))
                    (json->journal-entry object))
        do (setf start (1+ newline))))

(defun retention-removals (entries &key (generations +retained-generations+))
  "Op ids to delete from ENTRIES (oldest first): those for which every path
already has GENERATIONS newer ops."
  (let ((per-path (make-hash-table :test 'equal))
        (kept (make-hash-table :test 'equal)))
    (loop for entry in entries
          for index from 0
          do (dolist (path (journal-entry-paths entry))
               (push index (gethash path per-path))))
    (loop for indices being the hash-values of per-path
          do (loop for index in indices
                   repeat generations
                   do (setf (gethash index kept) t)))
    (loop for entry in entries
          for index from 0
          unless (gethash index kept)
            collect (journal-entry-op-id entry))))

(defun journal-referenced-blobs (entries)
  (let ((hashes '()))
    (dolist (entry entries hashes)
      (dolist (change (journal-entry-changes entry))
        (let ((before (change-result-before change)))
          (when (eq (entry-state-kind before) :file)
            (push (entry-state-hash before) hashes)))))))
