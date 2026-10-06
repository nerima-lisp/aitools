;;;; packages/core/store/src/domain/intent.lisp
;;;;
;;;; The write protocol's intent record, `commit/<op_id>.json`, is two newline-terminated
;;;; lines written in two phases:
;;;;
;;;;   1. the header: op id, the apply steps (with every temp file name), the
;;;;      journal entry to append, and the tx to clean up. Written and fsynced
;;;;      BEFORE any temp file is created, so a discard can always find them.
;;;;   2. {"checksum": SHA-256 of line 1}, appended and fsynced after every
;;;;      temp file. This line completing is the commit point.
;;;;
;;;; A record is complete only with both lines and a matching checksum.
;;;;
;;;; Steps apply in phases, then path order within a phase: mkdir (parents
;;;; first), move, replace/unlink/chmod/utime, rmdir (children first). Strict path
;;;; order alone would remove a directory before its contents and could
;;;; write a destination before a move vacated it.
(in-package #:aitools.store.domain)

(defstruct (intent-step (:copier nil))
  (op nil :type (member :mkdir :move :replace :unlink :rmdir :chmod :utime) :read-only t)
  (path nil :type string :read-only t)
  (from nil :type (or null string) :read-only t)
  ;; :REPLACE only: the workspace-relative temp file renamed onto PATH, and
  ;; whether it holds a regular file or a symlink.
  (temp nil :type (or null string) :read-only t)
  (kind nil :type (member nil :file :symlink) :read-only t)
  (mode nil :type (or null (integer 0 #o7777)) :read-only t)
  (target nil :type (or null string) :read-only t)
  ;; :UTIME, and a :REPLACE of a file whose new state carries one: the
  ;; modification time in Unix seconds.
  (mtime nil :type (or null (integer 0)) :read-only t))

(defun %step-phase (step)
  (ecase (intent-step-op step)
    (:mkdir 0) (:move 1) ((:replace :unlink :chmod :utime) 2) (:rmdir 3)))

(defun %step< (a b)
  (let ((phase-a (%step-phase a)) (phase-b (%step-phase b)))
    (cond ((/= phase-a phase-b) (< phase-a phase-b))
          ((= phase-a 3) (string> (intent-step-path a) (intent-step-path b)))
          (t (string< (intent-step-path a) (intent-step-path b))))))

(defun change-keeps-content-p (result)
  "True for a `touch` of an existing file: a modified file whose content hash
is unchanged and whose new state names an mtime. It is applied in place by
a :UTIME step (and its mode with it), since rewriting the bytes would set
the very time it is meant to control."
  (let ((before (change-result-before result))
        (after (change-result-after result)))
    (and (eq (change-result-action result) :modified)
         (eq (entry-state-kind before) :file)
         (entry-state-mtime after)
         (equal (entry-state-hash before) (entry-state-hash after)))))

(defun changes->steps (op-id results directory-exists-p)
  "Return the intent steps for RESULTS in apply order. DIRECTORY-EXISTS-P
(relative path) answers for the disk as it is now, before any step runs: a
temp file goes in the target's directory, or in its nearest existing
ancestor when that directory is created by this same operation (a rename
between directories of one filesystem is still atomic, and the parent then
only appears after the commit point)."
  (declare (type function directory-exists-p))
  (let ((counter 0))
    (flet ((temp-for (path)
             (let ((directory (parent-relative-path path)))
               (loop until (or (zerop (length directory)) (funcall directory-exists-p directory))
                     do (setf directory (parent-relative-path directory)))
               (aitools.workspace.domain:join-path directory (temp-file-name op-id (incf counter))))))
      (stable-sort
       (loop for result in results
             for path = (change-result-path result)
             for after = (change-result-after result)
             collect (ecase (change-result-action result)
                       ((:created :modified :linked)
                        (ecase (entry-state-kind after)
                          (:directory (make-intent-step :op :mkdir :path path))
                          (:file (if (change-keeps-content-p result)
                                   (make-intent-step :op :utime :path path :mode (entry-state-mode after)
                                                     :mtime (entry-state-mtime after))
                                   (make-intent-step :op :replace :path path :temp (temp-for path)
                                                     :kind :file :mode (entry-state-mode after)
                                                     :mtime (entry-state-mtime after))))
                          (:symlink (make-intent-step :op :replace :path path :temp (temp-for path)
                                                      :kind :symlink :target (entry-state-target after)))))
                       (:deleted
                        (make-intent-step :op (if (eq (entry-state-kind (change-result-before result)) :directory)
                                                  :rmdir
                                                  :unlink)
                                          :path path))
                       (:moved (make-intent-step :op :move :path path :from (change-result-from result)))
                       (:mode-changed (make-intent-step :op :chmod :path path :mode (entry-state-mode after)))))
       #'%step<))))

(defun %step->json (step)
  (apply #'json-object
         (append (list "op" (string-downcase (symbol-name (intent-step-op step)))
                       "path" (intent-step-path step))
                 (when (intent-step-from step) (list "from" (intent-step-from step)))
                 (when (intent-step-temp step)
                   (list "temp" (intent-step-temp step)
                         "kind" (string-downcase (symbol-name (intent-step-kind step)))))
                 (when (intent-step-mode step) (list "mode" (intent-step-mode step)))
                 (when (intent-step-target step) (list "target" (intent-step-target step)))
                 (when (intent-step-mtime step) (list "mtime" (intent-step-mtime step))))))

(defun %json->step (object)
  (flet ((relative (key required)
           (let ((value (%json-field object key :type 'string :required required)))
             (when (and value (not (valid-relative-path-p value)))
               (%format-error "intent step path has the wrong shape"))
             value)))
    (let* ((op-name (%json-field object "op" :type 'string))
           (op (or (find op-name '(:mkdir :move :replace :unlink :rmdir :chmod :utime)
                         :key (lambda (op) (string-downcase (symbol-name op))) :test #'string=)
                   (%format-error "unknown intent step")))
           (temp (relative "temp" (eq op :replace)))
           (kind-name (%json-field object "kind" :type 'string :required (eq op :replace))))
      (when (and temp (not (temp-file-name-p (subseq temp (1+ (or (position #\/ temp :from-end t) -1))))))
        (%format-error "intent temp is not a store temp file name"))
      (make-intent-step :op op
                        :path (relative "path" t)
                        :from (relative "from" (eq op :move))
                        :temp temp
                        :kind (cond ((null kind-name) nil)
                                    ((string= kind-name "file") :file)
                                    ((string= kind-name "symlink") :symlink)
                                    (t (%format-error "unknown intent temp kind")))
                        :mode (%json-mode object :required (member op '(:chmod :utime)))
                        :target (%json-field object "target" :type 'string
                                                            :required (and (eq op :replace)
                                                                           (equal kind-name "symlink")))
                        :mtime (%json-field object "mtime" :type '(integer 0) :required (eq op :utime))))))

(defstruct (intent (:copier nil))
  (op-id nil :type string :read-only t)
  (steps nil :type list :read-only t)
  (journal-entry nil :type journal-entry :read-only t)
  (tx-id nil :type (or null string) :read-only t))

(defun encode-intent-header (intent)
  "Line 1 of the intent record, without its newline."
  (json-kit:stringify
   (json-object "op_id" (intent-op-id intent)
                 "tx" (%json-null-or (intent-tx-id intent))
                 "steps" (mapcar #'%step->json (intent-steps intent))
                 "journal" (journal-entry->json (intent-journal-entry intent)))))

(defun encode-intent-checksum (header)
  "Line 2 of the intent record, without its newline."
  (json-kit:stringify (json-object "checksum" (content-hash (string-octets header)))))

(defun %decode-header (line)
  (let ((object (%parse-json line)))
    (let ((op-id (%json-field object "op_id" :type 'string))
          (tx-id (%json-field object "tx" :type 'string :required nil)))
      (unless (valid-op-id-p op-id)
        (%format-error "intent op_id has the wrong shape"))
      (when (and tx-id (not (valid-tx-id-p tx-id)))
        (%format-error "intent tx has the wrong shape"))
      (make-intent :op-id op-id
                   :tx-id tx-id
                   :steps (mapcar (lambda (step)
                                    (unless (hash-table-p step)
                                      (%format-error "intent step is not an object"))
                                    (%json->step step))
                                  (%json-list object "steps"))
                   :journal-entry (json->journal-entry (%json-field object "journal" :type 'hash-table))))))

(defun decode-intent (text)
  "Classify an intent record's TEXT. Returns (values :COMPLETE intent) when
both lines are present, newline-terminated, and the checksum matches;
otherwise (values :INCOMPLETE header), where HEADER is the parsed line 1
when it is itself complete and valid, else NIL (no temp file was created
before line 1 was durable, so there is nothing to find)."
  (let* ((first-newline (position #\Newline text))
         (header-line (and first-newline (subseq text 0 first-newline)))
         (header (and header-line
                      (handler-case (%decode-header header-line)
                        (store-format-error () nil)))))
    (unless header
      (return-from decode-intent (values :incomplete nil)))
    (let ((second-newline (position #\Newline text :start (1+ first-newline))))
      (if (and second-newline
               (= second-newline (1- (length text)))
               (string= (subseq text (1+ first-newline) second-newline)
                        (encode-intent-checksum header-line)))
          (values :complete header)
          (values :incomplete header)))))
