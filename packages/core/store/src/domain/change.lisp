;;;; packages/core/store/src/domain/change.lisp
;;;;
;;;; CHANGE-REQUEST is what a write command asks for (write these bytes,
;;;; delete, move, chmod, symlink, mkdir, set an mtime). CHANGE-RESULT is what the planner
;;;; decided it means against the current state: the write output's `action`, the
;;;; before/after ENTRY-STATEs, and the before/after bytes a caller needs to
;;;; render `diff`. CONFLICT is one entry of `conflicts[]` in a tx commit refusal.
(in-package #:aitools.store.domain)

(defstruct (change-request
            (:constructor %make-change-request (op path &key from content mode target mtime))
            (:copier nil))
  (op nil :type (member :write :delete :move :chmod :symlink :mkdir :mtime) :read-only t)
  (path nil :type string :read-only t)
  (from nil :type (or null string) :read-only t)
  (content nil :type (or null octets) :read-only t)
  (mode nil :type (or null (integer 0 #o7777)) :read-only t)
  (target nil :type (or null string) :read-only t)
  ;; Unix seconds: the modification time :MTIME sets, or a :WRITE applies to
  ;; its new file; NIL leaves the time to the filesystem.
  (mtime nil :type (or null (integer 0)) :read-only t))

(defun write-file-request (path content &key mode mtime)
  "Create or replace the regular file PATH with CONTENT. MODE defaults to the
existing file's mode, or DEFAULT-FILE-MODE for a new file. MTIME, when
given, becomes the new file's modification time."
  (%make-change-request :write path :content (coerce content 'octets) :mode mode :mtime mtime))

(defun mtime-request (path mtime &key mode)
  "Set the existing regular file PATH's modification time to MTIME (Unix
seconds), keeping its content; MODE, when given, is applied with it (a tx
that both chmods and touches a file commits both as one step)."
  (%make-change-request :mtime path :mtime mtime :mode mode))

(defun delete-request (path)
  "Remove a file, a symlink, or an empty directory."
  (%make-change-request :delete path))

(defun move-request (from to)
  (%make-change-request :move to :from from))

(defun chmod-request (path mode)
  (%make-change-request :chmod path :mode mode))

(defun symlink-request (path target)
  (%make-change-request :symlink path :target target))

(defun mkdir-request (path)
  (%make-change-request :mkdir path))

(defstruct (change-result (:copier nil))
  (path nil :type string :read-only t)
  (action nil :type (member :created :modified :deleted :moved :mode-changed :linked) :read-only t)
  (from nil :type (or null string) :read-only t)
  (before nil :type entry-state :read-only t)
  (after nil :type entry-state :read-only t)
  ;; A move's source state before the operation; NIL for every other action.
  (source-before nil :type (or null entry-state) :read-only t)
  (before-content nil :type (or null octets) :read-only t)
  (after-content nil :type (or null octets) :read-only t))

(defun change-result-hash-before (result)
  (entry-state-hash (change-result-before result)))

(defun change-result-hash-after (result)
  (entry-state-hash (change-result-after result)))

(defparameter *action-names*
  '((:created . "created") (:modified . "modified") (:deleted . "deleted")
    (:moved . "moved") (:mode-changed . "mode-changed") (:linked . "linked")))

(defun action-name (action)
  (or (cdr (assoc action *action-names*))
      (error "unknown change action ~S" action)))

(defun parse-action-name (name)
  (or (car (rassoc name *action-names* :test #'equal))
      (%format-error "unknown change action")))

(defstruct (conflict (:copier nil))
  (path nil :type string :read-only t)
  (kind nil :type (member :write :read) :read-only t)
  ;; The state the operation requires (tx `base`, the recorded read, or an
  ;; undone op's `after`) and the state found on disk.
  (base nil :type entry-state :read-only t)
  (current nil :type entry-state :read-only t))

(defun conflict->json (conflict)
  "The tx `conflicts[]` element: {path, kind, base, current}, where
BASE and CURRENT are the content hash, or null for a non-file state."
  (flet ((state-value (state)
           (%json-null-or (entry-state-hash state))))
    (json-object "path" (conflict-path conflict)
                  "kind" (string-downcase (symbol-name (conflict-kind conflict)))
                  "base" (state-value (conflict-base conflict))
                  "current" (state-value (conflict-current conflict)))))
