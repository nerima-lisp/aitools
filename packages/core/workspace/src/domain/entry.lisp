;;;; packages/core/workspace/src/domain/entry.lisp
;;;;
;;;; One directory entry as the WORKSPACE-HOST port reports it (lstat
;;;; semantics: a symlink is reported as :SYMLINK, never as its target), and
;;;; the sibling order that makes a depth-first walk emit paths in exactly
;;;; the order a plain string sort of the full relative paths would give.
(in-package #:aitools.workspace.domain)

(defstruct (workspace-entry (:copier nil))
  "KIND is :FILE, :DIRECTORY, :SYMLINK, or :OTHER. MTIME is whole seconds
since the Unix epoch. MODE holds the permission bits only."
  (name "" :type simple-string :read-only t)
  (kind :file :type (member :file :directory :symlink :other) :read-only t)
  (size 0 :type (integer 0) :read-only t)
  (mtime 0 :type integer :read-only t)
  (mode 0 :type (integer 0 #o7777) :read-only t))

(defun entry-order-key (entry)
  "A directory sorts as NAME/: every path below it starts with that prefix,
so ordering siblings by this key and descending depth-first yields the full
relative paths in code-point order (which is UTF-8 byte order, the order git
itself uses)."
  (if (eq (workspace-entry-kind entry) :directory)
      (concatenate 'string (workspace-entry-name entry) "/")
      (workspace-entry-name entry)))

(defun sort-entries (entries)
  "A fresh list of ENTRIES in walk order (see ENTRY-ORDER-KEY)."
  (sort (copy-list entries) #'string< :key #'entry-order-key))
