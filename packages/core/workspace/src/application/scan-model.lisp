(in-package #:aitools.workspace.application)

(defconstant +default-skip-larger-than+ (* 10 1024 1024)
  "The default `--skip-larger-than` (10MiB).")

(defparameter *scan-batch-size* 64
  "Entries handed to the ordered mapper at once when WORK is supplied.")

(defstruct (workspace-overlay (:copier nil))
  "LIST-DIRECTORY (relative-directory disk-entries) returns the entries the
walk should see instead of DISK-ENTRIES; READ-OCTETS (relative-path) returns
(VALUES OVERRIDE-P OCTETS), OCTETS being NIL for a staged deletion. Paths are
workspace-root-relative, \"\" naming the root. Either slot may be NIL."
  (list-directory nil :type (or null function) :read-only t)
  (read-octets nil :type (or null function) :read-only t))

(defstruct (scan-entry (:constructor %make-scan-entry (path absolute entry tracked-p)) (:copier nil))
  "PATH is workspace-root-relative; ABSOLUTE is lexical (below the root's
path). TRACKED-P is true when the git index lists the path."
  (path "" :type string :read-only t)
  (absolute "" :type string :read-only t)
  (entry nil :type workspace-entry :read-only t)
  (tracked-p nil :type boolean :read-only t))

(defun scan-entry-name (entry) (workspace-entry-name (scan-entry-entry entry)))

(defun scan-entry-kind (entry) (workspace-entry-kind (scan-entry-entry entry)))

(defun scan-entry-size (entry) (workspace-entry-size (scan-entry-entry entry)))

(defun scan-entry-mtime (entry) (workspace-entry-mtime (scan-entry-entry entry)))

(defun scan-entry-mode (entry) (workspace-entry-mode (scan-entry-entry entry)))
