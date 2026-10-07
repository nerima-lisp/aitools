;;;; packages/core/workspace/src/application/scan.lisp
;;;;
;;;; The ignore-aware workspace walk, shared by search, find, overview,
;;;; snapshot, and archive create. Ignore decisions happen during the walk,
;;;; so an ignored directory is never listed. Entries are emitted
;;;; in path order (see ENTRY-ORDER-KEY); optional per-file WORK runs on the
;;;; host's ordered mapper in batches, and results are still delivered in
;;;; path order, so parallelism never changes the output.
;;;;
;;;; A tx overlay (docs/src/reference/transactions.md) plugs in through WORKSPACE-OVERLAY: it
;;;; can add, remove, or replace directory entries and override the bytes of
;;;; any file the walk reads itself (.gitignore), so staged .gitignore edits
;;;; take effect in scans.
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

;;; ------------------------------------------------------------ helpers

(defun %root-relative (context top-relative)
  "TOP-RELATIVE re-expressed relative to the workspace root, or NIL when it
lies above the root (between the repository top and the workspace root)."
  (let ((prefix (ignore-context-prefix context)))
    (cond ((zerop (length prefix)) top-relative)
          ((string= top-relative prefix) "")
          ((and (> (length top-relative) (length prefix))
                (string= prefix top-relative :end2 (length prefix))
                (char= (char top-relative (length prefix)) #\/))
           (subseq top-relative (1+ (length prefix))))
          (t nil))))

(defun %list-entries (host overlay absolute root-relative)
  "(VALUES sorted-entries readable-p) for one directory, overlay applied."
  (multiple-value-bind (entries readable) (host-list-directory host absolute)
    (let ((entries (if readable entries '()))
          (hook (and overlay root-relative (workspace-overlay-list-directory overlay))))
      (when hook
        (setf entries (funcall hook root-relative entries)))
      (values (sort-entries entries) (or readable (and hook t))))))

(defun %read-file-octets (host overlay absolute root-relative)
  (let ((hook (and overlay root-relative (workspace-overlay-read-octets overlay))))
    (multiple-value-bind (override-p octets) (if hook (funcall hook root-relative) (values nil nil))
      (if override-p octets (host-read-octets host absolute)))))

(defun %directory-ignore-list (host context overlay directory-top-relative entry)
  "The IGNORE-LIST of DIRECTORY's .gitignore given its listing ENTRY (or
NIL). A symlinked .gitignore is not read, as git refuses to follow it."
  (when (and entry (eq (workspace-entry-kind entry) :file) (eq (ignore-context-source context) :gitignore))
    (let* ((file-top-relative (join-path directory-top-relative ".gitignore"))
           (octets (%read-file-octets host overlay
                                      (join-path (ignore-context-top context) file-top-relative)
                                      (%root-relative context file-top-relative))))
      (and octets (parse-ignore-octets octets :base directory-top-relative :source file-top-relative)))))

(defun %gitignore-entry (host context overlay directory-top-relative)
  "The .gitignore listing entry of a directory the walk has not listed."
  (let* ((root-relative (%root-relative context directory-top-relative))
         (absolute (join-path (ignore-context-top context) directory-top-relative)))
    (find ".gitignore" (%list-entries host overlay absolute root-relative)
          :key #'workspace-entry-name :test #'string=)))

(defun %tracked-p (context top-relative directory-p)
  (let ((tracked (ignore-context-tracked context)))
    (and tracked
         (if directory-p
             (sorted-paths-have-prefix-p tracked (concatenate 'string top-relative "/"))
             (sorted-paths-contains-p tracked top-relative)))))

(defun %ignored-p (context stack top-relative directory-p)
  (and stack
       (eq :ignored (ignore-stack-verdict stack top-relative directory-p
                                          :casefold (ignore-context-casefold context)))))

(defun %always-skipped-name-p (name)
  (or (git-metadata-name-p name) (aitools-temporary-name-p name)))

(defun %stack-above (host context overlay top-relative)
  "The ignore stack in force for entries of the directory TOP-RELATIVE,
excluding that directory's own .gitignore: every ancestor's .gitignore from
the repository top down, then the base stack."
  (let ((stack (ignore-context-base-stack context))
        (directory ""))
    (when (eq (ignore-context-source context) :gitignore)
      (dolist (component (path-components top-relative))
        (let ((list (%directory-ignore-list host context overlay directory
                                            (%gitignore-entry host context overlay directory))))
          (when list (push list stack)))
        (setf directory (join-path directory component))))
    stack))

(defun %relative-start (host root path)
  "PATH (absolute, or relative to the root) as a normalized root-relative
path, or NIL when it is outside the root. An absolute PATH maps through
WORKSPACE-RELATIVE-PATH, so a start typed from a working directory reached
through the root's real path is inside."
  (if (absolute-path-p path)
      (workspace-relative-path host root (normalize-path path))
      (let ((normalized (normalize-path (join-path (workspace-root-path root) path))))
        (and (path-inside-p (workspace-root-path root) normalized)
             (path-relative-to (workspace-root-path root) normalized)))))

(defun %starts (host root paths)
  "(VALUES starts bad-path): sorted root-relative starting points with any
start nested below another removed, or the first path outside the root."
  (let ((relatives '()))
    (dolist (path (or paths '("")))
      (let ((relative (%relative-start host root path)))
        (unless relative (return-from %starts (values nil path)))
        (push relative relatives)))
    (let ((sorted (sort (remove-duplicates relatives :test #'string=) #'string<)))
      (values (remove-if (lambda (start)
                           (some (lambda (other)
                                   (and (string/= other start)
                                        (or (string= other "")
                                            (and (> (length start) (length other))
                                                 (string= other start :end2 (length other))
                                                 (char= (char start (length other)) #\/)))))
                                 sorted))
                         sorted)
              nil))))

(defun %lookup-entry (host context overlay root-relative)
  "The WORKSPACE-ENTRY for ROOT-RELATIVE as its parent's listing shows it
(overlay applied), a synthetic directory entry for the root, or NIL."
  (if (string= root-relative "")
      (make-workspace-entry :name "" :kind :directory)
      (let* ((parent (or (path-parent root-relative) ""))
             (absolute (join-path (ignore-context-root-path context) parent)))
        (find (path-basename root-relative) (%list-entries host overlay absolute parent)
              :key #'workspace-entry-name :test #'string=))))

;;; ------------------------------------------------------------ the scan

(defun scan-filter-reason (entry path explicit filter lang skip-larger-than newer)
  "Return the first filter reason for ENTRY, or NIL when it is accepted."
  (cond
    ((and skip-larger-than (eq (workspace-entry-kind entry) :file)
          (> (workspace-entry-size entry) skip-larger-than))
     :too-large)
    ((and newer (<= (workspace-entry-mtime entry) newer))
     :too-old)
    ((and (not explicit) lang
          (not (and (eq (workspace-entry-kind entry) :file) (funcall lang path))))
     :language)
    ((and (not explicit) (not (glob-filter-accepts-p filter path)))
     :glob)))

(defun call-with-workspace-scan/k (host root &key paths glob lang no-ignore
                                                  (skip-larger-than +default-skip-larger-than+)
                                                  newer overlay work emit on-skip on-complete on-error)
  "Walk the workspace below ROOT (a WORKSPACE-ROOT) and call EMIT for each
entry that survives ignore rules and filters, in path order.

PATHS: starting points (absolute or root-relative); default the whole root.
  A start that names a file is emitted even when ignored (it was asked for);
  GLOB and LANG do not apply to it.
GLOB: list of glob strings (see MAKE-GLOB-FILTER).
LANG: a predicate on the root-relative path (the text context's
  LANGUAGE-PATH-PREDICATE); when given, only files it accepts are emitted.
NO-IGNORE: disable .gitignore and builtin excludes. `.git` and
  `.aitools-*.tmp` entries are skipped regardless.
SKIP-LARGER-THAN: files above this many bytes are reported to ON-SKIP with
  reason :TOO-LARGE instead of being emitted; NIL disables the limit.
NEWER: Unix seconds; only entries with a later mtime are emitted.
OVERLAY: a WORKSPACE-OVERLAY, or NIL.
WORK: a function of one SCAN-ENTRY run on the host's ordered mapper, or NIL.
EMIT (scan-entry result): RESULT is WORK's value (NIL without WORK).
  Returning :STOP ends the scan.
ON-SKIP (scan-entry reason): reason :TOO-LARGE or :UNREADABLE.
ON-COMPLETE (ignore-source stopped-p): the single normal exit;
  IGNORE-SOURCE is :GITIGNORE, :BUILTIN, or :NONE.
ON-ERROR (reason path): reason :OUTSIDE-ROOT or :NOT-FOUND for a start."
  (declare (type function emit on-complete on-error))
  (let* ((context (load-ignore-context host root :no-ignore no-ignore))
         (source (ignore-context-source context))
         (filter (make-glob-filter glob :casefold (ignore-context-casefold context)))
         (on-skip (or on-skip (lambda (entry reason) (declare (ignore entry reason)) nil))))
    (multiple-value-bind (starts bad-path) (%starts host root paths)
      (when bad-path
        (return-from call-with-workspace-scan/k (funcall on-error :outside-root bad-path)))
      (let ((start-entries
              (loop for start in starts
                    for entry = (%lookup-entry host context overlay start)
                    unless entry
                      do (return-from call-with-workspace-scan/k (funcall on-error :not-found start))
                    collect (cons start entry))))
        (block scan
          (labels
              ((run (mapper)
                 (let ((batch '()) (count 0))
                   (labels
                       ((deliver (candidate result)
                          (when (eq (funcall emit candidate result) :stop)
                            (return-from scan (funcall on-complete source t))))
                        (flush ()
                          (when batch
                            (let ((items (nreverse batch)))
                              (setf batch '() count 0)
                              (loop for candidate in items
                                    for result in (funcall mapper work items)
                                    do (deliver candidate result)))))
                        (offer (candidate explicit)
                          (let ((entry (scan-entry-entry candidate))
                                (path (scan-entry-path candidate)))
                            (let ((reason (scan-filter-reason entry path explicit filter lang
                                                              skip-larger-than newer)))
                              (if reason
                                  (when (eq reason :too-large)
                                    (funcall on-skip candidate :too-large))
                                  (if work
                                      (progn
                                        (push candidate batch)
                                        (when (>= (incf count) *scan-batch-size*) (flush)))
                                      (deliver candidate nil))))))
                        (walk (absolute root-relative top-relative stack inside-ignored)
                          (multiple-value-bind (entries readable)
                              (%list-entries host overlay absolute root-relative)
                            (unless readable
                              (funcall on-skip
                                       (%make-scan-entry root-relative absolute
                                                         (make-workspace-entry :name (path-basename root-relative)
                                                                               :kind :directory)
                                                         nil)
                                       :unreadable))
                            (let ((own (and (not inside-ignored)
                                            (%directory-ignore-list
                                             host context overlay top-relative
                                             (find ".gitignore" entries :key #'workspace-entry-name
                                                                        :test #'string=)))))
                              (when own (push own stack)))
                            (dolist (entry entries)
                              (let ((name (workspace-entry-name entry)))
                                (unless (%always-skipped-name-p name)
                                  (let* ((directory-p (eq (workspace-entry-kind entry) :directory))
                                         (child-top (join-path top-relative name))
                                         (child-root (join-path root-relative name))
                                         (child-absolute (join-path absolute name))
                                         (tracked (%tracked-p context child-top directory-p))
                                         (ignored (or inside-ignored
                                                      (%ignored-p context stack child-top directory-p))))
                                    (cond
                                      (directory-p
                                       (unless (and ignored (not tracked))
                                         (unless ignored
                                           (offer (%make-scan-entry child-root child-absolute entry nil) nil))
                                         (walk child-absolute child-root child-top stack ignored)))
                                      ((or (not ignored) tracked)
                                       (offer (%make-scan-entry child-root child-absolute entry tracked)
                                              nil))))))))))
                     (loop for (start . entry) in start-entries
                           for top-relative = (join-path (ignore-context-prefix context) start)
                           for absolute = (join-path (workspace-root-path root) start)
                           do (if (eq (workspace-entry-kind entry) :directory)
                                  (walk absolute start top-relative
                                        (%stack-above host context overlay top-relative) nil)
                                  (offer (%make-scan-entry start absolute entry
                                                           (%tracked-p context top-relative nil))
                                         t)))
                     (flush)
                     (funcall on-complete source nil)))))
            (declare (dynamic-extent #'run))
            (if work
                (host-call-with-ordered-mapper host #'run)
                (run nil))))))))

(defun workspace-path-ignored-p (host root path &key no-ignore overlay)
  "(VALUES IGNORED-P IGNORE-SOURCE) for `info`'s `ignored` field. PATH is
absolute or root-relative; a path outside the root is never ignored. An
ancestor directory being ignored (and not holding tracked files) makes PATH
ignored, as git would never list it."
  (let* ((context (load-ignore-context host root :no-ignore no-ignore))
         (source (ignore-context-source context))
         (relative (%relative-start host root path)))
    (cond
      ((null relative) (values nil source))
      ((some #'%always-skipped-name-p (path-components relative)) (values t source))
      ((eq source :none) (values nil source))
      (t
       (let* ((top-relative (join-path (ignore-context-prefix context) relative))
              (components (path-components top-relative))
              (stack (ignore-context-base-stack context))
              (directory ""))
         (loop for (component . rest) on components
               for child = (join-path directory component)
               do (let ((list (%directory-ignore-list host context overlay directory
                                                      (%gitignore-entry host context overlay directory))))
                    (when list (push list stack)))
                  (let* ((directory-p (if rest
                                          t
                                          (let ((entry (%lookup-entry host context overlay relative)))
                                            (and entry (eq (workspace-entry-kind entry) :directory)))))
                         (tracked (%tracked-p context child directory-p)))
                    (when (and (%ignored-p context stack child directory-p) (not tracked))
                      (return-from workspace-path-ignored-p (values t source))))
                  (setf directory child))
         (values nil source))))))
