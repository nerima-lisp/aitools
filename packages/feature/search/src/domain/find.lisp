;;;; packages/feature/search/src/domain/find.lisp
;;;;
;;;; The rules of `find`: the name-or-path pattern, the per-entry filters, the
;;;; three orderings, and `--output tree`. The application layer turns scan
;;;; entries into FOUND-ENTRY values and hands them here in path order.
(in-package #:aitools.search.domain)

(defstruct (found-entry (:constructor make-found-entry (path kind size mode mtime)) (:copier nil))
  "PATH is workspace-relative; KIND is :FILE, :DIRECTORY, :SYMLINK, or
:OTHER; SIZE is NIL for a directory whose size was not summed."
  (path "" :type string :read-only t)
  (kind :file :type keyword :read-only t)
  (size nil :type (or null (integer 0)) :read-only t)
  (mode 0 :type (integer 0) :read-only t)
  (mtime 0 :type integer :read-only t))

(defun %glob-pattern-p (pattern)
  (find-if (lambda (char) (find char "*?[")) pattern))

(defun find-pattern-matches-p (pattern path)
  "`find`: PATTERN is a glob when it holds `*`, `?`, or `[`, else a
substring. Without `/` it is matched against PATH's last component; with
`/`, against the whole workspace-relative PATH."
  (let ((subject (if (find #\/ pattern) path (aitools.workspace.domain:path-basename path))))
    (if (%glob-pattern-p pattern)
        (aitools.workspace.domain:wildmatch pattern subject :pathname (and (find #\/ pattern) t))
        (and (search pattern subject) t))))

(defun entry-depth (start path)
  "How many components PATH lies below START (both workspace-relative;
START \"\" is the root). START's own children are at depth 1."
  (- (length (aitools.workspace.domain:path-components path))
     (length (aitools.workspace.domain:path-components start))))

(defun sort-found-entries (entries order)
  "ENTRIES (already in path order) ordered by ORDER: :PATH keeps them,
:MTIME puts the newest first, :SIZE the largest first; ties keep path
order, so the result is deterministic."
  (ecase order
    (:path entries)
    (:mtime (stable-sort (copy-list entries) #'> :key #'found-entry-mtime))
    (:size (stable-sort (copy-list entries) #'> :key (lambda (entry) (or (found-entry-size entry) 0))))))

(defun kind-name (kind)
  (ecase kind
    (:file "file")
    (:directory "dir")
    (:symlink "symlink")
    (:other "other")))

(defun found-entry-json (entry)
  (json-object-from-alist (list (cons "path" (found-entry-path entry))
                     (cons "kind" (kind-name (found-entry-kind entry)))
                     (cons "size" (or (found-entry-size entry) (json-null)))
                     (cons "mode" (aitools.kernel.domain:octal-mode (found-entry-mode entry)))
                     (cons "mtime" (aitools.kernel.domain:iso8601-utc
                                    (aitools.kernel.domain:unix-seconds-to-universal-time
                                     (found-entry-mtime entry)))))))

;;; ------------------------------------------------------------ tree

(defstruct (tree-node (:constructor %make-tree-node (name kind size)) (:copier nil))
  (name "" :type string :read-only t)
  (kind :directory :type keyword :read-only t)
  (size nil :read-only t)
  (children '() :type list)
  (omitted 0 :type fixnum))

(defun %tree-json (node)
  (json-object-from-alist
   (append (list (cons "name" (tree-node-name node))
                 (cons "kind" (kind-name (tree-node-kind node))))
           (when (tree-node-size node) (list (cons "size" (tree-node-size node))))
           (when (or (tree-node-children node) (eq (tree-node-kind node) :directory))
             (list (cons "children" (mapcar #'%tree-json (reverse (tree-node-children node))))))
           (when (plusp (tree-node-omitted node))
             (list (cons "omitted" (tree-node-omitted node)))))))

(defun build-find-tree (start start-kind entries limit)
  "`find --output tree` for ENTRIES (matching entries below START, in the
scan's pre-order, so a directory precedes its contents). The first LIMIT
entries become nodes, with their ancestors below START added so the
nesting shows; each later entry counts toward the `omitted` of its nearest
node. An ancestor that did not match itself carries no size."
  (let ((root (%make-tree-node (if (string= start "") "." (aitools.workspace.domain:path-basename start))
                               start-kind nil))
        (nodes (make-hash-table :test 'equal)))
    (setf (gethash start nodes) root)
    (labels ((nearest (path)
               (loop for parent = (aitools.workspace.domain:path-parent path)
                       then (aitools.workspace.domain:path-parent parent)
                     for node = (and parent (gethash parent nodes))
                     when (or node (null parent)) return (or node root)))
             (ensure (path kind size)
               (or (gethash path nodes)
                   (let* ((parent-path (aitools.workspace.domain:path-parent path))
                          (parent (if (or (null parent-path) (string= parent-path start))
                                      root
                                      (ensure parent-path :directory nil)))
                          (node (%make-tree-node (aitools.workspace.domain:path-basename path) kind size)))
                     (push node (tree-node-children parent))
                     (setf (gethash path nodes) node)))))
      (loop for entry in entries
            for index from 0
            do (if (< index limit)
                   (ensure (found-entry-path entry) (found-entry-kind entry) (found-entry-size entry))
                   (incf (tree-node-omitted (nearest (found-entry-path entry)))))))
    (%tree-json root)))
