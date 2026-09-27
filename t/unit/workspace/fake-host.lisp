;;;; t/unit/workspace/fake-host.lisp
;;;;
;;;; An in-memory WORKSPACE-HOST: the unit tests' fake for the workspace
;;;; port. Nodes are keyed by absolute path; parents of
;;;; every file are created implicitly. STAT and LIST-DIRECTORY report
;;;; symlinks without following them, as lstat does; READ-OCTETS follows
;;;; them, as open(2) does.
(in-package #:aitools.workspace.test)

(defun %fake-parent (path)
  (let ((slash (position #\/ path :from-end t)))
    (cond ((null slash) nil) ((zerop slash) (if (string= path "/") nil "/")) (t (subseq path 0 slash)))))

(defun make-fake-host (&key files directories symlinks environment (home "/home/user") (cwd "/"))
  "FILES: alist of (path . string-or-octets); DIRECTORIES: list of paths;
SYMLINKS: alist of (path . target); ENVIRONMENT: alist of (name . value).
Returns (VALUES host nodes), NODES being the backing hash table."
  (let ((nodes (make-hash-table :test 'equal)))
    (labels ((ensure-directory (path)
               (when (and path (not (gethash path nodes)))
                 (setf (gethash path nodes) (list :directory))
                 (ensure-directory (%fake-parent path)))))
      (ensure-directory "/")
      (dolist (directory directories) (ensure-directory directory))
      (loop for (path . content) in files
            do (ensure-directory (%fake-parent path))
               (setf (gethash path nodes)
                     (list :file (if (stringp content) (string-bytes content) content))))
      (loop for (path . target) in symlinks
            do (ensure-directory (%fake-parent path))
               (setf (gethash path nodes) (list :symlink target))))
    (labels ((entry (path)
               (let ((node (gethash path nodes)))
                 (when node
                   (make-workspace-entry
                    :name (let ((slash (position #\/ path :from-end t)))
                            (if slash (subseq path (1+ slash)) path))
                    :kind (first node)
                    :size (if (eq (first node) :file) (length (second node)) 0)
                    :mtime 1000
                    :mode #o644))))
             (follow (path depth)
               (let ((node (gethash path nodes)))
                 (if (and node (eq (first node) :symlink) (< depth 40))
                     (let ((target (second node)))
                       (follow (if (char= (char target 0) #\/)
                                   target
                                   (normalize-path (join-path (%fake-parent path) target)))
                               (1+ depth)))
                     path))))
      (values
       (make-workspace-host
        :list-directory (lambda (path)
                          (let ((node (gethash path nodes)))
                            (if (and node (eq (first node) :directory))
                                (values (loop for key being the hash-keys of nodes
                                              when (and (string/= key "/") (equal (%fake-parent key) path))
                                                collect (entry key))
                                        t)
                                (values nil nil))))
        :stat #'entry
        :read-link (lambda (path)
                     (let ((node (gethash path nodes)))
                       (and node (eq (first node) :symlink) (second node))))
        :read-octets (lambda (path)
                       (let ((node (gethash (follow path 0) nodes)))
                         (and node (eq (first node) :file) (second node))))
        :getenv (lambda (name) (cdr (assoc name environment :test #'string=)))
        :home-directory (lambda () home)
        :current-directory (lambda () cwd))
       nodes))))

(defun scan-paths (host root &rest options)
  "The root-relative paths a scan emits, in emission order, and the ignore
source, as (VALUES paths source)."
  (let ((paths '()) (source nil))
    (apply #'call-with-workspace-scan/k host root
           :emit (lambda (entry result)
                   (declare (ignore result))
                   (push (scan-entry-path entry) paths)
                   nil)
           :on-complete (lambda (ignore-source stopped)
                          (declare (ignore stopped))
                          (setf source ignore-source))
           :on-error (lambda (reason path) (fail (format nil "scan error ~A ~A" reason path)))
           options)
    (values (nreverse paths) source)))

(defun resolved-root (host &rest options)
  (apply #'call-with-resolved-root/k host
         :on-resolved #'identity
         :on-error (lambda (reason path) (fail (format nil "root error ~A ~A" reason path)))
         options))
