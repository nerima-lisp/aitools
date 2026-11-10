;;;;
;;;; An in-memory workspace boundary context. Nodes are keyed by absolute
;;;; path; parents of every file are created implicitly. STAT and
;;;; LIST-DIRECTORY report symlinks without following them, as lstat does;
;;;; READ-OCTETS follows them, as open(2) does.
(in-package #:aitools.workspace.test)

(defun %fake-parent (path)
  (let ((slash (position #\/ path :from-end t)))
    (cond ((null slash) nil) ((zerop slash) (if (string= path "/") nil "/")) (t (subseq path 0 slash)))))

(defun make-fake-workspace-boundaries
    (&key files directories symlinks environment file-identities stat-failures
          (home "/home/user") (cwd "/"))
  "Build an in-memory boundary context with optional file identities and stat failures."
  (let ((nodes (make-hash-table :test (function equal)))
        (identities (make-hash-table :test (function equal)))
        (next-inode 0))
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
    (labels ((file-identity (path)
               (or (cdr (assoc path file-identities :test (function equal)))
                   (or (gethash path identities)
                       (setf (gethash path identities)
                             (cons 1 (incf next-inode))))))
             (entry (path)
               (let ((node (gethash path nodes)))
                 (when (and node
                            (not (member path stat-failures :test (function equal))))
                   (let ((identity (file-identity path)))
                     (make-workspace-entry
                      :name (let ((slash (position (code-char 47) path :from-end t)))
                              (if slash (subseq path (1+ slash)) path))
                      :kind (first node)
                      :size (if (eq (first node) :file) (length (second node)) 0)
                      :mtime 1000
                      :mode #o644
                      :device (car identity)
                      :inode (cdr identity))))))
             (follow (path depth)
               (let ((node (gethash path nodes)))
                 (if (and node (eq (first node) :symlink) (< depth 40))
                     (let ((target (second node)))
                       (follow (if (char= (char target 0) (code-char 47))
                                   target
                                   (normalize-path (join-path (%fake-parent path) target)))
                               (1+ depth)))
                     path))))
      (values
       (make-workspace-boundaries
        :list-directory
        (lambda (path)
          (let ((node (gethash path nodes))
                (complete-p t))
            (if (and node (eq (first node) :directory))
                (let ((entries (quote ())))
                  (maphash
                   (lambda (key child-node)
                     (declare (ignore child-node))
                     (when (and (string/= key "/")
                                (equal (%fake-parent key) path))
                       (let ((child (entry key)))
                         (if child
                             (push child entries)
                           (setf complete-p nil)))))
                   nodes)
                  (values entries t complete-p))
              (values nil nil nil))))
        :stat (function entry)
        :read-link
        (lambda (path)
          (let ((node (gethash path nodes)))
            (and node (eq (first node) :symlink) (second node))))
        :read-octets
        (lambda (path)
          (let ((node (gethash (follow path 0) nodes)))
            (and node (eq (first node) :file) (second node))))
        :environment (cl-boundary-kit:make-test-environment :initial-values environment)
        :working-directory
        (cl-boundary-kit:make-working-directory :get-fn (lambda () cwd))
        :home-directory (lambda () home))
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
