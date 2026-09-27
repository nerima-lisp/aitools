;;;; t/unit/search/fakes.lisp
;;;;
;;;; An in-memory filesystem behind both ports the search flows read
;;;; through: the workspace context's WORKSPACE-HOST (listing, lstat, the
;;;; ordered mapper) and the text context's TEXT-SOURCE (file bytes). Nodes
;;;; are keyed by absolute path; parents are created implicitly. Listing and
;;;; stat report symlinks without following them (lstat); reads follow them.
(in-package #:aitools.search.test)

(defun %fake-parent (path)
  (let ((slash (position #\/ path :from-end t)))
    (cond ((null slash) nil) ((zerop slash) (if (string= path "/") nil "/")) (t (subseq path 0 slash)))))

(defun %bytes (content)
  (coerce (if (stringp content) (string-bytes content) content) '(simple-array (unsigned-byte 8) (*))))

(defun make-fake-ports (&key files directories symlinks (cwd "/w") (now 100000) stdin
                          unreadable failing unlistable split-chunks (stdin-port :fake) (open-store nil))
  "FILES: list of (path content &key mtime mode), CONTENT a string or
octets; DIRECTORIES: list of paths; SYMLINKS: alist of (path . target).
STDIN: the octets or string standard input yields. Faults: a path in
UNREADABLE is listed but its content read finds nothing (a file removed
mid-scan), a path in FAILING signals FILE-ERROR when read, and a directory
in UNLISTABLE cannot be listed. SPLIT-CHUNKS reads files in the requested
chunk size, as the host source does; otherwise a file is one chunk, which
keeps the allocation specs free of the copying. STDIN-PORT replaces the standard-input
reader (NIL for none); OPEN-STORE is passed through."
  (let ((nodes (make-hash-table :test 'equal)))
    (labels ((ensure-directory (path)
               (when (and path (not (gethash path nodes)))
                 (setf (gethash path nodes) (list :directory nil 1000 #o755))
                 (ensure-directory (%fake-parent path)))))
      (ensure-directory "/")
      (dolist (directory directories) (ensure-directory directory))
      (dolist (spec files)
        (destructuring-bind (path content &key (mtime 1000) (mode #o644)) spec
          (ensure-directory (%fake-parent path))
          (setf (gethash path nodes) (list :file (%bytes content) mtime mode))))
      (loop for (path . target) in symlinks
            do (ensure-directory (%fake-parent path))
               (setf (gethash path nodes) (list :symlink target 1000 #o777))))
    (labels ((entry (path)
               (let ((node (gethash path nodes)))
                 (when node
                   (aitools.workspace.application:make-workspace-entry
                    :name (let ((slash (position #\/ path :from-end t))) (if slash (subseq path (1+ slash)) path))
                    :kind (first node)
                    :size (if (eq (first node) :file) (length (second node)) 0)
                    :mtime (third node)
                    :mode (fourth node)))))
             (follow (path depth)
               (let ((node (gethash path nodes)))
                 (if (and node (eq (first node) :symlink) (< depth 40))
                     (let ((target (second node)))
                       (follow (if (char= (char target 0) #\/)
                                   target
                                   (aitools.workspace.domain:normalize-path
                                    (aitools.workspace.domain:join-path (%fake-parent path) target)))
                               (1+ depth)))
                     path)))
             (file-octets (path)
               (let ((node (gethash (follow path 0) nodes)))
                 (and node (eq (first node) :file) (second node)))))
      (aitools.search.application:make-search-ports
       :workspace-host
       (aitools.workspace.application:make-workspace-host
        :list-directory (lambda (path)
                          (let ((node (gethash path nodes)))
                            (if (and node (eq (first node) :directory) (not (member path unlistable :test #'string=)))
                                (values (loop for key being the hash-keys of nodes
                                              when (and (string/= key "/") (equal (%fake-parent key) path))
                                                collect (entry key))
                                        t)
                                (values nil nil))))
        :stat #'entry
        :read-link (lambda (path) (let ((node (gethash path nodes))) (and node (eq (first node) :symlink) (second node))))
        :read-octets #'file-octets
        :getenv (constantly nil)
        :home-directory (constantly "/home/user")
        :current-directory (constantly cwd))
       :text-source
       (aitools.text.application:make-text-source
        :file-size (lambda (path) (let ((octets (file-octets path))) (and octets (length octets))))
        :read-prefix (lambda (path count)
                       (let ((octets (file-octets path)))
                         (and octets (subseq octets 0 (min count (length octets))))))
        :read-octets #'file-octets
        ;; Like the host source: no chunk for an empty file, and :STOP from
        ;; FUNCTION ends the read.
        :call-with-chunks (lambda (path size function)
                            (when (member path failing :test #'string=)
                              (error 'file-error :pathname path))
                            (let ((octets (and (not (member path unreadable :test #'string=)) (file-octets path))))
                              (and octets
                                   (loop with step = (if split-chunks size (max 1 (length octets)))
                                         for start from 0 below (length octets) by step
                                         until (eq (funcall function (if (and (zerop start) (>= step (length octets)))
                                                                         octets
                                                                         (subseq octets start (min (length octets) (+ start step)))))
                                                   :stop)
                                         finally (return t))))))
       :open-store open-store
       :unix-now (constantly now)
       :read-stdin-octets (if (eq stdin-port :fake)
                              (lambda (limit &key on-octets on-too-large on-failure)
                                (declare (ignore on-failure))
                                (let ((octets (%bytes (or stdin ""))))
                                  (if (> (length octets) limit)
                                      (funcall on-too-large)
                                      (funcall on-octets octets))))
                              stdin-port)))))

(defun run-flow (flow &rest arguments)
  "(VALUES KIND FIELDS) of the COMMAND-RESULT FLOW produces."
  (let ((result (aitools.protocol.application:call-with-command-result/k
                 (lambda (&rest continuations) (apply flow (append arguments continuations))))))
    (values (aitools.protocol.application:command-result-kind result)
            (aitools.protocol.application:command-result-fields result))))

(defun field (fields name &optional default)
  (let ((pair (assoc name fields :test #'string=)))
    (if pair (cdr pair) default)))

(defun jfield (object name)
  (json-alist-value object name))

(defun rendered (kind fields)
  "The JSON text of the envelope the composition root would write for a
flow's result, for byte-for-byte comparisons."
  (with-output-to-string (out)
    (aitools.protocol.infrastructure:write-envelope
     (aitools.protocol.domain:make-ok-envelope "search" fields :status (if (eq kind :partial) "partial" "ok"))
     out)))

(defun false-p (value)
  (json-kit:json-false-p value))
