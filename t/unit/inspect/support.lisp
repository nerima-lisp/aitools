;;;; t/unit/inspect/support.lisp
;;;;
;;;; Fakes for inspect flow tests (unit tests inject fake ports): an in-memory filesystem behind both the WORKSPACE-HOST and the
;;;; TEXT-SOURCE ports, and a flow runner returning the command result.
(in-package #:aitools.inspect.test)

(defun %parent (path)
  (let ((slash (position #\/ path :from-end t)))
    (cond ((null slash) nil) ((zerop slash) (if (string= path "/") nil "/")) (t (subseq path 0 slash)))))

(defun %basename (path)
  (subseq path (1+ (or (position #\/ path :from-end t) -1))))

(defun %as-octets (content)
  (if (stringp content)
      (coerce (string-bytes content) '(simple-array (unsigned-byte 8) (*)))
      (coerce content '(simple-array (unsigned-byte 8) (*)))))

(defun make-fake-filesystem (&key files directories symlinks unreadable)
  "A hash table path -> (:file octets mode unreadable-p) | (:directory) |
(:symlink target). FILES and UNREADABLE are alists of (absolute-path .
string-or-octets); parents are made. An UNREADABLE file exists (stat
succeeds) but its bytes cannot be read (EACCES), so the source reports
:UNREADABLE (contract-F1)."
  (let ((nodes (make-hash-table :test 'equal)))
    (labels ((ensure-directory (path)
               (when (and path (not (gethash path nodes)))
                 (setf (gethash path nodes) (list :directory))
                 (ensure-directory (%parent path)))))
      (ensure-directory "/")
      (dolist (directory directories) (ensure-directory directory))
      (loop for (path . content) in files
            do (ensure-directory (%parent path))
               (setf (gethash path nodes) (list :file (%as-octets content) #o644 nil)))
      (loop for (path . content) in unreadable
            do (ensure-directory (%parent path))
               (setf (gethash path nodes) (list :file (%as-octets content) #o000 t)))
      (loop for (path . target) in symlinks
            do (ensure-directory (%parent path))
               (setf (gethash path nodes) (list :symlink target))))
    nodes))

(defun %follow (nodes path)
  (loop repeat 40
        for node = (gethash path nodes)
        while (and node (eq (first node) :symlink))
        do (let ((target (second node)))
             (setf path (if (char= (char target 0) #\/)
                            target
                            (aitools.workspace.domain:normalize-path
                             (aitools.workspace.domain:join-path (%parent path) target)))))
        finally (return path)))

(defun %entry (nodes path)
  (let ((node (gethash path nodes)))
    (when node
      (aitools.workspace.application:make-workspace-entry
       :name (coerce (%basename path) 'simple-string)
       :kind (first node)
       :size (if (eq (first node) :file) (length (second node)) 0)
       :mtime 1700000000
       :mode (if (eq (first node) :file) (third node) #o755)))))

(defun make-fake-host (nodes &key (cwd "/work"))
  (aitools.workspace.application:make-workspace-host
   :list-directory (lambda (path)
                     (let ((node (gethash path nodes)))
                       (if (and node (eq (first node) :directory))
                           (values (loop for key being the hash-keys of nodes
                                         when (and (string/= key "/") (equal (%parent key) path))
                                           collect (%entry nodes key))
                                   t)
                           (values nil nil))))
   :stat (lambda (path) (%entry nodes path))
   :read-link (lambda (path) (let ((node (gethash path nodes))) (and node (eq (first node) :symlink) (second node))))
   :read-octets (lambda (path) (let ((node (gethash (%follow nodes path) nodes)))
                                 (and node (eq (first node) :file) (second node))))
   :getenv (constantly nil)
   :home-directory (constantly "/home/user")
   :current-directory (constantly cwd)))

(defun make-fake-source (nodes)
  (flet ((file (path)
           "(VALUES octets problem): PROBLEM is :UNREADABLE for a file denied
by permission (EACCES), NIL for a missing file (contract-F1)."
           (let ((node (gethash (%follow nodes path) nodes)))
             (cond ((or (null node) (not (eq (first node) :file))) (values nil nil))
                   ((fourth node) (values nil :unreadable))
                   (t (values (second node) nil))))))
    (aitools.text.application:make-text-source
     :file-size (lambda (path)
                  (multiple-value-bind (octets problem) (file path)
                    (if octets (length octets) (values nil problem))))
     :read-prefix (lambda (path count)
                    (multiple-value-bind (octets problem) (file path)
                      (if octets (subseq octets 0 (min count (length octets))) (values nil problem))))
     :read-octets #'file
     :call-with-chunks (lambda (path size function)
                         (let ((octets (file path)))
                           (when octets
                             (loop for start from 0 below (length octets) by size
                                   until (eq (funcall function (subseq octets start (min (length octets) (+ start size))))
                                             :stop))
                             t))))))

(defun make-test-ports (&key files directories symlinks unreadable (cwd "/work") open-store state-directory)
  "INSPECT-PORTS over an in-memory filesystem rooted at CWD (a directory
created implicitly). OPEN-STORE fails the test unless supplied. UNREADABLE
files exist but deny their bytes (EACCES)."
  (let ((nodes (make-fake-filesystem :files files :directories (cons cwd directories) :symlinks symlinks
                                     :unreadable unreadable)))
    (values (make-inspect-ports :workspace-host (make-fake-host nodes :cwd cwd)
                                :text-source (make-fake-source nodes)
                                :open-store (or open-store (lambda (root) (fail (format nil "open-store ~A" root))))
                                :state-directory-function (lambda () state-directory))
            nodes)))

(defun run-flow (function &rest arguments)
  "Run an inspect flow under CALL-WITH-COMMAND-RESULT/K and return (VALUES
KIND FIELDS): FIELDS is the ok/partial alist or the error plist."
  (let ((result (aitools.protocol.application:call-with-command-result/k
                 (lambda (&rest continuations)
                   (apply function (append arguments continuations))))))
    (values (aitools.protocol.application:command-result-kind result)
            (aitools.protocol.application:command-result-fields result))))

(defun field (alist name)
  (cdr (assoc name alist :test #'string=)))

(defun error-code (fields) (getf fields :code))

(defun lines-of (&rest lines)
  (coerce lines 'simple-vector))
