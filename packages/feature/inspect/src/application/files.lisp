;;;; packages/feature/inspect/src/application/files.lisp
;;;;
;;;; Locating and reading the file a command names. Reads are not bound by
;;;; the workspace, so a path resolves against the working
;;;; directory and may lie anywhere; only a path inside the root can be seen
;;;; through a tx or join a tx's read set (docs/src/reference/transactions.md).
;;;; A missing path comes back with the nearest workspace paths as
;;;; `candidates` (docs/src/reference/errors.md).
(in-package #:aitools.inspect.application)

(defstruct (file-target (:constructor %make-file-target) (:copier nil))
  "ARGUMENT is the path as typed; ABSOLUTE its lexical absolute form; REAL
the symlink-resolved path (NIL on a symlink loop); RELATIVE the real path
relative to the real root when inside the workspace, else NIL; KIND
:FILE, :DIRECTORY, :OTHER, or :ABSENT, symlinks followed and the tx
overlay applied."
  (argument "" :type string :read-only t)
  (absolute "" :type string :read-only t)
  (real nil :read-only t)
  (relative nil :read-only t)
  (kind :absent :read-only t))

(defun context-absolute (context path)
  (user-path-absolute (context-host context) path))

(defun probe-target (context path)
  (let* ((host (context-host context))
         (absolute (context-absolute context path))
         (real (resolve-real-path host absolute))
         (root-real (context-root-real context))
         (relative (and real (path-inside-p root-real real)
                        (let ((relative (path-relative-to root-real real)))
                          (if (string= relative ".") "" relative))))
         (view (inspect-context-view context))
         (kind (cond ((null real) :absent)
                     ((and view relative (plusp (length relative)))
                      (ecase (entry-state-kind (view-path-state view relative))
                        (:absent :absent) (:file :file) (:directory :directory) (:symlink :absent)))
                     (t (let ((entry (host-stat host real)))
                          (if entry
                              (case (workspace-entry-kind entry)
                                ((:file :directory) (workspace-entry-kind entry))
                                (:symlink :absent)
                                (t :other))
                              :absent))))))
    (%make-file-target :argument path :absolute absolute :real real :relative relative :kind kind)))

(defun %through-view-p (context target)
  (and (inspect-context-view context) (file-target-relative target)
       (plusp (length (file-target-relative target)))))

(defun target-display-path (target)
  "The path a command reports as `path` for TARGET. Root-relative
when the real path is inside the workspace root; otherwise the absolute real
path (reads are not workspace-bound), consistent with how an absolute
out-of-root path is reported elsewhere. A target with no real path
(missing, or a broken symlink) falls back to its lexical absolute form."
  (let ((relative (file-target-relative target)))
    (cond ((null relative) (or (file-target-real target) (file-target-absolute target)))
          ((string= relative "") ".")
          (t relative))))

(defun read-target-octets (context target)
  "(VALUES octets problem): every byte of the regular file TARGET (a :FILE
target, so its real path is known), through the tx view when there is one,
or NIL when it cannot be read as a file. PROBLEM is :UNREADABLE when the
file exists but was denied (EACCES), NIL for a genuinely missing file, so a
caller can answer environment.io versus input.not-found (contract-F1)."
  (if (%through-view-p context target)
      (values (view-read-file (inspect-context-view context) (file-target-relative target)) nil)
      (source-read-octets (context-source context) (file-target-real target))))

(defun read-target-range (context target start end)
  "(VALUES octets size): the bytes [START, END) of the :FILE target TARGET
clamped to its size, and that size, without reading the rest of a file on
disk (the tx view already holds its bytes). On failure (VALUES NIL problem)
as READ-TARGET-OCTETS."
  (if (%through-view-p context target)
      (let ((octets (view-read-file (inspect-context-view context) (file-target-relative target))))
        (if octets
            (let* ((end (min end (length octets)))
                   (start (min start end)))
              (values (subseq octets start end) (length octets)))
            (values nil nil)))
      (source-read-range (context-source context) (file-target-real target) start end)))

(defconstant +target-chunk-size+ 65536)

(defun map-target-chunks (context target function)
  "Call FUNCTION with successive chunks of the :FILE target TARGET's bytes,
so a whole-file walk holds one chunk at a time. Returns the byte size, or
(VALUES NIL problem) as READ-TARGET-OCTETS."
  (declare (type function function))
  (if (%through-view-p context target)
      (let ((octets (view-read-file (inspect-context-view context) (file-target-relative target))))
        (if octets
            (progn (funcall function octets) (length octets))
            (values nil nil)))
      (let ((source (context-source context))
            (path (file-target-real target))
            (size 0))
        (multiple-value-bind (known problem) (source-file-size source path)
          (cond ((null known) (values nil problem))
                ((source-call-with-chunks source path +target-chunk-size+
                                          (lambda (chunk) (incf size (length chunk)) (funcall function chunk) nil))
                 size)
                (t (values nil nil)))))))

(defun call-with-target-text/k (context target &key on-text on-binary on-missing on-unreadable)
  "The binary-checked read of the :FILE target TARGET for text use: ON-TEXT (octets),
ON-BINARY (prefix size) when the first 8 KiB hold a NUL, ON-MISSING () for a
genuinely absent file, or ON-UNREADABLE () for a file that exists but was
denied (EACCES). The tx view holds the bytes in memory; on disk
CALL-WITH-SNIFFED-OCTETS/K opens the file once and reads nothing past the
sniffed prefix of a binary file."
  (declare (type function on-text on-binary on-missing on-unreadable))
  (if (%through-view-p context target)
      (let ((octets (view-read-file (inspect-context-view context) (file-target-relative target))))
        (cond ((null octets) (funcall on-missing))
              ((binary-octets-p octets)
               (funcall on-binary (subseq octets 0 (min (length octets) +binary-sniff-length+)) (length octets)))
              (t (funcall on-text octets))))
      (aitools.text.application:call-with-sniffed-octets/k
       (context-source context) (file-target-real target)
       :on-text on-text
       :on-binary on-binary
       :on-missing (lambda (path) (declare (ignore path)) (funcall on-missing))
       :on-unreadable (lambda (path) (declare (ignore path)) (funcall on-unreadable)))))

(defconstant +candidate-scan-limit+ 20000
  "Workspace entries examined when ranking candidates for a missing path.")

(defun missing-path-candidates (context absolute &key (count 5))
  "Up to COUNT workspace paths nearest to ABSOLUTE, as {path} objects."
  (let* ((root (inspect-context-root context))
         (root-path (workspace-root-path root))
         (wanted (if (path-inside-p root-path absolute)
                     (path-relative-to root-path absolute)
                     (path-basename absolute)))
         (paths '())
         (seen 0))
    (call-with-workspace-scan/k (context-host context) root
                                :skip-larger-than nil
                                :emit (lambda (entry result)
                                        (declare (ignore result))
                                        (push (scan-entry-path entry) paths)
                                        (when (>= (incf seen) +candidate-scan-limit+) :stop))
                                :on-complete (lambda (source stopped) (declare (ignore source stopped)) nil)
                                :on-error (lambda (reason path) (declare (ignore reason path)) nil))
    (let ((name (path-basename wanted)))
      (mapcar (lambda (path)
                (json-object "path" path))
              (rank-similar wanted (nreverse paths)
                            :key (lambda (path)
                                   (if (find #\/ wanted) path (path-basename path)))
                            :count count
                            :max-distance (max 3 (floor (length name) 2)))))))

(defun fail-missing (context target on-error)
  (fail on-error "input.not-found"
        (format nil "file ~A does not exist" (file-target-argument target))
        :candidates (missing-path-candidates context (file-target-absolute target))
        :repairs (list (repair "find" "List nearby files."
                               (command-line context (list "find" (path-basename (file-target-absolute target)))
                                             :tx :none)))))

(defun fail-unreadable (context target on-error)
  "contract-F1: a path that exists but was denied (EACCES) is environment.io,
not input.not-found."
  (fail on-error "environment.io"
        (format nil "file ~A cannot be read: permission denied" (file-target-argument target))
        :repairs (list (repair "describe" "Show the file's mode and owner to fix its permissions."
                               (command-line context (list "info" (file-target-argument target)) :tx :none)))))

(defun fail-target-read (context target on-error problem)
  "Answer a failed read of an existing :FILE target: environment.io when
PROBLEM is :UNREADABLE (EACCES), else input.not-found for a vanished file."
  (if (eq problem :unreadable)
      (fail-unreadable context target on-error)
      (fail-missing context target on-error)))

(defun fail-not-a-file (context target on-error)
  "A directory is refused with a listing of it as the repair; anything else
that is not a regular file (a FIFO, socket, or device) with `info`."
  (let ((argument (file-target-argument target)))
    (if (eq (file-target-kind target) :directory)
        (fail on-error "refusal.not-a-file" (format nil "~A is a directory, not a file" argument)
              :repairs (list (repair "list-directory" "List the directory instead."
                                     (command-line context (list "find" "--depth" "1" argument) :tx :none))))
        (fail on-error "refusal.not-a-file" (format nil "~A is not a regular file" argument)
              :repairs (list (repair "describe" "Show what kind of file it is."
                                     (command-line context (list "info" argument) :tx :none)))))))

(defun call-with-readable-file/k (context path &key on-file on-error)
  "Probe PATH and call ON-FILE (target) for a regular file; a missing path
or a directory ends in ON-ERROR with its repairs."
  (declare (type function on-file on-error))
  (let ((target (probe-target context path)))
    (case (file-target-kind target)
      (:file (funcall on-file target))
      (:absent (fail-missing context target on-error))
      (t (fail-not-a-file context target on-error)))))

(defun record-read/k (context target &key on-recorded on-error)
  "With `--tx`, record TARGET's disk state in the tx's read set (a
path outside the workspace cannot be in it), then call ON-RECORDED ()."
  (declare (type function on-recorded on-error))
  (if (and (inspect-context-tx context) (file-target-relative target)
           (plusp (length (file-target-relative target))))
      (tx-record-read/k (inspect-context-store context) (inspect-context-tx context) (file-target-relative target)
                        :lock-timeout-ms (inspect-context-lock-timeout-ms context)
                        :on-recorded (lambda (state) (declare (ignore state)) (funcall on-recorded))
                        :on-not-found (lambda ()
                                        (fail on-error "input.not-found"
                                              (format nil "tx ~A does not exist" (inspect-context-tx context))
                                              :repairs (list (repair "list-tx" "List the open transactions."
                                                                     "aitools tx status"))))
                        :on-busy (lambda ()
                                   (fail on-error "environment.busy"
                                         "the tx lock could not be acquired within --lock-timeout"
                                         :repairs (list (repair "retry" "Retry once the tx is idle."
                                                                (command-line context (list "read" (file-target-argument target))))))))
      (funcall on-recorded)))

(defun call-with-inspect-file/k (ports path &key root tx lock-timeout record on-file on-error)
  "Resolve the inspect context, probe PATH as a readable regular file, and,
when RECORD (and --tx), record the read before calling ON-FILE (context
target). The context/readable-file/record chain every single-file flow opens
with."
  (declare (type function on-file on-error))
  (call-with-inspect-context/k
   ports :root root :tx tx :lock-timeout lock-timeout :on-error on-error
   :on-ready (lambda (context)
               (call-with-readable-file/k
                context path
                :on-error on-error
                :on-file (lambda (target)
                           (if record
                               (record-read/k context target
                                              :on-error on-error
                                              :on-recorded (lambda () (funcall on-file context target)))
                               (funcall on-file context target)))))))

;;; The change-detection hash rule `info` and every write's
;;; --expect-hash share is AITOOLS.STORE.APPLICATION:PATH-HASH: it bridges a
;;; store view and the workspace host, so it lives in the store context (see
;;; packages/core/store/src/application/view.lisp) and is imported here.
