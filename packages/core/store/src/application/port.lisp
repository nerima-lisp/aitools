;;;;
;;;; The application receives Store-specific operations as a cl-boundary-kit
;;;; boundary context. Infrastructure builds production contexts from POSIX
;;;; syscalls; tests derive fakes with BOUNDARY-CONTEXT-WITH. CLOCK and SLEEPER
;;;; are typed cl-boundary-kit objects stored directly on STORE.
;;;;
;;;; Boundary operations signal STORE-IO-ERROR on failure. Their contracts:
;;;;
;;;;   lstat (path)                  -> (values kind mode target mtime links identity); kind
;;;;                                    is :absent :file :directory :symlink
;;;;                                    :other, mtime Unix seconds and links the
;;;;                                    hard link count (NIL when absent)
;;;;   read-file (path &key max-bytes) -> octets, regular files only; NIL is unbounded
;;;;   create-file (path octets &key mode sync)
;;;;                                  exclusive create (fails if present) with
;;;;                                  MODE, default 0600 (state files are private);
;;;;                                  fsync before close when SYNC
;;;;   append-file (path octets &key sync)
;;;;   rename (from to) / unlink (path) / rmdir (path)
;;;;   mkdir (path &key mode)        MODE defaults to 0777 less the umask
;;;;   chmod (path mode)             never follows a symlink, and refuses a
;;;;                                    regular file with more than one link
;;;;   symlink (target path)
;;;;   set-mtime (path unix-seconds)  modification time of a regular file,
;;;;                                    access time kept; never follows a symlink
;;;;                                    and refuses a file with more than one link
;;;;   list-directory (path)         -> entry names, or NIL when absent
;;;;   try-lock (path &key create)   -> handle, or NIL when another open file
;;;;                                    description holds the exclusive lock
;;;;   unlock (handle)
;;;;   call-with-retry (policy thunk clock sleeper)
;;;;                                  -> thunk result; NIL on retry exhaustion
;;;;   random-hex (count)            -> COUNT lowercase hex digits
(in-package #:aitools.store.application)

(define-condition store-io-error (error)
  ((operation :initarg :operation :reader store-io-error-operation)
   (path :initarg :path :reader store-io-error-path)
   (errno :initarg :errno :initform nil :reader store-io-error-errno)
   (detail :initarg :detail :initform nil :reader store-io-error-detail))
  (:report (lambda (condition stream)
             (format stream "~A failed for ~A~@[: ~A~]"
                     (store-io-error-operation condition)
                     (store-io-error-path condition)
                     (store-io-error-detail condition)))))
(define-condition store-byte-limit-error (store-io-error) ())

(define-condition store-committed-error (store-io-error)
  ((op-id :initarg :op-id :reader store-committed-error-op-id))
  (:documentation "An I/O failure after the commit point of OP-ID. The
intent record stays in `commit/`, so recovery rolls the op forward once the
failing path (STORE-IO-ERROR-PATH) is usable again; the op is not rejected.")
  (:report (lambda (condition stream)
             (format stream "~A committed, but ~A failed for ~A~@[: ~A~]"
                     (store-committed-error-op-id condition)
                     (store-io-error-operation condition)
                     (store-io-error-path condition)
                     (store-io-error-detail condition)))))

(define-condition store-refusal (error)
  ((code :initarg :code :reader store-refusal-code)
   (message :initarg :message :reader store-refusal-message))
  (:documentation "The write protocol's own refusal of a planned op before
anything is prepared; COMMIT-CHANGES/K reports it through ON-REJECTED.")
  (:report (lambda (condition stream)
             (write-string (store-refusal-message condition) stream))))

(defstruct (store (:constructor %make-store (boundaries clock sleeper root state-directory temporary)) (:copier nil))
  (boundaries nil :read-only t)
  (clock nil :read-only t)
  (sleeper nil :read-only t)
  ;; The workspace root's real path, no trailing slash.
  (root nil :type string :read-only t)
  ;; <state>/<workspace-id>, see WORKSPACE-STATE-DIRECTORY.
  (state-directory nil :type string :read-only t)
  ;; True for a store over a workspace's mktemp area (`tmp/` in the state directory): its
  ;; writes are neither journaled nor recorded as intents (see
  ;; %COMMIT-RESULTS), and STATE-DIRECTORY is that workspace's.
  (temporary nil :type boolean :read-only t))

(defun %normalize-root (root)
  (unless (and (stringp root) (plusp (length root)) (char= (char root 0) #\/))
    (error "store root must be an absolute path: ~S" root))
  (if (and (> (length root) 1) (char= (char root (1- (length root))) #\/))
      (subseq root 0 (1- (length root)))
      root))

(defun state-directory-for-root (real-root &key xdg-state-home home)
  "The per-workspace state directory, `<state>/<workspace-id>`, for the
workspace whose real (symlink-resolved) root path is REAL-ROOT.
XDG-STATE-HOME and HOME are the environment's values, read by the caller.
Its `tmp/` (TMP-DIRECTORY) is `mktemp`'s area and `bg/` the bg logs."
  (workspace-state-directory (state-home xdg-state-home home) (%normalize-root real-root)))

(defun make-store (boundaries root state-home
                   &key temporary
                     (clock (cl-boundary-kit:boundary-context-require boundaries :clock))
                     (sleeper (cl-boundary-kit:boundary-context-require boundaries :sleeper)))
  "Create a store for the real workspace ROOT. CLOCK supplies transaction
timestamps and lock timing; SLEEPER waits between lock attempts. TEMPORARY
stores write under the workspace mktemp area without an intent record."
  (let ((root (%normalize-root root)))
    (if temporary
        (let* ((slash (position #\/ root :from-end t))
               (parent (subseq root 0 slash))
               (id-slash (position #\/ parent :from-end t))
               (id (and id-slash (subseq parent (1+ id-slash)))))
          (unless (and (string= (subseq root (1+ slash)) "tmp")
                       id (plusp (length id)) (not (member id '("." "..") :test #'string=)))
            (error "a temporary store root must be a workspace's tmp directory: ~S" root))
          (%make-store boundaries clock sleeper root (join-path state-home id) t))
        (%make-store boundaries clock sleeper root (workspace-state-directory state-home root) nil))))

(defun workspace-state-root (state-directory)
  "`<state-home>/aitools`, the directory holding STATE-DIRECTORY
(`<state-home>/aitools/<workspace-id>`, see STATE-DIRECTORY-FOR-ROOT and
STORE-STATE-DIRECTORY). This is the STATE-ROOT the write boundary refuses."
  (subseq state-directory 0 (position #\/ state-directory :from-end t)))

(defvar *fault-hook* nil
  "Test-only interruption hook. When non-NIL it is called with a step name
keyword and details at every write-protocol and tx step boundary (see FAULT-POINT
call sites for the names). Production never binds it.")

(declaim (inline fault-point))
(defun fault-point (point &rest details)
  (when *fault-hook*
    (apply *fault-hook* point details)))

;;; Dispatch Store-specific operations through the boundary context.

(defmacro %io (store primitive &rest arguments)
  `(funcall (cl-boundary-kit:boundary-context-require
             (store-boundaries ,store)
             ,(intern (symbol-name primitive) :keyword))
            ,@arguments))
