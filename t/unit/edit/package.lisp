;;;; t/unit/edit/package.lisp
;;;;
;;;; The edit context's test package (t/unit/edit/ and
;;;; t/integration/edit-*.lisp) and the helpers the flow tests share: a real
;;;; temporary workspace with its own state home, ports over the production
;;;; adapters, RUN to invoke a command the way presentation does, and
;;;; direct file access for fixtures and assertions.
(in-package #:cl-user)

(defpackage #:aitools.edit.test
  (:use #:cl #:aitools.edit.domain)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:expect #:expect-not #:signals)
  (:import-from #:aitools.edit.application
                #:make-edit-ports #:run-edit-command #:replay-edit-op #:options-argv #:parse-recorded-argv))

(in-package #:aitools.edit.test)

(defun bytes (string)
  (coerce (sb-ext:string-to-octets string :external-format :utf-8) '(simple-array (unsigned-byte 8) (*))))

(defun octet-vector (&rest values)
  (make-array (length values) :element-type '(unsigned-byte 8) :initial-contents values))

(defun doc (text)
  "A TEXT-DOCUMENT of TEXT's UTF-8 bytes (BOM and all)."
  (decode-text-document/k (bytes text) :on-decoded #'identity
                                       :on-binary (lambda () :binary)
                                       :on-invalid (lambda (offset) (list :invalid offset))))

(defun doc-string (document)
  (sb-ext:octets-to-string (render-document document) :external-format :utf-8))

(defun refusal-code (thunk)
  "The EDIT-REFUSAL code THUNK signals, or :NONE."
  (handler-case (progn (funcall thunk) :none)
    (edit-refusal (condition) (edit-refusal-code condition))))

;;; -------------------------------------------------------------- workspace

(defvar *root* nil "The current test workspace's real root path.")
(defvar *home* nil "Its state home.")
(defvar *ports* nil)
(defvar *stdin* nil "Bytes the fake stdin port returns, or NIL to fail loudly.")

(defun %stdin-port (limit &key on-octets on-too-large on-failure)
  (declare (ignore limit on-too-large))
  (if *stdin*
      (funcall on-octets *stdin*)
      (funcall on-failure "the test did not expect stdin to be read")))

(defun %temporary-area-p (root)
  "True when ROOT is `<*home*>/<workspace-id>/tmp`, a workspace's mktemp area,
so the store double matches the production adapter (MAKE-POSIX-STORE), which
opens such a root as a temporary store exempt from the state-directory guard."
  (let* ((root (string-right-trim "/" root))
         (slash (position #\/ root :from-end t))
         (parent (and slash (subseq root 0 slash)))
         (id-slash (and parent (position #\/ parent :from-end t))))
    (and slash id-slash
         (string= (subseq root (1+ slash)) "tmp")
         (< (1+ id-slash) (length parent))
         (string= (subseq parent 0 id-slash) (string-right-trim "/" *home*)))))

(defun open-store (root)
  (aitools.store.application:make-store (aitools.store.infrastructure:make-posix-store-io) root *home*
                                        :temporary (%temporary-area-p root)))

(defun call-with-workspace (function)
  (let* ((base (sb-posix:mkdtemp (format nil "~A/aitools-edit-XXXXXX"
                                         (string-right-trim "/" (or (sb-posix:getenv "TMPDIR") "/tmp")))))
         (base (string-right-trim "/" (sb-ext:native-namestring (truename (concatenate 'string base "/"))))))
    (unwind-protect
         (let* ((*root* (concatenate 'string base "/work"))
                (*home* (concatenate 'string base "/state"))
                (*stdin* nil)
                (*ports* (make-edit-ports
                          :workspace-host (aitools.workspace.infrastructure:make-host-workspace-host)
                          :open-store #'open-store
                          :text-source (aitools.text.infrastructure:make-host-text-source)
                          :read-stdin-octets #'%stdin-port
                          :unix-now #'aitools.edit.infrastructure:unix-now)))
           (sb-posix:mkdir *root* #o755)
           ;; Relative paths resolve against the working directory (as the
           ;; tools aitools replaces do), so the tests run from the root.
           (let ((previous (sb-posix:getcwd)))
             (sb-posix:chdir *root*)
             (unwind-protect (funcall function)
               (sb-posix:chdir previous))))
      (uiop:delete-directory-tree (sb-ext:parse-native-namestring (concatenate 'string base "/"))
                                  :validate (lambda (path) (search "aitools-edit-" (namestring path)))
                                  :if-does-not-exist :ignore))))

(defmacro with-workspace (() &body body)
  `(call-with-workspace (lambda () ,@body)))

(defun disk (relative)
  (concatenate 'string *root* "/" relative))

(defun put (relative content &key (mode #o644))
  "Write RELATIVE directly (fixture setup or an external edit). CONTENT is
a string (UTF-8) or octets."
  (let ((path (disk relative)))
    (ensure-directories-exist (sb-ext:parse-native-namestring path))
    (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output :if-exists :supersede
                                                               :element-type '(unsigned-byte 8))
      (write-sequence (if (stringp content) (bytes content) content) out))
    (sb-posix:chmod path mode)
    relative))

(defun kind (relative)
  (handler-case
      (let ((mode (sb-posix:stat-mode (sb-posix:lstat (disk relative)))))
        (case (logand mode #o170000)
          (#o100000 :file) (#o040000 :directory) (#o120000 :symlink) (t :other)))
    (sb-posix:syscall-error () :absent)))

(defun octets-of (relative)
  (with-open-file (in (sb-ext:parse-native-namestring (disk relative)) :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun text (relative)
  "RELATIVE's content as a string, or its kind when not a regular file."
  (if (eq (kind relative) :file)
      (sb-ext:octets-to-string (octets-of relative) :external-format :utf-8)
      (kind relative)))

(defun mode (relative)
  (logand (sb-posix:stat-mode (sb-posix:lstat (disk relative))) #o7777))

(defun hash (relative)
  (aitools.kernel.domain:content-hash (octets-of relative)))

(defun snapshot ()
  "Every path below the root with its kind, mode and bytes, for
\"nothing changed\" and \"undo restored everything\" assertions."
  (let ((entries '()))
    (labels ((walk (relative)
               (let ((names (funcall (aitools.store.application:store-io-list-directory
                                      (aitools.store.infrastructure:make-posix-store-io))
                                     (if (string= relative "") *root* (disk relative)))))
                 (dolist (name (sort (copy-list names) #'string<))
                   (let* ((child (if (string= relative "") name (concatenate 'string relative "/" name)))
                          (child-kind (kind child)))
                     (push (list child child-kind
                                 (and (member child-kind '(:file :directory)) (mode child))
                                 (case child-kind
                                   (:file (coerce (octets-of child) 'list))
                                   (:symlink (sb-posix:readlink (disk child)))))
                           entries)
                     (when (eq child-kind :directory) (walk child)))))))
      (walk ""))
    (nreverse entries)))

(defun run (name positionals &rest options)
  "Run edit command NAME as presentation would. Returns (values :ok fields)
or (values :error code message plist)."
  (let ((result nil))
    (run-edit-command *ports* name positionals options
                      :root *root*
                      :on-ok (lambda (fields) (setf result (list :ok fields)))
                      :on-error (lambda (code message &rest keys) (setf result (list :error code message keys))))
    (values-list result)))

(defun run-in (tx name positionals &rest options)
  (apply #'run name positionals :tx tx options))

(defun field (fields name)
  (cdr (assoc name fields :test #'string=)))

(defun json-field (object name)
  (cdr (assoc name (aitools.protocol.domain:json-object-members object) :test #'string=)))

(defun changes (fields)
  "(path action) of each `changes` entry."
  (mapcar (lambda (change) (list (json-field change "path") (json-field change "action")))
          (field fields "changes")))

(defun undo (op-id)
  "Undo OP-ID through the store. Returns :COMMITTED or the
rejection code."
  (aitools.store.application:undo-op/k (open-store *root*) op-id (list "undo" op-id)
                                       :on-committed (lambda (new-op results)
                                                       (declare (ignore new-op results))
                                                       :committed)
                                       :on-rejected (lambda (code message &rest keys)
                                                      (declare (ignore message keys))
                                                      code)
                                       :on-busy (lambda () :busy)))

(defun begin-tx ()
  (aitools.store.application:tx-begin/k (open-store *root*)
                                        :on-begun (lambda (tx-id name created) (declare (ignore name created)) tx-id)
                                        :on-busy (lambda () (error "tx begin: busy"))))

(defun commit-tx (tx)
  (aitools.store.application:tx-commit/k (open-store *root*) tx (list "tx" "commit" tx)
                                         :on-committed (lambda (&rest values) (declare (ignore values)) :committed)
                                         :on-rejected (lambda (code &rest values) (declare (ignore values)) code)
                                         :on-not-found (lambda () :not-found)
                                         :on-busy (lambda () :busy)))

(defun replay-record (record view commit reject)
  "The store's REPLAY shape adapted to the journal's replayer contract:
the journal context hands replayers the recorded argv."
  (replay-edit-op (aitools.store.domain:tx-op-record-argv record) view commit reject))
