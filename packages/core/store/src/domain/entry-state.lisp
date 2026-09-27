;;;; packages/core/store/src/domain/entry-state.lisp
;;;;
;;;; ENTRY-STATE is what a path looks like at one moment: absent, a regular
;;;; file (content hash, permission bits, and after a `touch` its mtime), a directory, or a symlink
;;;; (target). The journal's `before`/`after`, the tx index's `base`/
;;;; `staged`, and every conflict compare these values.
;;;;
;;;; Every decoder here reads a file the store wrote into the state
;;;; directory, which a user or another tool can edit. Decoders therefore
;;;; fail closed with STORE-FORMAT-ERROR on unknown kinds, missing fields,
;;;; and out-of-range values rather than guessing.
(in-package #:aitools.store.domain)

(define-condition store-format-error (error)
  ((detail :initarg :detail :reader store-format-error-detail))
  (:report (lambda (condition stream)
             (format stream "malformed store record: ~A" (store-format-error-detail condition)))))

(defun %format-error (detail)
  (error 'store-format-error :detail detail))

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun string-octets (string)
  (coerce (sb-ext:string-to-octets string :external-format :utf-8) 'octets))

(defun octets-string (octets)
  (decode-utf8-strict/k
   octets
   :on-decoded #'identity
   ;; Invalid bytes re-run the platform decoder so the native decoding error
   ;; still reaches the callers that handle it (application/recovery.lisp),
   ;; keeping the domain layer clear of SB-INT (see t/integration/structure-test.lisp).
   :on-invalid (lambda (position)
                 (declare (ignore position))
                 (sb-ext:octets-to-string octets :external-format :utf-8))))

;;; JSON access helpers shared by every decoder in this package. Records are
;;; built through AITOOLS.PROTOCOL.DOMAIN:JSON-OBJECT, whose ordered
;;; JSON-OBJECT keeps every record byte-identical for equal input.

(defun %json-field (object key &key (type t) (required t))
  (unless (hash-table-p object)
    (%format-error (format nil "expected an object holding ~S" key)))
  (multiple-value-bind (value present) (gethash key object)
    (cond ((not present)
           (when required (%format-error (format nil "missing field ~S" key)))
           nil)
          ((json-kit:json-null-p value) nil)
          ((typep value type) value)
          (t (%format-error (format nil "field ~S has the wrong type" key))))))

(defun %json-list (object key &key (required t))
  (coerce (or (%json-field object key :type 'vector :required required) #()) 'list))

(defun %json-null-or (value)
  (or value json-kit:+json-null+))

(defun %parse-json (text)
  (handler-case (json-kit:parse text)
    (json-kit:json-kit-error ()
      (%format-error "invalid JSON"))))

;;; ENTRY-STATE

(defstruct (entry-state
            (:constructor %make-entry-state (kind hash mode target &optional mtime))
            (:copier nil))
  (kind :absent :type (member :absent :file :directory :symlink) :read-only t)
  (hash nil :type (or null string) :read-only t)
  (mode nil :type (or null (integer 0 #o7777)) :read-only t)
  (target nil :type (or null string) :read-only t)
  ;; A regular file's modification time in Unix seconds, carried only by the
  ;; states a `touch` of an existing file produces and the disk states
  ;; compared with them (see ENTRY-STATE-EQUAL); NIL everywhere else.
  (mtime nil :type (or null integer) :read-only t))

(defun absent-state ()
  (%make-entry-state :absent nil nil nil))

(defun file-state (hash mode &optional mtime)
  (%make-entry-state :file hash mode nil mtime))

(defun directory-state (&optional mode)
  (%make-entry-state :directory nil mode nil))

(defun symlink-state (target)
  (%make-entry-state :symlink nil nil target))

(defun entry-state-absent-p (state)
  (eq (entry-state-kind state) :absent))

(defun entry-state-equal (a b)
  "True when A and B describe the same observable entry. Directory modes are
ignored: `mkdir` applies the process umask, so a directory's mode is not a
value any command promises to reproduce. File mtimes are compared only when
both sides carry one: a state records its mtime only where a command
promised it (`touch`), and a comparison that needs it reads the disk's."
  (and (eq (entry-state-kind a) (entry-state-kind b))
       (ecase (entry-state-kind a)
         (:absent t)
         (:directory t)
         (:file (and (equal (entry-state-hash a) (entry-state-hash b))
                     (eql (entry-state-mode a) (entry-state-mode b))
                     (or (null (entry-state-mtime a)) (null (entry-state-mtime b))
                         (= (entry-state-mtime a) (entry-state-mtime b)))))
         (:symlink (equal (entry-state-target a) (entry-state-target b))))))

(defun valid-blob-hash-p (string)
  "Blob names are CONTENT-HASH values: 64 lowercase hex digits. Checked
before a hash read from a record is joined into a blob path."
  (and (stringp string)
       (= (length string) 64)
       (every (lambda (char) (or (char<= #\0 char #\9) (char<= #\a char #\f))) string)))

(defun entry-state->json (state)
  (ecase (entry-state-kind state)
    (:absent (json-object "kind" "absent"))
    (:file (apply #'json-object "kind" "file" "hash" (entry-state-hash state) "mode" (entry-state-mode state)
                  (when (entry-state-mtime state) (list "mtime" (entry-state-mtime state)))))
    (:directory (json-object "kind" "directory" "mode" (%json-null-or (entry-state-mode state))))
    (:symlink (json-object "kind" "symlink" "target" (entry-state-target state)))))

(defun %json-mode (object &key required)
  (let ((mode (%json-field object "mode" :type 'integer :required required)))
    (when (and mode (not (<= 0 mode #o7777)))
      (%format-error "mode out of range"))
    mode))

(defun json->entry-state (object)
  (let ((kind (%json-field object "kind" :type 'string)))
    (cond
      ((string= kind "absent") (absent-state))
      ((string= kind "file")
       (let ((hash (%json-field object "hash" :type 'string)))
         (unless (valid-blob-hash-p hash)
           (%format-error "file hash is not a content hash"))
         (file-state hash (%json-mode object :required t)
                     (%json-field object "mtime" :type '(integer 0) :required nil))))
      ((string= kind "directory") (directory-state (%json-mode object)))
      ((string= kind "symlink") (symlink-state (%json-field object "target" :type 'string)))
      (t (%format-error "unknown entry kind")))))
