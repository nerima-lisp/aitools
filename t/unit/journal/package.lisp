;;;; t/unit/journal/package.lisp
;;;;
;;;; One test package for the journal context's unit tests (t/unit/journal/)
;;;; and its integration tests (t/integration/journal-*.lisp), with the
;;;; helpers both use: a real temporary workspace and store behind
;;;; JOURNAL-PORTS, flow invocation returning the COMMAND-RESULT, and JSON
;;;; field access.
(in-package #:cl-user)

(defpackage #:aitools.journal.test
  (:use #:cl #:aitools.journal.domain #:aitools.journal.application #:aitools.store.test-support)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:expect #:expect-not)
  (:import-from #:aitools.test.support #:json-alist #:json-alist-value))

(in-package #:aitools.journal.test)

(defun bytes (string)
  (aitools.store.domain:string-octets string))

(defun disk-path (store relative)
  (concatenate 'string (aitools.store.application:store-root store) "/" relative))

(defun put-file (store relative content &key (mode #o644))
  "Create or overwrite RELATIVE directly, outside the store (an external
edit, or fixture setup). CONTENT is a string or an octet vector."
  (let ((path (disk-path store relative)))
    (ensure-directories-exist (sb-ext:parse-native-namestring path))
    (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output :if-exists :supersede
                                                               :element-type '(unsigned-byte 8))
      (write-sequence (if (stringp content) (bytes content) content) out))
    (sb-posix:chmod path mode)))

(defun disk-state (store relative)
  (aitools.store.application:workspace-state store relative))

(defun disk-text (store relative)
  "RELATIVE's content as a string, or its kind (:absent, :directory ...)."
  (let ((state (disk-state store relative)))
    (if (eq (aitools.store.domain:entry-state-kind state) :file)
        (with-open-file (in (sb-ext:parse-native-namestring (disk-path store relative))
                            :element-type '(unsigned-byte 8))
          (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
            (read-sequence octets in)
            (aitools.store.domain:octets-string octets)))
        (aitools.store.domain:entry-state-kind state))))

(defun disk-mode (store relative)
  (aitools.store.domain:entry-state-mode (disk-state store relative)))

(defun ports-for (store)
  "JOURNAL-PORTS whose workspace is STORE's root (the working directory
too) and whose OPEN-STORE hands back STORE itself."
  (let ((root (aitools.store.application:store-root store)))
    (make-journal-ports
     :workspace-host (aitools.workspace.infrastructure:make-host-workspace-host
                      :current-directory (lambda () root)
                      :getenv (constantly nil))
     :open-store (lambda (real-root)
                   (assert (string= real-root root))
                   store))))

(defun context-for (store &key lock-timeout)
  (make-journal-context :root (aitools.store.application:store-root store) :lock-timeout lock-timeout))

(defun run (flow store &rest arguments)
  "Run FLOW against STORE's workspace; returns (values kind fields), KIND
:ok, :partial or :error, FIELDS the result alist or the error plist."
  (let ((result (aitools.protocol.application:call-with-command-result/k
                 (lambda (&rest continuations)
                   (apply flow (ports-for store) (context-for store) (append arguments continuations))))))
    (values (aitools.protocol.application:command-result-kind result)
            (aitools.protocol.application:command-result-fields result))))

(defun field (fields key)
  (cdr (assoc key fields :test #'string=)))

(defun member-value (object key &optional default)
  (json-alist-value object key default))

(defun commit-write (store requests &key (argv '("test")))
  "Commit REQUESTS as one journaled op, as a write command would. Returns the op id."
  (aitools.store.application:commit-changes/k
   store argv
   (lambda (commit reject)
     (declare (ignore reject))
     (funcall commit requests))
   :on-committed (lambda (op-id results) (declare (ignore results)) op-id)
   :on-rejected (lambda (code &rest rest) (error "commit rejected: ~A ~S" code rest))
   :on-busy (lambda () (error "commit busy"))))

(defun write-op (store path content &key mode)
  (commit-write store (list (aitools.store.domain:write-file-request path (bytes content) :mode mode))
                :argv (list "write" path)))

(defun repair-commands (error-fields)
  (mapcar (lambda (repair) (getf repair :command)) (getf error-fields :repairs)))
