;;;; t/unit/store/package.lisp
;;;;
;;;; One test package for the store context's unit tests (t/unit/store/)
;;;; and its integration tests (t/integration/store-*.lisp), plus the few
;;;; helpers both use to drive COMMIT-CHANGES/K and inspect the disk.
(in-package #:cl-user)

(defpackage #:aitools.store.test
  (:use #:cl #:aitools.store.domain #:aitools.store.application #:aitools.store.test-support)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:it-each #:expect #:expect-not #:signals)
  (:import-from #:aitools.store.infrastructure #:make-posix-store-io))

(in-package #:aitools.store.test)

(defun bytes (string)
  (string-octets string))

(defun disk-path (store relative)
  (concatenate 'string (store-root store) "/" relative))

(defun put-file (store relative string &key (mode #o644))
  "Create or overwrite RELATIVE directly, outside the store (an external
edit, or fixture setup)."
  (let ((path (disk-path store relative)))
    (ensure-directories-exist (sb-ext:parse-native-namestring path))
    (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output :if-exists :supersede
                                                               :element-type '(unsigned-byte 8))
      (write-sequence (bytes string) out))
    (sb-posix:chmod path mode)))

(defun disk-text (store relative)
  "RELATIVE's content as a string, or :ABSENT."
  (let ((state (workspace-state store relative)))
    (if (eq (entry-state-kind state) :file)
        (with-open-file (in (sb-ext:parse-native-namestring (disk-path store relative))
                            :element-type '(unsigned-byte 8))
          (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
            (read-sequence octets in)
            (octets-string octets)))
        (entry-state-kind state))))

(defun disk-mode (store relative)
  (entry-state-mode (workspace-state store relative)))

(defun temp-files (store)
  "Every `.aitools-*.tmp` left anywhere under the workspace."
  (let ((found '()))
    (labels ((walk (relative)
               (dolist (name (funcall (store-io-list-directory (store-io-port store))
                                      (if (string= relative "") (store-root store) (disk-path store relative))))
                 (let ((child (if (string= relative "") name (concatenate 'string relative "/" name))))
                   (when (temp-file-name-p name) (push child found))
                   (when (eq (entry-state-kind (workspace-state store child)) :directory)
                     (walk child))))))
      (walk ""))
    found))

(defun intent-files (store)
  (funcall (store-io-list-directory (store-io-port store)) (commit-directory (store-state-directory store))))

(defun commit (store requests &key (argv '("test")) dry-run)
  "Run COMMIT-CHANGES/K with a VALIDATE that commits REQUESTS. Returns
(values :committed op-id results), (values :rejected code message keys), or
:busy."
  (commit-changes/k store argv
                    (lambda (commit reject)
                      (declare (ignore reject))
                      (funcall commit requests))
                    :dry-run dry-run
                    :on-committed (lambda (op-id results) (values :committed op-id results))
                    :on-rejected (lambda (code message &rest keys) (values :rejected code message keys))
                    :on-busy (lambda () :busy)))

(defun undo (store op-id)
  (undo-op/k store op-id (list "undo" op-id)
             :on-committed (lambda (new-op results) (values :committed new-op results))
             :on-rejected (lambda (code message &rest keys) (values :rejected code message keys))
             :on-busy (lambda () :busy)))

(defun recover (store)
  "Run RECOVER/K; returns (values :none) or (values :recovered entries)."
  (let ((result (recover/k store
                           :on-rolled-forward (lambda (op-id) (declare (ignore op-id)))
                           :on-discarded (lambda (op-id) (declare (ignore op-id)))
                           :on-none (lambda () :none)
                           :on-busy (lambda () :busy))))
    (if (listp result) (values :recovered result) result)))

(defun actions (results)
  (mapcar (lambda (result) (list (change-result-path result) (change-result-action result))) results))
