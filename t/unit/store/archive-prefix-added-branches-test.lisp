(in-package #:aitools.store.test)

(describe "archive prefix added branch coverage"
  (it "rejects non-directory root and start handles and closes them"
    (call-with-temp-store
     (lambda (store)
       (let* ((io (store-boundaries store))
              (opened 0)
              (closed 0)
              (dot-opens 0)
              (fault :root)
              (tracked-io
                (cl-boundary-kit:boundary-context-with io
                 :open-search-root
                 (lambda (path)
                   (multiple-value-bind (fd identity)
                       (funcall (store-operation io :open-search-root) path)
                     (when fd (incf opened))
                     (values fd identity)))
                 :open-search-entry
                 (lambda (dirfd name path)
                   (multiple-value-bind (kind fd identity)
                       (funcall (store-operation io :open-search-entry) dirfd name path)
                     (when (and (string= name ".") (string= path "."))
                       (incf dot-opens)
                       (when (or (eq fault :root)
                                 (and (eq fault :start) (> dot-opens 1)))
                         (setf kind :file)))
                     (when fd (incf opened))
                     (values kind fd identity)))
                 :close-search-directory
                 (lambda (fd)
                   (incf closed)
                   (funcall (store-operation io :close-search-directory) fd))))
              (tracked-store (make-store tracked-io (store-root store) "/tmp")))
         (let ((condition nil))
           (handler-case
               (aitools.store.application:make-archive-symlink-prefix-checker
                (aitools.store.application::%make-store-view tracked-store nil)
                "" nil)
             (aitools.store.application:store-io-error (caught)
               (setf condition caught)))
           (expect condition :to-be-truthy)
           (expect opened :to-be 2)
           (expect closed :to-be 2))
         (setf fault nil dot-opens 0)
         (let ((checker
                 (aitools.store.application:make-archive-symlink-prefix-checker
                  (aitools.store.application::%make-store-view tracked-store nil)
                  "" nil)))
           (let ((before-open opened)
                 (before-close closed)
                 (condition nil))
             (setf fault :start)
             (handler-case
                 (funcall checker :start)
               (aitools.store.application:store-io-error (caught)
                 (setf condition caught)))
             (expect condition :to-be-truthy)
             (expect (- opened before-open) :to-be 1)
             (expect (- closed before-close) :to-be 1))
           (setf fault nil)
           (funcall checker :finish))
         (expect opened :to-be closed)))
     :io nil))

  (it "ignores dot components while indexing archive paths"
    (let ((symlink-root (make-hash-table :test (function equal)))
          (staged-root (make-hash-table :test (function equal))))
      (aitools.store.application::%archive-prefix-add-path symlink-root ".")
      (aitools.store.application::%archive-stage-add-path staged-root "." :file)
      (expect (nth-value 1 (gethash "." symlink-root)) :to-be nil)
      (expect (gethash :symlink symlink-root) :to-be t)
      (expect (nth-value 1 (gethash "." staged-root)) :to-be nil)
      (expect (gethash :kind staged-root) :to-be :file)))

  (it "keeps a disk symlink unsafe when a directory is staged"
    (call-with-temp-store
     (lambda (store)
       (sb-posix:mkdir (disk-path store "base") #o755)
       (sb-posix:symlink "target" (disk-path store "base/link"))
       (let* ((index (make-tx-index))
              (checker nil)
              (cursor nil))
         (tx-index-put index "base/link"
                       (symlink-state "target")
                       (directory-state #o755))
         (setf checker
               (aitools.store.application:make-archive-symlink-prefix-checker
                (aitools.store.application::%make-store-view store index)
                "base" nil :deletions-visible t)
               cursor (funcall checker :start))
         (unwind-protect
              (multiple-value-bind (next result)
                  (funcall checker :enter cursor "link")
                (setf cursor next)
                (expect result :to-be :symlink))
           (funcall checker :finish cursor)
           (funcall checker :finish))))
     :io nil))

  (it "rejects non-function search boundary capabilities"
    (call-with-temp-store
     (lambda (store)
       (let ((base-boundaries (store-boundaries store))
             (invalid (read-from-string "not-a-function")))
         (flet ((reject (key)
                  (let* ((boundaries
                           (cl-boundary-kit:boundary-context-with
                            base-boundaries key invalid))
                         (invalid-store
                           (make-store boundaries (store-root store) "/tmp")))
                    (signals aitools.store.application:store-io-error
                      (aitools.store.application:make-archive-symlink-prefix-checker
                       (aitools.store.application::%make-store-view invalid-store nil)
                       "" nil)))))
           (reject :open-search-root)
           (reject :open-search-entry)
           (reject :close-search-directory))))
     :io nil))

  (it "classifies search syscall errors"
    (call-with-temp-store
     (lambda (store)
       (let ((io (store-boundaries store))
             (file-path (disk-path store "plain"))
             (link-path (disk-path store "alias")))
         (with-open-file (stream file-path :direction :output)
           (write-string "file" stream))
         (sb-posix:symlink "plain" link-path)
         (expect (aitools.store.infrastructure::%mode-kind sb-posix:s-ifreg) :to-be :file)
         (expect (aitools.store.infrastructure::%mode-kind sb-posix:s-ifdir) :to-be :directory)
         (expect (aitools.store.infrastructure::%mode-kind sb-posix:s-iflnk) :to-be :symlink)
         (expect (aitools.store.infrastructure::%mode-kind 0) :to-be :other)
         #+linux
         (multiple-value-bind (root-fd identity)
             (funcall (store-operation io :open-search-root) (store-root store))
           (declare (ignore identity))
           (unwind-protect
                (progn
                  (multiple-value-bind (kind child-fd child-identity)
                      (aitools.store.infrastructure::%open-entry-linux root-fd "." ".")
                    (expect kind :to-be :directory)
                    (expect child-identity :to-be-truthy)
                    (funcall (store-operation io :close-search-directory) child-fd))
                  (multiple-value-bind (kind child-fd child-identity)
                      (aitools.store.infrastructure::%open-entry-linux root-fd "plain" "plain")
                    (expect kind :to-be :file)
                    (expect child-fd :to-be nil)
                    (expect child-identity :to-be nil))
                  (multiple-value-bind (kind child-fd child-identity)
                      (aitools.store.infrastructure::%open-entry-linux root-fd "alias" "alias")
                    (expect kind :to-be :symlink)
                    (expect child-fd :to-be nil)
                    (expect child-identity :to-be nil))
                  (multiple-value-bind (kind child-fd child-identity)
                      (aitools.store.infrastructure::%open-entry-linux root-fd "missing" "missing")
                    (expect kind :to-be :absent)
                    (expect child-fd :to-be nil)
                    (expect child-identity :to-be nil))
                  (let ((file-fd (sb-posix:open file-path sb-posix:o-rdonly 0)))
                    (unwind-protect
                         (multiple-value-bind (kind child-fd child-identity)
                             (aitools.store.infrastructure::%open-entry-linux file-fd "child" "plain/child")
                           (expect kind :to-be :absent)
                           (expect child-fd :to-be nil)
                           (expect child-identity :to-be nil))
                      (sb-posix:close file-fd)))
                  (signals aitools.store.application:store-io-error
                    (aitools.store.infrastructure::%open-entry-linux -1 "child" "child")))
             (funcall (store-operation io :close-search-directory) root-fd)))
         #+darwin
         (multiple-value-bind (root-fd identity)
             (funcall (store-operation io :open-search-root) (store-root store))
           (declare (ignore identity))
           (unwind-protect
                (progn
                  (multiple-value-bind (kind child-fd child-identity)
                      (aitools.store.infrastructure::%open-entry-darwin root-fd "." ".")
                    (expect kind :to-be :directory)
                    (expect child-identity :to-be-truthy)
                    (funcall (store-operation io :close-search-directory) child-fd))
                  (multiple-value-bind (kind child-fd child-identity)
                      (aitools.store.infrastructure::%open-entry-darwin root-fd "plain" "plain")
                    (expect kind :to-be :file)
                    (expect child-fd :to-be nil)
                    (expect child-identity :to-be nil))
                  (multiple-value-bind (kind child-fd child-identity)
                      (aitools.store.infrastructure::%open-entry-darwin root-fd "alias" "alias")
                    (expect kind :to-be :symlink)
                    (expect child-fd :to-be nil)
                    (expect child-identity :to-be nil))
                  (multiple-value-bind (kind child-fd child-identity)
                      (aitools.store.infrastructure::%open-entry-darwin root-fd "missing" "missing")
                    (expect kind :to-be :absent)
                    (expect child-fd :to-be nil)
                    (expect child-identity :to-be nil))
                  (let ((file-fd (sb-posix:open file-path sb-posix:o-rdonly 0)))
                    (unwind-protect
                         (multiple-value-bind (kind child-fd child-identity)
                             (aitools.store.infrastructure::%open-entry-darwin file-fd "child" "plain/child")
                           (expect kind :to-be :absent)
                           (expect child-fd :to-be nil)
                           (expect child-identity :to-be nil))
                      (sb-posix:close file-fd)))
                  (signals aitools.store.application:store-io-error
                    (aitools.store.infrastructure::%open-entry-darwin -1 "child" "child"))
                  (signals aitools.store.application:store-io-error
                    (aitools.store.infrastructure::%open-entry-darwin-metadata -1 "child" "child")))
             (funcall (store-operation io :close-search-directory) root-fd)))))
     :io nil)))
