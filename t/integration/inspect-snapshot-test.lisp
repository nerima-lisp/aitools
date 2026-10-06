;;;; t/integration/inspect-snapshot-test.lisp
;;;;
;;;; `snapshot create` and `snapshot diff` over a real temporary workspace
;;;; with the production workspace host, text source, and store adapters
;;;; (changes made outside aitools are found; content
;;;; that is rewritten unchanged is not reported as modified).
(in-package #:aitools.inspect.test)

(defun %temporary-directory ()
  (let ((path (sb-ext:native-namestring
               (uiop:ensure-directory-pathname
                (format nil "~Aaitools-inspect-snapshot-~36R/" (uiop:native-namestring (uiop:temporary-directory))
                        (random (expt 36 10)))))))
    (ensure-directories-exist path)
    (string-right-trim "/" (namestring (truename path)))))

(defun %write-text (path text)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create
                            :external-format :utf-8)
    (write-string text out)))

(defun %set-mtime (path seconds)
  (sb-posix:utimes path seconds seconds))

(defun %real-ports (state)
  (make-inspect-ports :workspace-host (aitools.workspace.infrastructure:make-host-workspace-host)
                      :text-source (aitools.text.infrastructure:make-host-text-source)
                      :open-store (lambda (root) (aitools.store.infrastructure:make-posix-store root))
                      :state-directory-function (lambda () state)))

(describe "snapshot create and diff on disk"
  (it "reports added, removed, and content-modified files, not touched ones"
    (let* ((root (%temporary-directory))
           (state (concatenate 'string (%temporary-directory) "/state"))
           (ports (%real-ports state)))
      (unwind-protect
           (progn
             (%write-text (format nil "~A/keep.txt" root) "same")
             (%write-text (format nil "~A/edit.txt" root) "before")
             (%write-text (format nil "~A/drop.txt" root) "bye")
             (%set-mtime (format nil "~A/edit.txt" root) 1000000000)
             (%set-mtime (format nil "~A/keep.txt" root) 1000000000)
             (multiple-value-bind (kind fields) (run-flow #'snapshot-create-flow ports :root root)
               (expect kind :to-be :ok)
               (expect (field fields "files") :to-be 3)
               (let ((id (field fields "snapshot_id")))
                 (%write-text (format nil "~A/edit.txt" root) "after!")
                 (%write-text (format nil "~A/keep.txt" root) "same")
                 (delete-file (format nil "~A/drop.txt" root))
                 (%write-text (format nil "~A/new.txt" root) "hi")
                 (multiple-value-bind (diff-kind diff) (run-flow #'snapshot-diff-flow ports id :root root)
                   (expect diff-kind :to-be :ok)
                   (expect (field diff "added") :to-equal '("new.txt"))
                   (expect (field diff "removed") :to-equal '("drop.txt"))
                   (expect (field diff "modified") :to-equal '("edit.txt"))))))
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname root) :validate t)
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname (subseq state 0 (- (length state) 6)))
                                    :validate t))))

  (it "fails with input.not-found for an unknown or path-like id"
    (let* ((root (%temporary-directory))
           (ports (%real-ports (concatenate 'string root "/.state"))))
      (unwind-protect
           (progn
             (expect (error-code (nth-value 1 (run-flow #'snapshot-diff-flow ports "snap-20231114T221320Z-0a1b2c3d"
                                                        :root root)))
                     :to-equal "input.not-found")
             (expect (error-code (nth-value 1 (run-flow #'snapshot-diff-flow ports "../x" :root root)))
                     :to-equal "input.not-found"))
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname root) :validate t)))))

(defun %call-with-snapshot-workspace (function)
  "Call FUNCTION with a fresh workspace root, its state directory, and real
ports writing snapshots there; remove both after."
  (let* ((root (%temporary-directory))
         (state-parent (%temporary-directory))
         (state (concatenate 'string state-parent "/state")))
    (unwind-protect (funcall function root state (%real-ports state))
      (uiop:delete-directory-tree (uiop:ensure-directory-pathname root) :validate t)
      (uiop:delete-directory-tree (uiop:ensure-directory-pathname state-parent) :validate t))))

(defun %create-snapshot-id (ports root &rest options)
  (multiple-value-bind (kind fields) (apply #'run-flow #'snapshot-create-flow ports :root root options)
    (unless (eq kind :ok) (fail (format nil "snapshot create: ~S" fields)))
    (field fields "snapshot_id")))

(describe "snapshot scan options on disk"
  (it "applies --lang and --skip-larger-than at create and again at diff"
    (%call-with-snapshot-workspace
     (lambda (root state ports)
       (declare (ignore state))
       (%write-text (format nil "~A/a.md" root) "# a")
       (%write-text (format nil "~A/b.txt" root) "b")
       (%write-text (format nil "~A/big.md" root) (make-string 2048 :initial-element #\x))
       (multiple-value-bind (kind fields)
           (run-flow #'snapshot-create-flow ports :root root :lang "markdown" :skip-larger-than "1KiB")
         (expect kind :to-be :ok)
         (expect (field fields "files") :to-be 1)
         (%write-text (format nil "~A/c.md" root) "# c")
         (%write-text (format nil "~A/d.txt" root) "d")
         (%write-text (format nil "~A/b.txt" root) "changed")
         (multiple-value-bind (diff-kind diff) (run-flow #'snapshot-diff-flow ports (field fields "snapshot_id") :root root)
           (expect diff-kind :to-be :ok)
           (expect (field diff "added") :to-equal '("c.md"))
           (expect (field diff "removed") :to-be nil)
           (expect (field diff "modified") :to-be nil))))))

  (it "keeps only files newer than a --newer duration or a --newer path"
    (%call-with-snapshot-workspace
     (lambda (root state ports)
       (declare (ignore state))
       (%write-text (format nil "~A/old.txt" root) "o")
       (%write-text (format nil "~A/ref.txt" root) "r")
       (%write-text (format nil "~A/new.txt" root) "n")
       (%set-mtime (format nil "~A/old.txt" root) 1000000000)
       (%set-mtime (format nil "~A/ref.txt" root) 1500000000)
       (expect (field (nth-value 1 (run-flow #'snapshot-create-flow ports :root root :newer "1h")) "files") :to-be 1)
       (expect (field (nth-value 1 (run-flow #'snapshot-create-flow ports :root root
                                             :newer (format nil "~A/old.txt" root)))
                      "files")
               :to-be 2)))))

  (it "treats an existing duration-shaped --newer path as a path"
    (%call-with-snapshot-workspace
     (lambda (root state ports)
       (declare (ignore state))
       (%write-text (format nil "~A/7d" root) "ref")
       (%write-text (format nil "~A/new.txt" root) "new")
       (%set-mtime (format nil "~A/7d" root) 1000)
       (%set-mtime (format nil "~A/new.txt" root) 1500)
       (multiple-value-bind (kind fields)
           (run-flow #'snapshot-create-flow ports :root root :newer (format nil "~A/7d" root))
         (expect kind :to-be :ok)
         (let ((snapshot nil)
               (id (field fields "snapshot_id")))
           (aitools.inspect.domain:decode-snapshot/k
            (uiop:read-file-string (format nil "~A/snapshots/~A.json" state id)) id
            :on-snapshot (lambda (value) (setf snapshot value))
            :on-invalid (lambda () (fail "snapshot record did not decode")))
           (expect (mapcar #'aitools.inspect.domain:snapshot-file-path
                           (aitools.inspect.domain:snapshot-files snapshot))
                   :to-equal '("new.txt")))))))

(describe "snapshot diff records"
  (it "offers existing snapshots for an unknown id, ignoring other files in the directory"
    (%call-with-snapshot-workspace
     (lambda (root state ports)
       (%write-text (format nil "~A/a.txt" root) "a")
       (let ((id (%create-snapshot-id ports root)))
         (%write-text (format nil "~A/snapshots/notes.txt" state) "not a record")
         (%write-text (format nil "~A/snapshots/bogus.json" state) "{}")
         (multiple-value-bind (kind fields) (run-flow #'snapshot-diff-flow ports "snap-20000101T000000Z-00000000" :root root)
           (expect kind :to-be :error)
           (expect (error-code fields) :to-equal "input.not-found")
           (expect (mapcar (lambda (candidate) (json-object-get candidate "snapshot_id")) (getf fields :candidates))
                   :to-equal (list id))
           (expect (getf (first (getf fields :repairs)) :command)
                   :to-equal (format nil "aitools --root ~A snapshot diff ~A" root id)))))))

  (it "is partial past --limit per list"
    (%call-with-snapshot-workspace
     (lambda (root state ports)
       (declare (ignore state))
       (let ((id (%create-snapshot-id ports root)))
         (dolist (name '("x.txt" "y.txt" "z.txt")) (%write-text (format nil "~A/~A" root name) name))
         (multiple-value-bind (kind fields) (run-flow #'snapshot-diff-flow ports id :root root :limit 2)
           (expect kind :to-be :partial)
           (expect (field fields "added") :to-equal '("x.txt" "y.txt"))
           (expect (field fields "truncated") :to-be t))))))

  (it-each (("text that is not a snapshot" "{\"snapshot_id\": 1}")
            ("bytes that are not UTF-8" #(#x7B #xFF #xFE #x7D)))
      "reports a record holding ~A as not found"
      (label content)
    (declare (ignore label))
    (%call-with-snapshot-workspace
     (lambda (root state ports)
       (let ((id (%create-snapshot-id ports root)))
         (with-open-file (out (format nil "~A/snapshots/~A.json" state id) :direction :output :if-exists :supersede
                                                                           :element-type '(unsigned-byte 8))
           (write-sequence (%as-octets content) out))
         (multiple-value-bind (kind fields) (run-flow #'snapshot-diff-flow ports id :root root)
           (expect kind :to-be :error)
           (expect (error-code fields) :to-equal "input.not-found")))))))

(describe "workspace scans past an unreadable directory"
  (it "snapshots the readable files and still ranks candidates for a missing path"
    (%call-with-snapshot-workspace
     (lambda (root state ports)
       (declare (ignore state))
       (let ((locked (format nil "~A/locked" root)))
         (%write-text (format nil "~A/notes.txt" root) "n")
         (ensure-directories-exist (format nil "~A/" locked))
         (%write-text (format nil "~A/hidden.txt" locked) "h")
         (sb-posix:chmod locked 0)
         (unwind-protect
              (progn
                (multiple-value-bind (kind fields) (run-flow #'snapshot-create-flow ports :root root)
                  (expect kind :to-be :ok)
                  (expect (field fields "files") :to-be 1))
                (multiple-value-bind (kind fields) (run-flow #'read-flow ports (format nil "~A/note.txt" root) :root root)
                  (expect kind :to-be :error)
                  (expect (error-code fields) :to-equal "input.not-found")
                  (expect (json-object-get (first (getf fields :candidates)) "path") :to-equal "notes.txt")))
           (sb-posix:chmod locked #o755)))))))
