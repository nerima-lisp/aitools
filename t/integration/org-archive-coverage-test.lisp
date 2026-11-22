(in-package #:aitools.edit.test)

;;; In-process coverage for `org archive` (FR-013): the application flow
;;; (ORG-ARCHIVE/K, %ARCHIVE-PLAN and the path helpers), the Org kit adapter
;;; (ARCHIVE-ORG-DOCUMENTS, %ARCHIVE-TARGET) and the CLI handler.

(defparameter +archive-cov-source+
  (format nil "#+TODO: TODO | DONE~%* Project~%** DONE Task~%:PROPERTIES:~%:OWNER: A~%:END:~%*** Child~%** TODO Other~%")
  "A source document with one DONE subtree below one TODO sibling.")

(defun %archive-cov-run (boundaries &rest arguments)
  "Run ORG-ARCHIVE/K; ARGUMENTS come first so they override the defaults."
  (apply #'run-org-flow #'aitools.org.application:org-archive/k boundaries
         (append arguments
                 (list :target-heading "Archived" :match "/DONE"
                       :at "2026-10-19T09:00:00" :tz "UTC"
                       :dst-policy "compatible" :root *root*))))

(defun %archive-cov-ok (boundaries &rest arguments)
  (multiple-value-bind (kind fields) (apply #'%archive-cov-run boundaries arguments)
    (expect kind :to-be :ok)
    fields))

(defun %archive-cov-error (boundaries &rest arguments)
  "The (code message) pair of the error ORG-ARCHIVE/K reports for ARGUMENTS."
  (multiple-value-bind (kind fields) (apply #'%archive-cov-run boundaries arguments)
    (expect kind :to-be :error)
    (list (getf fields :code) (getf fields :message))))

(defun %archive-cov-lines (relative)
  (let ((content (text relative)) (lines nil) (start 0))
    (loop for end = (position #\Newline content :start start)
          do (push (subseq content start end) lines)
             (if end (setf start (1+ end)) (return)))
    (nreverse lines)))

(defun %archive-cov-boundaries (&key parse write archive)
  "Production boundaries with the named org capabilities replaced."
  (let ((boundaries (make-org-write-flow-boundaries)))
    (loop for (key value) in (list (list :org-parse parse) (list :org-write write)
                                   (list :org-archive archive))
          when value
            do (setf boundaries
                     (cl-boundary-kit:boundary-context-with boundaries key value)))
    boundaries))

(defun %archive-cov-failing-call (real nth failure)
  "A wrapper of REAL that reports FAILURE (as a second value) on its NTH call."
  (let ((calls 0))
    (lambda (&rest arguments)
      (incf calls)
      (if (= calls nth)
          (values nil failure)
          (apply real arguments)))))

(defun %archive-cov-signalling (condition)
  (lambda (&rest arguments)
    (declare (ignore arguments))
    (error condition)))

(defun %archive-cov-options (&rest overrides)
  (append overrides
          (list :match "/DONE" :target-heading "Archived"
                :at "2026-10-19T09:00:00" :tz "UTC" :dst-policy "compatible")))

(describe "Org archive path helpers"
  (it "splits a slash-separated path into segments and maps the empty path to nothing"
    (expect (aitools.org.application::%archive-path-segments "") :to-be nil)
    (expect (aitools.org.application::%archive-path-segments "file.org")
            :to-equal '("file.org"))
    (expect (aitools.org.application::%archive-path-segments "a/b/c.org")
            :to-equal '("a" "b" "c.org"))
    (expect (aitools.org.application::%archive-path-segments "a/")
            :to-equal '("a" "")))
  (it "computes the source name relative to the destination directory"
    (flet ((relative (source destination)
             (aitools.org.application::%archive-relative-path source destination)))
      ;; Shared leading directories are dropped.
      (expect (relative "a/b/source.org" "a/b/dest.org") :to-equal "source.org")
      (expect (relative "a/b/source.org" "a/dest.org") :to-equal "b/source.org")
      ;; Diverging directories climb out of the destination directory.
      (expect (relative "task/file.org" "archive/2026/file.org")
              :to-equal "../../task/file.org")
      (expect (relative "a/x/source.org" "a/y/dest.org") :to-equal "../x/source.org")
      ;; The same file and root-level files need no prefix.
      (expect (relative "same.org" "same.org") :to-equal "same.org")
      (expect (relative "source.org" "dest.org") :to-equal "source.org")
      (expect (relative "source.org" "deep/dir/dest.org") :to-equal "../../source.org")
      ;; Both lists can run out before a mismatch.
      (expect (relative "a" "a/b/c.org") :to-equal "..")
      (expect (relative "" "a/b.org") :to-equal ".."))))

(describe "Org archive flow moves a subtree between files"
  (it "moves the selected subtree below the destination heading and records where it came from"
    (call-with-edit-workspace
     (lambda ()
       (put "task/file.org" +archive-cov-source+)
       (put "archive/2026/file.org" (format nil "* Archived~%"))
       (let ((fields (%archive-cov-ok (make-org-write-flow-boundaries)
                                      :path "task/file.org" :to "archive/2026/file.org")))
         (expect (field fields "changed") :to-be t))
       (expect (text "task/file.org")
               :to-equal (format nil "#+TODO: TODO | DONE~%* Project~%** TODO Other~%"))
       (let ((lines (%archive-cov-lines "archive/2026/file.org")))
         (expect (first lines) :to-equal "* Archived")
         (dolist (line '("** DONE Task" ":ARCHIVE_FILE: ../../task/file.org"
                         ":ARCHIVE_OLPATH: Project" ":ARCHIVE_CATEGORY: file"
                         ":ARCHIVE_TODO: DONE" ":OWNER: A" "*** Child"))
           (expect (member line lines :test #'string=) :to-be-truthy))
         (expect (some (lambda (line) (eql 0 (search ":ARCHIVE_TIME: " line))) lines)
                 :to-be-truthy)))))
  (it "parses the source under its destination-relative name and the target under its own path"
    (call-with-edit-workspace
     (lambda ()
       (put "task/file.org" +archive-cov-source+)
       (put "archive/2026/file.org" (format nil "* Archived~%"))
       (let* ((names nil)
              (real #'aitools.org.infrastructure:parse-org-octets)
              (boundaries
                (%archive-cov-boundaries
                 :parse (lambda (octets &key source-name)
                          (push source-name names)
                          (funcall real octets :source-name source-name)))))
         (%archive-cov-ok boundaries :path "task/file.org" :to "archive/2026/file.org")
         (let ((names (reverse names)))
           (expect (subseq names 0 2)
                   :to-equal '("../../task/file.org" "archive/2026/file.org")))))))
  (it "selects the addressed duplicate by heading path"
    (call-with-edit-workspace
     (lambda ()
       (put "source.org" (format nil "#+TODO: TODO | DONE~%* Project~%** DONE Task~%*** First~%** DONE Task~%*** Second~%"))
       (put "archive.org" (format nil "* Archived~%"))
       (%archive-cov-ok (make-org-write-flow-boundaries) :path "source.org" :to "archive.org"
                        :match nil
                        :heading-path "[{\"title\":\"Project\",\"index\":1},{\"title\":\"Task\",\"index\":2}]")
       (expect (text "source.org")
               :to-equal (format nil "#+TODO: TODO | DONE~%* Project~%** DONE Task~%*** First~%"))
       (let ((lines (%archive-cov-lines "archive.org")))
         (expect (member "*** Second" lines :test #'string=) :to-be-truthy)
         (expect (member "*** First" lines :test #'string=) :to-be nil)))))
  (it "creates the destination file and heading and removes the last heading from the source"
    (call-with-edit-workspace
     (lambda ()
       (put "single.org" (format nil "#+TODO: TODO | DONE~%* DONE Only~%"))
       (%archive-cov-ok (make-org-write-flow-boundaries) :path "single.org" :to "new/dir/new.org")
       (expect (text "single.org") :to-equal (format nil "#+TODO: TODO | DONE~%"))
       (let ((lines (%archive-cov-lines "new/dir/new.org")))
         (expect (first lines) :to-equal "* Archived")
         (expect (member "** DONE Only" lines :test #'string=) :to-be-truthy)))))
  (it "appends a missing destination heading after a destination that ends with a newline"
    (call-with-edit-workspace
     (lambda ()
       (put "source.org" +archive-cov-source+)
       (put "archive.org" (format nil "* Parent~%"))
       (%archive-cov-ok (make-org-write-flow-boundaries) :path "source.org" :to "archive.org")
       (let ((lines (%archive-cov-lines "archive.org")))
         (expect (subseq lines 0 3) :to-equal '("* Parent" "* Archived" "** DONE Task"))))))
  (it "appends a missing destination heading after a destination without a trailing newline"
    (call-with-edit-workspace
     (lambda ()
       (put "source.org" +archive-cov-source+)
       (put "archive.org" "* Parent")
       (%archive-cov-ok (make-org-write-flow-boundaries) :path "source.org" :to "archive.org")
       (let ((lines (%archive-cov-lines "archive.org")))
         (expect (subseq lines 0 3) :to-equal '("* Parent" "* Archived" "** DONE Task"))))))
  (it "nests the subtree under an existing destination heading and keeps its siblings"
    (call-with-edit-workspace
     (lambda ()
       (put "source.org" (format nil "#+TODO: TODO | DONE~%* DONE Task~%** Child~%"))
       (put "archive.org" (format nil "* Parent~%** Archived~%*** Existing~%* Sibling~%"))
       (%archive-cov-ok (make-org-write-flow-boundaries) :path "source.org" :to "archive.org")
       (let ((lines (%archive-cov-lines "archive.org")))
         (dolist (line '("* Parent" "** Archived" "*** Existing" "*** DONE Task"
                         "**** Child" "* Sibling"))
           (expect (member line lines :test #'string=) :to-be-truthy))
         (expect (< (position "** Archived" lines :test #'string=)
                    (position "*** DONE Task" lines :test #'string=)
                    (position "* Sibling" lines :test #'string=))
                 :to-be t))))))

(describe "Org archive flow within one file"
  (it "moves the subtree below an existing heading of the same file"
    (call-with-edit-workspace
     (lambda ()
       (put "same.org" (format nil "#+TODO: TODO | DONE~%* DONE Task~%** Child~%* Archived~%** Previous~%"))
       (let ((fields (%archive-cov-ok (make-org-write-flow-boundaries)
                                      :path "same.org" :to "same.org")))
         (expect (field fields "changed") :to-be t))
       (let ((lines (%archive-cov-lines "same.org")))
         (expect (subseq lines 0 4)
                 :to-equal '("#+TODO: TODO | DONE" "* Archived" "** Previous" "** DONE Task"))
         (expect (member "*** Child" lines :test #'string=) :to-be-truthy)
         (expect (member ":ARCHIVE_FILE: same.org" lines :test #'string=) :to-be-truthy)))))
  (it "adds the missing heading after text without a trailing newline"
    (call-with-edit-workspace
     (lambda ()
       (put "same.org" (format nil "#+TODO: TODO | DONE~%* DONE Task"))
       (%archive-cov-ok (make-org-write-flow-boundaries) :path "same.org" :to "same.org")
       (let ((lines (%archive-cov-lines "same.org")))
         (expect (subseq lines 0 3)
                 :to-equal '("#+TODO: TODO | DONE" "* Archived" "** DONE Task"))))))
  (it "leaves the file unchanged on a dry run"
    (call-with-edit-workspace
     (lambda ()
       (put "same.org" (format nil "#+TODO: TODO | DONE~%* DONE Task~%** Child~%* Archived~%"))
       (let ((before (octets-of "same.org"))
             (boundaries (make-org-write-flow-boundaries)))
         (let ((fields (%archive-cov-ok boundaries :path "same.org" :to "same.org"
                                                   :dry-run t)))
           (expect (field fields "changed") :to-be t))
         (expect (octets-of "same.org") :to-equalp before))))))
