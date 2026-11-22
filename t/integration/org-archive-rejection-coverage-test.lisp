(in-package #:aitools.edit.test)

;;; Rejection and argument paths of `org archive` (FR-013); the shared helpers
;;; live in org-archive-coverage-test.lisp.

(describe "Org archive flow rejections"
  (it "rejects a missing source, a directory source and a directory destination"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "archive.org" (format nil "* Archived~%"))
         (expect (%archive-cov-error boundaries :path "missing.org" :to "archive.org")
                 :to-equal '("input.not-found" "Org source does not exist"))
         (put "folder/entry.org" +archive-cov-source+)
         (expect (%archive-cov-error boundaries :path "folder" :to "archive.org")
                 :to-equal '("refusal.not-a-file" "Org source is not a regular file"))
         (put "source.org" +archive-cov-source+)
         (let ((before (octets-of "source.org")))
           (expect (%archive-cov-error boundaries :path "source.org" :to "folder")
                   :to-equal '("refusal.not-a-file" "Org destination is not a regular file"))
           (expect (octets-of "source.org") :to-equalp before))))))
  (it "rejects a NUL byte in the source or in the destination and preserves both files"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries))
             (nul-source (octet-vector 42 32 68 79 78 69 32 0 10))
             (clean (bytes (format nil "* Archived~%"))))
         (put "nul-source.org" nul-source)
         (put "clean.org" clean)
         (expect (%archive-cov-error boundaries :path "nul-source.org" :to "clean.org")
                 :to-equal '("input.unsupported-format" "Org input contains a NUL byte"))
         (put "source.org" +archive-cov-source+)
         (put "nul-target.org" nul-source)
         (expect (%archive-cov-error boundaries :path "source.org" :to "nul-target.org")
                 :to-equal '("input.unsupported-format" "Org input contains a NUL byte"))
         (expect (octets-of "nul-source.org") :to-equalp nul-source)
         (expect (octets-of "nul-target.org") :to-equalp nul-source)
         (expect (octets-of "clean.org") :to-equalp clean)
         (expect (text "source.org") :to-equal +archive-cov-source+)))))
  (it "rejects malformed UTF-8 in the source and in the destination"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries))
             (invalid (octet-vector 42 32 68 79 78 69 32 255 10)))
         (put "invalid.org" invalid)
         (put "clean.org" (format nil "* Archived~%"))
         (put "source.org" +archive-cov-source+)
         (expect (first (%archive-cov-error boundaries :path "invalid.org" :to "clean.org"))
                 :to-equal "input.not-utf8")
         (expect (first (%archive-cov-error boundaries :path "source.org" :to "invalid.org"))
                 :to-equal "input.not-utf8")
         (expect (octets-of "invalid.org") :to-equalp invalid)
         (expect (text "source.org") :to-equal +archive-cov-source+)))))
  (it "attributes a parse failure to the parsed document and stops before archiving"
    (call-with-edit-workspace
     (lambda ()
       (put "source.org" +archive-cov-source+)
       (put "archive.org" (format nil "* Archived~%"))
       (let* ((archived 0)
              (failure (aitools.org.domain:make-org-failure
                        :kind :syntax :message "bad syntax" :line 1 :column 2))
              (real-archive #'aitools.org.infrastructure::archive-org-documents)
              (counting-archive (lambda (&rest arguments)
                                  (incf archived)
                                  (apply real-archive arguments))))
         (dolist (nth '(1 2))
           (let ((boundaries
                   (%archive-cov-boundaries
                    :archive counting-archive
                    :parse (%archive-cov-failing-call
                            #'aitools.org.infrastructure:parse-org-octets nth failure))))
             (expect (%archive-cov-error boundaries :path "source.org" :to "archive.org")
                     :to-equal '("input.syntax-error" "bad syntax"))))
         (expect archived :to-be 0)
         (expect (text "source.org") :to-equal +archive-cov-source+)
         (expect (text "archive.org") :to-equal (format nil "* Archived~%"))))))
  (it "rejects selection and destination-heading failures without changing either file"
    (call-with-edit-workspace
     (lambda ()
       (let ((boundaries (make-org-write-flow-boundaries)))
         (put "source.org" (format nil "#+TODO: TODO | DONE~%* DONE One~%* DONE Two~%"))
         (put "archive.org" (format nil "* Archived~%"))
         (let ((source (octets-of "source.org")) (destination (octets-of "archive.org")))
           (expect (first (%archive-cov-error boundaries :path "source.org" :to "archive.org"
                                                         :match "/TODO"))
                   :to-equal "selection.no-match")
           (expect (first (%archive-cov-error boundaries :path "source.org" :to "archive.org"))
                   :to-equal "selection.ambiguous")
           (expect (first (%archive-cov-error boundaries :path "source.org" :to "archive.org"
                                                         :match nil))
                   :to-equal "argument.invalid")
           (expect (octets-of "source.org") :to-equalp source)
           (expect (octets-of "archive.org") :to-equalp destination))
         (put "single.org" (format nil "#+TODO: TODO | DONE~%* DONE Task~%"))
         (put "twice.org" (format nil "* Archived~%* Archived~%"))
         (let ((destination (octets-of "twice.org")))
           (expect (%archive-cov-error boundaries :path "single.org" :to "twice.org")
                   :to-equal '("selection.ambiguous" "archive target heading is ambiguous"))
           (expect (octets-of "twice.org") :to-equalp destination))
         (put "other-workflow.org" (format nil "#+TODO: TODO | FINISHED~%* Archived~%"))
         (let ((destination (octets-of "other-workflow.org")))
           (multiple-value-bind (code message)
               (values-list (%archive-cov-error boundaries :path "single.org"
                                                           :to "other-workflow.org"))
             (expect code :to-equal "input.unsupported-format")
             (expect (search "Target settings assign a different TODO meaning" message)
                     :to-be-truthy))
           (expect (octets-of "other-workflow.org") :to-equalp destination))
         (expect (text "single.org") :to-equal (format nil "#+TODO: TODO | DONE~%* DONE Task~%"))))))
  (it "reports an engine archive failure against the source"
    (call-with-edit-workspace
     (lambda ()
       (put "source.org" +archive-cov-source+)
       (put "archive.org" (format nil "* Archived~%"))
       (let ((boundaries
               (%archive-cov-boundaries
                :archive (lambda (&rest arguments)
                           (declare (ignore arguments))
                           (values nil nil (aitools.org.domain:make-org-failure
                                            :kind :too-large :message "archive exceeded"))))))
         (expect (%archive-cov-error boundaries :path "source.org" :to "archive.org")
                 :to-equal '("refusal.too-large" "archive exceeded"))
         (expect (text "source.org") :to-equal +archive-cov-source+)
         (expect (text "archive.org") :to-equal (format nil "* Archived~%"))))))
  (it "reports a writer failure for the source or the destination and writes neither file"
    (call-with-edit-workspace
     (lambda ()
       (put "source.org" +archive-cov-source+)
       (put "archive.org" (format nil "* Archived~%"))
       (put "same.org" (format nil "#+TODO: TODO | DONE~%* DONE Task~%"))
       (let ((failure (aitools.org.domain:make-org-failure
                       :kind :too-large :message "output exceeded")))
         (dolist (nth '(1 2))
           (let ((boundaries
                   (%archive-cov-boundaries
                    :write (%archive-cov-failing-call
                            #'aitools.org.infrastructure::write-org-octets nth failure))))
             (expect (%archive-cov-error boundaries :path "source.org" :to "archive.org")
                     :to-equal '("refusal.too-large" "output exceeded"))))
         (expect (%archive-cov-error
                  (%archive-cov-boundaries
                   :write (%archive-cov-failing-call
                           #'aitools.org.infrastructure::write-org-octets 1 failure))
                  :path "same.org" :to "same.org")
                 :to-equal '("refusal.too-large" "output exceeded")))
       (expect (text "source.org") :to-equal +archive-cov-source+)
       (expect (text "archive.org") :to-equal (format nil "* Archived~%"))
       (expect (text "same.org") :to-equal (format nil "#+TODO: TODO | DONE~%* DONE Task~%"))))))

(describe "Org archive flow arguments"
  (it "rejects an empty, missing or multi-line target heading before touching any file"
    (call-with-edit-workspace
     (lambda ()
       (put "source.org" +archive-cov-source+)
       (put "archive.org" (format nil "* Archived~%"))
       (let ((boundaries (make-org-write-flow-boundaries)))
         (dolist (heading (list "" nil (format nil "A~%B") (format nil "A~CB" #\Return)))
           (expect (%archive-cov-error boundaries :path "source.org" :to "archive.org"
                                                  :target-heading heading)
                   :to-equal '("argument.invalid"
                               "--target-heading must be a nonempty single-line title"))))
       (expect (text "source.org") :to-equal +archive-cov-source+)
       (expect (text "archive.org") :to-equal (format nil "* Archived~%")))))
  (it "reports the dry-run outcome without writing either file"
    (call-with-edit-workspace
     (lambda ()
       (put "source.org" +archive-cov-source+)
       (put "archive.org" (format nil "* Archived~%"))
       (let ((fields (%archive-cov-ok (make-org-write-flow-boundaries)
                                      :path "source.org" :to "archive.org" :dry-run t)))
         (expect (field fields "changed") :to-be t)
         (expect (length (field fields "changes")) :to-be 2))
       (expect (text "source.org") :to-equal +archive-cov-source+)
       (expect (text "archive.org") :to-equal (format nil "* Archived~%"))))))
