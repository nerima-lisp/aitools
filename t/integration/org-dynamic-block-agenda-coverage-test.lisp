(in-package #:aitools.edit.test)

(describe "Org dynamic block agenda files"
  (it "aggregates the agenda files the globs select and reports the totals"
    (call-with-edit-workspace
     (lambda ()
       (put "task/a.org" (format nil "* Work~%~A~%" +dyn-clock+))
       (put "task/sub/b.org" (format nil "* Deep~%~A~%" +dyn-clock+))
       (put "task/skip.org" (format nil "* Skipped~%~A~%" +dyn-clock+))
       (put "task/notes.txt" (format nil "* Not org~%~A~%" +dyn-clock+))
       (put "task/.git/hidden.org" (format nil "* Hidden~%~A~%" +dyn-clock+))
       (sb-posix:symlink (disk "task/a.org") (disk "task/link.org"))
       ;; Not selected by any glob, and not valid UTF-8: reading it would fail the update.
       (put "other/c.org" (coerce '(255 254) '(vector (unsigned-byte 8))))
       (%dyn-agenda-report ":scope agenda :tstart \"2026-10-01\" :tend \"2026-11-01\"")
       (let* ((envelope (%dyn-ok (%dyn-update-arguments
                                  "report.org" 1
                                  "--agenda-glob" "task/**" "--agenda-glob" "!task/skip.org")
                                 "org dynamic-block update"))
              (report (text "report.org")))
         (expect (aitools.test.support:envelope-value envelope "changed") :to-be t)
         (expect (%dyn-warnings envelope) :to-equal nil)
         (let ((first-file (search "| a.org " report))
               (second-file (search "| b.org " report)))
           (expect (and first-file second-file (< first-file second-file)) :to-be t))
         (expect (search "task/" report) :to-be nil)
         (expect (search "| Work " report) :to-be-truthy)
         (expect (search "| Deep " report) :to-be-truthy)
         (dolist (name '("skip.org" "notes.txt" "hidden.org" "link.org" "c.org"))
           (expect (search name report) :to-be nil))))))
  (it "orders the agenda paths and honors the glob filters"
    (call-with-edit-workspace
     (lambda ()
       (dolist (path '("task/b.org" "task/a.org" "task/sub/c.org" "task/skip.org"
                       "task/notes.txt" "task/.git/hidden.org" "other/d.org"
                       "top.org"))
         (put path (format nil "* H~%")))
       (sb-posix:symlink (disk "task/a.org") (disk "task/link.org"))
       (let ((view (%dyn-disk-view)))
         (expect (aitools.org.application::%org-agenda-paths view nil) :to-be nil)
         (expect (aitools.org.application::%org-agenda-paths view '("missing/*.org"))
                 :to-be nil)
         (expect (aitools.org.application::%org-agenda-paths
                  view '("task/**" "!task/skip.org"))
                 :to-equal '("task/a.org" "task/b.org" "task/sub/c.org"))
         (expect (aitools.org.application::%org-agenda-paths view '("task/*.org"))
                 :to-equal '("task/a.org" "task/b.org" "task/skip.org"))
         (expect (aitools.org.application::%org-agenda-paths view '("**/*.org"))
                 :to-equal '("other/d.org" "task/a.org" "task/b.org"
                             "task/skip.org" "task/sub/c.org" "top.org"))
         ;; Only negated globs include every .org file.
         (expect (aitools.org.application::%org-agenda-paths view '("!task/skip.org"))
                 :to-equal '("other/d.org" "task/a.org" "task/b.org"
                             "task/sub/c.org" "top.org"))))))
  (it "decides which directories a glob list can reach"
    (dolist (case-row
             '((:id :root-before-prefix :directory "" :globs ("task/*.org") :expected t)
               (:id :matching-prefix :directory "task" :globs ("task/*.org") :expected t)
               (:id :component-mismatch :directory "task/sub" :globs ("task/*.org") :expected nil)
               (:id :other-prefix :directory "other" :globs ("task/*.org") :expected nil)
               (:id :pattern-fully-consumed :directory "task/a" :globs ("task/*") :expected nil)
               (:id :names-exceed-pattern :directory "a/b/c/d" :globs ("a/b/c") :expected nil)
               (:id :names-end-with-pattern :directory "a/b/c" :globs ("a/b/c") :expected nil)
               (:id :globstar-matches-zero :directory "a" :globs ("**/*.org") :expected t)
               (:id :globstar-skips-components :directory "a/b" :globs ("**/*.org") :expected t)
               (:id :trailing-globstar :directory "a/b" :globs ("**") :expected t)
               (:id :globstar-then-mismatch :directory "a/b" :globs ("**/x/y") :expected t)
               (:id :basename-glob :directory "x/y" :globs ("*.org") :expected t)
               (:id :no-globs :directory "x" :globs nil :expected t)
               (:id :negated-only :directory "x" :globs ("!x/y") :expected t)
               (:id :empty-glob-is-not-negated :directory "x" :globs ("" "task/*") :expected t)
               (:id :second-glob-reaches :directory "other"
                :globs ("task/*.org" "other/*.org") :expected t)
               (:id :negated-glob-ignored :directory "other"
                :globs ("!other/*.org" "task/*.org") :expected nil)))
      (expect (cons (getf case-row :id)
                    (aitools.org.application::%org-glob-directory-p
                     (getf case-row :directory) (getf case-row :globs)))
              :to-equal (cons (getf case-row :id) (getf case-row :expected)))))
  (it "rejects an unreadable agenda file by name and leaves the report unchanged"
    (call-with-edit-workspace
     (lambda ()
       (%dyn-agenda-report)
       (dolist (row (list (list :id :nul-byte :path "task/nul.org"
                                :content (sb-ext:string-to-octets
                                          (format nil "* A~C~%" (code-char 0))
                                          :external-format :utf-8)
                                :code "input.unsupported-format"
                                :message "task/nul.org: Org input contains a NUL byte")
                          (list :id :invalid-utf8 :path "task/utf8.org"
                                :content (coerce '(255 254) '(vector (unsigned-byte 8)))
                                :code "input.not-utf8"
                                :message "task/utf8.org: Org input is not valid UTF-8")))
         (put (getf row :path) (getf row :content))
         (let* ((before (octets-of "report.org"))
                (envelope (%dyn-error (%dyn-update-arguments
                                       "report.org" 1 "--agenda-glob" "task/*.org")
                                      "org dynamic-block update" (getf row :code))))
           (expect (cons (getf row :id)
                         (aitools.test.support:envelope-value envelope "error" "message"))
                   :to-equal (cons (getf row :id) (getf row :message)))
           (expect (octets-of "report.org") :to-equalp before)
           (delete-file (disk (getf row :path))))))))
  (it "reads an agenda file within the input limit and refuses one over it"
    (call-with-edit-workspace
     (lambda ()
       (put "task/a.org" (format nil "* Work~%~A~%" +dyn-clock+))
       (let* ((boundaries (make-org-write-flow-boundaries))
              (view (%dyn-disk-view)))
         (multiple-value-bind (engine failure)
             (aitools.org.application::%org-agenda-document boundaries view "task/a.org")
           (expect failure :to-be nil)
           (expect (org-kit:document-source-name engine) :to-equal "task/a.org"))
         (let ((small (cl-boundary-kit:boundary-context-with
                       boundaries :org-max-input-bytes 8)))
           (multiple-value-bind (engine failure)
               (aitools.org.application::%org-agenda-document small view "task/a.org")
             (expect engine :to-be nil)
             (expect (aitools.org.domain:org-failure-kind failure) :to-be :too-large)
             (expect (aitools.org.domain:org-failure-message failure)
                     :to-equal "task/a.org exceeds the Org input limit of 8 bytes")))))))
  (it "carries the parse position of an agenda file failure"
    (call-with-edit-workspace
     (lambda ()
       (put "task/a.org" (format nil "* Work~%"))
       (let ((boundaries
               (cl-boundary-kit:boundary-context-with
                (make-org-write-flow-boundaries) :org-parse
                (lambda (octets &key source-name)
                  (declare (ignore octets))
                  (values nil (aitools.org.domain:make-org-failure
                               :kind :syntax :message (format nil "bad ~A" source-name)
                               :line 4 :column 2 :offset 17))))))
         (multiple-value-bind (engine failure)
             (aitools.org.application::%org-agenda-document
              boundaries (%dyn-disk-view) "task/a.org")
           (expect engine :to-be nil)
           (expect (aitools.org.domain:org-failure-kind failure) :to-be :syntax)
           (expect (aitools.org.domain:org-failure-message failure)
                   :to-equal "task/a.org: bad task/a.org")
           (expect (list (aitools.org.domain:org-failure-line failure)
                         (aitools.org.domain:org-failure-column failure)
                         (aitools.org.domain:org-failure-offset failure))
                   :to-equal '(4 2 17)))))))
  (it "refuses a write whose option preparation fails and leaves the file unchanged"
    (call-with-edit-workspace
     (lambda ()
       (put "plan.org"
            (format nil "#+BEGIN: clocktable :scope file~%old~%#+END:~%"))
       (let ((before (octets-of "plan.org"))
             (boundaries (make-org-write-flow-boundaries)))
         (multiple-value-bind (kind fields)
             (run-org-flow
              #'aitools.org.application::%org-write-flow boundaries "plan.org"
              "org.dynamic-block.update" :dynamic-block-update (list :block 1)
              :prepare-options
              (lambda (view)
                (expect (aitools.store.application:view-path-state view "plan.org")
                        :to-be-truthy)
                (values nil (aitools.org.domain:make-org-failure
                             :kind :unsupported :message "preparation refused")))
              :root *root*)
           (expect kind :to-be :error)
           (expect (getf fields :code) :to-equal "input.unsupported-format")
           (expect (getf fields :message) :to-equal "preparation refused"))
         (expect (octets-of "plan.org") :to-equalp before))))))
