(in-package #:aitools.edit.test)

;;; CLI handler and Org kit adapter of `org archive` (FR-013); the shared helpers
;;; live in org-archive-coverage-test.lisp.

(describe "Org archive command"
  (it "runs through the CLI, including dry run and staged transaction"
    (call-with-edit-workspace
     (lambda ()
       (put "source.org" +archive-cov-source+)
       (put "archive.org" (format nil "* Archived~%"))
       (flet ((run (&rest arguments)
                (multiple-value-bind (code envelope stream)
                    (aitools.test.support:run-aitools
                     *root*
                     (append (list "org" "archive" "source.org" "--to" "archive.org"
                                   "--target-heading" "Archived" "--match" "/DONE"
                                   "--at" "2026-10-19T09:00:00" "--tz" "UTC")
                             arguments))
                  (values code envelope stream))))
         (multiple-value-bind (code envelope stream) (run "--dry-run")
           (expect code :to-be 0)
           (expect stream :to-be :stdout)
           (expect (aitools.test.support:envelope-value envelope "command")
                   :to-equal "org archive")
           (expect (aitools.test.support:envelope-value envelope "changed") :to-be t))
         (expect (text "source.org") :to-equal +archive-cov-source+)
         (let ((tx (aitools.test.support:envelope-value
                    (nth-value 1 (aitools.test.support:run-aitools *root* '("tx" "begin")))
                    "tx")))
           (multiple-value-bind (code envelope) (run "--tx" tx)
             (expect code :to-be 0)
             (expect (aitools.test.support:envelope-value envelope "tx") :to-equal tx))
           (expect (text "source.org") :to-equal +archive-cov-source+)
           (expect (aitools.test.support:run-aitools *root* (list "tx" "commit" tx))
                   :to-be 0))
         (expect (text "source.org")
                 :to-equal (format nil "#+TODO: TODO | DONE~%* Project~%** TODO Other~%"))
         (expect (member "** DONE Task" (%archive-cov-lines "archive.org") :test #'string=)
                 :to-be-truthy)))))
  (it "reports a missing source as an error envelope on stderr"
    (call-with-edit-workspace
     (lambda ()
       (put "archive.org" (format nil "* Archived~%"))
       (multiple-value-bind (code envelope stream)
           (aitools.test.support:run-aitools
            *root* '("org" "archive" "missing.org" "--to" "archive.org"
                     "--target-heading" "Archived" "--match" "/DONE"
                     "--at" "2026-10-19T09:00:00" "--tz" "UTC"))
         (expect code :to-be (aitools.protocol.domain:error-code-exit-code "input.not-found"))
         (expect stream :to-be :stderr)
         (expect (aitools.test.support:envelope-value envelope "error" "code")
                 :to-equal "input.not-found"))))))

(describe "Org archive adapter"
  (it "finds destination headings by exact title among all node types"
    (let* ((document (%org-coverage-document
                      (format nil "Intro text~%* Archived~%** Archived~%* Archived two~%body~%")))
           (engine (aitools.org.domain:org-document-engine-value document)))
      (expect (length (aitools.org.infrastructure::%archive-target engine "Archived"))
              :to-be 2)
      (expect (length (aitools.org.infrastructure::%archive-target engine "Archived two"))
              :to-be 1)
      (expect (aitools.org.infrastructure::%archive-target engine "Missing") :to-be nil)))
  (it "returns the edited source and destination documents for separate and identical files"
    (let* ((source (aitools.org.domain:org-document-engine-value
                    (%org-coverage-document +archive-cov-source+)))
           (destination (aitools.org.domain:org-document-engine-value
                         (%org-coverage-document (format nil "* Archived~%")))))
      (multiple-value-bind (source-result target-result failure)
          (aitools.org.infrastructure::archive-org-documents
           source destination (%archive-cov-options) nil)
        (expect failure :to-be nil)
        (expect (search "DONE Task" (org-kit:document-text source-result)) :to-be nil)
        (expect (search "** DONE Task" (org-kit:document-text target-result)) :to-be-truthy))
      (multiple-value-bind (source-result target-result failure)
          (aitools.org.infrastructure::archive-org-documents
           source source (%archive-cov-options) t)
        (expect failure :to-be nil)
        (expect (org-kit:document-text source-result) :to-be-truthy)
        (expect (search "** DONE Task" (org-kit:document-text target-result)) :to-be-truthy))))
  (it "returns the selector failure before editing anything"
    (let ((source (aitools.org.domain:org-document-engine-value
                   (%org-coverage-document +archive-cov-source+))))
      (multiple-value-bind (source-result target-result failure)
          (aitools.org.infrastructure::archive-org-documents
           source source (%archive-cov-options :match nil) nil)
        (expect source-result :to-be nil)
        (expect target-result :to-be nil)
        (expect (aitools.org.domain:org-failure-kind failure) :to-be :argument))))
  (it "maps kit failures raised while relocating the subtree"
    (let ((source (aitools.org.domain:org-document-engine-value
                   (%org-coverage-document +archive-cov-source+)))
          (destination (aitools.org.domain:org-document-engine-value
                        (%org-coverage-document (format nil "* Archived~%")))))
      (dolist (case (list (list (make-condition 'org-kit:org-edit-error
                                                :message "structure refused"
                                                :reason :uneditable-structure)
                                :unsupported "structure refused")
                          (list (make-condition 'org-kit:org-edit-error
                                                :message "ambiguous"
                                                :reason :ambiguous-target)
                                :selection-ambiguous nil)
                          (list (make-condition 'org-kit:org-resource-limit-error
                                                :message "limit" :limit-name :max-output-length
                                                :limit 1 :actual 2)
                                :too-large "archive exceeded a resource limit")
                          (list (make-condition 'org-kit:org-kit-error :message "kit failed")
                                :internal "archive edit failed")))
        (cl-weave:with-mocked-functions
            (((symbol-function 'org-kit:archive-subtree-to)
              (%archive-cov-signalling (first case))))
          (multiple-value-bind (source-result target-result failure)
              (aitools.org.infrastructure::archive-org-documents
               source destination (%archive-cov-options) nil)
            (expect source-result :to-be nil)
            (expect target-result :to-be nil)
            (expect (aitools.org.domain:org-failure-kind failure) :to-be (second case))
            (when (third case)
              (expect (aitools.org.domain:org-failure-message failure)
                      :to-equal (third case)))))))))
