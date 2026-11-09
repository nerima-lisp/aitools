(in-package #:aitools.integration.md-write-test)
(describe "Markdown replacement edge routes"
  (it "handles input failures, missing write ports, and plan race outcomes"
    (labels ((ready-ports (&key workspace-host open-store)
               (aitools.md.application:make-md-ports
                :workspace-host workspace-host
                :open-store open-store
                :read-stdin-octets
                (lambda (limit)
                  (declare (ignore limit))
                  (list :octets (sb-ext:string-to-octets "")))
                :parse-markdown
                (function aitools.md.infrastructure:parse-md-octets))))
      (dolist (ports
               (list (ready-ports
                      :open-store
                      (function aitools.store.infrastructure:make-posix-store))
                     (ready-ports
                      :workspace-host
                      (aitools.workspace.infrastructure:make-host-workspace-boundaries))))
        (expect (%md-error-code (%md-replace-result ports nil "notes.md" 1))
                :to-equal "internal.unexpected")) (expect (%md-error-code
               (%md-replace-result
                (%md-write-ports
                 nil
                 (lambda (limit)
                   (declare (ignore limit))
                   (list :failure "closed")))
                nil "notes.md" 1))
              :to-equal "environment.io") (expect (%md-error-code
               (%md-replace-result (%md-write-ports nil) nil nil 1))
              :to-equal "argument.invalid") (aitools.test.support:with-workspace (root state :chdir t)
        (declare (ignore state))
        (let* ((ports (%md-write-ports "# R~%new~%"))
               (view
                 (aitools.store.application:disk-view
                  (funcall (aitools.md.application:md-ports-open-store ports) root)))
               (code-for
                 (lambda (path)
                   (nth 2
                        (multiple-value-list
                         (aitools.md.application::%md-prepare-write
                          ports
                          (aitools.edit.application::make-write-context
                           :view view :paths (list path))
                          1 #()))))))
          (expect (funcall code-for "missing.md") :to-equal "input.not-found")
          (sb-posix:mkdir (format nil "~A/folder" root) #o755)
          (expect (funcall code-for "folder") :to-equal "refusal.not-a-file")
          (aitools.test.support:write-text
           (format nil "~A/notes.md" root)
           (format nil "# A~%body~%"))
          (cl-weave:with-mocked-functions
              (((symbol-function
                 (find-symbol "VIEW-READ-FILE" "AITOOLS.STORE.APPLICATION"))
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (error
                   (make-condition
                    (find-class
                     (find-symbol "STORE-BYTE-LIMIT-ERROR" "AITOOLS.STORE.APPLICATION"))
                    :operation "read" :path "notes.md")))))
            (expect (funcall code-for "notes.md")
                    :to-equal "refusal.too-large"))))))
)
