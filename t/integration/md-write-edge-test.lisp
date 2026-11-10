(in-package #:aitools.integration.md-write-test)
(describe "Markdown replacement edge routes"
  (it "handles input failures, missing write ports, and plan race outcomes"
    (labels ((ready-boundaries (&key workspace-host open-store)
               (%make-md-test-boundaries :workspace-host workspace-host
                :open-store open-store
                :read-stdin-octets
                (lambda (limit)
                  (declare (ignore limit))
                  (list :octets (sb-ext:string-to-octets "")))
                :parse-markdown
                (function aitools.md.infrastructure:parse-md-octets))))
      (dolist (boundaries
               (list (ready-boundaries
                      :open-store
                      (function aitools.store.infrastructure:make-posix-store))
                     (ready-boundaries
                      :workspace-host
                      (aitools.workspace.infrastructure:make-host-workspace-boundaries))))
        (expect (getf (aitools.protocol.application:command-result-fields (%md-flow-result #'aitools.md.application:md-replace/k boundaries "notes.md" 1 :expect-hash "test" :stdin t)) :code)
                :to-equal "internal.unexpected")) (expect (getf (aitools.protocol.application:command-result-fields (%md-flow-result #'aitools.md.application:md-replace/k (%md-write-boundaries "" (lambda (limit)
                   (declare (ignore limit))
                   (list :failure "closed"))) "notes.md" 1 :expect-hash "test" :stdin t)) :code)
              :to-equal "environment.io") (expect (getf (aitools.protocol.application:command-result-fields (%md-flow-result #'aitools.md.application:md-replace/k (%md-write-boundaries "") nil 1 :expect-hash "test" :stdin t)) :code)
              :to-equal "argument.invalid") (aitools.test.support:with-workspace (root state :chdir t)
        (declare (ignore state))
        (let* ((boundaries (%md-write-boundaries "# R~%new~%"))
               (view
                 (aitools.store.application:disk-view
                  (funcall (cl-boundary-kit:boundary-context-require boundaries :md-open-store) root)))
               (code-for
                 (lambda (path)
                   (nth 2
                        (multiple-value-list
                         (aitools.md.application::%md-prepare-write
                          boundaries
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
(describe "Markdown replacement in-process coverage"
  (it "classifies stdin and parser outcomes"
    (labels ((reader (result)
               (lambda (limit)
                 (declare (ignore limit))
                 result))
             (read-outcome (boundaries)
               (let ((outcome nil))
                 (aitools.md.application::%md-read-replacement/k
                  boundaries "md replace"
                  :on-replacement
                  (lambda (octets) (setf outcome (list :replacement octets)))
                  :on-error
                  (lambda (code message &key diagnostics repairs)
                    (declare (ignore message repairs))
                    (setf outcome (list :error code diagnostics))))
                 outcome))
             (boundaries (result &optional (parser #'aitools.md.infrastructure:parse-md-octets))
               (%make-md-test-boundaries :read-stdin-octets (reader result)
                :parse-markdown parser)))
      (let* ((valid (sb-ext:string-to-octets "# A~%new~%" :external-format :utf-8))
             (invalid (make-array 1 :element-type '(unsigned-byte 8) :initial-element 255))
             (replacement (read-outcome (boundaries (list :octets valid))))
             (decode-error (read-outcome (boundaries (list :octets invalid))))
             (syntax-error (read-outcome
                            (boundaries (list :octets valid)
                                   (lambda (octets)
                                     (declare (ignore octets))
                                     (values nil "input.syntax-error" "invalid markdown" nil))))))
        (expect (second (read-outcome (%make-md-test-boundaries :parse-markdown #'aitools.md.infrastructure:parse-md-octets)))
                :to-equal "internal.unexpected")
        (expect (second (read-outcome (%make-md-test-boundaries :read-stdin-octets (reader (list :octets valid)))))
                :to-equal "internal.unexpected")
        (expect (second (read-outcome (boundaries '(:failure "closed")))) :to-equal "environment.io")
        (expect (second (read-outcome (boundaries '(:too-large)))) :to-equal "refusal.too-large")
        (expect (first replacement) :to-be :replacement)
        (expect (second replacement) :to-equalp valid)
        (expect (second decode-error) :to-equal "input.not-utf8")
        (expect (length (third decode-error)) :to-be 1)
        (expect (second syntax-error) :to-equal "input.syntax-error")
        (expect (third syntax-error) :to-be nil))))
  (it "keeps no-op and dry-run targets unchanged and rejects stale guards and invalid replacement bytes"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (labels ((error-code (result)
                 (getf (aitools.protocol.application:command-result-fields result) :code)))
        (let* ((source (format nil "# A~%body~%"))
               (bytes (sb-ext:string-to-octets source :external-format :utf-8))
               (hash (aitools.kernel.domain:content-hash bytes))
               (path (concatenate 'string root "/notes.md")))
          (aitools.test.support:write-bytes path bytes)
          (let ((result (%md-flow-result #'aitools.md.application:md-replace/k
                                         (%md-write-boundaries source) "notes.md" 1
                                         :root root :expect-hash hash :stdin t)))
            (expect (aitools.protocol.application:command-result-kind result) :to-be :ok)
            (expect (aitools.test.support:read-bytes path) :to-equalp bytes))
          (let ((result (%md-flow-result #'aitools.md.application:md-replace/k
                                         (%md-write-boundaries "# A revised~%new~%") "notes.md" 1
                                         :root root :expect-hash hash :stdin t :dry-run t)))
            (expect (aitools.protocol.application:command-result-kind result) :to-be :ok)
            (expect (%field (aitools.protocol.application:command-result-fields result) "dry_run")
                    :to-be t)
            (expect (aitools.test.support:read-bytes path) :to-equalp bytes))
          (expect (error-code (%md-flow-result #'aitools.md.application:md-replace/k
                                               (%md-write-boundaries "# A revised~%new~%") "notes.md" 1
                                               :root root :expect-hash "stale" :stdin t))
                  :to-equal "refusal.target-changed")
          (expect (aitools.test.support:read-bytes path) :to-equalp bytes)
          (progn
            (expect (error-code (%md-flow-result #'aitools.md.application:md-replace/k
                                                 (%md-write-boundaries ""
                                                                  (lambda (limit)
                                                                    (declare (ignore limit))
                                                                    (list :octets
                                                                          (make-array 1 :element-type '(unsigned-byte 8)
                                                                                      :initial-element 255))))
                                                 "notes.md" 1 :root root :expect-hash hash :stdin t))
                    :to-equal "input.not-utf8")
            (let ((result (%md-flow-result #'aitools.md.application:md-replace/k
                                           (%md-write-boundaries "# A~%body~%") "notes.md" 2
                                           :root root :expect-hash hash :stdin t)))
              (expect (error-code result) :to-equal "selection.no-match")
              (expect (aitools.test.support:read-bytes path) :to-equalp bytes)))))))
  (it "maps prepare-write read failures, parser errors, and section plans"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let* ((boundaries (%md-write-boundaries "# A~%new~%"))
             (view (aitools.store.application:disk-view
                    (funcall (cl-boundary-kit:boundary-context-require boundaries :md-open-store) root)))
             (replacement (sb-ext:string-to-octets "# A~%new~%" :external-format :utf-8)))
        (labels ((prepare (boundaries path section)
                   (multiple-value-list
                    (aitools.md.application::%md-prepare-write
                     boundaries
                     (aitools.edit.application::make-write-context
                      :view view :paths (list path))
                     section replacement)))
                 (code (outcome) (nth 2 outcome)))
          (aitools.test.support:write-text (concatenate 'string root "/notes.md")
                                           (format nil "# A~%body~%"))
          (cl-weave:with-mocked-functions
              (((symbol-function (find-symbol "VIEW-READ-FILE" "AITOOLS.STORE.APPLICATION"))
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  nil)))
            (expect (code (prepare boundaries "notes.md" 1)) :to-equal "input.not-found"))
          (cl-weave:with-mocked-functions
              (((symbol-function (find-symbol "VIEW-READ-FILE" "AITOOLS.STORE.APPLICATION"))
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (error 'aitools.store.application:store-io-error
                         :operation "read" :path "notes.md"))))
            (expect (code (prepare boundaries "notes.md" 1)) :to-equal "environment.io"))
          (aitools.test.support:write-bytes
           (concatenate 'string root "/notes.md")
           (make-array 1 :element-type '(unsigned-byte 8) :initial-element 255))
          (let ((outcome (prepare boundaries "notes.md" 1)))
            (expect (code outcome) :to-equal "input.not-utf8")
            (expect (length (fifth outcome)) :to-be 1))
          (aitools.test.support:write-text (concatenate 'string root "/notes.md")
                                           (format nil "# A~%body~%"))
          (let ((outcome (prepare boundaries "notes.md" 2)))
            (expect (code outcome) :to-equal "selection.no-match"))
          (let ((outcome (prepare boundaries "notes.md" 1)))
            (expect (code outcome) :to-be nil)
            (expect (length (first outcome)) :to-be 1)
            (expect (cdr (assoc "section" (second outcome) :test #'string=)) :to-be 1))
          (let* ((syntax-boundaries
                   (%make-md-test-boundaries :parse-markdown
                    (lambda (octets)
                      (declare (ignore octets))
                      (values nil "input.syntax-error" "invalid markdown" 3))))
                 (outcome (prepare syntax-boundaries "notes.md" 1)))
            (expect (code outcome) :to-equal "input.syntax-error")
            (expect (length (fifth outcome)) :to-be 1))))))
  (it "bounds descriptor stdin at the limit and reports descriptor read failures"
    (aitools.test.support:with-workspace (root state :chdir t)
      (declare (ignore state))
      (let ((path (concatenate 'string root "/stdin")))
        (aitools.test.support:write-bytes
         path (make-array 0 :element-type '(unsigned-byte 8)))
        (with-open-file (stream (sb-ext:parse-native-namestring path)
                                :element-type '(unsigned-byte 8))
          (expect (aitools.md.infrastructure::%read-bounded stream 0)
                  :to-equalp '(:octets #())))
        (aitools.test.support:write-bytes path #(1 2))
        (with-open-file (stream (sb-ext:parse-native-namestring path)
                                :element-type '(unsigned-byte 8))
          (expect (aitools.md.infrastructure::%read-bounded stream 2)
                  :to-equalp '(:octets #(1 2))))
        (aitools.test.support:with-stdin-from (path)
          (expect (aitools.md.infrastructure::read-stdin-octets 2)
                  :to-equalp '(:octets #(1 2))))
        (aitools.test.support:write-bytes path #(1 2 3))
        (with-open-file (stream (sb-ext:parse-native-namestring path)
                                :element-type '(unsigned-byte 8))
          (expect (aitools.md.infrastructure::%read-bounded stream 2)
                  :to-equal '(:too-large)))
        (aitools.test.support:with-stdin-from (path)
          (expect (aitools.md.infrastructure::read-stdin-octets 2)
                  :to-equal '(:too-large)))
        (aitools.test.support:with-stdin-from (root)
          (expect (first (aitools.md.infrastructure::read-stdin-octets 1))
                  :to-be :failure))))))
