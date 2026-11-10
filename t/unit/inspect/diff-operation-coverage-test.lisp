(in-package #:aitools.inspect.test)

(defun %commit-inspect-op (store requests)
  (aitools.store.application:commit-changes/k store
                                              '("inspect-test")
                                              (lambda (commit reject)
                                                (declare (ignore reject))
                                                (funcall commit requests))
                                              :on-committed
                                              (lambda (op-id results)
                                                (declare (ignore results))
                                                op-id)
                                              :on-rejected
                                              (lambda (code message &rest keys)
                                                (declare (ignore keys))
                                                (fail
                                                 (format nil
                                                         "store rejected ~A: ~A"
                                                         code
                                                         message)))
                                              :on-busy
                                              (lambda ()
                                                (fail "store is busy"))))

(defun %diff-inspect-op (store op &key files (limit 100))
  (let ((boundaries
         (make-test-boundaries :files
                          files
                          :open-store
                          (lambda (root)
                            (declare (ignore root))
                            store))))
    (run-flow #'diff-flow boundaries :root "/work" :op op :limit limit)))

(describe "diff operation coverage"
          (it "renders a deleted file from its retained before blob"
              (aitools.test.support:with-workspace (root state)
                                                   (declare (ignore state))
                                                   (aitools.test.support:write-text
                                                    (format nil
                                                            "~A/before.txt"
                                                            root)
                                                    "before\n")
                                                   (let* ((store
                                                           (aitools.store.infrastructure:make-posix-store
                                                            root))
                                                          (op
                                                           (%commit-inspect-op
                                                            store
                                                            (list
                                                             (aitools.store.domain:delete-request
                                                              "before.txt")))))
                                                     (multiple-value-bind (kind
                                                                           fields)
                                                         (%diff-inspect-op store
                                                                           op)
                                                       (let* ((change
                                                               (first
                                                                (field fields
                                                                       "changes")))
                                                              (diff
                                                               (json-object-get change "diff")))
                                                         (expect kind
                                                                 :to-be
                                                                 :ok)
                                                         (expect
                                                          (json-object-get change "action")
                                                          :to-equal
                                                          "deleted")
                                                         (expect
                                                          (search "-before"
                                                                  diff)
                                                          :to-be-truthy))))))
          (it "reports the source path for a moved file"
              (aitools.test.support:with-workspace (root state)
                                                   (declare (ignore state))
                                                   (aitools.test.support:write-text
                                                    (format nil
                                                            "~A/source.txt"
                                                            root)
                                                    "content\n")
                                                   (let* ((store
                                                           (aitools.store.infrastructure:make-posix-store
                                                            root))
                                                          (op
                                                           (%commit-inspect-op
                                                            store
                                                            (list
                                                             (aitools.store.domain:move-request
                                                              "source.txt"
                                                              "moved.txt")))))
                                                     (multiple-value-bind (kind
                                                                           fields)
                                                         (%diff-inspect-op store
                                                                           op)
                                                       (let ((change
                                                              (first
                                                               (field fields
                                                                      "changes"))))
                                                         (expect kind
                                                                 :to-be
                                                                 :ok)
                                                         (expect
                                                          (json-object-get change "action")
                                                          :to-equal
                                                          "moved")
                                                         (expect
                                                          (json-object-get change "from")
                                                          :to-equal
                                                          "source.txt"))))))
          (it "reads a missing after blob from disk when its hash still matches"
              (aitools.test.support:with-workspace (root state)
                                                   (declare (ignore state))
                                                   (let* ((store
                                                           (aitools.store.infrastructure:make-posix-store
                                                            root))
                                                          (content
                                                           (aitools.test.support:string-bytes
                                                            "after\n"))
                                                          (op
                                                           (%commit-inspect-op
                                                            store
                                                            (list
                                                             (aitools.store.domain:write-file-request
                                                              "fallback.txt"
                                                              content))))
                                                          (blob
                                                           (aitools.store.domain:blob-file-path
                                                            (aitools.store.application:store-state-directory
                                                             store)
                                                            (aitools.kernel.domain:content-hash
                                                             content))))
                                                     (expect (probe-file blob) :to-be nil)
                                                     (multiple-value-bind (kind
                                                                           fields)
                                                         (%diff-inspect-op store
                                                                           op
                                                                           :files
                                                                           '(("/work/fallback.txt"
                                                                              .
                                                                              "after\n")))
                                                       (let* ((change
                                                               (first
                                                                (field fields
                                                                       "changes")))
                                                              (diff
                                                               (json-object-get change "diff")))
                                                         (expect kind
                                                                 :to-be
                                                                 :ok)
                                                         (expect
                                                          (search "+after" diff)
                                                          :to-be-truthy))))))
          (it
           "returns a partial op diff when the change limit truncates the journal entry"
           (aitools.test.support:with-workspace (root state)
                                                (declare (ignore state))
                                                (let* ((store
                                                        (aitools.store.infrastructure:make-posix-store
                                                         root))
                                                       (op
                                                        (%commit-inspect-op
                                                         store
                                                         (list
                                                          (aitools.store.domain:write-file-request
                                                           "a.txt"
                                                           (aitools.test.support:string-bytes
                                                            "a\n"))
                                                          (aitools.store.domain:write-file-request
                                                           "b.txt"
                                                           (aitools.test.support:string-bytes
                                                            "b\n"))))))
                                                  (multiple-value-bind (kind
                                                                        fields)
                                                      (%diff-inspect-op store
                                                                        op
                                                                        :limit
                                                                        1)
                                                    (let ((changes
                                                           (field fields
                                                                  "changes")))
                                                      (expect kind
                                                              :to-be
                                                              :partial)
                                                      (expect
                                                       (field fields
                                                              "truncated")
                                                       :to-be
                                                       t)
                                                      (expect (length changes)
                                                              :to-be
                                                              1)
                                                      (expect
                                                       (member
                                                        (json-object-get (first changes) "path")
                                                        '("a.txt" "b.txt")
                                                        :test
                                                        #'equal)
                                                       :to-be-truthy)))))))
