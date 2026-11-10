(in-package #:aitools.store.test)

(defun %domain-final-entry (suffix &rest paths)
  (make-journal-entry :op-id
                      (format-op-id 3964000000 suffix)
                      :argv
                      '()
                      :time
                      "t"
                      :changes
                      (mapcar
                       (lambda (path)
                         (make-change-result :path
                                             path
                                             :action
                                             :created
                                             :before
                                             (absent-state)
                                             :after
                                             (directory-state)))
                       paths)))

(describe "Store final domain coverage"
          (it "covers JSON object, missing, null, type and list cases"
              (let ((object
                     (json-kit:parse
                      "{\"list\":[],\"value\":null,\"text\":\"ok\",\"number\":1}")))
                (signals store-format-error
                         (aitools.store.domain::%json-field :not-an-object
                                                            "text"))
                (expect
                 (aitools.store.domain::%json-field object "text" :type 'string)
                 :to-equal
                 "ok")
                (expect
                 (aitools.store.domain::%json-field object
                                                    "value"
                                                    :type
                                                    'string)
                 :to-be
                 nil)
                (expect
                 (aitools.store.domain::%json-field object
                                                    "missing"
                                                    :required
                                                    nil)
                 :to-be
                 nil)
                (signals store-format-error
                         (aitools.store.domain::%json-field object "missing"))
                (signals store-format-error
                         (aitools.store.domain::%json-field object
                                                            "number"
                                                            :type
                                                            'string))
                (expect (aitools.store.domain::%json-list object "list")
                        :to-equal
                        '())
                (expect
                 (aitools.store.domain::%json-list object
                                                   "missing"
                                                   :required
                                                   nil)
                 :to-equal
                 '())
                (signals store-format-error
                         (aitools.store.domain::%json-list object "number"))))
          (it "compares every observable entry-state field"
              (let ((hash (make-string 64 :initial-element #\a))
                    (other-hash (make-string 64 :initial-element #\b)))
                (expect (entry-state-equal (absent-state) (absent-state))
                        :to-be-truthy)
                (expect
                 (entry-state-equal (directory-state #o700)
                                    (directory-state #o755))
                 :to-be-truthy)
                (expect
                 (entry-state-equal (file-state hash #o644)
                                    (file-state hash #o644 7))
                 :to-be-truthy)
                (expect
                 (entry-state-equal (file-state hash #o644 7)
                                    (file-state hash #o644))
                 :to-be-truthy)
                (expect
                 (entry-state-equal (file-state hash #o644 7)
                                    (file-state hash #o644 7))
                 :to-be-truthy)
                (expect
                 (entry-state-equal (file-state hash #o644 7)
                                    (file-state hash #o644 8))
                 :to-be-falsy)
                (expect
                 (entry-state-equal (file-state hash #o644)
                                    (file-state other-hash #o644))
                 :to-be-falsy)
                (expect
                 (entry-state-equal (file-state hash #o644)
                                    (file-state hash #o600))
                 :to-be-falsy)
                (expect
                 (entry-state-equal (symlink-state "target")
                                    (symlink-state "target"))
                 :to-be-truthy)
                (expect
                 (entry-state-equal (symlink-state "target")
                                    (symlink-state "other"))
                 :to-be-falsy)
                (expect (entry-state-equal (absent-state) (directory-state))
                        :to-be-falsy)))
          (it "covers empty, non-text and truncated change output"
              (multiple-value-bind (changes longest) (changes->json '())
                (expect changes :to-equal '())
                (expect longest :to-be nil))
              (let* ((directory
                      (make-change-result :path
                                          "dir"
                                          :action
                                          :created
                                          :before
                                          (absent-state)
                                          :after
                                          (directory-state)))
                     (content (bytes (format nil "one~%two~%")))
                     (text
                      (make-change-result :path
                                          "file.txt"
                                          :action
                                          :created
                                          :before
                                          (absent-state)
                                          :after
                                          (file-state
                                           (aitools.kernel.domain:content-hash
                                            content)
                                           #o644)
                                          :after-content
                                          content)))
                (let ((change (first (changes->json (list directory)))))
                  (expect
                   (aitools.test.support:json-alist-value change
                                                          "diff"
                                                          :missing)
                   :to-be
                   :missing))
                (multiple-value-bind (changes longest)
                    (changes->json (list text) :max-diff-lines 0)
                  (expect
                   (aitools.test.support:json-alist-value (first changes)
                                                          "diff")
                   :to-equal
                   "")
                  (expect
                   (aitools.test.support:json-alist-value (first changes)
                                                          "diff_truncated")
                   :to-be
                   t)
                  (expect longest :to-be 3)
                  (let ((fields
                         (write-result-fields (list text)
                                              :op-id
                                              "op-x"
                                              :max-diff-lines
                                              0)))
                    (expect (cdr (assoc "next_commands" fields :test #'string=))
                            :to-equal
                            '("aitools diff --op op-x")))))
              (let ((fields (write-result-fields '())))
                (expect (mapcar #'car fields) :to-equal '("changes" "op_id"))
                (expect (cdr (assoc "op_id" fields :test #'string=))
                        :to-be
                        json-kit:+json-null+)))
          (it
           "retains a multi-path operation until every path has a newer generation"
           (let* ((pathless (%domain-final-entry "00000001"))
                  (old (%domain-final-entry "00000002" "a" "b"))
                  (new-a (%domain-final-entry "00000003" "a"))
                  (new-b (%domain-final-entry "00000004" "b")))
             (expect (retention-removals (list pathless))
                     :to-equal
                     (list (journal-entry-op-id pathless)))
             (expect (retention-removals (list old new-a) :generations 1)
                     :to-equal
                     '())
             (expect (retention-removals (list old new-a new-b) :generations 1)
                     :to-equal
                     (list (journal-entry-op-id old)))
             (expect (retention-removals (list old) :generations 0)
                     :to-equal
                     (list (journal-entry-op-id old))))))
