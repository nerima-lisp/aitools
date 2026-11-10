(in-package #:aitools.store.test)

  (defun %domain-coverage-replace-char (text index replacement)
    (concatenate 'string
                 (subseq text 0 index)
                 (string replacement)
                 (subseq text (1+ index))))

  (describe "Store domain coverage"
    (it "rejects non-object JSON fields and reads absent optional lists"
      (let ((object (json-kit:parse "{}")))
        (signals store-format-error
          (aitools.store.domain::%json-field "wrong-shaped" "kind"))
        (expect (aitools.store.domain::%json-field object "optional" :required nil)
                :to-be nil)
        (expect (aitools.store.domain::%json-list object "items" :required nil)
                :to-equal '())))

    (it "compares file mtimes only when both states carry one"
      (let ((hash (make-string 64 :initial-element #\a)))
        (expect (entry-state-equal (file-state hash #o644)
                                   (file-state hash #o644 7))
                :to-be-truthy)
        (expect (entry-state-equal (file-state hash #o644 7)
                                   (file-state hash #o644 7))
                :to-be-truthy)
        (expect (entry-state-equal (file-state hash #o644 7)
                                   (file-state hash #o644 8))
                :to-be-falsy)))

    (it "validates every component of transaction ids before joining paths"
      (let* ((id (format-tx-id 3964000000 "0123abcd"))
             (invalid (list nil
                            "tx-short"
                            (format-op-id 3964000000 "0123abcd")
                            (%domain-coverage-replace-char id 3 #\x)
                            (%domain-coverage-replace-char id 11 #\x)
                            (%domain-coverage-replace-char id 12 #\x)
                            (%domain-coverage-replace-char id 18 #\x)
                            (%domain-coverage-replace-char id 20 #\G))))
        (expect (valid-tx-id-p id) :to-be-truthy)
        (dolist (value invalid)
          (expect (valid-tx-id-p value) :to-be-falsy))
        (expect (blob-file-path "/state" (make-string 64 :initial-element #\a))
                :to-equal (concatenate 'string "/state/blobs/"
                                       (make-string 64 :initial-element #\a)))
        (expect (tx-directory "/state" id)
                :to-equal (concatenate 'string "/state/tx/" id))
        (signals store-format-error (blob-file-path "/state" "bad"))
        (signals store-format-error (tx-directory "/state" "../escape"))))

    (it "returns planner refusals for chmod and mkdir type mismatches"
      (expect (nth-value 1 (plan '() (list (chmod-request "missing" #o600))))
              :to-equal "input.not-found")
      (expect (nth-value 1 (plan (list (cons "link" (symlink-state "target")))
                                 (list (chmod-request "link" #o600))))
              :to-equal "refusal.not-a-file")
      (expect (nth-value 1 (plan (list (cons "file" "contents"))
                                 (list (mkdir-request "file"))))
              :to-equal "refusal.exists")
      (expect (nth-value 1 (plan (list (cons "link" (symlink-state "target")))
                                 (list (mkdir-request "link"))))
              :to-equal "refusal.exists"))

    (it "keeps journal entries with no paths until path generations can expire them"
      (let ((entry (make-journal-entry
                    :op-id (format-op-id 3964000000 "deadbeef")
                    :argv '() :time "t" :changes '())))
        (expect (retention-removals (list entry))
                :to-equal (list (journal-entry-op-id entry)))
        (expect (retention-removals nil) :to-equal '())))

    (it "renders and points to a truncated text diff"
      (let* ((content (bytes (format nil "one~%two~%")))
             (result (make-change-result
                      :path "a.txt" :action :created
                      :before (absent-state)
                      :after (file-state (aitools.kernel.domain:content-hash content) #o644)
                      :after-content content)))
        (multiple-value-bind (changes longest)
            (changes->json (list result) :max-diff-lines 1)
          (expect (aitools.test.support:json-alist-value (first changes) "diff_truncated")
                  :to-be t)
          (expect longest :to-be 3))
        (let ((fields (write-result-fields (list result) :op-id "op-x" :max-diff-lines 1)))
          (expect (cdr (assoc "next_commands" fields :test #'string=))
                  :to-equal '("aitools diff --op op-x"))))))
