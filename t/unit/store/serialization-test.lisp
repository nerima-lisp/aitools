(in-package #:aitools.store.test)
(defun %store-render-result (path action before-text after-text &key from binary)
  (flet ((octets (text)
                 (and text
                      (let ((data (bytes text)))
                        (if binary
                            (concatenate 'aitools.store.domain:octets data #(0))
                          data))))
         (state (octets)
                (if octets
                    (aitools.store.domain:file-state
                     (aitools.kernel.domain:content-hash octets) #o644)
                  (aitools.store.domain:absent-state))))
    (let ((before (octets before-text))
          (after (octets after-text)))
      (make-change-result :path path :action action :from from
                          :before (state before) :after (state after)
                          :before-content before :after-content after))))

(defun %store-render-lines (count)
  (format nil "~{line ~D~%~}" (loop for i from 1 to count collect i)))


(describe "aitools.store changes->json"
    (it "renders path, action, hashes and a hunk-only diff"
      (let* ((before-text (format nil "one~%two~%"))
             (after-text (format nil "one~%2~%"))
             (result (%store-render-result "a.txt" :modified before-text after-text))
             (change (first (changes->json (list result)))))
        (expect (mapcar #'car (aitools.test.support:json-alist change))
                :to-equal '("path" "action" "hash_before" "hash_after" "diff"))
        (expect (aitools.test.support:json-alist-value change "action") :to-equal "modified")
        (expect (aitools.test.support:json-alist-value change "hash_before")
                :to-equal (aitools.kernel.domain:content-hash (bytes before-text)))
        (expect (aitools.test.support:json-alist-value change "diff")
                :to-equal (format nil "@@ -1,2 +1,2 @@~% one~%-two~%+2~%"))))

    (it "renders created and deleted text, including missing-side hashes"
      (let ((created (first (changes->json
                             (list (%store-render-result "new.txt" :created nil "x")))))
            (deleted (first (changes->json
                             (list (%store-render-result "gone.txt" :deleted "old" nil)))))
            (null json-kit:+json-null+))
        (expect (aitools.test.support:json-alist-value created "hash_before") :to-be null)
        (expect (aitools.test.support:json-alist-value created "diff")
                :to-equal (format nil "@@ -0,0 +1,1 @@~%+x~%\\ No newline at end of file~%"))
        (expect (aitools.test.support:json-alist-value deleted "hash_after") :to-be null)
        (expect (aitools.test.support:json-alist-value deleted "diff")
                :to-equal (format nil "@@ -1,1 +0,0 @@~%-old~%\\ No newline at end of file~%"))))

    (it "renders an empty file without treating it as absent"
      (let ((change (first (changes->json
                            (list (%store-render-result "empty.txt" :created nil "")))))
            (absent :not-present))
        (expect (aitools.test.support:json-alist-value change "hash_after")
                :to-equal (aitools.kernel.domain:content-hash (bytes "")))
        (expect (aitools.test.support:json-alist-value change "diff" absent)
                :to-equal "")
        (expect (aitools.test.support:json-alist-value change "diff_truncated" absent)
                :to-be absent)))

    (it "shows an LF to CRLF conversion as a change"
      (let ((change (first (changes->json
                            (list (%store-render-result
                                   "e.txt" :modified (format nil "a~%")
                                   (format nil "a~C~%" #\Return)))))))
        (expect (search "+a" (aitools.test.support:json-alist-value change "diff"))
                :to-be-truthy)))

    (it "omits diffs for binary, moved, mode-only, directory and symlink results"
      (let* ((binary (%store-render-result "b.bin" :modified "a" "b" :binary t))
             (moved (%store-render-result "to.txt" :moved nil "same" :from "from.txt"))
             (mode-only (%store-render-result "mode.txt" :mode-changed "a" "a"))
             (directory (make-change-result :path "dir" :action :created
                                            :before (aitools.store.domain:absent-state)
                                            :after (aitools.store.domain:directory-state)))
             (link (make-change-result :path "link" :action :linked
                                       :before (aitools.store.domain:absent-state)
                                       :after (aitools.store.domain:symlink-state "target")))
             (removed-directory (make-change-result :path "gone-dir" :action :deleted
                                                    :before (aitools.store.domain:directory-state)
                                                    :after (aitools.store.domain:absent-state)))
             (changes (changes->json (list binary moved mode-only directory link removed-directory)))
             (absent :not-present))
        (loop for change in changes
              do (expect (aitools.test.support:json-alist-value change "diff" absent)
                         :to-be absent))
        (expect (aitools.test.support:json-alist-value (second changes) "from")
                :to-equal "from.txt")
        (expect (aitools.test.support:json-alist-value (second changes) "action")
                :to-equal "moved")))
    (it-each ((:before) (:after))
             "omits a diff when the ~A content is binary"
             (side)
             (let* ((text (bytes "text"))
                    (binary (concatenate (quote aitools.store.domain:octets) text #(0)))
                    (before (if (eq side :before) binary text))
                    (after (if (eq side :after) binary text))
                    (result (make-change-result
                             :path "a.txt" :action :modified
                             :before (aitools.store.domain:file-state
                                      (aitools.kernel.domain:content-hash before) #o644)
                             :after (aitools.store.domain:file-state
                                     (aitools.kernel.domain:content-hash after) #o644)
                             :before-content before :after-content after))
                    (change (first (changes->json (list result))))
                    (absent :not-present))
               (expect (aitools.test.support:json-alist-value change "diff" absent)
                       :to-be absent)))

    (it "cuts diffs at the requested line limits and reports the longest cut"
      (multiple-value-bind (changes longest)
          (changes->json (list (%store-render-result "big.txt" :created nil
                                                     (%store-render-lines 10)))
                         :max-diff-lines 4)
        (let ((diff (aitools.test.support:json-alist-value (first changes) "diff")))
          (expect (count #\Newline diff) :to-be 4)
          (expect (aitools.test.support:json-alist-value (first changes) "diff_truncated")
                  :to-be t)
          (expect longest :to-be 11)))
      (multiple-value-bind (changes longest)
          (changes->json (list (%store-render-result "small.txt" :created nil
                                                     (%store-render-lines 1)))
                         :max-diff-lines 0)
        (expect (aitools.test.support:json-alist-value (first changes) "diff") :to-equal "")
        (expect (aitools.test.support:json-alist-value (first changes) "diff_truncated") :to-be t)
        (expect longest :to-be 2)))

    (it "leaves a diff within the limit whole and rejects negative limits"
      (multiple-value-bind (changes longest)
          (changes->json (list (%store-render-result "small.txt" :created nil
                                                     (%store-render-lines 3))))
        (expect (aitools.test.support:json-alist-value (first changes) "diff_truncated" :none)
                :to-be :none)
        (expect longest :to-be nil))
      (signals type-error (changes->json nil :max-diff-lines -1)))

    (it "builds committed, tx and dry-run fields"
      (let* ((big (list (%store-render-result "big.txt" :created nil
                                              (%store-render-lines 300))))
             (op-fields (write-result-fields big :op-id "op-x"))
             (tx-fields (write-result-fields big :tx "tx-1"))
             (dry-fields (write-result-fields big :dry-run t))
             (null json-kit:+json-null+))
        (expect (mapcar #'car op-fields) :to-equal '("changes" "op_id" "next_commands"))
        (expect (aitools.test.support:json-alist-value
                 (first (cdr (assoc "changes" tx-fields :test (function string=)))) "diff_truncated")
                :to-be t)
        (expect (cdr (assoc "tx_op" tx-fields :test (function string=))) :to-be null)
        (expect (cdr (assoc "next_commands" tx-fields :test (function string=)))
                :to-equal '("aitools tx diff tx-1 --max-diff-lines 301"))
        (expect (mapcar #'car dry-fields) :to-equal '("changes" "dry_run"))
        (expect (cdr (assoc "dry_run" dry-fields :test (function string=))) :to-be t)))

    (it "leaves an uncommitted cut diff without a next command"
      (let ((fields (write-result-fields
                     (list (%store-render-result "big.txt" :created nil
                                                 (%store-render-lines 300))))))
        (expect (mapcar #'car fields) :to-equal '("changes" "op_id"))
        (expect (cdr (assoc "op_id" fields :test (function string=)))
                :to-be json-kit:+json-null+))))
(describe "aitools.store conflict->json"
    (it-each ((:files :write :file) (:directories :read :directory))
        "renders ~A conflict hashes as values or null"
        (state-name conflict-kind state-kind)
      (let* ((base (if (eq state-kind :file)
                       (aitools.store.domain:file-state
                        (aitools.kernel.domain:content-hash (bytes "base")) #o644)
                       (aitools.store.domain:directory-state)))
             (current (if (eq state-kind :file)
                          (aitools.store.domain:file-state
                           (aitools.kernel.domain:content-hash (bytes "current")) #o644)
                          (aitools.store.domain:directory-state)))
             (value (conflict->json
                     (make-conflict :path "file" :kind conflict-kind
                                    :base base :current current)))
             (null json-kit:+json-null+))
        (expect (aitools.test.support:json-alist-value value "path") :to-equal "file")
        (expect (aitools.test.support:json-alist-value value "kind")
                :to-equal (string-downcase (symbol-name conflict-kind)))
        (expect (aitools.test.support:json-alist-value value "base")
                :to-equal (if (eq state-kind :file)
                              (aitools.kernel.domain:content-hash (bytes "base"))
                              null))
        (expect (aitools.test.support:json-alist-value value "current")
                :to-equal (if (eq state-kind :file)
                              (aitools.kernel.domain:content-hash (bytes "current"))
                              null))
        (expect state-name :to-be-truthy))))

