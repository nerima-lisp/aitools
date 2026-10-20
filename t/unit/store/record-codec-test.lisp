(in-package #:aitools.store.test)

(describe "aitools.store.domain codec properties"
  (it-property "round-trips generated journal entries through ops.jsonl"
      ((argument (gen-string :min-length 0 :max-length 40 :alphabet "abcXYZ0123 /é✓"))
       (path-part (gen-string :min-length 1 :max-length 30 :alphabet "abcXYZ0123_-é✓"))
       (before-text (gen-string :min-length 0 :max-length 40 :alphabet "abcXYZ0123 é✓"))
       (after-text (gen-string :min-length 0 :max-length 40 :alphabet "abcXYZ0123 é✓"))
       (time (gen-string :min-length 0 :max-length 30 :alphabet "abcXYZ0123 /é✓")))
    (let* ((op-id (format-op-id 3964000000 "0123abcd"))
           (undoes (format-op-id 3964000001 "abcd0123"))
           (path (format nil "file-~A" path-part))
           (change (make-change-result
                    :path path
                    :action :modified
                    :before (file-state (aitools.kernel.domain:content-hash (bytes before-text)) #o644)
                    :after (file-state (aitools.kernel.domain:content-hash (bytes after-text)) #o600)))
           (entry (make-journal-entry :op-id op-id
                                      :argv (list argument path)
                                      :time time
                                      :changes (list change)
                                      :undoes undoes))
           (encoded (encode-journal (list entry)))
           (decoded (first (decode-journal encoded))))
      (expect (encode-journal (list decoded)) :to-equal encoded)
      (expect (journal-entry-argv decoded) :to-equal (list argument path))
      (expect (journal-entry-time decoded) :to-equal time)
      (expect (journal-entry-undoes decoded) :to-equal undoes)))
  (it-property "round-trips generated fields through an intent header and checksum"
      ((argument (gen-string :min-length 0 :max-length 40 :alphabet "abcXYZ0123 /é✓"))
       (path-part (gen-string :min-length 1 :max-length 30 :alphabet "abcXYZ0123_-é✓"))
       (time (gen-string :min-length 0 :max-length 30 :alphabet "abcXYZ0123 /é✓")))
    (let* ((op-id (format-op-id 3964000000 "0123abcd"))
           (tx-id (format-tx-id 3964000001 "abcd0123"))
           (path (format nil "file-~A" path-part))
           (entry (make-journal-entry :op-id op-id
                                      :argv (list argument)
                                      :time time
                                      :changes nil))
           (intent (make-intent :op-id op-id
                                :steps (list (make-intent-step :op :replace
                                                               :path path
                                                               :temp (temp-file-name op-id 1)
                                                               :kind :file
                                                               :mode #o644
                                                               :mtime 1700000000))
                                :journal-entry entry
                                :tx-id tx-id))
           (header (encode-intent-header intent))
           (record (format nil "~A~%~A~%"
                           header
                           (encode-intent-checksum header))))
      (multiple-value-bind (status decoded) (decode-intent record)
        (expect status :to-be :complete)
        (expect (encode-intent-header decoded) :to-equal header)
        (expect (intent-tx-id decoded) :to-equal tx-id)
        (expect (intent-step-path (first (intent-steps decoded))) :to-equal path)))))
