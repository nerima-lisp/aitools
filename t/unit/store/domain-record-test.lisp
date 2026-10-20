(in-package #:aitools.store.test)

(defun record-json (text)
  "Parse TEXT after expanding @H to a valid content hash and @T to a valid
store temp file name."
  (json-kit:parse
   (with-output-to-string (out)
     (loop with start = 0
           for at = (position #\@ text :start start)
           do (write-string text out :start start :end (or at (length text)))
              (unless at (return))
              (write-string (ecase (char text (1+ at))
                              (#\H (make-string 64 :initial-element #\a))
                              (#\T (temp-file-name (format-op-id 3964000000 "00000001") 1)))
                            out)
              (setf start (+ at 2))))))

(defun format-detail (thunk)
  "The STORE-FORMAT-ERROR detail THUNK signals, or :ACCEPTED."
  (handler-case (progn (funcall thunk) :accepted)
    (store-format-error (condition) (store-format-error-detail condition))))

(describe "aitools.store.domain entry-state decoding"
  (it-each (("a file with a short hash" "{\"kind\":\"file\",\"hash\":\"ab\",\"mode\":420}"
             "file hash is not a content hash")
            ("a mode past 7777" "{\"kind\":\"file\",\"hash\":\"@H\",\"mode\":4096}" "mode out of range")
            ("a file without a mode" "{\"kind\":\"file\",\"hash\":\"@H\"}" "missing field \"mode\"")
            ("a negative mtime" "{\"kind\":\"file\",\"hash\":\"@H\",\"mode\":420,\"mtime\":-1}"
             "field \"mtime\" has the wrong type")
            ("an unknown kind" "{\"kind\":\"fifo\"}" "unknown entry kind")
            ("a numeric symlink target" "{\"kind\":\"symlink\",\"target\":3}" "field \"target\" has the wrong type")
            ("an array" "[1]" "expected an object holding \"kind\""))
      "refuses ~A"
      (label text detail)
    (declare (ignore label))
    (expect (format-detail (lambda () (json->entry-state (record-json text)))) :to-equal detail))

  (it "reads a directory with a null mode and a file with an mtime"
    (expect (entry-state-mode (json->entry-state (record-json "{\"kind\":\"directory\",\"mode\":null}"))) :to-be nil)
    (expect (entry-state-mtime (json->entry-state (record-json "{\"kind\":\"file\",\"hash\":\"@H\",\"mode\":420,\"mtime\":7}")))
            :to-be 7))
  (it-property "entry-state JSON round-trips generated fields"
      ((hash (gen-string :min-length 64 :max-length 64 :alphabet "0123456789abcdef"))
       (mode-text (gen-string :min-length 1 :max-length 4 :alphabet "01234567"))
       (mtime-text (gen-string :min-length 1 :max-length 8 :alphabet "0123456789"))
       (nullable-mode (gen-string :min-length 1 :max-length 1 :alphabet "01"))
       (target (gen-string :min-length 0 :max-length 40 :alphabet "abcXYZ0123 /é✓")))
    (let* ((mode (parse-integer mode-text :radix 8))
           (mtime (parse-integer mtime-text))
           (states (list (absent-state)
                         (file-state hash mode mtime)
                         (directory-state (unless (string= nullable-mode "0") mode))
                         (symlink-state target))))
      (dolist (state states)
        (let ((decoded (json->entry-state (json-kit:parse (json-kit:stringify (entry-state->json state))))))
          (expect
           (list (entry-state-kind decoded)
                 (entry-state-hash decoded)
                 (entry-state-mode decoded)
                 (entry-state-target decoded)
                 (entry-state-mtime decoded))
           :to-equal
           (list (entry-state-kind state)
                 (entry-state-hash state)
                 (entry-state-mode state)
                 (entry-state-target state)
                 (entry-state-mtime state))))))))

(describe "aitools.store.domain change action names"
  (it "maps every action name both ways and refuses an unknown one"
    (dolist (action '(:created :modified :deleted :moved :mode-changed :linked))
      (expect (parse-action-name (action-name action)) :to-be action))
    (expect (format-detail (lambda () (parse-action-name "renamed"))) :to-equal "unknown change action")
    (expect (handler-case (action-name :renamed) (error (condition) (princ-to-string condition)))
            :to-equal "unknown change action :RENAMED")))

(describe "aitools.store.domain intent step decoding"
  (it-each (("an unknown op" "{\"op\":\"frob\",\"path\":\"a\"}" "unknown intent step")
            ("an absolute path" "{\"op\":\"unlink\",\"path\":\"/a\"}" "intent step path has the wrong shape")
            ("a temp that is not a store temp" "{\"op\":\"replace\",\"path\":\"a\",\"temp\":\"a.tmp\",\"kind\":\"file\"}"
             "intent temp is not a store temp file name")
            ("a replace without a kind" "{\"op\":\"replace\",\"path\":\"a\",\"temp\":\"@T\"}" "missing field \"kind\"")
            ("an unknown temp kind" "{\"op\":\"replace\",\"path\":\"a\",\"temp\":\"@T\",\"kind\":\"fifo\"}"
             "unknown intent temp kind")
            ("a symlink replace without a target"
             "{\"op\":\"replace\",\"path\":\"a\",\"temp\":\"@T\",\"kind\":\"symlink\"}" "missing field \"target\"")
            ("a move without its source" "{\"op\":\"move\",\"path\":\"a\"}" "missing field \"from\"")
            ("a chmod without a mode" "{\"op\":\"chmod\",\"path\":\"a\"}" "missing field \"mode\"")
            ("a utime without an mtime" "{\"op\":\"utime\",\"path\":\"a\",\"mode\":420}" "missing field \"mtime\""))
      "refuses ~A"
      (label text detail)
    (declare (ignore label))
    (expect (format-detail (lambda () (aitools.store.domain::%json->step (record-json text)))) :to-equal detail))

  (it "decodes a symlink replace and a utime step"
    (let ((link (aitools.store.domain::%json->step
                 (record-json "{\"op\":\"replace\",\"path\":\"a\",\"temp\":\"@T\",\"kind\":\"symlink\",\"target\":\"b\"}")))
          (utime (aitools.store.domain::%json->step
                  (record-json "{\"op\":\"utime\",\"path\":\"a\",\"mode\":420,\"mtime\":9}"))))
      (expect (list (intent-step-kind link) (intent-step-target link)) :to-equal '(:symlink "b"))
      (expect (list (intent-step-op utime) (intent-step-mode utime) (intent-step-mtime utime))
              :to-equal '(:utime #o644 9))))

  (it-each (("a malformed op_id" "{\"op_id\":\"op-x\",\"tx\":null,\"steps\":[],\"journal\":{}}"
             "intent op_id has the wrong shape")
            ("a malformed tx id" "{\"op_id\":\"@O\",\"tx\":\"../t\",\"steps\":[],\"journal\":{}}"
             "intent tx has the wrong shape")
            ("a step that is not an object" "{\"op_id\":\"@O\",\"tx\":null,\"steps\":[1],\"journal\":{}}"
             "intent step is not an object"))
      "refuses an intent header with ~A"
      (label text detail)
    (declare (ignore label))
    (let ((line (let ((at (search "@O" text)))
                  (if at
                      (concatenate 'string (subseq text 0 at) (format-op-id 3964000000 "00000001") (subseq text (+ at 2)))
                      text))))
      (expect (format-detail (lambda () (aitools.store.domain::%decode-header line))) :to-equal detail)
      (expect (multiple-value-list (decode-intent (format nil "~A~%" line))) :to-equal '(:incomplete nil)))))

(describe "aitools.store.domain journal entry decoding"
  (it-each (("a malformed op_id" "{\"op_id\":\"x\",\"argv\":[],\"time\":\"t\",\"changes\":[]}"
             "journal op_id has the wrong shape")
            ("a numeric argv element" "{\"op_id\":\"@O\",\"argv\":[1],\"time\":\"t\",\"changes\":[]}"
             "journal argv holds a non-string")
            ("a malformed undoes" "{\"op_id\":\"@O\",\"argv\":[],\"time\":\"t\",\"changes\":[],\"undoes\":\"x\"}"
             "journal undoes has the wrong shape")
            ("a change that is not an object" "{\"op_id\":\"@O\",\"argv\":[],\"time\":\"t\",\"changes\":[\"a\"]}"
             "journal change is not an object")
            ("a change path outside the workspace"
             "{\"op_id\":\"@O\",\"argv\":[],\"time\":\"t\",\"changes\":[{\"path\":\"../a\",\"action\":\"created\"}]}"
             "journal change path has the wrong shape")
            ("a malformed move source"
             "{\"op_id\":\"@O\",\"argv\":[],\"time\":\"t\",\"changes\":[{\"path\":\"a\",\"from\":\"/b\",\"action\":\"moved\"}]}"
             "journal change path has the wrong shape")
            ("an unknown action"
             "{\"op_id\":\"@O\",\"argv\":[],\"time\":\"t\",\"changes\":[{\"path\":\"a\",\"action\":\"renamed\"}]}"
             "unknown change action")
            ("a line that is not an object" "[]" "journal line is not an object"))
      "refuses ~A"
      (label text detail)
    (declare (ignore label))
    (let ((line (let ((at (search "@O" text)))
                  (if at
                      (concatenate 'string (subseq text 0 at) (format-op-id 3964000000 "00000001") (subseq text (+ at 2)))
                      text))))
      (expect (format-detail (lambda () (decode-journal (format nil "~A~%" line)))) :to-equal detail))))

(defun tx-op-line (n &key (argv "[]") (paths "[]") (replayable "true") (previous "[]"))
  (format nil "{\"tx_op\":~D,\"argv\":~A,\"paths\":~A,\"previous\":~A,\"after\":[],\"replayable\":~A,\"time\":\"t\"}~%"
          n argv paths previous replayable))

(describe "aitools.store.domain tx record decoding"
  (it "refuses ops out of sequence, an index past the last op, and malformed fields"
    (expect (format-detail (lambda () (decode-tx-ops (concatenate 'string (tx-op-line 1) (tx-op-line 3)) 2)))
            :to-equal "tx ops are not numbered consecutively")
    (expect (format-detail (lambda () (decode-tx-ops (tx-op-line 1) 2))) :to-equal "tx index refers to a missing op")
    (expect (format-detail (lambda () (decode-tx-ops (tx-op-line 1 :argv "[1]") 1)))
            :to-equal "tx op argv or paths malformed")
    (expect (format-detail (lambda () (decode-tx-ops (tx-op-line 1 :paths "[\"../a\"]") 1)))
            :to-equal "tx op argv or paths malformed")
    (expect (format-detail (lambda () (decode-tx-ops (tx-op-line 1 :replayable "1") 1)))
            :to-equal "tx op replayable is not a boolean")
    (expect (format-detail (lambda () (decode-tx-ops (tx-op-line 1 :previous "[{\"path\":\"/a\",\"state\":null}]") 1)))
            :to-equal "tx path has the wrong shape")
    (expect (format-detail (lambda () (decode-tx-meta "{\"tx\":\"x\",\"name\":null,\"created\":\"t\"}")))
            :to-equal "tx meta id has the wrong shape"))

  (it "keeps only the records the index covers, and reads a null previous state as not staged"
    (let ((records (decode-tx-ops (concatenate 'string (tx-op-line 1 :replayable "false"
                                                                     :previous "[{\"path\":\"a\",\"state\":null}]")
                                               (format nil "~%")
                                               (tx-op-line 2))
                                  1)))
      (expect (mapcar #'tx-op-record-tx-op records) :to-equal '(1))
      (expect (tx-op-record-replayable (first records)) :to-be nil)
      (expect (tx-op-record-previous (first records)) :to-equal '(("a")))))

  (it "reads a tx meta with and without a name"
    (let ((id (format-tx-id 3964000000 "0123abcd")))
      (expect (tx-meta-name (decode-tx-meta (format nil "{\"tx\":~S,\"name\":\"n\",\"created\":\"t\"}" id))) :to-equal "n")
      (expect (tx-meta-name (decode-tx-meta (format nil "{\"tx\":~S,\"name\":null,\"created\":\"t\"}" id))) :to-be nil))))
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
