;;;; t/unit/store/domain-test.lisp
(in-package #:aitools.store.test)

(describe "aitools.store.domain layout"
  (it "prefers an absolute XDG_STATE_HOME and falls back to ~/.local/state"
    (expect (state-home "/x/state" "/home/u") :to-equal "/x/state/aitools")
    (expect (state-home "relative" "/home/u") :to-equal "/home/u/.local/state/aitools")
    (expect (state-home nil "/home/u") :to-equal "/home/u/.local/state/aitools"))

  (it "derives the workspace id from the directory name and the real path's hash"
    (let ((a (workspace-id "/srv/a/proj"))
          (b (workspace-id "/srv/b/proj")))
      (expect (subseq a 0 5) :to-equal "proj-")
      (expect (length a) :to-be 21)
      (expect a :not :to-equal b)
      (expect (workspace-id "/srv/a/proj/") :to-equal a)))

  (it "replaces unsafe characters so the id is one path component"
    (expect (every (lambda (char) (or (alphanumericp char) (find char "._-")))
                   (workspace-id "/tmp/my dir:x"))
            :to-be-truthy))

  (it "accepts only clean workspace-relative paths"
    (dolist (good '("a" "a/b.txt" ".hidden" "a/.b"))
      (expect (valid-relative-path-p good) :to-be-truthy))
    (dolist (bad (list "" "/a" "a//b" "a/" "./a" "a/../b" ".." (format nil "a~Cb" (code-char 0))))
      (expect (valid-relative-path-p bad) :to-be-falsy)))

  (it "checks op and tx ids exactly before they become path components"
    (let ((op (format-op-id 3964000000 "0123abcd")))
      (expect (valid-op-id-p op) :to-be-truthy)
      (expect (valid-tx-id-p op) :to-be-falsy)
      (expect (valid-op-id-p (concatenate 'string op "/..")) :to-be-falsy)
      (expect (valid-op-id-p "op-../../etc") :to-be-falsy)
      (expect (valid-tx-id-p (format-tx-id 3964000000 "0123abcd")) :to-be-truthy)))

  (it "gives the composition root the same state directory a store uses"
    (expect (state-directory-for-root "/srv/proj/" :xdg-state-home "/x/state" :home "/home/u")
            :to-equal (concatenate 'string "/x/state/aitools/" (workspace-id "/srv/proj")))
    (expect (state-directory-for-root "/srv/proj" :home "/home/u")
            :to-equal (store-state-directory (make-store (make-posix-store-io) "/srv/proj/"
                                                         "/home/u/.local/state/aitools"))))

  (it "recognises the write protocol's temp file names"
    (expect (temp-file-name-p (temp-file-name "op-x" 3)) :to-be-truthy)
    (expect (temp-file-name-p "notes.tmp") :to-be-falsy)))

;;; Planner over an in-memory view.

(defun make-lookups (entries)
  "ENTRIES: alist of (path . value); VALUE is an ENTRY-STATE, a content string
(a regular file, mode 644), or (content-string mode). Returns plist keyword args for PLAN-CHANGES/K."
  (flet ((state-of (value)
           (typecase value
             (string (file-state (aitools.kernel.domain:content-hash (bytes value)) #o644))
             (cons (file-state (aitools.kernel.domain:content-hash (bytes (first value))) (second value)))
             (t value))))
    (list :lookup-state (lambda (path)
                          (let ((entry (assoc path entries :test #'string=)))
                            (if entry (state-of (cdr entry)) (absent-state))))
          :lookup-content (lambda (path)
                            (let ((value (cdr (assoc path entries :test #'string=))))
                              (bytes (if (consp value) (first value) value))))
          :list-children (lambda (path)
                           (loop for (child) in entries
                                 when (string= (parent-relative-path child) path) collect child)))))

(defun plan (entries requests)
  (apply #'plan-changes/k requests
         :on-planned (lambda (results) (values :planned results))
         :on-rejected (lambda (code message) (values :rejected code message))
         (make-lookups entries)))

(describe "aitools.store.domain plan-changes/k"
  (it "classifies each request as a write-output action"
    (multiple-value-bind (status results)
        (plan (list (cons "old.txt" "x") (cons "keep.txt" "k") (cons "gone.txt" "g") (cons "mv.txt" "m"))
              (list (write-file-request "new.txt" (bytes "n"))
                    (write-file-request "keep.txt" (bytes "k2"))
                    (delete-request "gone.txt")
                    (move-request "mv.txt" "moved.txt")
                    (chmod-request "old.txt" #o755)
                    (symlink-request "link" "old.txt")))
      (expect status :to-be :planned)
      (expect (actions results)
              :to-equal '(("new.txt" :created) ("keep.txt" :modified) ("gone.txt" :deleted)
                          ("moved.txt" :moved) ("old.txt" :mode-changed) ("link" :linked)))))

  (it "creates missing parents as directory results before the file"
    (multiple-value-bind (status results) (plan '() (list (write-file-request "a/b/c.txt" (bytes "x"))))
      (expect status :to-be :planned)
      (expect (actions results) :to-equal '(("a" :created) ("a/b" :created) ("a/b/c.txt" :created)))))

  (it "keeps the existing mode on modify and uses 644 for a new file"
    (multiple-value-bind (status results)
        (plan (list (cons "x" (list "a" #o755)))
              (list (write-file-request "x" (bytes "b")) (write-file-request "y" (bytes "c"))))
      (declare (ignore status))
      (expect (mapcar (lambda (r) (entry-state-mode (change-result-after r))) results) :to-equal '(#o755 #o644))))

  (it "yields no result for mkdir of an existing directory"
    (multiple-value-bind (status results) (plan (list (cons "d" (directory-state #o755))) (list (mkdir-request "d")))
      (expect status :to-be :planned)
      (expect results :to-equal '())))

  (it "refuses what the protocol cannot apply"
    (expect (nth-value 1 (plan '() (list (delete-request "missing")))) :to-equal "input.not-found")
    (expect (nth-value 1 (plan (list (cons "d" (directory-state)) (cons "d/f" "x")) (list (delete-request "d"))))
            :to-equal "refusal.not-a-file")
    (expect (nth-value 1 (plan (list (cons "f" "x")) (list (write-file-request "f/g" (bytes "y")))))
            :to-equal "refusal.not-a-file")
    (expect (nth-value 1 (plan (list (cons "d" (directory-state)) (cons "e" "x")) (list (move-request "e" "d"))))
            :to-equal "refusal.exists")
    (expect (nth-value 1 (plan '() (list (write-file-request "../x" (bytes "y"))))) :to-equal "argument.invalid")
    (expect (nth-value 1 (plan '() (list (write-file-request "x" (bytes "1")) (write-file-request "x" (bytes "2")))))
            :to-equal "argument.invalid"))

  (it "sees earlier requests: a directory emptied by the same op may be deleted"
    (multiple-value-bind (status results)
        (plan (list (cons "d" (directory-state)) (cons "d/f" "x"))
              (list (delete-request "d/f") (delete-request "d")))
      (expect status :to-be :planned)
      (expect (actions results) :to-equal '(("d/f" :deleted) ("d" :deleted)))))

  (it "lets a move's source be rewritten, but nothing else twice"
    (expect (plan (list (cons "a" "1") (cons "b" "2"))
                  (list (move-request "b" "a") (write-file-request "b" (bytes "3"))))
            :to-be :planned)
    (expect (nth-value 1 (plan (list (cons "a" "1")) (list (move-request "a" "b") (mkdir-request "a"))))
            :to-equal "argument.invalid")))

(describe "aitools.store.domain changes->steps"
  (it "orders mkdir, move, file steps, then rmdir children-first"
    (multiple-value-bind (status results)
        (plan (list (cons "z" (directory-state)) (cons "z/y" (directory-state)) (cons "m" "m"))
              (list (delete-request "z/y") (delete-request "z")
                    (write-file-request "n/f.txt" (bytes "x"))
                    (move-request "m" "k")))
      (declare (ignore status))
      (let ((steps (changes->steps "op-t" results (lambda (path) (declare (ignore path)) nil))))
        (expect (mapcar (lambda (s) (list (intent-step-op s) (intent-step-path s))) steps)
                :to-equal '((:mkdir "n") (:move "k") (:replace "n/f.txt") (:rmdir "z/y") (:rmdir "z"))))))

  (it "puts a temp file in the nearest existing directory"
    (multiple-value-bind (status results) (plan '() (list (write-file-request "a/b/c" (bytes "x"))))
      (declare (ignore status))
      (let ((replace (find :replace (changes->steps "op-t" results
                                                    (lambda (path) (string= path "a")))
                           :key #'intent-step-op)))
        ;; "a" exists on disk in this lookup, "a/b" does not.
        (expect (intent-step-temp replace) :to-equal (concatenate 'string "a/" (temp-file-name "op-t" 1)))))))

(defun sample-intent (op-id)
  (make-intent :op-id op-id
               :steps (list (make-intent-step :op :replace :path "a.txt"
                                              :temp (temp-file-name op-id 1) :kind :file :mode #o644))
               :journal-entry (make-journal-entry :op-id op-id :argv '("write" "a.txt") :time "t"
                                                  :changes '())))

(describe "aitools.store.domain intent record"
  (it "is complete only with both lines and a matching checksum"
    (let* ((op-id (format-op-id 3964000000 "00000001"))
           (header (encode-intent-header (sample-intent op-id)))
           (full (format nil "~A~%~A~%" header (encode-intent-checksum header))))
      (expect (decode-intent full) :to-be :complete)
      (expect (intent-op-id (nth-value 1 (decode-intent full))) :to-equal op-id)
      (multiple-value-bind (status intent) (decode-intent (format nil "~A~%" header))
        (expect status :to-be :incomplete)
        (expect (intent-step-temp (first (intent-steps intent))) :to-equal (temp-file-name op-id 1)))
      (expect (decode-intent (subseq full 0 (- (length full) 3))) :to-be :incomplete)
      (expect (decode-intent (subseq header 0 20)) :to-be :incomplete)
      (expect (nth-value 1 (decode-intent (subseq header 0 20))) :to-be nil)
      (expect (decode-intent (format nil "~A~%{\"checksum\":\"00\"}~%" header)) :to-be :incomplete)))

  (it "rejects a temp name that is not a store temp file"
    (let* ((op-id (format-op-id 3964000000 "00000001"))
           (header (encode-intent-header
                    (make-intent :op-id op-id
                                 :steps (list (make-intent-step :op :replace :path "a" :temp "a" :kind :file))
                                 :journal-entry (make-journal-entry :op-id op-id :argv '() :time "t" :changes '())))))
      ;; An edited record must not make recovery unlink an arbitrary file.
      (expect (nth-value 1 (decode-intent (format nil "~A~%" header))) :to-be nil))))

(defun entry-touching (n &rest paths)
  (make-journal-entry :op-id (format-op-id (+ 3964000000 n) "00000000") :argv '() :time "t"
                      :changes (mapcar (lambda (path)
                                         (make-change-result :path path :action :modified
                                                             :before (absent-state) :after (absent-state)))
                                       paths)))

(describe "aitools.store.domain journal retention"
  (it "drops an op once each of its paths has 20 newer generations"
    (let* ((entries (loop for n from 0 below 21 collect (entry-touching n "a"))))
      (expect (retention-removals entries) :to-equal (list (journal-entry-op-id (first entries))))
      (expect (retention-removals (rest entries)) :to-equal '())))

  (it "keeps an op while any of its paths is still recent"
    (let* ((first (entry-touching 0 "a" "b"))
           (entries (cons first (loop for n from 1 to 25 collect (entry-touching n "a")))))
      (expect (member (journal-entry-op-id first) (retention-removals entries) :test #'string=) :to-be-falsy)))

  (it "round-trips entries through ops.jsonl text"
    (let* ((entry (make-journal-entry
                   :op-id (format-op-id 3964000000 "0000abcd") :argv '("move" "a" "b") :time "2026-01-01T00:00:00Z"
                   :changes (list (make-change-result :path "b" :action :moved :from "a"
                                                      :before (absent-state) :after (symlink-state "x")
                                                      :source-before (symlink-state "x")))
                   :undoes nil))
           (decoded (first (decode-journal (encode-journal (list entry))))))
      (expect (journal-entry-argv decoded) :to-equal '("move" "a" "b"))
      (expect (change-result-from (first (journal-entry-changes decoded))) :to-equal "a")))

  (it "fails closed on a damaged line"
    (signals store-format-error (decode-journal (format nil "{\"op_id\":\"../x\"}~%"))))

  (it "drops a truncated final line, keeping the entries that parse"
    ;; With per-op O_APPEND, a crash mid-append can leave a partial last line
    ;; with no terminating newline; DECODE-JOURNAL keeps only complete lines.
    (let* ((entries (list (entry-touching 0 "a") (entry-touching 1 "b")))
           (torn (concatenate 'string (encode-journal entries) "{\"op_id\":\"op-2026")))
      (expect (mapcar #'journal-entry-op-id (decode-journal torn))
              :to-equal (mapcar #'journal-entry-op-id entries)))))

(describe "aitools.store.domain tx model"
  (it "restores previous staged states newest first on drop"
    (let ((index (make-tx-index :last-tx-op 2))
          (a0 (file-state (make-string 64 :initial-element #\a) #o644))
          (a1 (file-state (make-string 64 :initial-element #\b) #o644))
          (a2 (file-state (make-string 64 :initial-element #\c) #o644)))
      (tx-index-put index "a" a0 a2)
      (tx-index-put index "b" (absent-state) (directory-state))
      (let* ((ops (list (make-tx-op-record :tx-op 1 :argv '() :paths '("a") :previous (list (cons "a" nil))
                                           :after (list (cons "a" a1)) :time "t")
                        (make-tx-op-record :tx-op 2 :argv '() :paths '("a" "b")
                                           :previous (list (cons "a" a1) (cons "b" nil))
                                           :after (list (cons "a" a2) (cons "b" (directory-state))) :time "t")))
             (dropped-2 (tx-drop-index index ops 2))
             (dropped-1 (tx-drop-index index ops 1)))
        (expect (entry-state-equal (tx-path-staged (tx-index-find dropped-2 "a")) a1) :to-be-truthy)
        (expect (tx-index-find dropped-2 "b") :to-be nil)
        (expect (tx-index-last-tx-op dropped-2) :to-be 1)
        (expect (hash-table-count (tx-index-paths dropped-1)) :to-be 0)
        ;; The original index is not modified.
        (expect (entry-state-equal (tx-path-staged (tx-index-find index "a")) a2) :to-be-truthy))))

  (it "reports write conflicts before read conflicts, once per path"
    (let ((index (make-tx-index))
          (base (file-state (make-string 64 :initial-element #\a) #o644))
          (moved (file-state (make-string 64 :initial-element #\f) #o644)))
      (tx-index-put index "w" base (absent-state))
      (let ((conflicts (tx-commit-conflicts index (list (cons "w" base) (cons "r" base))
                                            (lambda (path) (declare (ignore path)) moved))))
        (expect (mapcar (lambda (c) (list (conflict-path c) (conflict-kind c))) conflicts)
                :to-equal '(("w" :write) ("r" :read))))
      (expect (length (tx-commit-conflicts index (list (cons "r" base))
                                           (lambda (path) (declare (ignore path)) moved)
                                           :ignore-stale-reads t))
              :to-be 1))))

;;; Record decoders fail closed: each malformed shape names its defect.

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
            :to-be 7)))

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
