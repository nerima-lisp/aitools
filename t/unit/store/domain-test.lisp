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


