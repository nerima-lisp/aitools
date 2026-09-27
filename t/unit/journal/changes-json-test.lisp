;;;; t/unit/journal/changes-json-test.lisp
;;;;
;;;; The shared write-output renderer in AITOOLS.STORE.DOMAIN (changes-json.lisp),
;;;; which the journal writer owns: `changes[]`, per-change diff truncation,
;;;; the op_id / tx / dry_run variants and `next_commands`, and `recovered[]`.
(in-package #:aitools.journal.test)

(defun %result (path action before-text after-text &key from binary)
  "A CHANGE-RESULT from BEFORE-TEXT to AFTER-TEXT (NIL = absent)."
  (flet ((octets (text) (and text (if binary (concatenate 'aitools.store.domain:octets (bytes text) #(0)) (bytes text))))
         (state (octets) (if octets
                             (aitools.store.domain:file-state (aitools.kernel.domain:content-hash octets) #o644)
                             (aitools.store.domain:absent-state))))
    (let ((before (octets before-text)) (after (octets after-text)))
      (aitools.store.domain:make-change-result
       :path path :action action :from from
       :before (state before) :after (state after)
       :before-content before :after-content after))))

(defun %lines (count)
  (format nil "~{line ~D~%~}" (loop for i from 1 to count collect i)))

(describe "aitools.store changes->json"
  (it "renders path, action, hashes and a hunk-only diff"
    (let* ((result (%result "a.txt" :modified (format nil "one~%two~%") (format nil "one~%2~%")))
           (change (first (aitools.store.domain:changes->json (list result)))))
      (expect (mapcar #'car (json-alist change))
              :to-equal '("path" "action" "hash_before" "hash_after" "diff"))
      (expect (member-value change "action") :to-equal "modified")
      (expect (member-value change "hash_before")
              :to-equal (aitools.kernel.domain:content-hash (bytes (format nil "one~%two~%"))))
      (expect (member-value change "diff")
              :to-equal (format nil "@@ -1,2 +1,2 @@~% one~%-two~%+2~%"))))

  (it "diffs a created file against nothing and marks a missing final newline"
    (let ((change (first (aitools.store.domain:changes->json (list (%result "n.txt" :created nil "x"))))))
      (expect (member-value change "hash_before") :to-be json-kit:+json-null+)
      (expect (member-value change "diff")
              :to-equal (format nil "@@ -0,0 +1,1 @@~%+x~%\\ No newline at end of file~%"))))

  (it "shows an LF to CRLF conversion as a change"
    (let ((change (first (aitools.store.domain:changes->json
                          (list (%result "e.txt" :modified (format nil "a~%") (format nil "a~C~%" #\Return)))))))
      (expect (search "+a" (member-value change "diff")) :to-be-truthy)))

  (it "omits the diff for binary content, moves and mode changes, and keeps from for a move"
    (let* ((binary (%result "b.bin" :modified "a" "b" :binary t))
           (moved (%result "to.txt" :moved nil "same" :from "from.txt"))
           (changes (aitools.store.domain:changes->json (list binary moved))))
      (expect (member-value (first changes) "diff" :none) :to-be :none)
      (expect (member-value (second changes) "diff" :none) :to-be :none)
      (expect (member-value (second changes) "from") :to-equal "from.txt")
      (expect (member-value (second changes) "action") :to-equal "moved")))

  (it "cuts a diff at --max-diff-lines and reports the longest cut"
    (multiple-value-bind (changes longest)
        (aitools.store.domain:changes->json (list (%result "big.txt" :created nil (%lines 10))) :max-diff-lines 4)
      (let ((diff (member-value (first changes) "diff")))
        (expect (count #\Newline diff) :to-be 4)
        (expect (member-value (first changes) "diff_truncated") :to-be t)
        ;; hunk header + 10 added lines
        (expect longest :to-be 11))))

  (it "leaves a diff within the limit whole and unmarked"
    (multiple-value-bind (changes longest)
        (aitools.store.domain:changes->json (list (%result "s.txt" :created nil (%lines 3))))
      (expect (member-value (first changes) "diff_truncated" :none) :to-be :none)
      (expect longest :to-be nil))))

(defun %big () (list (%result "big.txt" :created nil (%lines 300))))

(describe "aitools.store write-result-fields"
  (it "gives op_id and points a cut diff at aitools diff --op"
    (let ((fields (aitools.store.domain:write-result-fields (%big) :op-id "op-x")))
      (expect (mapcar #'car fields) :to-equal '("changes" "op_id" "next_commands"))
      (expect (field fields "next_commands") :to-equal '("aitools diff --op op-x"))))

  (it "gives tx and tx_op for a --tx write and points at tx diff"
    (let ((fields (aitools.store.domain:write-result-fields (%big) :tx "tx-1" :tx-op 3)))
      (expect (mapcar #'car fields) :to-equal '("changes" "tx" "tx_op" "next_commands"))
      (expect (field fields "tx_op") :to-be 3)
      (expect (field fields "next_commands") :to-equal '("aitools tx diff tx-1 --max-diff-lines 301"))))

  (it "gives dry_run without op_id or next_commands"
    (let ((fields (aitools.store.domain:write-result-fields (%big) :dry-run t)))
      (expect (mapcar #'car fields) :to-equal '("changes" "dry_run"))
      (expect (field fields "dry_run") :to-be t)))

  (it "omits next_commands when nothing was cut"
    (expect (mapcar #'car (aitools.store.domain:write-result-fields
                           (list (%result "a" :created nil "a")) :op-id "op-x"))
            :to-equal '("changes" "op_id"))))

(describe "aitools.store recovered->json"
  (it "renders recover/k's entries as {op_id, action}"
    (expect (mapcar #'json-alist (aitools.store.domain:recovered->json '(("op-1" . "rolled-forward"))))
            :to-equal '((("op_id" . "op-1") ("action" . "rolled-forward"))))))
