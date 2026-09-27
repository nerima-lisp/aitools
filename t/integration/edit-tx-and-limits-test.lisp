;;;; t/integration/edit-tx-and-limits-test.lisp
;;;;
;;;; Edit writes inside a tx: staging, `tx rebase` replays and scans that see
;;;; staged files; the remaining undo cases; further positions, formats and
;;;; limits; and the mktemp cleanup.
(in-package #:aitools.edit.test)

(describe "aitools edit writes inside a tx"
  (it "stages without touching the working tree, checks hashes against the tx, and commits"
    (with-workspace ()
      (put "a.txt" (format nil "one~%"))
      (let ((tx (begin-tx))
            (before (snapshot)))
        (multiple-value-bind (kind fields) (run-in tx "edit" '("a.txt") :old "one" :new "two")
          (expect kind :to-be :ok)
          (expect (field fields "tx") :to-equal tx)
          (expect (field fields "tx_op") :to-be 1))
        (expect-unchanged before)
        (with-error (code) (run-in tx "edit" '("a.txt") :range "1" :new "x" :expect-hash (list (hash "a.txt")))
          (expect code :to-equal "refusal.target-changed"))
        (expect (run-in tx "edit" '("a.txt") :range "1" :new "three"
                           :expect-hash (list (aitools.kernel.domain:content-hash (bytes (format nil "two~%")))))
                :to-be :ok)
        (expect (commit-tx tx) :to-be :committed)
        (expect (text "a.txt") :to-equal (format nil "three~%")))))

  (it "replays a content-based op on the moved base during tx rebase"
    (with-workspace ()
      (put "a.txt" (format nil "head~%x = 1~%"))
      (let ((tx (begin-tx)))
        (expect (run-in tx "edit" '("a.txt") :old "x = 1" :new "x = 2") :to-be :ok)
        (put "a.txt" (format nil "new head~%head~%x = 1~%"))
        (expect (aitools.store.application:tx-rebase/k (open-store *root*) tx #'replay-record
                                                       :on-rebased (lambda (paths) paths)
                                                       :on-conflict (lambda (conflicts) (declare (ignore conflicts)) :conflict)
                                                       :on-not-found (lambda () :not-found)
                                                       :on-busy (lambda () :busy))
                :to-equal '("a.txt"))
        (expect (commit-tx tx) :to-be :committed)
        (expect (text "a.txt") :to-equal (format nil "new head~%head~%x = 2~%")))))

  (it "does not replay a position-based op"
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (let ((tx (begin-tx)))
        (expect (run-in tx "edit" '("a.txt") :range "1" :new "b" :expect-hash (list (hash "a.txt"))) :to-be :ok)
        (put "a.txt" (format nil "c~%"))
        (expect (aitools.store.application:tx-rebase/k (open-store *root*) tx #'replay-record
                                                       :on-rebased (lambda (paths) paths)
                                                       :on-conflict (lambda (conflicts) (declare (ignore conflicts)) :conflict)
                                                       :on-not-found (lambda () :not-found)
                                                       :on-busy (lambda () :busy))
                :to-be :conflict))))

  (it "records a canonical argv that parses back to the same command"
    (let ((argv (options-argv "json.set" '("c.json" "/a" "-1") '(:expect-hash ("h") :dry-run t))))
      (expect argv :to-equal '("json" "set" "--expect-hash" "h" "--" "c.json" "/a" "-1"))
      (expect (parse-recorded-argv argv :on-parsed #'list :on-invalid #'identity)
              :to-equal '("json.set" ("c.json" "/a" "-1") (:expect-hash ("h")))))))

(describe "aitools edit commands: remaining undo cases"
  (it "restores bytes, modes and existence after apply, json writes, mkdir, overwrites, copy and move of files"
    (with-workspace ()
      (put "src/a.txt" (format nil "one~%two~%three~%"))
      (put "c.json" (format nil "{\"a\": 1, \"b\": [1]}~%"))
      (put "x.sh" "x" :mode #o755)
      (let ((before (snapshot)))
        (dolist (call (list (list "apply" '() :stdin-data +git-diff+)
                            (list "json.delete" '("c.json" "/a"))
                            (list "json.merge" '("c.json") :stdin-data "{\"z\": 2}")
                            (list "json.patch" '("c.json") :stdin-data "[{\"op\":\"add\",\"path\":\"/b/0\",\"value\":0}]")
                            (list "json.fmt" '("c.json") :indent "4")
                            (list "mkdir" '("new/dir"))
                            (list "write" '("x.sh") :content '("y") :overwrite t :expect-hash (list (hash "x.sh")))
                            (list "copy" '("x.sh" "copied.sh"))
                            (list "move" '("x.sh" "moved.sh"))))
          (multiple-value-bind (kind fields) (apply #'run call)
            (expect kind :to-be :ok)
            (expect (undo (field fields "op_id")) :to-be :committed)
            (expect (snapshot) :to-equal before)))))))

(describe "aitools edit commands: more positions, formats and limits"
  (it "moves lines to the end and after a symbol"
    (with-workspace ()
      (put "a.lisp" (format nil "(defun a ()~%  1)~%;; note~%(defun b ()~%  2)~%"))
      (expect (run "move-lines" '("a.lisp") :match "^;; note" :expect-count "1" :to-position "end") :to-be :ok)
      (expect (text "a.lisp") :to-equal (format nil "(defun a ()~%  1)~%(defun b ()~%  2)~%;; note~%"))
      (expect (run "move-lines" '("a.lisp") :match "^;; note" :expect-count "1" :to-position "after-symbol:a"
                                            :expect-hash (list (hash "a.lisp")))
              :to-be :ok)
      (expect (text "a.lisp") :to-equal (format nil "(defun a ()~%  1)~%;; note~%(defun b ()~%  2)~%"))))

  (it "rejects malformed durations and sizes"
    (with-workspace ()
      (put "a.txt" "a")
      (let ((result nil))
        (run-edit-command *ports* "edit" '("a.txt") '(:old "a" :new "b") :root *root* :lock-timeout "-1s"
                          :on-ok (lambda (fields) (setf result fields))
                          :on-error (lambda (code &rest rest) (declare (ignore rest)) (setf result code)))
        (expect result :to-equal "argument.invalid"))
      (with-error (code) (run "copy" '("a.txt" "b.txt") :max-bytes "10XB") (expect code :to-equal "argument.invalid"))
      (expect (text "a.txt") :to-equal "a")))

  (it "reports environment.busy with the same command as repair when the lock is held"
    (with-workspace ()
      (put "a.txt" "a")
      (let ((result nil))
        ;; The store's lock is reentrant within one dynamic extent, so hold
        ;; the flock through the port directly, as another process would.
        (let* ((store (open-store *root*))
               (io (aitools.store.application:store-io-port store))
               (path (aitools.store.domain:lock-file-path (aitools.store.application:store-state-directory store))))
          (aitools.store.application:call-with-workspace-lock/k store 1000 :on-acquired (lambda () nil)
                                                                          :on-timeout (lambda () nil))
          (let ((handle (funcall (aitools.store.application:store-io-try-lock io) path :create t)))
            (unwind-protect
                 (run-edit-command *ports* "edit" '("a.txt") '(:old "a" :new "b") :root *root* :lock-timeout "20ms"
                                   :display-argv '("edit" "a.txt" "--old" "a" "--new" "b")
                                   :on-ok (lambda (fields) (setf result fields))
                                   :on-error (lambda (code message &key repairs &allow-other-keys)
                                               (declare (ignore message))
                                               (setf result (list code (getf (first repairs) :command)))))
              (funcall (aitools.store.application:store-io-unlock io) handle))))
        (expect result :to-equal '("environment.busy" "aitools edit a.txt --old a --new b"))
        (expect (text "a.txt") :to-equal "a"))))

  (it "plans against the tx with --dry-run --tx, and stages a directory move per path"
    (with-workspace ()
      (put "d/a.txt" "a")
      (put "d/e/b.txt" "b")
      (let ((tx (begin-tx)))
        (multiple-value-bind (kind fields) (run-in tx "edit" '("d/a.txt") :old "a" :new "z" :dry-run t)
          (expect kind :to-be :ok)
          (expect (field fields "dry_run") :to-be t))
        (expect (run-in tx "move" '("d" "m")) :to-be :ok)
        (expect (kind "d/a.txt") :to-be :file)
        (expect (commit-tx tx) :to-be :committed)
        (expect (text "m/e/b.txt") :to-equal "b")
        (expect (kind "d") :to-be :absent))))

  (it "splits at matching lines and by bytes"
    (with-workspace ()
      (put "log.txt" (format nil "== a~%1~%== b~%2~%"))
      (multiple-value-bind (kind fields) (run "split" '("log.txt") :at-match "^==" :prefix "part-" :suffix-digits "2")
        (expect kind :to-be :ok)
        (expect (changes fields) :to-equal '(("part-01" "created") ("part-02" "created"))))
      (expect (text "part-02") :to-equal (format nil "== b~%2~%"))
      (expect (run "split" '("log.txt") :bytes "5" :prefix "b-") :to-be :ok)
      (expect (text "b-001") :to-equal (format nil "== a~%")))))

(describe "aitools mktemp cleanup"
  (it "removes tmp/ entries older than 7 days and keeps newer ones"
    (with-workspace ()
      (multiple-value-bind (kind old) (run "mktemp" '() :dir t)
        (declare (ignore kind))
        (multiple-value-bind (kind recent) (run "mktemp" '())
          (declare (ignore kind))
          (let ((old-path (field old "path"))
                (recent-path (field recent "path"))
                (eight-days-ago (- (aitools.edit.infrastructure:unix-now) (* 8 24 60 60))))
            (with-open-file (out (concatenate 'string old-path "/inner") :direction :output) (write-string "x" out))
            (sb-posix:utimes old-path eight-days-ago eight-days-ago)
            (run "mktemp" '())
            (expect (probe-file (concatenate 'string old-path "/")) :to-be nil)
            (expect (probe-file recent-path) :to-be-truthy)))))))

(describe "aitools replace inside a tx"
  (it "finds files the tx created when it scans"
    (with-workspace ()
      (put "a.txt" (format nil "foo~%"))
      (let ((tx (begin-tx)))
        (expect (run-in tx "write" '("staged.txt") :content (list (format nil "foo~%"))) :to-be :ok)
        (multiple-value-bind (kind fields) (run-in tx "replace" '("foo" "bar") :expect-count "2")
          (expect kind :to-be :ok)
          (expect (changes fields) :to-equal '(("a.txt" "modified") ("staged.txt" "modified"))))
        (expect (kind "staged.txt") :to-be :absent)
        (expect (commit-tx tx) :to-be :committed)
        (expect (text "staged.txt") :to-equal (format nil "bar~%"))))))

(defun replay-argv (argv)
  "REPLAY-EDIT-OP of ARGV against the workspace on disk: (:commit paths) or
(:reject code message)."
  (replay-edit-op argv (aitools.store.application:disk-view (open-store *root*))
                  (lambda (requests) (list :commit (mapcar #'aitools.store.domain:change-request-path requests)))
                  (lambda (code message &rest keys) (declare (ignore keys)) (list :reject code message))))

(describe "aitools tx rebase replays only replayable recorded ops"
  (it-each (("an argv naming no edit command" ("frobnicate" "x") "argument.invalid" "no edit command")
            ("write, which is never replayed" ("write" "--content" "x" "a.txt") "argument.invalid" "write cannot be replayed")
            ("archive create, which is never replayed" ("archive" "create" "o.tar" "a.txt") "argument.invalid" "archive.create cannot be replayed")
            ("touch, which is never replayed" ("touch" "a.txt") "argument.invalid" "touch cannot be replayed")
            ("a position-based edit" ("edit" "--range" "1" "--new" "x" "a.txt") "refusal.target-changed" "position-based")
            ("an op its command refuses" ("edit" "a.txt") "argument.invalid" "edit needs --old")
            ("a value option at the end" ("edit" "a.txt" "--old") "argument.invalid" "--old needs a value")
            ("a repeatable option at the end" ("edit" "--old" "a" "--new" "b" "a.txt" "--expect-hash") "argument.invalid" "--expect-hash needs a value")
            ("a two-value option with one value" ("edit" "a.txt" "--between" "x") "argument.invalid" "--between needs two values")
            ("a damaged record asking for stdin" ("json" "set" "--stdin" "c.json") "environment.unavailable" "standard input")
            ("a bare command word" ("edit") "argument.invalid" "edit takes exactly one path")
            ("a hash-guarded edit" ("edit" "--old" "a" "--new" "b" "--expect-hash" "h" "a.txt") "refusal.target-changed" "edit is position-based")
            ("a hash-guarded insert" ("insert" "--match" "a" "--after" "--content" "x" "--expect-hash" "h" "a.txt") "refusal.target-changed" "insert is position-based")
            ("a hash-guarded replace" ("replace" "--expect-hash" "h" "a" "b" "a.txt") "refusal.target-changed" "replace is position-based")
            ("a hash-guarded apply" ("apply" "--stdin-data" "--- a/a.txt
+++ b/a.txt
@@ -1 +1 @@
-a
+b
" "--expect-hash" "h") "refusal.target-changed" "apply is position-based")
            ("a hash-guarded transform" ("transform" "--op" "sort" "--expect-hash" "h" "a.txt") "refusal.target-changed" "transform is position-based")
            ("a hash-guarded move-lines" ("move-lines" "--match" "a" "--to-position" "end" "--expect-hash" "h" "a.txt") "refusal.target-changed" "move-lines is position-based")
            ("a hash-guarded json set" ("json" "set" "--expect-hash" "h" "c.json" "/a" "1") "refusal.target-changed" "json.set is position-based")
            ("a hash-guarded json delete" ("json" "delete" "--expect-hash" "h" "c.json" "/a") "refusal.target-changed" "json.delete is position-based")
            ("a hash-guarded json merge" ("json" "merge" "--stdin-data" "{}" "--expect-hash" "h" "c.json") "refusal.target-changed" "json.merge is position-based")
            ("a hash-guarded json fmt" ("json" "fmt" "--expect-hash" "h" "c.json") "refusal.target-changed" "json.fmt is position-based")
            ("a transform by line range" ("transform" "--op" "sort" "--range" "1" "a.txt") "refusal.target-changed" "transform is position-based"))
      "rejects ~A"
      (name argv code fragment)
    (declare (ignore name))
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (put "c.json" "{}")
      (destructuring-bind (kind actual-code message) (replay-argv argv)
        (expect kind :to-be :reject)
        (expect actual-code :to-equal code)
        (expect (search fragment message) :to-be-truthy))))

  (it "re-runs a content-based op against the view, reading its recorded stdin input"
    (with-workspace ()
      (put "c.json" "{\"a\":1}")
      (expect (replay-argv '("json" "set" "--stdin-data" "{\"pointer\":\"/a\",\"value\":2}" "c.json"))
              :to-equal '(:commit ("c.json")))
      (expect (replay-argv '("edit" "--between" "^a$" "^c$" "--exclusive" "--new" "B" "x.txt"))
              :to-equal (list :reject "input.not-found" "x.txt does not exist"))))

  (it "parses a flag, a two-word command it does not know as one word, and positionals after --"
    (destructuring-bind (name positionals options)
        (parse-recorded-argv '("edit" "--exclusive" "--between" "a" "b" "--new" "x" "--" "-f")
                             :on-parsed #'list :on-invalid #'identity)
      (expect (list name positionals) :to-equal '("edit" ("-f")))
      (expect (list (getf options :exclusive) (getf options :between) (getf options :new))
              :to-equal '(t ("a" "b") "x")))
    (expect (parse-recorded-argv '("edit" "json") :on-parsed #'list :on-invalid #'identity)
            :to-equal '("edit" ("json") ())))

  (it "records a command outside the table in its options' order"
    (expect (options-argv "decode.to" '("x") '(:force t :level "3" :dry-run t :absent nil))
            :to-equal '("decode" "to" "--force" "--level" "3" "x"))
    (expect (options-argv "edit" '("") '(:old "a")) :to-equal '("edit" "--old" "a" "")))

  (it "replays a replace op on the moved base"
    (with-workspace ()
      (put "a.txt" (format nil "x = 1~%"))
      (let ((tx (begin-tx)))
        (expect (run-in tx "replace" '("x = 1" "x = 2" "a.txt") :expect-count "1") :to-be :ok)
        (put "a.txt" (format nil "head~%x = 1~%"))
        (expect (aitools.store.application:tx-rebase/k (open-store *root*) tx #'replay-record
                                                       :on-rebased (lambda (paths) paths)
                                                       :on-conflict (lambda (conflicts) (declare (ignore conflicts)) :conflict)
                                                       :on-not-found (lambda () :not-found)
                                                       :on-busy (lambda () :busy))
                :to-equal '("a.txt"))
        (expect (commit-tx tx) :to-be :committed)
        (expect (text "a.txt") :to-equal (format nil "head~%x = 2~%"))))))
