(in-package #:aitools.edit.test)

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
    (with-edit-workspace ()
      (put "a.txt" (format nil "a~%"))
      (put "c.json" "{}")
      (destructuring-bind (kind actual-code message) (replay-argv argv)
        (expect kind :to-be :reject)
        (expect actual-code :to-equal code)
        (expect (search fragment message) :to-be-truthy))))

  (it "re-runs a content-based op against the view, reading its recorded stdin input"
    (with-edit-workspace ()
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
    (with-edit-workspace ()
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
