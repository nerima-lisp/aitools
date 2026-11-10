(in-package #:aitools.edit.test)
(describe "aitools archive creation from transaction overlays"
  (it "archives staged additions and replacements and omits staged deletions"
    (call-with-edit-workspace
     (lambda ()
       (put "src/replaced.txt" "old")
       (put "src/deleted.txt" "delete")
       (let ((tx (begin-tx)))
         (expect
          (run-in tx "write" '("src/replaced.txt") :content '("new") :overwrite t
                  :expect-hash (list (format nil "src/replaced.txt=~A"
                                             (hash "src/replaced.txt"))))
          :to-be :ok)
         (expect (run-in tx "write" '("src/added.txt") :content '("added")) :to-be :ok)
         (expect (run-in tx "rm" '("src/deleted.txt")) :to-be :ok)
         (expect (run-in tx "archive.create" '("snapshot.tar" "src") :format "tar")
                 :to-be :ok)
         (expect (run-in tx "archive.extract" '("snapshot.tar") :to "unpacked")
                 :to-be :ok)
         (expect (text "src/replaced.txt") :to-equal "old")
         (expect (text "src/deleted.txt") :to-equal "delete")
         (expect (kind "src/added.txt") :to-be :absent)
         (expect (kind "snapshot.tar") :to-be :absent)
         (expect (kind "unpacked") :to-be :absent)
         (expect (commit-tx tx) :to-be :committed)
         (expect (text "src/replaced.txt") :to-equal "new")
         (expect (text "src/added.txt") :to-equal "added")
         (expect (kind "src/deleted.txt") :to-be :absent)
         (expect (text "unpacked/src/replaced.txt") :to-equal "new")
         (expect (text "unpacked/src/added.txt") :to-equal "added")
         (expect (kind "unpacked/src/deleted.txt") :to-be :absent))))))
(describe "aitools archive create member kinds and host-read fallback"
  (it "preserves directories and symlinks when a view read falls back to the host"
    (call-with-edit-workspace
     (lambda ()
       (put "src/nested/fallback.txt" "from host")
       (put "src/regular.txt" "regular")
       (sb-posix:symlink "regular.txt" (disk "src/link"))
       (let ((real-read (symbol-function 'aitools.store.application:view-read-file))
             (fallback-seen nil))
         (cl-weave:with-mocked-functions
             (((symbol-function 'aitools.store.application:view-read-file)
               (lambda (view path &rest keys)
                 (if (string= path "src/nested/fallback.txt")
                     (progn (setf fallback-seen t) nil)
                     (apply real-read view path keys)))))
           (expect (run "archive.create" '("bundle.tar" "src") :format "tar") :to-be :ok))
         (expect fallback-seen :to-be t)
         (expect (run "archive.extract" '("bundle.tar") :to "unpacked") :to-be :ok)
         (expect (kind "unpacked/src") :to-be :directory)
         (expect (kind "unpacked/src/nested") :to-be :directory)
         (expect (text "unpacked/src/nested/fallback.txt") :to-equal "from host")
         (expect (text "unpacked/src/regular.txt") :to-equal "regular")
         (expect (kind "unpacked/src/link") :to-be :symlink)
         (expect (sb-posix:readlink (disk "unpacked/src/link")) :to-equal "regular.txt"))))))

(defun replay-argv (argv)
  "REPLAY-EDIT-OP of ARGV against the workspace on disk: (:commit paths) or
(:reject code message)."
  (replay-edit-op argv
                  (aitools.store.application:disk-view (open-store *root*))
                  (lambda (requests) (list :commit (mapcar #'aitools.store.domain:change-request-path requests)))
                  (lambda (code message &rest keys) (declare (ignore keys)) (list :reject code message))
                  :serializer (lambda (format value)
                                (declare (ignore format))
                                value)))

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
            ("a damaged record asking for stdin" ("doc" "set" "--stdin" "c.json") "environment.unavailable" "standard input")
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
            ("a hash-guarded doc set" ("doc" "set" "--expect-hash" "h" "c.json" "/a" "1") "refusal.target-changed" "doc.set is position-based")
            ("a hash-guarded doc delete" ("doc" "delete" "--expect-hash" "h" "c.json" "/a") "refusal.target-changed" "doc.delete is position-based")
            ("a hash-guarded doc merge" ("doc" "merge" "--stdin-data" "{}" "--expect-hash" "h" "c.json") "refusal.target-changed" "doc.merge is position-based")
            ("a hash-guarded doc fmt" ("doc" "fmt" "--expect-hash" "h" "c.json") "refusal.target-changed" "doc.fmt is position-based")
            ("a transform by line range" ("transform" "--op" "sort" "--range" "1" "a.txt") "refusal.target-changed" "transform is position-based"))
      "rejects ~A"
      (name argv code fragment)
    (declare (ignore name))
    (call-with-edit-workspace (lambda () (put "a.txt" (format nil "a~%"))
      (put "c.json" "{}")
      (destructuring-bind (kind actual-code message) (replay-argv argv)
        (expect kind :to-be :reject)
        (expect actual-code :to-equal code)
        (expect (search fragment message) :to-be-truthy)))))

  (it "re-runs a content-based op against the view, reading its recorded stdin input"
    (call-with-edit-workspace (lambda () (put "c.json" "{\"a\":1}")
      (expect (replay-argv '("doc" "set" "--stdin-data" "{\"pointer\":\"/a\",\"value\":2}" "c.json"))
              :to-equal '(:commit ("c.json")))
      (expect (replay-argv '("edit" "--between" "^a$" "^c$" "--exclusive" "--new" "B" "x.txt"))
              :to-equal (list :reject "input.not-found" "x.txt does not exist")))))

  (it "requires a serializer to replay structured document commands"
    (call-with-edit-workspace
     (lambda ()
       (let ((result
               (replay-edit-op
                (list "doc" "fmt" "doc.yaml")
                (aitools.store.application:disk-view (open-store *root*))
                (lambda (requests) (list :commit requests))
                (lambda (code message &rest keys)
                  (declare (ignore keys))
                  (list :reject code message)))))
         (expect result :to-equal
                 (list :reject "argument.invalid" "doc.fmt cannot be replayed"))))))

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
    (expect (options-argv "edit" '("") '(:old "a"))
            :to-equal '("edit" "--old" "a" ""))
    (expect (options-argv "org.todo.set" '("notes.org") '(:state "DONE"))
            :to-equal '("org" "todo" "set" "--state" "DONE" "notes.org")))

  (it "replays a replace op on the moved base"
    (call-with-edit-workspace (lambda () (put "a.txt" (format nil "x = 1~%"))
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
        (expect (text "a.txt") :to-equal (format nil "head~%x = 2~%")))))))
(describe "aitools tx rebase replays doc write commands"
  (it "replays every doc write verb with an explicit format across JSON, YAML, and TOML bases"
    (labels ((write-in-tx (tx verb path format)
               (cond
                 ((string= verb "set")
                  (run-in tx "doc.set" (list path "/key" "\"updated\"") :format format))
                 ((string= verb "delete")
                  (run-in tx "doc.delete" (list path "/drop") :format format))
                 ((string= verb "merge")
                  (run-in tx "doc.merge" (list path) :format format
                          :stdin-data "{\"key\":\"updated\"}"))
                 ((string= verb "patch")
                  (run-in tx "doc.patch" (list path) :format format
                          :stdin-data "[{\"op\":\"replace\",\"path\":\"/key\",\"value\":\"updated\"}]"))
                 (t (run-in tx "doc.fmt" (list path) :format format))))
             (base-document (format moved)
               (cond
                 ((string= format "json")
                  (if moved
                      "{ \"key\" : \"old\", \"drop\" : true, \"head\" : true }"
                      "{ \"key\" : \"old\", \"drop\" : true }"))
                 ((string= format "yaml")
                  (if moved
                      (format nil "key:    old~%drop:   true~%head: true~%")
                      (format nil "key:    old~%drop:   true~%")))
                 (t
                  (if moved
                      (format nil "key    = \"old\"~%drop   = true~%head = true~%")
                      (format nil "key    = \"old\"~%drop   = true~%"))))))
      (call-with-edit-workspace
       (lambda ()
         (dolist (format (list "json" "yaml" "toml"))
           (dolist (verb (list "set" "delete" "merge" "patch" "fmt"))
             (let ((path (format nil "doc-~A.~A" verb format)))
               (put path (base-document format nil))
               (let ((tx (begin-tx)))
                 (expect (write-in-tx tx verb path format) :to-be :ok)
                 (put path (base-document format t))
                 (expect
                  (aitools.store.application:tx-rebase/k
                   (open-store *root*) tx (function replay-record)
                   :on-rebased (lambda (paths) paths)
                   :on-conflict
                   (lambda (conflicts)
                     (declare (ignore conflicts))
                     :conflict)
                   :on-not-found (lambda () :not-found)
                   :on-busy (lambda () :busy))
                  :to-equal (list path))
                 (expect (commit-tx tx) :to-be :committed)
                 (let ((content (text path)))
                   (progn
                     (expect (search "head" content) :to-be-truthy)
                     (when (string= verb "fmt")
                       (expect
                        (search
                         (cond
                           ((string= format "json") "\"key\":")
                           ((string= format "yaml") "key: old")
                           (t "key = \"old\""))
                         content)
                        :to-be-truthy)))
                   (expect
                    (search
                     (if (member verb (list "set" "merge" "patch")
                                 :test (function string=))
                         "updated"
                         "old")
                     content)
                    :to-be-truthy)
                   (if (string= verb "delete")
                       (expect (search "drop" content) :to-be nil)
                       (expect (search "drop" content) :to-be-truthy))))))))))))
