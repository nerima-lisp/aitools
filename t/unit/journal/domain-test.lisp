;;;; t/unit/journal/domain-test.lisp
;;;;
;;;; Pure rendering: command lines, history items, tx path actions,
;;;; commit-conflict repairs, and the replay registry lookup.
(in-package #:aitools.journal.test)

(defun %file (text) (aitools.store.domain:file-state (aitools.kernel.domain:content-hash (bytes text)) #o644))

(describe "aitools.journal command lines"
  (it "quotes only words that need it"
    (expect (command-line "aitools" "read" "a b.txt" "--tx" "tx-1") :to-equal "aitools read 'a b.txt' --tx tx-1")
    (expect (command-line "aitools" nil (list "--root" "/w") "history") :to-equal "aitools --root /w history")
    (expect (command-line "it's") :to-equal "'it'\\''s'")
    (expect (command-line "") :to-equal "''"))

  (it "renders a recorded argv as the aitools line that ran it"
    (expect (argv-command-line '("edit" "a.txt" "--old" "x y")) :to-equal "aitools edit a.txt --old 'x y'")
    (expect (argv-command-line '("aitools" "undo" "op-1")) :to-equal "aitools undo op-1")))

(describe "aitools.journal history items"
  (it "has op_id, command, paths and time, and undoes only for an undo"
    (let* ((change (aitools.store.domain:make-change-result
                    :path "b.txt" :action :moved :from "a.txt"
                    :before (aitools.store.domain:absent-state) :after (%file "x")))
           (plain (aitools.store.domain:make-journal-entry
                   :op-id "op-20260101T000000Z-00000001" :argv '("move" "a.txt" "b.txt")
                   :time "2026-01-01T00:00:00Z" :changes (list change)))
           (undo (aitools.store.domain:make-journal-entry
                  :op-id "op-20260101T000001Z-00000002" :argv '("undo" "op-20260101T000000Z-00000001")
                  :time "2026-01-01T00:00:01Z" :changes (list change)
                  :undoes "op-20260101T000000Z-00000001")))
      (expect (json-alist (history-item plain))
              :to-equal '(("op_id" . "op-20260101T000000Z-00000001")
                          ("command" . "aitools move a.txt b.txt")
                          ("paths" . ("b.txt" "a.txt"))
                          ("time" . "2026-01-01T00:00:00Z")))
      (expect (member-value (history-item undo) "undoes") :to-equal "op-20260101T000000Z-00000001"))))

(describe "aitools.journal tx path actions"
  (flet ((action (base staged)
           (tx-path-action (aitools.store.domain:make-tx-path :path "p" :base base :staged staged))))
    (it "names what committing the path would do"
      (let ((absent (aitools.store.domain:absent-state)))
        (expect (action absent (%file "a")) :to-equal "created")
        (expect (action (%file "a") (%file "b")) :to-equal "modified")
        (expect (action (%file "a") absent) :to-equal "deleted")
        (expect (action (%file "a") (aitools.store.domain:symlink-state "t")) :to-equal "linked")
        (expect (action (%file "a")
                        (aitools.store.domain:file-state (aitools.kernel.domain:content-hash (bytes "a")) #o755))
                :to-equal "mode-changed")
        (expect (action (%file "a") (%file "a")) :to-equal "unchanged")))

    (it "tells a directory chmod from a kind change"
      (expect (action (aitools.store.domain:directory-state #o755) (aitools.store.domain:directory-state #o700))
              :to-equal "mode-changed")
      (expect (action (aitools.store.domain:directory-state #o755) (%file "a")) :to-equal "modified")
      (expect (action (%file "a") (aitools.store.domain:directory-state #o755)) :to-equal "modified"))))

(describe "aitools.journal commit conflict repairs"
  (it "offers rebase for write conflicts, and a re-read plus --ignore-stale-reads for read conflicts"
    (let* ((write (aitools.store.domain:make-conflict :path "w.txt" :kind :write
                                                      :base (%file "a") :current (%file "b")))
           (read (aitools.store.domain:make-conflict :path "r.txt" :kind :read
                                                     :base (%file "a") :current (%file "b")))
           (repairs (commit-conflict-repairs "tx-1" (list write read) :globals '("--root" "/w"))))
      (expect (mapcar (lambda (r) (getf r :command)) repairs)
              :to-equal '("aitools --root /w tx rebase tx-1"
                          "aitools --root /w read r.txt --tx tx-1"
                          "aitools --root /w tx commit tx-1 --ignore-stale-reads"))
      (expect (mapcar (lambda (r) (getf r :command)) (commit-conflict-repairs "tx-1" (list write)))
              :to-equal '("aitools tx rebase tx-1")))))

(describe "aitools.journal tx replayer registry"
  (it "finds a group command before its group word, skipping a leading aitools"
    (let ((aitools.journal.application::*tx-replayers* (make-hash-table :test 'equal)))
      (register-tx-replayer "json" #'identity)
      (register-tx-replayer "json.set" #'list)
      (register-tx-replayer "edit" #'values)
      (expect (find-tx-replayer '("json" "set" "a.json")) :to-be #'list)
      (expect (find-tx-replayer '("json" "fmt" "a.json")) :to-be #'identity)
      (expect (find-tx-replayer '("aitools" "edit" "a")) :to-be #'values)
      (expect (find-tx-replayer '("write" "a")) :to-be nil)
      (expect (find-tx-replayer '("edit")) :to-be #'values)
      (expect (find-tx-replayer '("aitools")) :to-be nil)
      (expect (find-tx-replayer '()) :to-be nil))))
