;;;; t/integration/dispatch-recovery-failure-test.lisp
;;;;
;;;; DISPATCH when store recovery cannot finish: the envelope names
;;;; the committed op and the record that blocked it, and the op completes
;;;; once the record is readable. Workspace helpers come from
;;;; dispatch-test.lisp.
(in-package #:aitools.integration.dispatch-test)

(defun intent-record-names ()
  "The file names of the intent records left in `commit/`, sorted."
  (sort (mapcar #'file-namestring
                (directory (merge-pathnames "*.json" (sb-ext:parse-native-namestring
                                                   (concatenate 'string (state-directory) "/commit/")))))
        #'string<))

(defun write-file (path text &key (if-exists :error))
  (with-open-file (out (sb-ext:parse-native-namestring path) :direction :output :if-exists if-exists
                                                             :external-format :utf-8)
    (write-string text out)))

(defun break-recovery (scenario)
  "Leave the workspace in a state whose store recovery fails outside any one
record, and return the path the recovery repair must name."
  (put "a.txt" (format nil "one~%"))
  (expect (run-aitools "edit" (disk "a.txt") "--old" "one" "--new" "two") :to-be 0)
  (ecase scenario
    (:commit-not-a-directory
     ;; The finished edit left `commit/` empty; a regular file in its place
     ;; makes the listing recovery starts with fail.
     (let ((commit (concatenate 'string (state-directory) "/commit")))
       (sb-posix:rmdir commit)
       (write-file commit "x")
       commit))
    (:malformed-journal
     ;; A record past its commit point replays its journal step, which
     ;; rewrites the journal from its parsed lines; a line that is not JSON
     ;; is a STORE-FORMAT-ERROR, which carries no path, so the repair names
     ;; the workspace root.
     (write-file (concatenate 'string (state-directory) "/journal/ops.jsonl") (format nil "not json~%")
                 :if-exists :append)
     (expect (with-fault-at (:after-apply) (run-aitools "edit" (disk "a.txt") "--old" "two" "--new" "three"))
             :to-be-truthy)
     *root*)))

(describe "aitools dispatch: store recovery that cannot finish"
  (it "names the committed op and the record that blocked it, then completes the op once the record is readable"
    (with-dispatch-workspace ()
      (put "a.txt" (format nil "one~%"))
      (expect (with-fault-at (:after-apply) (run-aitools "edit" (disk "a.txt") "--old" "one" "--new" "two"))
              :to-be-truthy)
      (expect (length (intent-record-names)) :to-be 1)
      (let* ((name (first (intent-record-names)))
             (op-id (subseq name 0 (- (length name) (length ".json"))))
             (record (concatenate 'string (state-directory) "/commit/" name))
             (aside (concatenate 'string (state-directory) "/" name ".aside")))
        ;; A directory where the committed record must be read.
        (sb-posix:rename record aside)
        (sb-posix:mkdir record #o755)
        (multiple-value-bind (code envelope stream) (run-aitools "read" (disk "a.txt"))
          (expect (list code stream) :to-equal '(1 :stderr))
          (expect (value envelope "command") :to-equal "read")
          (expect (value envelope "error" "code") :to-equal "environment.io")
          (expect (value envelope "error" "message")
                  :to-equal (format nil "operation ~A is past its commit point at ~A; recovery will complete it on the next run"
                                    op-id record))
          (expect (repair-commands envelope) :to-equal (list (format nil "aitools info ~A" record)))
          (expect (value envelope "recovered") :to-be nil))
        (sb-posix:rmdir record)
        (sb-posix:rename aside record)
        (multiple-value-bind (code envelope) (run-aitools "read" (disk "a.txt"))
          (expect code :to-be 0)
          (expect (length (value envelope "recovered")) :to-be 1)
          (expect (value envelope "recovered" 0 "op_id") :to-equal op-id)
          (expect (value envelope "recovered" 0 "action") :to-equal "rolled-forward"))
        (expect (intent-record-names) :to-be nil)
        (multiple-value-bind (code envelope) (run-aitools "history")
          (expect code :to-be 0)
          (expect (value envelope "items" 0 "op_id") :to-equal op-id)))))

  (it-each ((:commit-not-a-directory "opendir failed for ")
            (:malformed-journal "malformed store record: "))
      "answers environment.io with an inspect repair when recovery fails outside a record (~A)"
      (scenario detail)
    (with-dispatch-workspace ()
      (let ((blocking (break-recovery scenario)))
        (dotimes (attempt 2)
          (multiple-value-bind (code envelope stream) (run-aitools "read" (disk "a.txt"))
            (expect (list code stream) :to-equal '(1 :stderr))
            (expect (value envelope "command") :to-equal "read")
            (expect (value envelope "error" "code") :to-equal "environment.io")
            (let ((message (value envelope "error" "message")))
              (expect (search (format nil "workspace recovery could not complete: ~A" detail) message) :to-be 0)
              (expect (search "past its commit point" message) :to-be nil))
            (expect (repair-commands envelope) :to-equal (list (format nil "aitools info ~A" blocking))))))))

  (it-each (("soon") ("-5ms"))
      "rejects --lock-timeout ~S for the typed command before recovery runs"
      (text)
    (with-dispatch-workspace ()
      (crash-an-edit)
      (multiple-value-bind (code envelope stream) (run-aitools "--lock-timeout" text "read" (disk "a.txt"))
        (expect (list code stream) :to-equal '(1 :stderr))
        (expect (value envelope "command") :to-equal "read")
        (expect (value envelope "error" "code") :to-equal "argument.invalid")
        (expect (value envelope "error" "message")
                :to-equal (format nil "--lock-timeout ~S is not a duration (<n>ms|s|m|h|d)" text))
        (expect (repair-commands envelope) :to-equal '("aitools schema read"))
        (expect (value envelope "recovered") :to-be nil))
      ;; The unfinished op is still there for the next valid invocation.
      (expect (length (intent-record-names)) :to-be 1)
      (multiple-value-bind (code envelope) (run-aitools "--lock-timeout" "5s" "read" (disk "a.txt"))
        (expect code :to-be 0)
        (expect (value envelope "recovered" 0 "action") :to-equal "discarded")))))
