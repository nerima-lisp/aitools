;;;; t/integration/journal-cli-test.lisp
;;;;
;;;; `history`, `undo` and `tx` end to end through the composition root:
;;;; argv parsing with the global `--root`, the production ports and store,
;;;; the envelope, and the exit code. XDG_STATE_HOME points at a
;;;; temporary directory for the duration of each test so no state reaches
;;;; the user's real state directory.
(in-package #:aitools.journal.test)

(defun run-aitools (&rest arguments)
  "Return (VALUES EXIT-CODE ENVELOPE STREAM): the parsed JSON envelope and
whether it went to :STDOUT or :STDERR."
  (multiple-value-bind (app registry) (aitools/cli:build-app)
    (let* ((out (make-string-output-stream))
           (err (make-string-output-stream))
           (code (aitools/cli:dispatch app registry (cons "aitools" arguments) :stdout out :stderr err))
           (stdout (get-output-stream-string out))
           (stderr (get-output-stream-string err)))
      (if (plusp (length stdout))
          (values code (json-kit:parse stdout) :stdout)
          (values code (json-kit:parse stderr) :stderr)))))

(defun envelope-value (object &rest keys)
  (reduce (lambda (value key) (and value (gethash key value))) keys :initial-value object))

(defun call-with-cli-workspace (function)
  "Call FUNCTION with (ROOT STORE): a fresh workspace and the store the
production CLI opens for it, with XDG_STATE_HOME redirected meanwhile."
  (with-temp-store (scratch)
    (let* ((root (aitools.store.application:store-root scratch))
           (xdg (concatenate 'string root "/../xdg"))
           (previous (sb-posix:getenv "XDG_STATE_HOME")))
      (sb-posix:mkdir xdg #o700)
      (unwind-protect
           (progn
             (sb-posix:setenv "XDG_STATE_HOME" xdg 1)
             (funcall function root (aitools.store.infrastructure:make-posix-store root)))
        (if previous
            (sb-posix:setenv "XDG_STATE_HOME" previous 1)
            (sb-posix:unsetenv "XDG_STATE_HOME"))))))

(defmacro with-cli-workspace ((root store) &body body)
  `(call-with-cli-workspace (lambda (,root ,store) ,@body)))

(describe "aitools journal commands through dispatch"
  (it "undoes an op with exit 0, then refuses the same undo with exit 2 and conflicts"
    (with-cli-workspace (root store)
      (put-file store "a.txt" "before")
      (let ((op (write-op store "a.txt" "after")))
        (multiple-value-bind (code envelope stream) (run-aitools "--root" root "history")
          (expect (list code stream) :to-equal '(0 :stdout))
          (expect (envelope-value envelope "command") :to-equal "history")
          (expect (envelope-value (aref (envelope-value envelope "items") 0) "op_id") :to-equal op))
        (multiple-value-bind (code envelope) (run-aitools "--root" root "undo" op)
          (expect code :to-be 0)
          (expect (envelope-value envelope "undoes") :to-equal op)
          (expect (envelope-value (aref (envelope-value envelope "changes") 0) "action") :to-equal "modified"))
        (expect (disk-text store "a.txt") :to-equal "before")
        (multiple-value-bind (code envelope stream) (run-aitools "--root" root "undo" op)
          (expect (list code stream) :to-equal '(2 :stderr))
          (expect (envelope-value envelope "error" "code") :to-equal "refusal.target-changed")
          (expect (length (envelope-value envelope "error" "conflicts")) :to-be 1)))))

  (it "answers a bare undo with argument.invalid and aitools history as the repair"
    (with-cli-workspace (root store)
      (declare (ignore store))
      (multiple-value-bind (code envelope) (run-aitools "--root" root "undo")
        (expect code :to-be 1)
        (expect (envelope-value envelope "error" "code") :to-equal "argument.invalid")
        (expect (envelope-value (aref (envelope-value envelope "error" "repairs") 0) "command")
                :to-equal (format nil "aitools --root ~A history" root)))))

  (it "runs a tx from begin to commit"
    (with-cli-workspace (root store)
      (put-file store "a.txt" "a")
      (let ((tx (envelope-value (nth-value 1 (run-aitools "--root" root "tx" "begin" "--name" "t")) "tx")))
        (expect (aitools.store.domain:valid-tx-id-p tx) :to-be-truthy)
        (stage-append store tx "a.txt" "+1")
        (multiple-value-bind (code envelope) (run-aitools "--root" root "tx" "status")
          (expect code :to-be 0)
          (expect (envelope-value (aref (envelope-value envelope "items") 0) "ops") :to-be 1))
        (multiple-value-bind (code envelope) (run-aitools "--root" root "tx" "diff" tx)
          (expect code :to-be 0)
          (expect (length (envelope-value envelope "changes")) :to-be 1))
        (multiple-value-bind (code envelope) (run-aitools "--root" root "tx" "commit" tx)
          (expect code :to-be 0)
          (expect (aitools.store.domain:valid-op-id-p (envelope-value envelope "op_id")) :to-be-truthy))
        (expect (disk-text store "a.txt") :to-equal "a+1")
        (expect (run-aitools "--root" root "tx" "abort" tx) :to-be 1)))))

(describe "aitools journal store failures through dispatch"
  (it "serialises a post-commit tx commit failure as environment.io with the op_id diagnostic, then recovers it"
    (with-cli-workspace (root store)
      (put-file store "a.txt" "a")
      (let ((tx (envelope-value (nth-value 1 (run-aitools "--root" root "tx" "begin")) "tx")))
        (stage-append store tx "a.txt" "+1")
        (multiple-value-bind (code envelope stream)
            (call-with-io-fault-at :after-apply (lambda () (run-aitools "--root" root "tx" "commit" tx)))
          (expect stream :to-be :stderr)
          (expect code :to-be 1)
          (expect (envelope-value envelope "error" "code") :to-equal "environment.io")
          (let* ((diagnostics (envelope-value envelope "error" "diagnostics"))
                 (op-id (envelope-value (aref diagnostics 0) "op_id")))
            (expect (length diagnostics) :to-be 1)
            (expect (aitools.store.domain:valid-op-id-p op-id) :to-be-truthy)
            (expect (envelope-value (aref diagnostics 0) "recovery") :to-equal "pending")
            (expect (envelope-value (aref (envelope-value envelope "error" "repairs") 0) "command")
                    :to-equal (format nil "aitools --root ~A history" root))
            (multiple-value-bind (code envelope) (run-aitools "--root" root "history")
              (expect code :to-be 0)
              (expect (envelope-value (aref (envelope-value envelope "items") 0) "op_id") :to-equal op-id))
            (expect (disk-text store "a.txt") :to-equal "a+1"))))))

  (it "drops and rebases a tx through dispatch"
    (call-with-append-replayer
     (lambda ()
       (with-cli-workspace (root store)
         (put-file store "a.txt" "a")
         (let ((tx (envelope-value (nth-value 1 (run-aitools "--root" root "tx" "begin")) "tx")))
           (stage-append store tx "a.txt" "+1")
           (stage-append store tx "a.txt" "+2")
           (multiple-value-bind (code envelope) (run-aitools "--root" root "tx" "drop" tx "2")
             (expect code :to-be 0)
             (expect (coerce (envelope-value envelope "dropped") 'list) :to-equal '(2)))
           (put-file store "a.txt" "external")
           (multiple-value-bind (code envelope) (run-aitools "--root" root "tx" "rebase" tx)
             (expect code :to-be 0)
             (expect (coerce (envelope-value envelope "rebased") 'list) :to-equal '("a.txt")))
           (expect (tx-text store tx "a.txt") :to-equal "external+1")))))))

(describe "aitools journal flags through dispatch"
  (it "passes undo --dry-run and tx commit --ignore-stale-reads to the flows"
    (with-cli-workspace (root store)
      (put-file store "a.txt" "before")
      (let ((op (write-op store "a.txt" "after")))
        (multiple-value-bind (code envelope) (run-aitools "--root" root "undo" op "--dry-run")
          (expect code :to-be 0)
          (expect (envelope-value envelope "dry_run") :to-be t)
          (expect (envelope-value envelope "op_id") :to-be nil))
        (expect (disk-text store "a.txt") :to-equal "after"))
      (put-file store "r.txt" "r")
      (let ((tx (envelope-value (nth-value 1 (run-aitools "--root" root "tx" "begin")) "tx")))
        (record-read store tx "r.txt")
        (stage-append store tx "w.txt" "w")
        (put-file store "r.txt" "r2")
        (expect (run-aitools "--root" root "tx" "commit" tx) :to-be 2)
        (expect (run-aitools "--root" root "tx" "commit" tx "--ignore-stale-reads") :to-be 0)
        (expect (disk-text store "w.txt") :to-equal "w")))))
