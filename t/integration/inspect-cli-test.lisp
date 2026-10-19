;;;; t/integration/inspect-cli-test.lisp
;;;;
;;;; `read`, `info`, `check`, and `diff` end to end through the composition
;;;; root and the production adapters, in a real temporary workspace: argv
;;;; parsing with the global --root, the envelope and exit codes, symlinks
;;;; out of the workspace, standard digests, `diff --op` against a real
;;;; journal, and `--tx` reads (the staged overlay and the read set). XDG_STATE_HOME
;;;; points into the temporary directory for the duration of each test.
;;;; The json/table/archive/snapshot groups are in inspect-cli-commands-test.lisp;
;;;; --tx and store edge cases in inspect-cli-store-test.lisp.
(in-package #:cl-user)

(defpackage #:aitools.integration.inspect-cli-test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect #:fail))

(in-package #:aitools.integration.inspect-cli-test)

(defun run-aitools (&rest arguments)
  "(VALUES exit-code envelope stream) for `aitools ARGUMENTS...`; ENVELOPE is
the parsed JSON (hash tables) from whichever stream received output."
  (multiple-value-bind (app registry) (aitools/cli:build-app)
    (let* ((out (make-string-output-stream))
           (err (make-string-output-stream))
           (code (aitools/cli:dispatch app registry (cons "aitools" arguments) :stdout out :stderr err))
           (stdout (get-output-stream-string out))
           (stderr (get-output-stream-string err)))
      (if (plusp (length stdout))
          (values code (json-kit:parse stdout) :stdout)
          (values code (json-kit:parse stderr) :stderr)))))

(defun value-at (object &rest keys)
  (reduce (lambda (value key)
            (cond ((null value) nil)
                  ((integerp key) (and (< key (length value)) (elt value key)))
                  (t (gethash key value))))
          keys :initial-value object))

(defun inspect-test-path (workspace relative)
  (format nil "~A/~A" workspace relative))
(defun write-bytes (path content)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (write-sequence (if (stringp content) (sb-ext:string-to-octets content :external-format :utf-8) content) out))
  path)

(defun numbered (count)
  (format nil "~{line ~D~%~}" (loop for n from 1 to count collect n)))

(describe "inspect commands through dispatch"
  (it "reads with a partial status, exit 3, and the next --range"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (write-bytes (inspect-test-path workspace "big.txt") (numbered 100))
      (multiple-value-bind (code envelope stream)
          (run-aitools "--root" workspace "read" (inspect-test-path workspace "big.txt") "--max-lines" "40")
        (expect code :to-be 3)
        (expect stream :to-be :stdout)
        (expect envelope :to-match-envelope '("status" "partial" "command" "read"))
        (expect (length (value-at envelope "lines")) :to-be 40)
        (expect (value-at envelope "next_commands" 0) :to-contain "--range 41:80"))))

  (it "exits 1 with candidates and a repair command for a missing file"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (write-bytes (inspect-test-path workspace "notes.md") "x")
      (multiple-value-bind (code envelope stream)
          (run-aitools "--root" workspace "read" (inspect-test-path workspace "note.md"))
        (expect code :to-be 1)
        (expect stream :to-be :stderr)
        (expect (value-at envelope "error" "code") :to-equal "input.not-found")
        (expect (value-at envelope "error" "candidates" 0 "path") :to-contain "notes.md")
        (expect (plusp (length (value-at envelope "error" "repairs" 0 "command"))) :to-be t))))

  (it "matches the standard sha256, sha1, and md5 digests"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (let ((file (write-bytes (inspect-test-path workspace "abc.txt") "abc")))
        (loop for (algorithm expected) in '(("sha256" "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
                                            ("sha1" "a9993e364706816aba3e25717850c26c9cd0d89d")
                                            ("md5" "900150983cd24fb0d6963f7d28e17f72"))
              do (multiple-value-bind (code envelope) (run-aitools "--root" workspace "info" (namestring file) "--digest" algorithm)
                   (expect code :to-be 0)
                   (expect (value-at envelope "digest" "value") :to-equal expected))))))

  (it "reports a symlink that leaves the workspace"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (let ((outside (inspect-test-path workspace "../outside.txt")))
        (write-bytes outside "o")
        (sb-posix:symlink (namestring (truename outside)) (inspect-test-path workspace "link"))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "info" (inspect-test-path workspace "link"))
          (expect code :to-be 0)
          (expect (value-at envelope "relative") :to-equal "link")
          (expect (value-at envelope "inside_workspace") :to-be json-kit:+json-false+)
          (expect (value-at envelope "real") :to-equal (namestring (truename outside)))))))

  (it "reports exists:false with --allow-missing and exit 0"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (multiple-value-bind (code envelope) (run-aitools "--root" workspace "info" (inspect-test-path workspace "nope") "--allow-missing")
        (expect code :to-be 0)
        (expect (value-at envelope "exists") :to-be json-kit:+json-false+))))

  (it "reports a JSON syntax error with diagnostics and exit 1"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (write-bytes (inspect-test-path workspace "bad.json") "[1, 2,]")
      (multiple-value-bind (code envelope) (run-aitools "--root" workspace "check" (inspect-test-path workspace "bad.json"))
        (expect code :to-be 1)
        (expect (value-at envelope "error" "code") :to-equal "input.syntax-error")
        (expect (value-at envelope "error" "diagnostics" 0 "line") :to-be 1))))

  (it "diffs two files and exits 0"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (write-bytes (inspect-test-path workspace "a") (format nil "x~%y~%"))
      (write-bytes (inspect-test-path workspace "b") (format nil "x~%z~%"))
      (multiple-value-bind (code envelope) (run-aitools "--root" workspace "diff" (inspect-test-path workspace "a")
                                                        (inspect-test-path workspace "b") "--output" "stat")
        (expect code :to-be 0)
        (expect (value-at envelope "added") :to-be 1)
        (expect (value-at envelope "deleted") :to-be 1)))))

(defun commit-write (store relative content)
  "Commit one file write through the store's write protocol and return its op id."
  (aitools.store.application:commit-changes/k
   store (list "write" relative)
   (lambda (commit reject)
     (declare (ignore reject))
     (funcall commit (list (aitools.store.domain:write-file-request
                            relative (sb-ext:string-to-octets content :external-format :utf-8)))))
   :on-committed (lambda (op-id results) (declare (ignore results)) op-id)
   :on-rejected (lambda (code message &rest keys) (declare (ignore keys)) (fail (format nil "~A ~A" code message)))
   :on-busy (lambda () (fail "busy"))))

(defun open-store (workspace)
  (aitools.store.infrastructure:make-posix-store (string-right-trim "/" workspace)))

(describe "diff --op and --tx reads against a real store"
  (it "shows every change of a journal op with its diff, and input.not-found for an unknown op"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (let* ((store (open-store workspace))
             (first-op (commit-write store "f.txt" (format nil "one~%")))
             (second-op (commit-write store "f.txt" (format nil "one~%two~%"))))
        (declare (ignore first-op))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "diff" "--op" second-op)
          (expect code :to-be 0)
          (expect (value-at envelope "mode") :to-equal "op")
          (expect (value-at envelope "changes" 0 "path") :to-equal "f.txt")
          (expect (value-at envelope "changes" 0 "action") :to-equal "modified")
          (expect (value-at envelope "changes" 0 "diff") :to-contain "+two"))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "diff" "--op" "op-19700101T000000Z-00000000")
          (expect code :to-be 1)
          (expect (value-at envelope "error" "code") :to-equal "input.not-found")))))

  (it "reads the tx's staged content and records the read set"
    (aitools.test.support:with-workspace (workspace state)
      (declare (ignore state))
      (write-bytes (inspect-test-path workspace "cfg.txt") (format nil "disk~%"))
      (write-bytes (inspect-test-path workspace "other.txt") (format nil "o~%"))
      (let* ((store (open-store workspace))
             (tx (aitools.store.application:tx-begin/k store :on-begun (lambda (id name created)
                                                                        (declare (ignore name created)) id)
                                                             :on-busy (lambda () (fail "busy")))))
        (aitools.store.application:tx-stage/k
         store tx (list "write" "cfg.txt")
         (lambda (view commit reject)
           (declare (ignore view reject))
           (funcall commit (list (aitools.store.domain:write-file-request
                                  "cfg.txt" (sb-ext:string-to-octets (format nil "staged~%") :external-format :utf-8)))))
         :on-staged (lambda (tx-op results) (declare (ignore results)) tx-op)
         :on-rejected (lambda (code message &rest keys) (declare (ignore keys)) (fail (format nil "~A ~A" code message)))
         :on-not-found (lambda () (fail "tx not found"))
         :on-busy (lambda () (fail "busy")))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "read" (inspect-test-path workspace "cfg.txt") "--tx" tx)
          (expect code :to-be 0)
          (expect (value-at envelope "lines" 0) :to-equal "staged"))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "read" (inspect-test-path workspace "cfg.txt"))
          (expect code :to-be 0)
          (expect (value-at envelope "lines" 0) :to-equal "disk"))
        (expect (run-aitools "--root" workspace "info" (inspect-test-path workspace "other.txt") "--tx" tx) :to-be 0)
        (write-bytes (inspect-test-path workspace "other.txt") (format nil "changed~%"))
        (expect (aitools.store.application:tx-status/k
                 store tx :on-status #'aitools.store.application:tx-status-stale-reads
                          :on-not-found (lambda () (fail "tx vanished")))
                :to-equal '("other.txt"))
        (multiple-value-bind (code envelope) (run-aitools "--root" workspace "read" (inspect-test-path workspace "cfg.txt") "--tx" "tx-nope")
          (expect code :to-be 1)
          (expect (value-at envelope "error" "code") :to-equal "input.not-found"))))))
