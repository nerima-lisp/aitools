;;;; t/integration/edit-text-pipeline-test.lisp
;;;;
;;;; The write pipeline's argument, root, target, hash and archive checks,
;;;; and --stdin failures.
(in-package #:aitools.edit.test)

(defun mktemp-path (&rest options)
  (multiple-value-bind (kind fields) (apply #'run "mktemp" '() options)
    (expect kind :to-be :ok)
    (field fields "path")))

(defmacro with-stdin-port ((read-stdin-octets) &body body)
  "BODY with *PORTS* reading standard input through READ-STDIN-OCTETS."
  `(let ((*ports* (make-edit-ports
                   :workspace-host (aitools.edit.application::edit-ports-workspace-host *ports*)
                   :open-store #'open-store
                   :text-source (aitools.edit.application::edit-ports-text-source *ports*)
                   :read-stdin-octets ,read-stdin-octets
                   :unix-now #'aitools.edit.infrastructure:unix-now)))
     ,@body))

(describe "aitools write pipeline: argument and root checks"
  (it-each (("--expect-count that is not a count" "replace" ("a" "b" "a.txt") (:expect-count "x")
             "argument.invalid" "--expect-count \"x\" is not a non-negative integer")
            ("--expect-hash with an empty hash" "edit" ("a.txt") (:old "a" :new "b" :expect-hash ("a.txt="))
             "argument.invalid" "non-empty hash")
            ("--expect-hash naming only the other file of a two-file write" "move-lines" ("a.txt")
             (:range "1" :to "b.txt" :to-position "start" :expect-hash ("a.txt=00"))
             "argument.invalid" "this write needs --expect-hash b.txt=<hash>"))
      "refuses ~A before touching the workspace"
      (name command positionals options code fragment)
    (declare (ignore name))
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (put "b.txt" (format nil "b~%"))
      (let ((before (snapshot)))
        (with-error (actual message) (apply #'run command positionals options)
          (expect actual :to-equal code)
          (expect (search fragment message) :to-be-truthy))
        (expect-unchanged before))))

  (it "refuses a --root that does not exist or is a file, pointing at aitools schema"
    (with-workspace ()
      (put "file" "x")
      (dolist (case (list (list (disk "missing") "input.not-found" "does not exist")
                          (list (disk "file") "argument.invalid" "is not a directory")))
        (destructuring-bind (root code fragment) case
          (let ((result nil))
            (run-edit-command *ports* "write" '("a.txt") '(:content ("x")) :root root
                              :on-ok (lambda (fields) (setf result fields))
                              :on-error (lambda (code message &key repairs &allow-other-keys)
                                          (setf result (list code message (getf (first repairs) :command)))))
            (expect (first result) :to-equal code)
            (expect (search fragment (second result)) :to-be-truthy)
            (expect (third result) :to-equal "aitools schema"))))))

  (it "refuses a bad --root when another context drives the pipeline directly"
    (with-workspace ()
      (let ((result nil))
        (aitools.edit.application:run-write-command/k
         *ports*
         (aitools.edit.application:make-write-plan
          :command "write" :targets (list (aitools.edit.application:make-write-target "a.txt"))
          :plan (lambda (context commit reject) (declare (ignore context reject)) (funcall commit '())))
         :root (disk "missing")
         :on-ok (lambda (fields) (setf result fields))
         :on-error (lambda (code message &rest keys) (declare (ignore keys)) (setf result (list code message))))
        (expect (first result) :to-equal "input.not-found")
        (expect (search "does not exist" (second result)) :to-be-truthy)))))

(describe "aitools write pipeline: targets, hashes and the store"
  (it "refuses one write reaching both the workspace and the mktemp area"
    (with-workspace ()
      (let ((temporary (mktemp-path)))
        (let ((before (snapshot)))
          (with-error (code message) (run "copy" (list temporary "copied.txt"))
            (expect code :to-equal "argument.invalid")
            (expect (search "cannot mix" message) :to-be-truthy))
          (expect-unchanged before)))))

  (it "refuses to stage a write to the mktemp area in a tx"
    (with-workspace ()
      (let ((temporary (mktemp-path))
            (tx (begin-tx)))
        (with-error (code message) (run-in tx "write" (list temporary) :content '("x") :overwrite t
                                                                       :expect-hash (list (aitools.kernel.domain:content-hash (octet-vector))))
          (expect code :to-equal "argument.invalid")
          (expect (search "--tx cannot stage" message) :to-be-truthy)))))

  (it "checks --expect-hash PATH=HASH for a file the write does not change"
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (put "b.txt" (format nil "b~%"))
      (let ((before (snapshot)))
        (with-error (code message keys) (run "edit" '("a.txt") :old "a" :new "z" :expect-hash '("b.txt=0000"))
          (expect code :to-equal "refusal.target-changed")
          (expect (json-field (first (getf keys :conflicts)) "path") :to-equal "b.txt")
          (expect (json-field (first (getf keys :conflicts)) "current") :to-equal (hash "b.txt")))
        (with-error (code message keys) (run "edit" '("a.txt") :old "a" :new "z" :expect-hash '("gone.txt=0000"))
          (expect code :to-equal "refusal.target-changed")
          (expect (search "found none" message) :to-be-truthy)
          (expect (json-field (first (getf keys :conflicts)) "current") :to-be (aitools.protocol.domain:json-null)))
        (expect-unchanged before))
      (expect (run "edit" '("a.txt") :old "a" :new "z" :expect-hash (list (format nil "b.txt=~A" (hash "b.txt"))))
              :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "z~%"))))

  (it "checks --expect-hash PATH=HASH in the other area than the write's, and refuses a path outside"
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (let ((temporary (mktemp-path))
            (empty (aitools.kernel.domain:content-hash (octet-vector))))
        ;; A workspace write guarded by a mktemp file's hash, and a mktemp
        ;; write guarded by a workspace file's: each hash is the named file's.
        (with-error (code) (run "edit" '("a.txt") :old "a" :new "z" :expect-hash (list (format nil "~A=0000" temporary)))
          (expect code :to-equal "refusal.target-changed"))
        (expect (run "edit" '("a.txt") :old "a" :new "z" :expect-hash (list (format nil "~A=~A" temporary empty)))
                :to-be :ok)
        (with-error (code) (run "write" (list temporary) :content '("t") :overwrite t
                                                         :expect-hash (list empty "a.txt=0000"))
          (expect code :to-equal "refusal.target-changed"))
        (expect (run "write" (list temporary) :content '("t") :overwrite t
                                              :expect-hash (list empty (format nil "a.txt=~A" (hash "a.txt"))))
                :to-be :ok)
        (expect (with-open-file (in temporary) (read-line in)) :to-equal "t"))
      (with-error (code) (run "edit" '("a.txt") :old "z" :new "y" :expect-hash '("../outside.txt=0000"))
        (expect code :to-equal "refusal.target-changed"))
      (expect (text "a.txt") :to-equal (format nil "z~%"))))

  (it-each ((nil) (t))
      "reports input.not-found with a tx status repair for a missing tx (dry run ~A)"
      (dry-run)
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (with-error (code message keys) (run-in "no-such-tx" "edit" '("a.txt") :old "a" :new "b" :dry-run dry-run)
        (expect code :to-equal "input.not-found")
        (expect (repair-commands keys) :to-equal '("aitools tx status")))))

  (it "plans a directory delete inside a tx without writing"
    (with-workspace ()
      (sb-posix:mkdir (disk "d") #o755)
      (put "d/a.txt" "a")
      (let ((tx (begin-tx)))
        (with-error (code message) (run-in tx "delete" '("d") :dry-run t)
          (expect code :to-equal "refusal.not-a-file")
          (expect message :to-equal "d is a non-empty directory"))
        (expect (run-in tx "delete" '("d/a.txt")) :to-be :ok)
        (multiple-value-bind (kind fields) (run-in tx "delete" '("d") :dry-run t)
          (expect kind :to-be :ok)
          (expect (changes fields) :to-equal '(("d" "deleted"))))
        (expect (kind "d/a.txt") :to-be :file))))

  (it "reports environment.io, not an internal error, when the state home cannot be created"
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (with-open-file (out (sb-ext:parse-native-namestring *home*) :direction :output :if-does-not-exist :create))
      (with-error (code) (run "edit" '("a.txt") :old "a" :new "b")
        (expect code :to-equal "environment.io"))
      (expect (text "a.txt") :to-equal (format nil "a~%"))))

  (it "reports environment.io when a target cannot be read"
    (with-workspace ()
      (put "locked.txt" (format nil "a~%") :mode #o000)
      (unwind-protect
           (if (zerop (sb-posix:getuid))
               (expect (sb-posix:getuid) :to-be 0) ; root reads mode 000 files; nothing to observe
               (with-error (code) (run "edit" '("locked.txt") :old "a" :new "b")
                 (expect code :to-equal "environment.io")))
        (sb-posix:chmod (disk "locked.txt") #o644)))))

(describe "aitools write pipeline: archive refusals reach the caller as input errors"
  (it "reports an unsupported zip as input.unsupported-format and a corrupt one as input.syntax-error"
    (with-workspace ()
      (let* ((zip (zip-of (member-file "a.txt" "A")))
             (multi-disk (copy-seq zip))
             (corrupt (copy-seq zip))
             (eocd (search (octet-vector #x50 #x4B 5 6) zip :from-end t)))
        (setf (aref multi-disk (+ eocd 4)) 1)
        (setf (aref corrupt (+ eocd 16)) #xFF)
        (put "multi.zip" multi-disk)
        (put "corrupt.zip" corrupt)
        (let ((before (snapshot)))
          (with-error (code) (run "archive.extract" '("multi.zip") :to "out")
            (expect code :to-equal "input.unsupported-format"))
          (with-error (code) (run "archive.extract" '("corrupt.zip") :to "out")
            (expect code :to-equal "input.syntax-error"))
          (expect-unchanged before))))))

(describe "aitools --stdin failures"
  (it-each (("too large" :too-large "refusal.too-large")
            ("unreadable" :failure "environment.io"))
      "refuses standard input that is ~A"
      (name outcome code)
    (declare (ignore name))
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (with-stdin-port ((lambda (limit &key on-octets on-too-large on-failure)
                          (declare (ignore on-octets))
                          (expect limit :to-be aitools.edit.application:+max-input-bytes+)
                          (if (eq outcome :too-large) (funcall on-too-large) (funcall on-failure "read failed"))))
        (with-error (actual) (run "insert" '("a.txt") :at "end" :stdin t)
          (expect actual :to-equal code)))
      (expect (text "a.txt") :to-equal (format nil "a~%"))))

  (it-each (("text that is not UTF-8" "insert" ("a.txt") (:at "end" :stdin t) "input.not-utf8")
            ("a diff that is not UTF-8" "apply" () (:stdin t) "input.not-utf8")
            ("JSON input that is not JSON" "json.merge" ("c.json") (:stdin t) "input.syntax-error"))
      "refuses --stdin ~A"
      (name command positionals options code)
    (declare (ignore name))
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (put "c.json" "{}")
      (setf *stdin* (if (string= command "json.merge") (bytes "{nope") (octet-vector 97 #xFF)))
      (let ((before (snapshot)))
        (with-error (actual) (apply #'run command positionals options)
          (expect actual :to-equal code))
        (expect-unchanged before)))))
