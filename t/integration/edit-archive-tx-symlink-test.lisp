(in-package #:aitools.edit.test)

(defun %archive-symlink-bytes (name target)
  (aitools.text.domain:write-tar
   (list (aitools.text.domain:make-archive-member
          :name name :kind :symlink :link-target target))))

(defun %put-archive-symlink (path name target)
  (put path (%archive-symlink-bytes name target)))

(defun %archive-file-bytes (name)
  (aitools.text.domain:write-tar (list (member-file name "payload"))))

(defun %depth-32000-entry-name ()
  (format nil "~{~A~}x" (loop repeat 32000 collect "a/")))

(defun %depth-32000-probe-source ()
  "A child process keeps the pre-fix heap exhaustion from killing the runner."
  "(write-line \"LOAD_BEGIN\") (finish-output)
(require :asdf)
(asdf:load-system \"aitools/feature/edit\")
(write-line \"LOAD_DONE\") (finish-output)
(let ((octets
        (with-open-file (input \"depth.tar.gz\" :element-type '(unsigned-byte 8))
          (let ((bytes (make-array (file-length input) :element-type '(unsigned-byte 8))))
            (read-sequence bytes input)
            bytes))))
  (write-line \"PLAN_BEGIN\") (finish-output)
  (handler-case
      (progn
        (aitools.edit.domain:plan-archive-extraction
         octets :tar-gz \"dest\" :max-bytes 131072 :max-entries 100000
         :lookup-kind (constantly :absent)
         :make-prefix-checker (constantly nil))
        (write-line \"RESULT=accepted\"))
    (aitools.edit.domain:edit-refusal (condition)
      (format t \"RESULT=~A~%\" (aitools.edit.domain:edit-refusal-code condition)))))
")

(defun %run-depth-32000-probe ()
  (let* ((started (get-internal-real-time))
         (deadline (* 45 internal-time-units-per-second))
         (program
           (sb-ext:run-program
            sb-ext:*runtime-pathname*
            (list "--dynamic-space-size" "512" "--noinform" "--no-sysinit"
                  "--no-userinit" "--disable-debugger" "--script"
                  (disk "depth-probe.lisp"))
            :directory *root*
            :output (disk "depth-probe.stdout")
            :error (disk "depth-probe.stderr")
            :wait nil)))
    (loop while (and (sb-ext:process-alive-p program)
                     (< (- (get-internal-real-time) started) deadline))
          do (sleep 1/20))
    (let ((timed-out (sb-ext:process-alive-p program)))
      (when timed-out
        (sb-ext:process-kill program 9))
      (sb-ext:process-wait program)
      (values (sb-ext:process-exit-code program)
              timed-out
              (uiop:read-file-string (disk "depth-probe.stdout"))
              (uiop:read-file-string (disk "depth-probe.stderr"))))))

(describe "archive extraction symlinks in transaction views"
  #+darwin
  (it-each (("T1 APFS case alias" "E" "." "e/..")
            ("T3 APFS normalization alias" "é" "." "é/.."))
      "refuses a transaction symlink alias for ~A before staging the archive link"
      (case-name staged-name staged-target archive-target)
    (call-with-edit-workspace
     (lambda ()
       (let ((tx (begin-tx)))
         (expect (run-in tx "link" (list staged-target staged-name)) :to-be :ok)
         (%put-archive-symlink "alias.tar" "s" archive-target)
         (let ((before (snapshot)))
           (multiple-value-bind (kind code)
               (run-in tx "archive.extract" '("alias.tar") :to ".")
             (expect kind :to-be :error)
             (expect code :to-equal "refusal.outside-workspace"))
           (expect-unchanged before)
           (expect (commit-tx tx) :to-be :committed)
           (expect (kind staged-name) :to-be :symlink)
           (expect (kind "s") :to-be :absent))))))

  (it "preserves the exact-name staged-symlink refusal control T2"
    (call-with-edit-workspace
     (lambda ()
       (let ((tx (begin-tx)))
         (expect (run-in tx "link" '("." "e")) :to-be :ok)
         (%put-archive-symlink "exact.tar" "s" "e/..")
         (multiple-value-bind (kind code)
             (run-in tx "archive.extract" '("exact.tar") :to ".")
           (expect kind :to-be :error)
           (expect code :to-equal "refusal.outside-workspace"))
         (expect (commit-tx tx) :to-be :committed)
         (expect (kind "e") :to-be :symlink)
         (expect (kind "s") :to-be :absent)))))
  (it "treats a staged deletion as visible to commit symlink recheck"
    (call-with-edit-workspace
     (lambda ()
       (%put-archive-symlink "initial.tar" "current" ".")
       (expect (nth-value 0 (run "archive.extract" '("initial.tar") :to "."))
               :to-be :ok)
       (%put-archive-symlink "later.tar" "s" "current/..")
       (let ((tx (begin-tx)))
         (expect (run-in tx "rm" '("current")) :to-be :ok)
         (expect (run-in tx "archive.extract" '("later.tar") :to ".")
                 :to-be :ok)
         (let ((before (snapshot)))
           (expect (commit-tx tx) :to-equal "refusal.outside-workspace")
           (expect-unchanged before)
           (expect (kind "current") :to-be :symlink)
           (expect (kind "s") :to-be :absent))))))

  (it "preserves the exact-name staged-parent refusal control T7"
    (call-with-edit-workspace
     (lambda ()
       (put "sub/existing" "kept")
       (let ((tx (begin-tx)))
         (expect (run-in tx "link" '("sub" "S")) :to-be :ok)
         (put "parent.tar" (%archive-file-bytes "S/PWNED"))
         (multiple-value-bind (kind code)
             (run-in tx "archive.extract" '("parent.tar") :to ".")
           (expect kind :to-be :error)
           (expect code :to-equal "refusal.outside-workspace"))
         (expect (commit-tx tx) :to-be :committed)
         (expect (kind "sub/PWNED") :to-be :absent)
         (expect (kind "S/PWNED") :to-be :absent)))))

  #+darwin
  (it "rechecks disk APFS aliases without a transaction control T9"
    (call-with-edit-workspace
     (lambda ()
       (%put-archive-symlink "first.tar" "E" ".")
       (expect (nth-value 0 (run "archive.extract" '("first.tar") :to ".")) :to-be :ok)
       (%put-archive-symlink "second.tar" "s" "e/..")
       (let ((before (snapshot)))
         (multiple-value-bind (kind code)
             (run "archive.extract" '("second.tar") :to ".")
           (expect kind :to-be :error)
           (expect code :to-equal "refusal.outside-workspace"))
         (expect-unchanged before)
         (expect (kind "s") :to-be :absent)))))

  #+darwin
  (it "rechecks staged archive symlinks at commit against later transaction entries"
    (call-with-edit-workspace
     (lambda ()
       (%put-archive-symlink "safe-at-plan.tar" "s" "e/..")
       (let ((tx (begin-tx))
             (before (snapshot)))
         (expect (run-in tx "archive.extract" '("safe-at-plan.tar") :to ".") :to-be :ok)
         (expect (run-in tx "link" '("." "E")) :to-be :ok)
         (expect (commit-tx tx) :to-equal "refusal.outside-workspace")
         (expect-unchanged before)
         (expect (kind "E") :to-be :absent)
         (expect (kind "s") :to-be :absent)))))

  #+darwin
  (it "refuses the second archive's APFS alias in one transaction T8"
    (call-with-edit-workspace
     (lambda ()
       (%put-archive-symlink "first.tar" "E" ".")
       (%put-archive-symlink "second.tar" "s" "e/..")
       (let ((tx (begin-tx)))
         (expect (run-in tx "archive.extract" '("first.tar") :to ".") :to-be :ok)
         (let ((before (snapshot)))
           (multiple-value-bind (kind code)
               (run-in tx "archive.extract" '("second.tar") :to ".")
             (expect kind :to-be :error)
             (expect code :to-equal "refusal.outside-workspace"))
           (expect-unchanged before)
           (expect (commit-tx tx) :to-be :committed)
           (expect (kind "E") :to-be :symlink)
           (expect (kind "s") :to-be :absent))))))

  #+linux
  (it "folds archive symlink prefixes when resolving on Linux"
    (call-with-edit-workspace
     (lambda ()
       (put "folded.tar"
            (aitools.text.domain:write-tar
             (list (aitools.text.domain:make-archive-member
                    :name "X/D" :kind :symlink :link-target "..")
                   (aitools.text.domain:make-archive-member
                    :name "s" :kind :symlink :link-target "x/d/.."))))
       (let ((before (snapshot)))
         (multiple-value-bind (kind code)
             (run "archive.extract" '("folded.tar") :to ".")
           (expect kind :to-be :error)
           (expect code :to-equal "refusal.outside-workspace"))
         (expect-unchanged before)))))

  (it "bounds a depth-32000 tar.gz probe in a child process"
    (call-with-edit-workspace
     (lambda ()
       (let* ((name (%depth-32000-entry-name))
              (tar (aitools.text.domain:write-tar (list (member-file name "p"))))
              (archive (aitools.text.domain:gzip-compress tar)))
         (expect (count #\/ name) :to-be 32000)
         (expect (length tar) :to-be 71680)
         (expect (plusp (length archive)) :to-be t)
         (put "depth.tar.gz" archive)
         (with-open-file (script (disk "depth-probe.lisp")
                                 :direction :output :if-exists :supersede)
           (write-string (%depth-32000-probe-source) script))
         (multiple-value-bind (status timed-out stdout stderr)
             (%run-depth-32000-probe)
           (unless (and (eql status 0) (not timed-out)
                        (search "RESULT=refusal.too-large" stdout))
             (format *error-output*
                     "depth-32000 probe: exit=~S timed-out=~S stdout=~S stderr=~S~%"
                     status timed-out stdout stderr))
           (expect status :to-be 0)
           (expect timed-out :to-be nil)
           (expect (not (null (search "RESULT=refusal.too-large" stdout))) :to-be t)
         (when (search "RESULT=refusal.too-large" stdout)
           (let ((before (snapshot)))
             (multiple-value-bind (kind code)
                 (run "archive.extract" '("depth.tar.gz") :to "dest")
               (expect kind :to-be :error)
               (expect code :to-equal "refusal.too-large"))
             (expect-unchanged before)))))))))
