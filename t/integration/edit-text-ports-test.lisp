;;;; t/integration/edit-text-ports-test.lisp
;;;;
;;;; The --stdin and file-input production ports, the boundary rechecked
;;;; under the store lock, and the remaining write-guard cases (secret redaction
;;;; markers and the workspace boundary).
(in-package #:aitools.edit.test)

(defun call-with-file-octets (octets function)
  "Call FUNCTION with the native path of a fresh file holding OCTETS."
  (let ((path (disk "port-input.bin")))
    (put "port-input.bin" octets)
    (funcall function path)))

(defun call-with-standard-input-from (path function)
  "Call FUNCTION with file descriptor 0 reading PATH, restoring the real
standard input afterwards: the production adapter reads descriptor 0 itself."
  (let ((fd (sb-posix:open path sb-posix:o-rdonly))
        (saved (sb-posix:dup 0)))
    (unwind-protect
         (progn (sb-posix:dup2 fd 0) (funcall function))
      (sb-posix:dup2 saved 0)
      (sb-posix:close saved)
      (sb-posix:close fd))))

(defun read-stdin-outcome (limit)
  (aitools.edit.infrastructure:read-stdin-octets
   limit
   :on-octets (lambda (octets) (list :octets octets))
   :on-too-large (lambda () (list :too-large))
   :on-failure (lambda (message) (list :failure (stringp message)))))

(describe "aitools edit production ports"
  (it-each (("an empty stream" 0 10 :ok)
            ("a stream exactly at the limit" 5 5 :ok)
            ("a stream one byte over the limit" 6 5 :too-large)
            ("a stream spanning read chunks under the limit" 70000 70000 :ok)
            ("a stream spanning read chunks over the limit" 70000 69999 :too-large))
      "reads ~A with a bounded read"
      (name size limit outcome)
    (declare (ignore name))
    (with-workspace ()
      (let ((octets (make-array size :element-type '(unsigned-byte 8) :initial-element 65)))
        (call-with-file-octets
         octets
         (lambda (path)
           (with-open-file (in (sb-ext:parse-native-namestring path) :element-type '(unsigned-byte 8))
             (multiple-value-bind (read too-large) (aitools.edit.infrastructure::%read-bounded in limit)
               (if (eq outcome :ok)
                   (progn (expect too-large :to-be nil)
                          (expect read :to-equalp octets)
                          (expect (typep read '(simple-array (unsigned-byte 8) (*))) :to-be t))
                   (progn (expect too-large :to-be t)
                          (expect read :to-be nil))))))))))

  (it "reads descriptor 0 as raw bytes, refusing more than the limit"
    (with-workspace ()
      (call-with-file-octets
       (octet-vector 0 255 13 10)
       (lambda (path)
         (call-with-standard-input-from path (lambda () (expect (read-stdin-outcome 10) :to-equalp (list :octets (octet-vector 0 255 13 10)))))
         (call-with-standard-input-from path (lambda () (expect (read-stdin-outcome 3) :to-equal '(:too-large))))))))

  (it "reports a descriptor 0 that cannot be read as a failure"
    (with-workspace ()
      (sb-posix:mkdir (disk "dir") #o755)
      (call-with-standard-input-from (disk "dir") (lambda () (expect (read-stdin-outcome 10) :to-equal '(:failure t))))))

  (it "builds ports over the production adapters without doing I/O"
    (let ((ports (aitools.edit.infrastructure:make-production-edit-ports :workspace-host :host :open-store #'identity
                                                                         :text-source :source :ignored t)))
      (expect (aitools.edit.application::edit-ports-read-stdin-octets ports)
              :to-be #'aitools.edit.infrastructure:read-stdin-octets)
      (expect (aitools.edit.application::edit-ports-unix-now ports) :to-be #'aitools.edit.infrastructure:unix-now)
      (expect (aitools.edit.application::edit-ports-workspace-host ports) :to-be :host)
      (expect (aitools.edit.application::edit-ports-text-source ports) :to-be :source))
    (let ((now (aitools.edit.infrastructure:unix-now)))
      (expect (<= 0 (- (aitools.kernel.domain:universal-time-to-unix-seconds (get-universal-time)) now) 1)
              :to-be t)))

  (it "builds write-only ports with no stdin, text source or clock"
    (let ((ports (aitools.edit.application:make-write-edit-ports :workspace-host :host :open-store #'identity)))
      (expect (list (aitools.edit.application::edit-ports-read-stdin-octets ports)
                    (aitools.edit.application::edit-ports-text-source ports)
                    (aitools.edit.application::edit-ports-unix-now ports))
              :to-equal '(nil nil nil)))))

(defun call-with-fault-at-lock (function thunk)
  "Call THUNK with FUNCTION run once when a write takes the workspace lock
(the store's test-only fault hook), as a concurrent process would act."
  (let ((fired nil))
    (let ((aitools.store.application:*fault-hook*
            (lambda (point &rest details)
              (declare (ignore details))
              (when (and (eq point :after-lock) (not fired))
                (setf fired t)
                (funcall function)))))
      (funcall thunk))))

(describe "aitools write pipeline: the boundary rechecked under the lock"
  (it "refuses a write whose target a symlink swap moved to another workspace path"
    (with-workspace ()
      (put "a/x.txt" (format nil "a~%"))
      (put "b/x.txt" (format nil "b~%"))
      (sb-posix:symlink "a" (disk "link"))
      (call-with-fault-at-lock
       (lambda () (sb-posix:unlink (disk "link")) (sb-posix:symlink "b" (disk "link")))
       (lambda ()
         (with-error (code message) (run "edit" '("link/x.txt") :old "a" :new "z")
           (expect code :to-equal "refusal.target-changed")
           (expect message :to-equal "a target path now resolves elsewhere"))))
      (expect (text "a/x.txt") :to-equal (format nil "a~%"))
      (expect (text "b/x.txt") :to-equal (format nil "b~%"))))

  (it "refuses a write whose target a symlink swap moved outside the workspace"
    (with-workspace ()
      (put "a/x.txt" (format nil "a~%"))
      (sb-posix:symlink "a" (disk "link"))
      (call-with-fault-at-lock
       (lambda () (sb-posix:unlink (disk "link")) (sb-posix:symlink "/" (disk "link")))
       (lambda ()
         (with-error (code) (run "edit" '("link/x.txt") :old "a" :new "z")
           (expect code :to-equal "refusal.outside-workspace"))))
      (expect (text "a/x.txt") :to-equal (format nil "a~%"))))

  (it "reports a damaged store record met outside a tx as environment.io with a tx status repair"
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (call-with-fault-at-lock
       (lambda () (error 'aitools.store.domain:store-format-error :detail "injected"))
       (lambda ()
         (with-error (code message keys) (run "edit" '("a.txt") :old "a" :new "z")
           (expect code :to-equal "environment.io")
           (expect (repair-commands keys) :to-equal '("aitools tx status")))))
      (expect (text "a.txt") :to-equal (format nil "a~%")))))

(describe "aitools write guards: remaining boundary and marker cases"
  (it "keeps a file that already holds the redaction marker editable"
    (with-workspace ()
      (put "a.txt" (format nil "[REDACTED_SECRET]~%x~%"))
      (expect (run "edit" '("a.txt") :old "x" :new "y") :to-be :ok)
      (expect (text "a.txt") :to-equal (format nil "[REDACTED_SECRET]~%y~%"))))

  (it "refuses an empty --expect-count"
    (with-workspace ()
      (put "a.txt" (format nil "a~%"))
      (with-error (code) (run "replace" '("a" "b" "a.txt") :expect-count "")
        (expect code :to-equal "argument.invalid"))))

  (it "refuses to move or delete a .git entry the target names itself"
    (with-workspace ()
      (put "a.txt" "a")
      (sb-posix:mkdir (disk ".git") #o755)
      (with-error (code message) (run "move" '("a.txt" ".git"))
        (expect code :to-equal "refusal.outside-workspace")
        (expect (search "inside .git" message) :to-be-truthy))
      (with-error (code) (run "delete" '(".git")) (expect code :to-equal "refusal.outside-workspace"))
      (expect (kind ".git") :to-be :directory)))

  (it "deletes and moves mktemp entries, which are valid write targets"
    (with-workspace ()
      (let* ((file (mktemp-path))
             (directory (mktemp-path :dir t))
             (inner (concatenate 'string directory "/x")))
        (expect (run "write" (list inner) :content '("x")) :to-be :ok)
        (expect (run "move" (list inner (concatenate 'string directory "/y"))) :to-be :ok)
        (expect (probe-file inner) :to-be nil)
        (expect (run "delete" (list (concatenate 'string directory "/y"))) :to-be :ok)
        (expect (run "delete" (list file)) :to-be :ok)
        (expect (probe-file file) :to-be nil))))

  (it "names a symlink loop as the reason a target is refused"
    (with-workspace ()
      (sb-posix:symlink "l2" (disk "l1"))
      (sb-posix:symlink "l1" (disk "l2"))
      (with-error (code message) (run "write" '("l1/x") :content '("x"))
        (expect code :to-equal "refusal.outside-workspace")
        (expect (search "a symlink loop" message) :to-be-truthy))))

  (it "refuses a link pointing into the mktemp area"
    (with-workspace ()
      (let ((temporary (mktemp-path)))
        (with-error (code message) (run "link" (list temporary "l"))
          (expect code :to-equal "refusal.outside-workspace")
          (expect (search "is in the mktemp area" message) :to-be-truthy))
        (expect (kind "l") :to-be :absent))))

  (it "refuses a bad --root when another context drives the pipeline directly with a file as root"
    (with-workspace ()
      (put "file" "x")
      (let ((result nil))
        (aitools.edit.application:run-write-command/k
         *ports*
         (aitools.edit.application:make-write-plan
          :command "write" :targets (list (aitools.edit.application:make-write-target "a.txt"))
          :plan (lambda (context commit reject) (declare (ignore context reject)) (funcall commit '())))
         :root (disk "file")
         :on-ok (lambda (fields) (setf result fields))
         :on-error (lambda (code message &rest keys) (declare (ignore keys)) (setf result (list code message))))
        (expect (first result) :to-equal "argument.invalid")
        (expect (search "is not a directory" (second result)) :to-be-truthy)))))
