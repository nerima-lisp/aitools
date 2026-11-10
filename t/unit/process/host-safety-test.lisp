(in-package #:aitools.process.test)
#+linux
(describe "aitools Linux argv truncation"
  (it "rejects a terminated prefix with an unterminated final argument"
    (let ((bytes
            (make-array 3 :element-type (quote (unsigned-byte 8))
                        :initial-contents (list 65 0 66))))
      (expect (aitools.process.infrastructure::%linux-argv bytes)
              :to-be nil))))
  (defun process-test-safe-target (expected actual &key ancestor)
    (cl-weave:with-mocked-functions
        (((symbol-function
           (quote aitools.process.infrastructure::%safe-process-info))
          (lambda (pid)
            (when (= pid (aitools.process.domain:process-identity-pid expected))
              actual)))
         ((symbol-function
           (quote aitools.process.infrastructure::%ancestor-p))
          (lambda (pid)
            (declare (ignore pid))
            ancestor)))
      (aitools.process.infrastructure::%safe-target-p expected)))
  (describe "aitools process safe signal target validation"
    (it "allows an unchanged same-user process outside the caller tree"
      (let* ((uid (sb-posix:getuid))
             (current-pid (sb-posix:getpid))
             (current-pgid (sb-posix:getpgid 0))
             (identity (aitools.process.domain:make-process-identity
                        :pid 2147483646 :ppid 111 :pgid (1+ current-pgid)
                        :uid uid :ruid uid :start "start"
                        :command-line "tool --serve"))
             (self (aitools.process.domain:make-process-identity
                    :pid current-pid :ppid 111 :pgid (1+ current-pgid)
                    :uid uid :ruid uid :start "start"
                    :command-line "tool --serve"))
             (group (aitools.process.domain:make-process-identity
                     :pid 2147483646 :ppid 111 :pgid current-pgid
                     :uid uid :ruid uid :start "start"
                     :command-line "tool --serve")))
        (expect (process-test-safe-target identity identity) :to-be t)
        (expect (process-test-safe-target self self) :to-be nil)
        (expect (process-test-safe-target group group) :to-be nil)
        (expect (process-test-safe-target identity identity :ancestor t)
                :to-be nil)))
    (it "short-circuits invalid PIDs without process inspection"
      (let ((calls 0))
        (cl-weave:with-mocked-functions
            (((symbol-function
               (quote aitools.process.infrastructure::%safe-process-info))
              (lambda (pid)
                (declare (ignore pid))
                (incf calls)
                nil)))
          (expect (aitools.process.infrastructure::%safe-target-p
                   (aitools.process.domain:make-process-identity :pid 1))
                  :to-be nil)
          (expect (aitools.process.infrastructure::%safe-target-p
                   (aitools.process.domain:make-process-identity :pid nil))
                  :to-be nil)
          (expect calls :to-be 0))))
    (it "fails closed when the observed identity differs"
      (let* ((uid (sb-posix:getuid))
             (pgid (1+ (sb-posix:getpgid 0)))
             (expected (aitools.process.domain:make-process-identity
                        :pid 2147483646 :ppid 111 :pgid pgid :uid uid :ruid uid
                        :start "old" :command-line "tool --serve"))
             (actual (aitools.process.domain:make-process-identity
                      :pid 2147483646 :ppid 111 :pgid pgid :uid uid :ruid uid
                      :start "new" :command-line "tool --serve")))
        (expect (process-test-safe-target expected actual) :to-be nil)))
    (it "rejects an otherwise matching target with a different effective uid"
      (let* ((uid (sb-posix:getuid))
             (identity (aitools.process.domain:make-process-identity
                        :pid 2147483646 :ppid 111 :pgid (1+ (sb-posix:getpgid 0))
                        :uid (1+ uid) :ruid uid :start "start"
                        :command-line "tool --serve")))
        (expect (process-test-safe-target identity identity) :to-be nil)))
    (it "rejects an otherwise matching target with a different real uid"
      (let* ((uid (sb-posix:getuid))
             (identity (aitools.process.domain:make-process-identity
                        :pid 2147483646 :ppid 111 :pgid (1+ (sb-posix:getpgid 0))
                        :uid uid :ruid (1+ uid) :start "start"
                        :command-line "tool --serve")))
        (expect (process-test-safe-target identity identity) :to-be nil))))
  (describe "aitools process ancestor inspection"
    (it "fails closed when parent inspection is unavailable"
      (let ((parent (sb-posix:getppid)))
        (cl-weave:with-mocked-functions
            (((symbol-function
               (quote aitools.process.infrastructure::%safe-process-info))
              (lambda (pid)
                (declare (ignore pid))
                nil)))
          (expect (aitools.process.infrastructure::%ancestor-p (1+ parent))
                  :to-be t))))
    (it "fails closed when parent inspection cycles"
      (let* ((pid (sb-posix:getppid))
             (parent (aitools.process.domain:make-process-identity
                      :pid pid :ppid pid :pgid pid :uid (sb-posix:getuid)
                      :ruid (sb-posix:getuid) :start "start"
                      :command-line "parent")))
        (cl-weave:with-mocked-functions
            (((symbol-function
               (quote aitools.process.infrastructure::%safe-process-info))
              (lambda (observed-pid)
                (when (= observed-pid pid) parent))))
          (expect (aitools.process.infrastructure::%ancestor-p (1+ pid))
                  :to-be t)))))

