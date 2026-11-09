(in-package #:aitools.process.test)

(progn
  (describe "aitools process host signal guards"
    (it "refuses an unavailable target before issuing a signal"
      (let ((called nil)
            (target (aitools.process.domain:make-process-identity
                     :pid 2147483647 :ppid 1 :pgid 2147483647
                     :uid 0 :ruid 0 :start "missing" :command-line "missing")))
        (cl-weave:with-mocked-functions
            (((symbol-function (quote sb-posix:kill))
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (setf called t))))
          (signals aitools.process.application::process-target-changed
            (aitools.process.infrastructure::%signal-process target 15)))
        (expect called :to-be nil)))
    (it "rejects invalid group ids before probing or signalling"
      (expect (aitools.process.infrastructure::%group-alive-p 1) :to-be nil)
      (expect (aitools.process.infrastructure::%signal-group 1 15) :to-be nil)
      (expect (aitools.process.infrastructure::%signal-group 0 15) :to-be nil))
    (it "recognizes the direct parent as an ancestor"
      (expect (aitools.process.infrastructure::%ancestor-p (sb-posix:getppid))
              :to-be t)))

  (describe "aitools process temporary name suffix"
    (it "uses sixteen hexadecimal digits when random bytes are available"
      (let ((suffix (aitools.process.infrastructure::%random-temp-suffix)))
        (expect (length suffix) :to-be 16)
        (expect (every (lambda (character)
                         (or (char<= #\0 character #\9)
                             (char<= #\A character #\F)))
                       suffix)
                :to-be t)))
    (it "falls back to pid and clock when the random source read fails"
      (let ((suffix nil))
        (cl-weave:with-mocked-functions
            (((symbol-function
               (quote aitools.process.infrastructure::%read-input-sequence))
              (lambda (sequence stream)
                (declare (ignore sequence stream))
                (error (quote file-error) :pathname #p"/dev/urandom"))))
          (setf suffix (aitools.process.infrastructure::%random-temp-suffix)))
        (expect (search "-" suffix) :not :to-equal nil))))

  #+darwin
  (progn
    (defun process-darwin-test-bytes (&rest octets)
      (make-array (length octets)
                  :element-type (quote (unsigned-byte 8))
                  :initial-contents octets))
    (defun process-darwin-test-put-le (bytes offset width value)
      (loop for index below width
            do (setf (aref bytes (+ offset index))
                     (ldb (byte 8 (* index 8)) value)))
      bytes)
    (describe "aitools process Darwin argv decoding"
      (it "skips executable and zero padding before joining argv"
        (let ((bytes (process-darwin-test-bytes
                      2 0 0 0 47 98 105 110 47 115 104 0 0 0
                      116 111 111 108 0 97 114 103 0)))
          (multiple-value-bind (argc offset)
              (aitools.process.infrastructure::%darwin-argv-layout bytes)
            (expect argc :to-be 2)
            (expect offset :to-be 14))
          (expect (aitools.process.infrastructure::%darwin-parse-argv bytes)
                  :to-equal "tool arg")))
      (it "rejects short headers, missing executable terminators, and invalid argc"
        (expect (aitools.process.infrastructure::%darwin-argv-layout
                 (make-array 5 :element-type (quote (unsigned-byte 8))
                               :initial-element 0))
                :to-be nil)
        (expect (aitools.process.infrastructure::%darwin-argv-layout
                 (process-darwin-test-bytes 1 0 0 0 47 120))
                :to-be nil)
        (expect (aitools.process.infrastructure::%darwin-argv-layout
                 (process-darwin-test-bytes 0 0 0 0 47 120 0))
                :to-be nil)
        (expect (aitools.process.infrastructure::%darwin-argv-layout
                 (process-darwin-test-bytes 0 16 0 0 47 120 0))
                :to-be nil))
      (it "rejects argv whose declared arguments do not all terminate"
        (expect (aitools.process.infrastructure::%darwin-parse-argv
                 (process-darwin-test-bytes
                  2 0 0 0 47 120 0 116 111 111 108))
                :to-be nil))
      (it "bounds the sysctl result size before reading argv bytes"
        (let ((size 5)
              (reads 0))
          (cl-weave:with-mocked-functions
              (((symbol-function
                 (quote aitools.process.infrastructure::%darwin-query-argv-size))
                (lambda (pid) (declare (ignore pid)) size))
               ((symbol-function
                 (quote aitools.process.infrastructure::%darwin-query-argv-bytes))
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (incf reads))))
            (expect (aitools.process.infrastructure::%darwin-argv 55) :to-be nil)
            (setf size 1048577)
            (expect (aitools.process.infrastructure::%darwin-argv 55) :to-be nil)
            (expect reads :to-be 0))))
      (it "reads and decodes argv only after an accepted size"
        (let* ((bytes (process-darwin-test-bytes
                       2 0 0 0 47 120 0 0 116 111 111 108 0 97 114 103 0))
               (observed nil))
          (cl-weave:with-mocked-functions
              (((symbol-function
                 (quote aitools.process.infrastructure::%darwin-query-argv-size))
                (lambda (pid) (declare (ignore pid)) (length bytes)))
               ((symbol-function
                 (quote aitools.process.infrastructure::%darwin-query-argv-bytes))
                (lambda (pid size)
                  (setf observed (list pid size))
                  bytes)))
            (expect (aitools.process.infrastructure::%darwin-argv 55)
                    :to-equal "tool arg")
            (expect observed :to-equal (list 55 (length bytes))))))
      (it "decodes identity fields and keeps other users argv private"
        (let ((bytes (make-array 136 :element-type (quote (unsigned-byte 8))
                                     :initial-element 0))
              (argv-pid nil))
          (process-darwin-test-put-le bytes 12 4 4321)
          (process-darwin-test-put-le bytes 16 4 123)
          (process-darwin-test-put-le bytes 20 4 (sb-posix:getuid))
          (process-darwin-test-put-le bytes 28 4 (sb-posix:getuid))
          (process-darwin-test-put-le bytes 100 4 4321)
          (process-darwin-test-put-le bytes 120 8 9)
          (process-darwin-test-put-le bytes 128 8 42)
          (cl-weave:with-mocked-functions
              (((symbol-function
                 (quote aitools.process.infrastructure::%darwin-command-line))
                (lambda (uid ruid pid)
                  (declare (ignore uid ruid))
                  (setf argv-pid pid)
                  "tool arg")))
            (let ((identity
                    (aitools.process.infrastructure::%darwin-process-identity 4321 bytes)))
              (expect (aitools.process.domain:process-identity-pid identity) :to-be 4321)
              (expect (aitools.process.domain:process-identity-ppid identity) :to-be 123)
              (expect (aitools.process.domain:process-identity-pgid identity) :to-be 4321)
              (expect (aitools.process.domain:process-identity-start identity)
                      :to-equal "9.000042")
              (expect (aitools.process.domain:process-identity-command-line identity)
                      :to-equal "tool arg")
              (expect argv-pid :to-be 4321)))
          (cl-weave:with-mocked-functions
              (((symbol-function
                 (quote aitools.process.infrastructure::%darwin-argv))
                (lambda (pid)
                  (setf argv-pid pid)
                  "private command")))
            (expect (aitools.process.infrastructure::%darwin-command-line
                     (1+ (sb-posix:getuid)) (sb-posix:getuid) 987)
                    :to-equal "")
            (expect argv-pid :to-be 4321)
            (expect (aitools.process.infrastructure::%darwin-command-line
                     (sb-posix:getuid) (sb-posix:getuid) 987)
                    :to-equal "private command")
            (expect argv-pid :to-be 987)))))))

(describe "aitools process signal syscall outcomes"
  (it "treats an ESRCH after the safety check as a vanished process"
    (let* ((pid 2147483647)
           (original-kill (symbol-function (quote sb-posix:kill)))
           (target (aitools.process.domain:make-process-identity
                    :pid pid :ppid 1 :pgid pid :uid 0 :ruid 0
                    :start "vanished" :command-line "vanished")))
      (expect (aitools.process.infrastructure::%safe-process-info pid)
              :to-be nil)
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.process.infrastructure::%safe-target-p))
            (lambda (expected) (declare (ignore expected)) t))
           ((symbol-function (quote sb-posix:kill))
            (lambda (target-pid signal)
              (declare (ignore signal))
              (funcall original-kill target-pid 0))))
        (expect (aitools.process.infrastructure::%signal-process target 15)
                :to-be nil))))
  (it "reports non-ESRCH syscall failures as port errors"
    (let* ((pid (sb-posix:getpid))
           (target (aitools.process.domain:make-process-identity
                    :pid pid :ppid 1 :pgid pid :uid 0 :ruid 0
                    :start "current" :command-line "current")))
      (cl-weave:with-mocked-functions
          (((symbol-function
             (quote aitools.process.infrastructure::%safe-target-p))
            (lambda (expected) (declare (ignore expected)) t)))
        (signals aitools.process.application:process-port-error
          (aitools.process.infrastructure::%signal-process target 999))))))
