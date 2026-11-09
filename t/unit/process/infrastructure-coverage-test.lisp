(in-package #:aitools.process.test)

(describe "aitools process infrastructure file boundary"
  (it "reads octets and maps a missing input file to a port error"
    (aitools.test.support:call-with-temporary-directory
     (lambda (directory)
       (let ((path (merge-pathnames "input.bin" directory))
             (missing (merge-pathnames "missing.bin" directory)))
         (with-open-file (stream path :direction :output
                                      :element-type '(unsigned-byte 8))
           (write-sequence #(65 0 255) stream))
         (expect (coerce (aitools.process.infrastructure::%read-octets path 1 9) 'list)
                 :to-equal '(0 255))
         (expect (aitools.process.infrastructure::%open-input-file
                  missing :if-does-not-exist nil)
                 :to-be nil)
         (signals aitools.process.application:process-port-error
           (aitools.process.infrastructure::%read-octets missing 0 1)))))))

(describe "aitools process supervisor tracking"
  (it "remembers and reaps an exited launched supervisor"
    (let ((pid (gensym "SUPERVISOR-"))
          (handle (gensym "HANDLE-"))
          (waited nil))
      (unwind-protect
           (progn
             (cl-weave:with-mocked-functions
                 (((symbol-function 'process-kit:process-id)
                   (lambda (process) (declare (ignore process)) pid))
                  ((symbol-function 'process-kit:process-try-wait)
                   (lambda (process)
                     (setf waited (eq process handle)))))
               (aitools.process.infrastructure::%remember-supervisor handle)
               (expect (gethash pid aitools.process.infrastructure::*launched-supervisors*)
                       :to-be handle)
               (aitools.process.infrastructure::%reap-launched-supervisor pid)
               (expect waited :to-be t)
               (expect (gethash pid aitools.process.infrastructure::*launched-supervisors*)
                       :to-be nil)))
        (remhash pid aitools.process.infrastructure::*launched-supervisors*)))))

(describe "aitools process group signal probes"
  (it "reports a live group while its leader remains in the session"
  (let ((pid 812345) (session-probed nil))
    (cl-weave:with-mocked-functions
        (((symbol-function 'aitools.process.infrastructure::%kill-probe-p)
          (lambda (target) (declare (ignore target)) t))
         ((symbol-function 'aitools.process.infrastructure::%session-leader-p)
          (lambda (target)
            (declare (ignore target))
            (setf session-probed t)
            t)))
      (expect (aitools.process.infrastructure::%group-alive-p pid) :to-be t)
      (expect session-probed :to-be t))))
  (it "keeps a live group after the leader probe reports ESRCH"
  (let ((pid 812346) (session-probed nil))
    (cl-weave:with-mocked-functions
        (((symbol-function 'aitools.process.infrastructure::%kill-probe-p)
          (lambda (target) (= target (- pid))))
         ((symbol-function 'aitools.process.infrastructure::%session-leader-p)
          (lambda (target)
            (declare (ignore target))
            (setf session-probed t)
            nil)))
      (expect (aitools.process.infrastructure::%group-alive-p pid) :to-be t)
      (expect session-probed :to-be nil))))
  (it "reports a vanished group when its group probe returns ESRCH"
  (let ((pid 812347) (session-probed nil))
    (cl-weave:with-mocked-functions
        (((symbol-function 'aitools.process.infrastructure::%kill-probe-p)
          (lambda (target) (plusp target)))
         ((symbol-function 'aitools.process.infrastructure::%session-leader-p)
          (lambda (target)
            (declare (ignore target))
            (setf session-probed t)
            t)))
      (expect (aitools.process.infrastructure::%group-alive-p pid) :to-be nil)
      (expect session-probed :to-be nil))))
  (it "handles successful and failed host probes"
  (let ((pid (sb-posix:getpid)))
    (expect (aitools.process.infrastructure::%kill-probe-p pid) :to-be t)
    (expect (aitools.process.infrastructure::%kill-probe-p 2147483647) :to-be nil)
    (expect (aitools.process.infrastructure::%session-leader-p pid)
            :to-be (= (sb-posix:getsid pid) pid))
    (expect (aitools.process.infrastructure::%session-leader-p -1) :to-be t)))
  (it "treats ESRCH as a vanished group and wraps other signal failures"
  (let ((pgid (sb-posix:getpgid 0)))
    (cl-weave:with-mocked-functions
        (((symbol-function 'aitools.process.infrastructure::%group-alive-p)
          (lambda (pid) (declare (ignore pid)) t)))
      (expect (aitools.process.infrastructure::%signal-group 2147483647 15)
              :to-be nil)
      (signals aitools.process.application:process-port-error
        (aitools.process.infrastructure::%signal-group pgid 99999))))))

#+linux
(progn
  (describe "aitools Linux process inspection"
    (it "bounds proc file reads and decodes argv and pid directory names"
      (aitools.test.support:call-with-temporary-directory
       (lambda (directory)
         (let ((path (merge-pathnames "small" directory))
               (large (merge-pathnames "large" directory)))
           (with-open-file (stream path :direction :output :element-type '(unsigned-byte 8))
             (write-sequence #(65 0 255) stream))
           (expect (coerce (aitools.process.infrastructure::%linux-file-bytes path) 'list)
                   :to-equal '(65 0 255))
           (with-open-file (stream large :direction :output :element-type '(unsigned-byte 8))
             (write-sequence (make-array 1048577 :element-type '(unsigned-byte 8)
                                         :initial-element 0)
                             stream))
           (expect (aitools.process.infrastructure::%linux-file-bytes large) :to-be nil)
           (expect (aitools.process.infrastructure::%linux-file-bytes
                    (merge-pathnames "absent" directory))
                   :to-be nil))))
      (expect (aitools.process.infrastructure::%linux-argv
               (make-array 4 :element-type '(unsigned-byte 8)
                           :initial-contents '(65 0 66 0)))
              :to-equal "A B")
      (expect (aitools.process.infrastructure::%linux-argv #()) :to-be nil)
      (expect (aitools.process.infrastructure::%linux-argv #(65 66)) :to-be nil)
      (expect (aitools.process.infrastructure::%linux-pid-name "123") :to-be 123)
      (expect (aitools.process.infrastructure::%linux-pid-name "12x") :to-be nil)
      (expect (aitools.process.infrastructure::%linux-pid-name "") :to-be nil))
    (it "parses proc status uids and stat identity fields"
      (let ((fields (make-list 20 :initial-element "0")))
        (setf (nth 1 fields) "12"
              (nth 2 fields) "34"
              (nth 19 fields) "56")
        (expect (aitools.process.infrastructure::%linux-status-uids
                 (format nil "Name: tool~%Uid: 1000 1001 1002 1003~%"))
                :to-equal '(1000 1001 1002 1003))
        (expect (aitools.process.infrastructure::%linux-status-uids (format nil "Name: tool~%"))
                :to-be nil)
        (cl-weave:with-mocked-functions
            (((symbol-function 'aitools.process.infrastructure::%linux-command-line)
              (lambda (base uid ruid)
                (declare (ignore base uid ruid)) "tool --flag")))
          (let ((identity
                  (aitools.process.infrastructure::%linux-process-identity
                   99 "/proc/99/" '(1000 1001) fields)))
            (expect (aitools.process.domain:process-identity-ppid identity) :to-be 12)
            (expect (aitools.process.domain:process-identity-pgid identity) :to-be 34)
            (expect (aitools.process.domain:process-identity-start identity) :to-be 56)
            (expect (aitools.process.domain:process-identity-command-line identity)
                    :to-equal "tool --flag")
            (expect (aitools.process.infrastructure::%linux-process-identity
                     99 "/proc/99/" '(1000) fields)
                    :to-be nil)
            (expect (aitools.process.infrastructure::%linux-process-identity
                     99 "/proc/99/" '(1000 1000) nil)
                    :to-be nil)))))
    (it "reads argv only for a same-user process"
      (let ((path nil))
        (cl-weave:with-mocked-functions
            (((symbol-function 'sb-posix:getuid) (lambda () 1000))
             ((symbol-function 'aitools.process.infrastructure::%linux-file-bytes)
              (lambda (filename)
                (setf path filename)
                (make-array 9 :element-type '(unsigned-byte 8)
                            :initial-contents '(116 111 111 108 0 97 114 103 0)))))
          (expect (aitools.process.infrastructure::%linux-command-line
                   "/proc/99/" 1000 1000)
                  :to-equal "tool arg")
          (expect path :to-equal "/proc/99/cmdline")
          (expect (aitools.process.infrastructure::%linux-command-line
                   "/proc/99/" 1001 1000)
                  :to-equal "")
          (expect path :to-equal "/proc/99/cmdline")))))

  (describe "aitools Linux safe process target"
    (it "accepts an unchanged same-user target outside the caller group"
      (let* ((uid (sb-posix:getuid))
             (pid (1+ (sb-posix:getpid)))
             (pgid (1+ (sb-posix:getpgid 0)))
             (identity (aitools.process.domain:make-process-identity
                        :pid pid :ppid 1 :pgid pgid :uid uid :ruid uid
                        :start "start" :command-line "tool --serve")))
        (cl-weave:with-mocked-functions
            (((symbol-function 'aitools.process.infrastructure::%safe-process-info)
              (lambda (observed-pid) (declare (ignore observed-pid)) identity))
             ((symbol-function 'aitools.process.infrastructure::%ancestor-p)
              (lambda (observed-pid) (declare (ignore observed-pid)) nil)))
          (expect (aitools.process.infrastructure::%safe-target-p identity) :to-be t))))
    (it "fails closed on a syscall error during ancestry validation"
      (let* ((uid (sb-posix:getuid))
             (identity (aitools.process.domain:make-process-identity
                        :pid (1+ (sb-posix:getpid)) :ppid 1
                        :pgid (1+ (sb-posix:getpgid 0)) :uid uid :ruid uid
                        :start "start" :command-line "tool --serve")))
        (cl-weave:with-mocked-functions
            (((symbol-function 'aitools.process.infrastructure::%safe-process-info)
              (lambda (pid) (declare (ignore pid)) identity))
             ((symbol-function 'aitools.process.infrastructure::%ancestor-p)
              (lambda (pid)
                (declare (ignore pid))
                (sb-posix:getpgid 2147483647))))
          (expect (aitools.process.infrastructure::%safe-target-p identity) :to-be nil)))))
  )

