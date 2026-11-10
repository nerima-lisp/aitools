(in-package #:aitools.process.test)

(describe "Linux pidfd syscall wrappers"
  (it "opens and closes a handle for the current process"
    (multiple-value-bind (fd errno)
        (aitools.process.infrastructure::%linux-pidfd-open (sb-posix:getpid))
      (unwind-protect
           (progn
             (expect fd :to-be-truthy)
             (expect errno :to-be nil))
        (when fd (sb-posix:close fd)))))
  (it "returns errno values when opening or signalling an invalid target"
    (multiple-value-bind (fd errno)
        (aitools.process.infrastructure::%linux-pidfd-open 2147483647)
      (expect fd :to-be nil)
      (expect (member errno (list sb-posix:esrch sb-posix:enosys))
              :to-be-truthy))
    (multiple-value-bind (sent errno)
        (aitools.process.infrastructure::%linux-pidfd-send-signal -1 15)
      (expect sent :to-be nil)
      (expect (member errno (list sb-posix:ebadf sb-posix:enosys))
              :to-be-truthy))))

(defun make-pidfd-test-boundaries (pidfd-open pidfd-send-signal)
  (let* ((uid (sb-posix:getuid))
         (target
           (aitools.process.domain:make-process-identity
            :pid 2101 :ppid 100 :pgid 2101 :uid uid :ruid uid
            :start 1 :command-line "sleep pidfd fixture"))
         (boundaries
           (make-test-process-boundaries
            :process-list-pids (lambda () (list 2101))
            :process-process-info (lambda (pid) (when (= pid 2101) target))
            :process-safe-target-p (lambda (candidate)
                                     (declare (ignore candidate)) t)
            :process-pidfd-open pidfd-open
            :process-pidfd-send-signal pidfd-send-signal)))
    (values
     (cl-boundary-kit:boundary-context-with
      boundaries :process-signal-process
      (aitools.process.infrastructure::%make-signal-process-boundary boundaries))
     target)))

(describe "Linux pidfd signal boundary"
  (it "maps injected pidfd_open ENOSYS to the existing refusal"
    (let ((opened nil)
          (sent nil))
      (multiple-value-bind (boundaries target)
          (make-pidfd-test-boundaries
           (lambda (pid)
             (setf opened pid)
             (values nil sb-posix:enosys))
           (lambda (fd signal)
             (setf sent (list fd signal))
             (values t nil)))
        (multiple-value-bind (kind fields)
            (signal-flow boundaries
                         :pid (aitools.process.domain:process-identity-pid target)
                         :expect-command "sleep")
          (expect kind :to-be :error)
          (expect (field fields :code) :to-equal "refusal.target-changed")
          (expect opened :to-be 2101)
          (expect sent :to-be nil)))))
  (it "maps ESRCH and other open errors to their established results"
    (dolist (case (list (list sb-posix:esrch "refusal.target-changed")
                        (list sb-posix:eacces "environment.io")))
      (destructuring-bind (errno expected-code) case
        (multiple-value-bind (boundaries target)
            (make-pidfd-test-boundaries
             (lambda (pid)
               (declare (ignore pid))
               (values nil errno))
             (lambda (fd signal)
               (declare (ignore fd signal))
               (values t nil)))
          (multiple-value-bind (kind fields)
              (signal-flow boundaries
                           :pid (aitools.process.domain:process-identity-pid target)
                           :expect-command "sleep")
            (expect kind :to-be :error)
            (expect (field fields :code) :to-equal expected-code))))))
  (it "maps pidfd_send_signal ENOSYS to refusal and closes the handle"
    (let ((fd (sb-posix:open "/dev/null" sb-posix:o-rdonly))
          (sent nil))
      (unwind-protect
           (multiple-value-bind (boundaries target)
               (make-pidfd-test-boundaries
                (lambda (pid)
                  (declare (ignore pid))
                  (values fd nil))
                (lambda (opened-fd signal)
                  (setf sent (list opened-fd signal))
                  (values nil sb-posix:enosys)))
             (cl-weave:with-mocked-functions
                 (((symbol-function
                    (quote aitools.process.infrastructure::%safe-target-p))
                   (lambda (expected)
                     (declare (ignore expected))
                     t)))
               (multiple-value-bind (kind fields)
                   (signal-flow boundaries
                                :pid (aitools.process.domain:process-identity-pid target)
                                :expect-command "sleep")
                 (expect kind :to-be :error)
                 (expect (field fields :code) :to-equal "refusal.target-changed")
                 (expect sent :to-equal (list fd 15))
                 (expect (handler-case
                             (progn (sb-posix:fstat fd) t)
                           (sb-posix:syscall-error () nil))
                         :to-be nil))))
        (ignore-errors (sb-posix:close fd))))))
