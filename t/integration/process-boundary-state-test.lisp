(in-package #:aitools.process.integration-test)

;;;;
;;;; These tests exercise production PROCESS-PORTS adapters on the real
;;;; filesystem, signals, and sockets, including failures surfaced as
;;;; PROCESS-PORT-ERROR. Package setup is in
;;;; t/support/process-test-support.lisp; directory helpers are in
;;;; t/support/workspace.lisp. `invoke` and `call-with-listener` are in
;;;; t/integration/process-test.lisp; shared port helpers are in
;;;; t/integration/process-ports-test.lisp.

(defun call-with-mode (path mode function)
  "Call FUNCTION with PATH's permission bits set to MODE, restoring 0700 (a
directory) or 0600 after, so the temporary tree can always be deleted."
  (let ((native (uiop:native-namestring path)))
    (sb-posix:chmod native mode)
    (unwind-protect (funcall function)
      (sb-posix:chmod native (if (uiop:directory-pathname-p path) #o700 #o600)))))

(describe-skip-if (zerop (sb-posix:geteuid))
    "aitools process production ports: permission failures (integration; skipped as root)"
  (it "reports an unreadable bg directory as a port error instead of listing nothing"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (let ((bg (merge-pathnames "bg/" directory)))
        (ensure-directories-exist bg)
        (write-file (merge-pathnames "bg-1.json" bg) "{}")
        (call-with-mode bg #o300 (lambda ()
          (expect (starts-with-p (format nil "listing ~A failed: " bg)
                                 (boundary-error-message
                                  (lambda ()
                                    (call-capability (production-boundaries) :process-list-directory bg))))
                  :to-be t)))))
    :prefix "aitools-process"))

  (it "fails bg status with environment.io when the bg directory cannot be listed"
    (aitools.test.support:call-with-temporary-directory (lambda (state)
      (let ((bg (merge-pathnames "bg/" state)))
        (ensure-directories-exist bg)
        (call-with-mode bg #o300 (lambda ()
          (multiple-value-bind (code envelope) (invoke (list "bg" "status") :state-directory state)
            (expect code :to-be 1)
            (expect (value envelope "error" "code") :to-equal "environment.io")
            (expect (starts-with-p "listing " (value envelope "error" "message")) :to-be t))))))
    :prefix "aitools-process"))

  (it "reports port errors when it cannot inspect a log or remove a file"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (let ((ports (production-boundaries))
            (log (write-file (merge-pathnames "bg-1.log" directory) "x"))
            (locked (merge-pathnames "locked/" directory)))
        (call-with-mode log 0 (lambda ()
          (expect (starts-with-p (format nil "inspecting ~A failed: " log)
                                 (boundary-error-message
                                  (lambda () (call-capability ports :process-file-size log))))
                  :to-be t)))
        (ensure-directories-exist locked)
        (let ((inside (write-file (merge-pathnames "bg-1.exit" locked) "0")))
          (call-with-mode locked #o500 (lambda ()
          (expect (starts-with-p (format nil "removing ~A failed: " inside)
                                   (boundary-error-message
                                    (lambda ()
                                      (call-capability ports :process-remove-file inside))))
                    :to-be t)))
          (expect (uiop:read-file-string inside) :to-equal "0"))))
    :prefix "aitools-process")))

(describe "aitools process production ports: state directory (integration)"
  (it "has no bg or temporary directory without a state directory"
    (let ((ports (production-boundaries)))
      (expect (call-capability ports :process-bg-directory) :to-be nil)
      (expect (call-capability ports :process-temporary-directory) :to-be nil)))

  (it "creates bg/ on first use and names the mktemp area without a trailing slash"
    (aitools.test.support:call-with-temporary-directory (lambda (state)
      (let* ((ports (production-boundaries state))
             (bg (call-capability ports :process-bg-directory)))
        (expect (namestring bg) :to-equal (namestring (merge-pathnames "bg/" state)))
        (expect (uiop:directory-exists-p bg) :not :to-be nil)
        (expect (call-capability ports :process-temporary-directory)
                :to-equal (concatenate 'string (uiop:native-namestring state) "tmp"))))
    :prefix "aitools-process"))

  (it "reports a state directory that is a regular file as a port error"
    (aitools.test.support:call-with-temporary-directory (lambda (directory)
      (let ((state (write-file (merge-pathnames "state" directory) "")))
        (expect (starts-with-p (format nil "creating ~A failed: " (merge-pathnames "state/bg/" directory))
                               (boundary-error-message
                                (lambda ()
                                  (call-capability (production-boundaries (uiop:ensure-directory-pathname state)) :process-bg-directory))))
                :to-be t)))
    :prefix "aitools-process")))

(describe "aitools process production ports: sockets and clock (integration)"
  (it "treats a socket that cannot be created as not connectable"
    (expect (aitools.process.infrastructure::%connectable-p
             (lambda () (error 'sb-bsd-sockets:socket-error :errno 24 :syscall "socket"))
             #(127 0 0 1) 9)
            :to-be nil))

  (it "reports a port nothing listens on as not connectable"
    (let ((port (call-with-listener #'identity)))
      (expect (call-capability (production-boundaries) :process-tcp-connectable-p port)
              :to-be nil)))

  (it "measures at least the requested duration with the monotonic clock"
    (let* ((ports (production-boundaries))
           (before (call-capability ports :process-monotonic-ms)))
      (call-capability ports :process-sleep-ms 30)
      (expect (>= (- (call-capability ports :process-monotonic-ms) before) 30)
              :to-be t))))
