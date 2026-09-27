;;;; t/integration/process-ports-test.lisp
;;;;
;;;; The production PROCESS-PORTS adapters exercised one port at a time on the
;;;; real filesystem, signals, and sockets, including the failure arms the
;;;; flows only see as PROCESS-PORT-ERROR. The package and the temporary
;;;; directory helpers are in process-test.lisp; the run-program and
;;;; launcher ports are in process-ports-launch-test.lisp, which uses the
;;;; helpers defined here.
(in-package #:aitools.process.integration-test)

(defun production-ports (&optional state-directory)
  (aitools.process.infrastructure:make-production-process-ports
   :state-directory-function (and state-directory (lambda () state-directory))))

(defun call-port (ports accessor &rest arguments)
  "Apply the port ACCESSOR (a PROCESS-PORTS slot reader) reads from PORTS."
  (apply (funcall accessor ports) arguments))

(defun port-error-message (function)
  "The printed PROCESS-PORT-ERROR FUNCTION signals, or :NO-ERROR."
  (handler-case (progn (funcall function) :no-error)
    (aitools.process.application:process-port-error (condition)
      (princ-to-string condition))))

(defun starts-with-p (prefix text)
  (and (stringp text) (eql 0 (search prefix text))))

(defun write-file (path text)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (write-string text out))
  path)

(defun file-mode (path)
  (logand #o777 (sb-posix:stat-mode (sb-posix:lstat (uiop:native-namestring path)))))

(defun make-executable (path text)
  (write-file path text)
  (sb-posix:chmod (uiop:native-namestring path) #o755)
  path)

(defun call-with-environment (bindings function)
  "Call FUNCTION with each (NAME . VALUE) of BINDINGS set in the process
environment (VALUE NIL unsets NAME), restoring the previous values after."
  (let ((saved (mapcar (lambda (binding) (cons (car binding) (sb-posix:getenv (car binding)))) bindings)))
    (flet ((apply-bindings (pairs)
             (dolist (pair pairs)
               (if (cdr pair)
                   (sb-posix:setenv (car pair) (cdr pair) 1)
                   (sb-posix:unsetenv (car pair))))))
      (unwind-protect (progn (apply-bindings bindings) (funcall function))
        (apply-bindings saved)))))

(defmacro with-environment ((&rest bindings) &body body)
  `(call-with-environment (list ,@(mapcar (lambda (binding) `(cons ,(first binding) ,(second binding))) bindings))
                          (lambda () ,@body)))

;;; ---------------------------------------------------------------- files

(describe "aitools process production ports: files (integration)"
  (it "creates a private file exclusively and refuses an existing file or a symlink"
    (with-temporary-directory (directory)
      (let ((ports (production-ports))
            (path (merge-pathnames "bg-1.log" directory))
            (link (merge-pathnames "bg-2.log" directory))
            (target (merge-pathnames "target" directory)))
        (expect (call-port ports 'aitools.process.application::process-ports-create-file-exclusive path "one")
                :to-be t)
        (expect (uiop:read-file-string path) :to-equal "one")
        (expect (file-mode path) :to-be #o600)
        (expect (call-port ports 'aitools.process.application::process-ports-create-file-exclusive path "two")
                :to-be nil)
        (expect (uiop:read-file-string path) :to-equal "one")
        (sb-posix:symlink (uiop:native-namestring target) (uiop:native-namestring link))
        (expect (call-port ports 'aitools.process.application::process-ports-create-file-exclusive link "x")
                :to-be nil)
        (expect (probe-file target) :to-be nil))))

  (it "reports a failed exclusive create other than an existing path as a port error"
    (with-temporary-directory (directory)
      (let ((path (merge-pathnames "missing/bg-1.log" directory)))
        (expect (starts-with-p (format nil "creating ~A failed: " path)
                               (port-error-message
                                (lambda ()
                                  (call-port (production-ports)
                                             'aitools.process.application::process-ports-create-file-exclusive
                                             path ""))))
                :to-be t))))

  (it "replaces a record atomically, private, and leaves no temporary file behind"
    (with-temporary-directory (directory)
      (let ((ports (production-ports))
            (path (merge-pathnames "bg-1.json" directory)))
        (write-file path "old")
        (call-port ports 'aitools.process.application::process-ports-replace-file path "new")
        (expect (uiop:read-file-string path) :to-equal "new")
        (expect (file-mode path) :to-be #o600)
        (expect (call-port ports 'aitools.process.application::process-ports-list-directory directory)
                :to-equal '("bg-1.json")))))

  (it "retries past a planted temporary name, never writing through it, and gives up after its attempts"
    (with-temporary-directory (directory)
      (let* ((ports (production-ports))
             (path (merge-pathnames "bg-1.json" directory))
             (victim (merge-pathnames "victim" directory))
             (planted (merge-pathnames ".bg-1.tmp-PLANTED.json" directory))
             (original (fdefinition 'aitools.process.infrastructure::%random-temp-suffix))
             (suffixes '()))
        (sb-posix:symlink (uiop:native-namestring victim) (uiop:native-namestring planted))
        (unwind-protect
             (progn
               ;; The first two names collide with the planted symlink.
               (setf suffixes (list "PLANTED" "PLANTED" "FRESH")
                     (fdefinition 'aitools.process.infrastructure::%random-temp-suffix)
                     (lambda () (or (pop suffixes) "FRESH")))
               (call-port ports 'aitools.process.application::process-ports-replace-file path "v1")
               (expect (uiop:read-file-string path) :to-equal "v1")
               (expect suffixes :to-equal '())
               (setf (fdefinition 'aitools.process.infrastructure::%random-temp-suffix)
                     (lambda () "PLANTED"))
               (expect (starts-with-p (format nil "writing ~A failed: " path)
                                      (port-error-message
                                       (lambda ()
                                         (call-port ports 'aitools.process.application::process-ports-replace-file
                                                    path "v2"))))
                       :to-be t))
          (setf (fdefinition 'aitools.process.infrastructure::%random-temp-suffix) original))
        (expect (uiop:read-file-string path) :to-equal "v1")
        (expect (probe-file victim) :to-be nil))))

  (it "reports a replace into a missing directory as a port error"
    (with-temporary-directory (directory)
      (let ((path (merge-pathnames "missing/bg-1.json" directory)))
        (expect (starts-with-p (format nil "writing ~A failed: " path)
                               (port-error-message
                                (lambda ()
                                  (call-port (production-ports)
                                             'aitools.process.application::process-ports-replace-file path "x"))))
                :to-be t))))

  (it "removes a file and ignores one that is already gone"
    (with-temporary-directory (directory)
      (let ((ports (production-ports))
            (path (write-file (merge-pathnames "bg-1.log" directory) "x")))
        (call-port ports 'aitools.process.application::process-ports-remove-file path)
        (expect (probe-file path) :to-be nil)
        (expect (port-error-message
                 (lambda () (call-port ports 'aitools.process.application::process-ports-remove-file path)))
                :to-be :no-error))))

  (it "lists only file names, leaving subdirectories out"
    (with-temporary-directory (directory)
      (write-file (merge-pathnames "bg-1.json" directory) "{}")
      (write-file (merge-pathnames "bg-1.log" directory) "")
      (ensure-directories-exist (merge-pathnames "bg-2.json/" directory))
      (expect (sort (call-port (production-ports) 'aitools.process.application::process-ports-list-directory
                               directory)
                    #'string<)
              :to-equal '("bg-1.json" "bg-1.log"))))

  (it "reads byte ranges, sizes, and text with replacement, and NIL for a missing file"
    (with-temporary-directory (directory)
      (let ((ports (production-ports))
            (path (merge-pathnames "bg-1.log" directory))
            (missing (merge-pathnames "bg-9.log" directory)))
        (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
          (write-sequence (coerce '(97 98 99 255 100) '(vector (unsigned-byte 8))) out))
        (expect (coerce (call-port ports 'aitools.process.application::process-ports-read-file-octets path 1 3) 'list)
                :to-equal '(98 99))
        (expect (coerce (call-port ports 'aitools.process.application::process-ports-read-file-octets path 3 99) 'list)
                :to-equal '(255 100))
        (expect (call-port ports 'aitools.process.application::process-ports-file-size path) :to-be 5)
        (expect (call-port ports 'aitools.process.application::process-ports-read-file-text path)
                :to-equal (coerce (list #\a #\b #\c (code-char #xfffd) #\d) 'string))
        (expect (call-port ports 'aitools.process.application::process-ports-file-size missing) :to-be nil)
        (expect (call-port ports 'aitools.process.application::process-ports-read-file-text missing) :to-be nil)
        (expect (starts-with-p (format nil "reading ~A failed: " missing)
                               (port-error-message
                                (lambda ()
                                  (call-port ports 'aitools.process.application::process-ports-read-file-octets
                                             missing 0 1))))
                :to-be t)))))

(defun call-with-mode (path mode function)
  "Call FUNCTION with PATH's permission bits set to MODE, restoring 0700 (a
directory) or 0600 after, so the temporary tree can always be deleted."
  (let ((native (uiop:native-namestring path)))
    (sb-posix:chmod native mode)
    (unwind-protect (funcall function)
      (sb-posix:chmod native (if (uiop:directory-pathname-p path) #o700 #o600)))))

(defmacro with-mode ((path mode) &body body)
  `(call-with-mode ,path ,mode (lambda () ,@body)))

;; Root bypasses permission bits, so these refusals cannot be produced as root.
(describe-skip-if (zerop (sb-posix:geteuid))
    "aitools process production ports: permission failures (integration; skipped as root)"
  (it "reports an unreadable bg directory as a port error instead of listing nothing"
    (with-temporary-directory (directory)
      (let ((bg (merge-pathnames "bg/" directory)))
        (ensure-directories-exist bg)
        (write-file (merge-pathnames "bg-1.json" bg) "{}")
        (with-mode (bg #o300)
          (expect (starts-with-p (format nil "listing ~A failed: " bg)
                                 (port-error-message
                                  (lambda ()
                                    (call-port (production-ports)
                                               'aitools.process.application::process-ports-list-directory bg))))
                  :to-be t)))))

  (it "fails bg status with environment.io when the bg directory cannot be listed"
    (with-temporary-directory (state)
      (let ((bg (merge-pathnames "bg/" state)))
        (ensure-directories-exist bg)
        (with-mode (bg #o300)
          (multiple-value-bind (code envelope) (invoke (list "bg" "status") :state-directory state)
            (expect code :to-be 1)
            (expect (value envelope "error" "code") :to-equal "environment.io")
            (expect (starts-with-p "listing " (value envelope "error" "message")) :to-be t))))))

  (it "reports an unreadable log's size and a refused removal as port errors"
    (with-temporary-directory (directory)
      (let ((ports (production-ports))
            (log (write-file (merge-pathnames "bg-1.log" directory) "x"))
            (locked (merge-pathnames "locked/" directory)))
        (with-mode (log 0)
          (expect (starts-with-p (format nil "inspecting ~A failed: " log)
                                 (port-error-message
                                  (lambda () (call-port ports 'aitools.process.application::process-ports-file-size log))))
                  :to-be t))
        (ensure-directories-exist locked)
        (let ((inside (write-file (merge-pathnames "bg-1.exit" locked) "0")))
          (with-mode (locked #o500)
            (expect (starts-with-p (format nil "removing ~A failed: " inside)
                                   (port-error-message
                                    (lambda ()
                                      (call-port ports 'aitools.process.application::process-ports-remove-file inside))))
                    :to-be t))
          (expect (uiop:read-file-string inside) :to-equal "0"))))))

;;; ------------------------------------------------------ state directory

(describe "aitools process production ports: state directory (integration)"
  (it "has no bg or temporary directory without a state directory"
    (let ((ports (production-ports)))
      (expect (call-port ports 'aitools.process.application::process-ports-bg-directory) :to-be nil)
      (expect (call-port ports 'aitools.process.application::process-ports-temporary-directory) :to-be nil)))

  (it "creates bg/ on first use and names the mktemp area without a trailing slash"
    (with-temporary-directory (state)
      (let* ((ports (production-ports state))
             (bg (call-port ports 'aitools.process.application::process-ports-bg-directory)))
        (expect (namestring bg) :to-equal (namestring (merge-pathnames "bg/" state)))
        (expect (uiop:directory-exists-p bg) :not :to-be nil)
        (expect (call-port ports 'aitools.process.application::process-ports-temporary-directory)
                :to-equal (concatenate 'string (uiop:native-namestring state) "tmp")))))

  (it "reports a state directory that is a regular file as a port error"
    (with-temporary-directory (directory)
      (let ((state (write-file (merge-pathnames "state" directory) "")))
        (expect (starts-with-p (format nil "creating ~A failed: " (merge-pathnames "state/bg/" directory))
                               (port-error-message
                                (lambda ()
                                  (call-port (production-ports (uiop:ensure-directory-pathname state))
                                             'aitools.process.application::process-ports-bg-directory))))
                :to-be t)))))

;;; ------------------------------------------------------ sockets, clock

(describe "aitools process production ports: sockets and clock (integration)"
  (it "treats a socket that cannot be created as not connectable"
    (expect (aitools.process.infrastructure::%connectable-p
             (lambda () (error 'sb-bsd-sockets:socket-error :errno 24 :syscall "socket"))
             #(127 0 0 1) 9)
            :to-be nil))

  (it "reports a port nothing listens on as not connectable"
    (let ((port (call-with-listener #'identity)))
      (expect (call-port (production-ports) 'aitools.process.application::process-ports-tcp-connectable-p port)
              :to-be nil)))

  (it "sleeps at least the requested milliseconds on a clock that never runs backwards"
    (let* ((ports (production-ports))
           (before (call-port ports 'aitools.process.application::process-ports-monotonic-ms)))
      (call-port ports 'aitools.process.application::process-ports-sleep-ms 30)
      (expect (>= (- (call-port ports 'aitools.process.application::process-ports-monotonic-ms) before) 30)
              :to-be t))))
